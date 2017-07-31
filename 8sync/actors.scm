;;; 8sync --- Asynchronous programming for Guile
;;; Copyright © 2016, 2017 Christopher Allan Webber <cwebber@dustycloud.org>
;;;
;;; This file is part of 8sync.
;;;
;;; 8sync is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; 8sync is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with 8sync.  If not, see <http://www.gnu.org/licenses/>.

(define-module (8sync actors)
  #:use-module (oop goops)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 receive)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module (8sync inbox)
  #:use-module (8sync rmeta-slot)
  #:export (;; utilities... ought to go in their own module
            big-random-number
            big-random-number-string

            <actor>
            actor-id
            actor-message-handler

            ;;; Commenting out the <address> type for now;
            ;;; it may be back when we have better serializers
            ;; <address>
            make-address
            address-actor-id address-hive-id

            address->string
            actor-id-actor
            actor-id-hive
            actor-id-string

            actor-init! actor-cleanup!

            actor-alive?

            build-actions

            define-actor

            ;; <hive>
            ;; make-hive
            ;; ;; There are more methods for the hive, but there's
            ;; ;; no reason for the outside world to look at them maybe?
            ;; hive-id
            bootstrap-actor bootstrap-actor*

            create-actor create-actor*
            self-destruct

            <message>
            make-message message?
            message-to message-action message-from
            message-id message-body message-in-reply-to
            message-wants-reply

            <- <-wait

            spawn-hive run-hive))

;; For ids
(set! *random-state* (random-state-from-platform))

;; Same size as a uuid4 I think...
(define random-number-size (expt 2 128))

(define (big-random-number)
  (random random-number-size))

;; Would be great to get this base64 encoded instead.
(define (big-random-number-string)
  ;; @@: This is slow.  Using format here is wasteful.
  (format #f "~x" (big-random-number)))

;; @@: This is slow-ish.  A mere ~275k / second on my (old) machine.
;;   The main cost seems to be in number->string.
(define (simple-message-id-generator)
  ;; Prepending this cookie makes message ids unique per hive
  (let ((prefix (format #f "~x:" (big-random-number)))
        (counter 0))
    (lambda ()
      (set! counter (1+ counter))
      (string-append prefix (number->string counter)))))



;;; Messages
;;; ========

(define-record-type <message>
  (make-message-intern id to from action
                       body in-reply-to wants-reply)
  message?
  ;; @@: message-ids are removed.  They could be re-enabled
  ;;   if we had thread-safe promises...
  (id message-id)                    ; id of this message
  (to message-to)                    ; actor id this is going to
  (from message-from)                ; actor id of sender
  (action message-action)            ; action (a symbol) to be handled
  (body message-body)                ; argument list "body" of message
  (in-reply-to message-in-reply-to)  ; message id this is in reply to, if any
  (wants-reply message-wants-reply)) ; whether caller is waiting for reply


(define* (make-message id to from action body
                       #:key in-reply-to wants-reply)
  (make-message-intern id to from action body
                       in-reply-to wants-reply))

(define (kwarg-list-to-alist args)
  (let loop ((remaining args)
             (result '()))
    (match remaining
      (((? keyword? key) val rest ...)
       (loop rest
             (cons (cons (keyword->symbol key) val) 
                   result)))
      (() result)
      (_ (throw 'invalid-kwarg-list
                "Invalid keyword argument list"
                args)))))


;;; See: https://web.archive.org/web/20081223021934/http://mumble.net/~jar/articles/oo-moon-weinreb.html
;;;   (also worth seeing: http://mumble.net/~jar/articles/oo.html )

;; This is the internal, generalized message sending method.
;; Users shouldn't use it!  Use the <-foo forms instead.

(define-inlinable (%<- wants-reply from-actor to action args message-id in-reply-to)
  ;; Okay, we need to deal with message ids.
  ;; Could we get rid of them? :\
  ;; It seems if we can use eq? and have messages be immutable then
  ;; it should be possible to identify follow-up replies.
  ;; If we need to track replies across hive boundaries we could
  ;; register unique ids across the ambassador barrier.
  (match to
    (#(_ _ (? channel? channel) dead?)
     (let ((message (make-message message-id to
                                  (and from-actor (actor-id from-actor))
                                  action args
                                  #:wants-reply wants-reply
                                  #:in-reply-to in-reply-to)))
       (perform-operation
        (choice-operation
         (put-operation channel message)
         (wait-operation dead?)))))
    ;; TODO: put remote addresses here.
    (#(actor-id hive-id #f #f)
     ;; Here we'd make a call to our hive...
     'TODO)
    ;; A message sent to nobody goes nowhere.
    ;; TODO: Should we display a warning here, probably?
    (#f #f)))

(define (<- to action . args)
  (define from-actor (*current-actor*))
  (%<- #f from-actor to action args
       (or (and from-actor
                ((actor-msg-id-generator from-actor)))
           (big-random-number-string))
       #f))

;; TODO: this should abort to the prompt, then check for errors
;;   when resuming.

(define (<-wait to action . args)
  (define prompt (*actor-prompt*))
  (when (not prompt)
    (error "Tried to <-wait without being in an actor's context..."))

  (let ((reply (abort-to-prompt prompt to action args)))
    (cond ((eq? action '*error*)
           (throw 'hive-unresumable-coroutine
                  "Won't resume coroutine; got an *error* as a reply"
                  #:message reply))
          (else (apply values (message-body reply))))))


;;; Main actor implementation
;;; =========================

(define (actor-inheritable-message-handler actor message)
  (define action (message-action message))
  (define method
    (class-rmeta-ref (class-of actor) 'actions action
                     #:equals? eq? #:cache-set! hashq-set!
                     #:cache-ref hashq-ref))
  (unless method
    (throw 'action-not-found
           "No appropriate action handler found for actor"
           #:action action
           #:actor actor
           #:message message))
  (apply method actor message (message-body message)))

(define-syntax-rule (wrap-apply body)
  "Wrap possibly multi-value function in a procedure, applies all arguments"
  (lambda args
    (apply body args)))

(define-syntax-rule (build-actions (symbol method) ...)
  "Construct an alist of (symbol . method), where the method is wrapped
with wrap-apply to facilitate live hacking and allow the method definition
to come after class definition."
  (build-rmeta-slot
   (list (cons (quote symbol)
               (wrap-apply method)) ...)))

(define-class <actor> ()
  ;; An address object... a vector of #(actor-id hive-id inbox-channel dead?)
  ;;  - inbox-channel is the receiving channel (as opposed to actor-inbox-deq)
  ;;  - dead? is a fibers condition variable which is set once this actor
  ;;    kicks the bucket
  (id #:init-keyword #:address
      #:getter actor-id)
  ;; The connection to the hive we're connected to.
  (hive-channel #:init-keyword #:hive-channel
                #:accessor actor-hive-channel)

  ;; Our queue to send/receive messages on
  (inbox-deq #:init-thunk make-channel
             #:accessor actor-inbox-deq)

  (msg-id-generator #:init-thunk simple-message-id-generator
                    #:getter actor-msg-id-generator)

  ;; How we receive and process new messages
  (message-handler #:init-value actor-inheritable-message-handler
                   ;; @@: There's no reason not to use #:class instead of
                   ;;   #:each-subclass anywhere in this file, except for
                   ;;   Guile bug #25211 (#:class is broken in Guile 2.2)
                   #:allocation #:each-subclass
                   #:getter actor-message-handler)

  ;; valid values are:
  ;;  - #t as in, send the init message, but don't wait (default)
  ;;  - 'wait, as in wait on the init message
  ;;  - #f as in don't bother to init
  (should-init #:init-value #t
               #:getter actor-should-init
               #:allocation #:each-subclass)

  ;; This is the default, "simple" way to inherit and process messages.
  (actions #:init-thunk (build-actions)
           #:allocation #:each-subclass))

;;; Actors may specify an "init" action that occurs before the actor
;;; actually begins to run.
;;; During actor-init!, an actor may send a message to itself or others
;;; via <- but *may not* use <-wait.
(define-method (actor-init! (actor <actor>))
  'no-op)

(define-method (actor-cleanup! (actor <actor>))
  'no-op)

;;; Addresses are vectors where the first part is the actor-id and
;;; the second part is the hive-id.  This works well enough... they
;;; look decent being pretty-printed.

(define (make-address actor-id hive-id channel dead?)
  (vector actor-id hive-id channel dead?))

(define (address-actor-id address)
  (vector-ref address 0))

(define (address-hive-id address)
  (vector-ref address 1))

(define (address-channel address)
  (vector-ref address 2))

(define (address-dead? address)
  (vector-ref address 3))

(define (address->string address)
  (string-append (address-actor-id address) "@"
                 (address-hive-id address)))

(define (address-equal? address1 address2)
  "Check whether or not the two addresses are equal.

This compares the actor-id and hive-id but ignores the channel and
dead? condition."
  (match address1
    (#(actor-id-1 hive-id-1 _ _)
     (match address2
       (#(actor-id-2 hive-id-2)
        (and (equal? actor-id-1 actor-id-2)
             (and (equal? hive-id-1 hive-id-2))))
       (_ #f)))
    (_ #f)))

(define (actor-id-actor actor)
  "Get the actor id component of the actor-id"
  (address-actor-id (actor-id actor)))

(define (actor-id-hive actor)
  "Get the hive id component of the actor-id"
  (address-hive-id (actor-id actor)))

(define (actor-id-string actor)
  "Render the full actor id as a human-readable string"
  (address->string (actor-id actor)))

(define (actor-inbox-enq actor)
  (address-channel (actor-id actor)))

(define *current-actor*
  (make-parameter #f))

(define *actor-prompt*
  (make-parameter #f))

(define (actor-main-loop actor)
  "Main loop of the actor.  Loops around, pulling messages off its queue
and handling them."
  ;; @@: Maybe establish some sort of garbage collection routine for these...
  (define waiting
    (make-hash-table))
  (define message-handler
    (actor-message-handler actor))
  (define dead?
    (address-dead? (actor-id actor)))
  (define prompt (make-prompt-tag (actor-id-actor actor)))

  (define (handle-message message)
    (catch #t
      (lambda ()
        (call-with-values
            (lambda ()
              (message-handler actor message))
          (lambda vals
            ;; Return reply if necessary
            (when (message-wants-reply message)
              (when (message-wants-reply message)
                (%<- #f actor (message-from message) '*reply*
                     vals ((actor-msg-id-generator actor))
                     (message-id message)))))))
      (const #t)
      (let ((err (current-error-port)))
        (lambda (key . args)
          (false-if-exception
           (let ((stack (make-stack #t 4)))
             (format err "Uncaught exception when handling message ~a:\n"
                     message)
             (display-backtrace stack err)
             (print-exception err (stack-ref stack 0)
                              key args)
             (newline err)
             ;; If the other actor is waiting on a reply, let's let them
             ;; know there was an error...
             (when (message-wants-reply message)
               (%<- #f actor (message-from message) '*error*
                    (list key) ((actor-msg-id-generator actor))
                    (message-id message)))))))))
  
  (define (resume-handler message)
    (define in-reply-to (message-in-reply-to message))
    (cond
     ((hash-ref waiting in-reply-to) =>
      (lambda (kont)
        (hash-remove! waiting in-reply-to)
        (kont message)))
     (else
      (format (current-error-port)
              "Tried to resume nonexistant message: ~a\n"
              (message-id message)))))

  (define halt-or-handle-message
    ;; It would be nice if we could give priorities to certain operations.
    ;; halt should always win over getting a message...
    (choice-operation
     (wrap-operation (wait-operation dead?)
                     (const #f))  ; halt and return
     (wrap-operation (get-operation (actor-inbox-deq actor))
                     (lambda (message)
                       (call-with-prompt prompt
                         (lambda ()
                           (if (message-in-reply-to message)
                               ;; resume a continuation which was waiting on a reply
                               (resume-handler message)
                               ;; start handling a new message
                               (handle-message message)))
                         ;; Here's where we abort to if we're doing <-wait
                         ;; @@: maybe use match-lambda if we're going to end up
                         ;;   handling multiple ~commands
                         (lambda (kont to action message-args)
                           (define message-id
                             ((actor-msg-id-generator actor)))
                           (hash-set! waiting message-id kont)
                           (%<- #t actor to action message-args message-id #f)))
                       #t))))   ; loop again

  ;; Mutate the parameter; this should be fine since each fiber
  ;; runs in its own dynamic state with with-dynamic-state.
  ;; See with-dynamic-state discussion in
  ;;   https://wingolog.org/archives/2017/06/27/growing-fibers
  (*current-actor* actor)
  ;; We temporarily set the *actor-prompt* to #f to make sure that
  ;; actor-init! doesn't try to do a <-wait message (and not accidentally use
  ;; the parent fiber's *actor-prompt* either.)
  (*actor-prompt* #f)
  (actor-init! actor)
  (*actor-prompt* prompt)

  (let loop ()
    (and (perform-operation halt-or-handle-message)
         (loop))))


;;; Actor utilities
;;; ===============

(define-syntax-rule (define-actor class inherits
                      (action ...)
                      slots ...)
  (define-class class inherits
    (actions #:init-thunk (build-actions action ...)
             #:allocation #:each-subclass)
    slots ...))


;;; The Hive
;;; ========
;;;   Every actor has a hive, which keeps track of other actors, manages
;;;   cleanup, and performs inter-hive communication.

(define-class <hive> ()
  (id #:init-keyword #:id
      #:getter hive-id)
  (actor-registry #:init-thunk make-hash-table
                  #:getter hive-actor-registry)
  ;; TODO: Rename "ambassadors" to "relays"
  ;; Ambassadors are used (or will be) for inter-hive communication.
  ;; These are special actors that know how to route messages to other
  ;; hives.
  (ambassadors #:init-thunk make-weak-key-hash-table
               #:getter hive-ambassadors)
  (channel #:init-thunk make-channel
           #:getter hive-channel)
  (halt? #:init-thunk make-condition
         #:getter hive-halt?))

(define* (make-hive #:key hive-id)
  (make <hive> #:id (or hive-id
                        (big-random-number-string))))

(define (gen-actor-id cookie)
  (if cookie
      (string-append cookie ":" (big-random-number-string))
      (big-random-number-string)))

(define (hive-main-loop hive)
  "The main loop of the hive.  This listens for messages on the hive-channel
for certain actions to perform.

`messages' here is not the same as a <message> object; these are a list of
values, the first value being a symbol"
  (define channel (hive-channel hive))
  (define halt? (hive-halt? hive))
  (define registry (hive-actor-registry hive))

  ;; not the same as a <message> ;P
  (define handle-message
    (match-lambda
      (('register-actor actor-id address actor)
       (hash-set! registry actor-id (vector address actor)))
      ;; Remove the actor from hive
      (('remove-actor actor-id)
       (hash-remove! (hive-actor-registry hive) actor-id))
      (('register-ambassador hive-id ambassador-actor-id)
       'TODO)
      (('unregister-ambassador hive-id ambassador-actor-id)
       'TODO)
      (('forward-message from-actor-id message)
       'TODO)))

  (define halt-or-handle
    (choice-operation
     (wrap-operation (get-operation channel)
                     (lambda (msg)
                       (handle-message msg)
                       #t))
     (wrap-operation (wait-operation halt?)
                     (const #f))))

  (let lp ()
    (and (perform-operation halt-or-handle)
         (lp))))

(define *current-hive* (make-parameter #f))

(define* (spawn-hive proc #:key (hive (make-hive)))
  "Spawn a hive in a fiber running PROC, passing it the fresh hive"
  (spawn-fiber (lambda () (hive-main-loop hive)))
  (proc hive))

(define (run-hive proc . args)
  "Spawn a hive and run it in run-fibers.  Takes a PROC as would be passed
to spawn-hive... all remaining arguments passed to run-fibers."
  (apply run-fibers
         (lambda ()
           (spawn-hive proc))
         args))

(define (%create-actor hive-channel hive-id
                       actor-class init-args id-cookie send-init?)
  (let* ((actor-id (gen-actor-id id-cookie))
         (dead? (make-condition))
         (inbox-enq (make-channel))
         (address (make-address actor-id hive-id
                                inbox-enq dead?))
         (actor (apply make actor-class
                       #:hive-channel hive-channel
                       #:address address
                       init-args))
         (should-init (actor-should-init actor)))

    ;; start the main loop
    (spawn-fiber (lambda ()
                   ;; start the inbox loop
                   (spawn-fiber
                    (lambda ()
                      (delivery-agent inbox-enq (actor-inbox-deq actor)
                                      dead?))
                    ;; this one is decidedly non-parallel, because we want
                    ;; the delivery agent to be in the same thread as its actor
                    #:parallel? #f)

                   (actor-main-loop actor))
                 #:parallel? #t)

    (put-message hive-channel (list 'register-actor actor-id address actor))
    
    ;; return the address
    address))

(define* (bootstrap-actor hive actor-class #:rest init-args)
  "Create an actor on HIVE using ACTOR-CLASS passing in INIT-ARGS args"
  (%create-actor (hive-channel hive) (hive-id hive) actor-class
                 init-args (symbol->string (class-name actor-class))
                 #f))

(define* (bootstrap-actor* hive actor-class id-cookie #:rest init-args)
  "Create an actor, but also allow customizing a 'cookie' added to the id
for debugging"
  (%create-actor (hive-channel hive) (hive-id hive) actor-class
                 init-args id-cookie
                 #f))

(define* (create-actor from-actor actor-class #:rest init-args)
  "Create an instance of actor-class.  Return the new actor's id.

This is the method actors should call directly (unless they want
to supply an id-cookie, in which case they should use
create-actor*)."
  (%create-actor (actor-hive-channel from-actor) (actor-id-hive from-actor)
                 actor-class init-args #f #t))


(define* (create-actor* from-actor actor-class id-cookie #:rest init-args)
  "Create an instance of actor-class.  Return the new actor's id.

Like create-actor, but permits supplying an id-cookie."
  (%create-actor (actor-hive-channel from-actor) (actor-id-hive from-actor)
                 actor-class init-args id-cookie #t))

(define* (self-destruct actor #:key (cleanup #t))
  "Remove an actor from the hive.

Unless #:cleanup is set to #f, this will first have the actor handle
its '*cleanup* action handler."
  (signal-condition! (address-dead? (actor-id actor)))
  (put-message (actor-hive-channel actor) (list 'remove-actor (actor-id-actor actor)))
  ;; Set *actor-prompt* to nothing to prevent actor-cleanup! from sending
  ;; a message with <-wait
  (*actor-prompt* #f)
  (actor-cleanup! actor))

;; From a patch I sent to Fibers...
(define (condition-signalled? cvar)
  "Return @code{#t} if @var{cvar} has already been signalled.

In general you will want to use @code{wait} or @code{wait-operation} to
wait on a condition.  However, sometimes it is useful to see whether or
not a condition has already been signalled without blocking."
  (atomic-box-ref ((@@ (fibers conditions) condition-signalled?) cvar)))

(define (actor-alive? actor)
  (condition-signalled? (address-dead? (actor-id actor))))

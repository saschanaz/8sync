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
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 atomic)
  #:use-module ((ice-9 ports internal)
                #:select (port-read-wait-fd port-write-wait-fd))
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 suspendable-ports)
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

            *current-actor*

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

            build-actions

            define-actor

            actor-spawn-fiber
            with-actor-nonblocking-ports

            ;; <hive>
            ;; make-hive
            ;; ;; There are more methods for the hive, but there's
            ;; ;; no reason for the outside world to look at them maybe?
            ;; hive-id
            create-actor create-actor*
            self-destruct

            <message>
            make-message message?
            message-to message-action message-from
            message-id message-body message-in-reply-to
            message-wants-reply

            <- <-wait

            spawn-hive run-hive

            ;; Maybe the wrong place for this, or for it to be exported.
            ;; But it's used in websockets' server implementation at least...
            live-wrap))

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

(define (%<- wants-reply from-actor to action args message-id in-reply-to)
  ;; Okay, we need to deal with message ids.
  ;; Could we get rid of them? :\
  ;; It seems if we can use eq? and have messages be immutable then
  ;; it should be possible to identify follow-up replies.
  ;; If we need to track replies across hive boundaries we could
  ;; register unique ids across the ambassador barrier.
  (match to
    (($ <address> _ _ (? channel? channel) dead?)
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
    (($ <address> actor-id hive-id #f #f)
     ;; Here we'd make a call to our hive...
     'TODO)
    ;; A message sent to nobody goes nowhere.
    ;; TODO: Should we display a warning here, probably?
    (#f #f)
    ;; We shouldn't technically be passing in actors but rather their
    ;; addresses, but often actors want to message themselves and
    ;; this makes that slightly easier.
    ((? (lambda (x) (is-a? x <actor>)) actor)
     (%<- wants-reply from-actor (actor-id actor) action
          args message-id in-reply-to))))

(define (<- to action . args)
  (define from-actor (*current-actor*))
  (%<- #f from-actor to action args
       (or (and from-actor
                ((actor-msg-id-generator from-actor)))
           (big-random-number-string))
       #f))

(define (<-wait to action . args)
  (define prompt (*actor-prompt*))
  (when (not prompt)
    (error "Tried to <-wait without being in an actor's context..."))

  (let ((reply (abort-to-prompt prompt '<-wait to action args)))
    (cond ((eq? (message-action reply) '*error*)
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

(define-syntax-rule (live-wrap body)
  "Wrap possibly multi-value function in a procedure, applies all arguments"
  (lambda args
    (apply body args)))

(define-syntax-rule (build-actions (symbol method) ...)
  "Construct an alist of (symbol . method), where the method is wrapped
with `live-wrap' to facilitate live hacking and allow the method definition
to come after class definition."
  (build-rmeta-slot
   (list (cons (quote symbol)
               (live-wrap method)) ...)))

(define-class <actor> ()
  ;; An <address> object
  (id #:init-keyword #:address
      #:getter actor-id)

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

;;; Every actor has an address, which is how its identified.
;;; We also pack in some routing information.
(define-record-type <address>
  (make-address actor-id hive-id channel dead?)
  address?
  ;; Unique-to-this-actor-on-this-hive part
  (actor-id address-actor-id)
  ;; Unique identifier for the hive we're connected to.
  ;; If we don't have a "direct" link to the other actor through
  ;; a channel, we'll have to look up if our hive has a way to route
  ;; to the other hive.
  (hive-id address-hive-id)
  ;; The receiving channel (as opposed to actor-inbox-deq)
  (channel address-channel)
  ;; A fibers condition variable which is set once this actor kicks
  ;; the bucket
  (dead? address-dead?))

(set-record-type-printer!
 <address>
 (lambda (address port)
   (format port "<address ~a ~a>"
           (address->string address)
           (if (address-channel address)
               (string-append
                ":local"
                (if (atomic-box-ref
                     ((@@ (fibers conditions) condition-signalled?)
                      (address-dead? address)))
                    " :dead" ""))
               ":remote"))))

(define (address->string address)
  (string-append (address-actor-id address) "@"
                 (address-hive-id address)))

(define (address-equal? address1 address2)
  "Check whether or not the two addresses are equal.

This compares the actor-id and hive-id but ignores the channel and
dead? condition."
  (and (equal? (address-actor-id address1)
               (address-actor-id address2))
       (equal? (address-hive-id address1)
               (address-hive-id address2))))

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

(define *resume-io-channel*
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
  ;; Not always used, only if with-actor-nonblocking-ports is used
  (define resume-io-channel
    (make-channel))

  (define (handle-message message)
    (catch #t
      (lambda ()
        (call-with-values
            (lambda ()
              (message-handler actor message))
          (lambda vals
            ;; Return reply if necessary
            (when (message-wants-reply message)
              (%<- #f actor (message-from message) '*reply*
                   vals ((actor-msg-id-generator actor))
                   (message-id message))))))
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

  (define (call-with-actor-prompt thunk)
    (call-with-prompt prompt
      thunk
      ;; Here's where we abort to if we're doing <-wait
      ;; @@: maybe use match-lambda if we're going to end up
      ;;   handling multiple ~commands
      (match-lambda*
        ((kont '<-wait to action message-args)
         (define message-id
           ((actor-msg-id-generator actor)))
         (hash-set! waiting message-id kont)
         (%<- #t actor to action message-args message-id #f))
        ((kont 'run-me proc)
         (proc kont)))))

  (define halt-or-handle-message
    ;; It would be nice if we could give priorities to certain operations.
    ;; halt should always win over getting a message...
    (choice-operation
     (wrap-operation (wait-operation dead?)
                     (const #f))  ; halt and return
     (wrap-operation (get-operation (actor-inbox-deq actor))
                     (lambda (message)
                       (call-with-actor-prompt
                        (lambda ()
                          (if (message-in-reply-to message)
                              ;; resume a continuation which was waiting on a reply
                              (resume-handler message)
                              ;; start handling a new message
                              (handle-message message))))
                       #t))   ; loop again
     (wrap-operation (get-operation resume-io-channel)
                     (lambda (thunk)
                       (call-with-actor-prompt
                        (lambda ()
                          (thunk)))
                       #t))))

  ;; Mutate the parameter; this should be fine since each fiber
  ;; runs in its own dynamic state with with-dynamic-state.
  ;; See with-dynamic-state discussion in
  ;;   https://wingolog.org/archives/2017/06/27/growing-fibers
  (*current-actor* actor)
  (*resume-io-channel* resume-io-channel)

  ;; We temporarily set the *actor-prompt* to #f to make sure that
  ;; actor-init! doesn't try to do a <-wait message (and not accidentally use
  ;; the parent fiber's *actor-prompt* either.)
  (*actor-prompt* #f)
  (actor-init! actor)
  (*actor-prompt* prompt)

  (let loop ()
    (and (perform-operation halt-or-handle-message)
         (loop))))


;; @@: So in order for this to work, we're going to have to add
;; another channel to actors, which is resumable i/o.  We'll have to
;; spawn a fiber that wakes up a thunk on the actor when its port is
;; available.  Funky...

(define (%suspend-io-to-actor wait-for-read/write)
  (lambda (port)
    (define prompt (*actor-prompt*))
    (define resume-channel (*resume-io-channel*))
    (define (run-at-prompt k)
      (spawn-fiber
       (lambda ()
         (wait-for-read/write port)
         ;; okay, we're awake again, tell the actor to resume this
         ;; continuation
         (put-message resume-channel k))
       #:parallel? #f))
    (when (not prompt)
      (error "Attempt to abort to actor prompt outside of actor"))
    (abort-to-prompt (*actor-prompt*)
                     'run-me run-at-prompt)))

(define suspend-read-to-actor
  (%suspend-io-to-actor (@@ (fibers) wait-for-readable)))

(define suspend-write-to-actor
  (%suspend-io-to-actor (@@ (fibers) wait-for-writable)))

(define (with-actor-nonblocking-ports thunk)
  "Runs THUNK in dynamic context in which attempting to read/write
from a port that would otherwise block an actor's correspondence with
other actors (note that reading from a nonblocking port should never
block other fibers) will instead permit reading other messages while
I/O is waiting to complete.

Note that currently "
  (parameterize ((current-read-waiter suspend-read-to-actor)
                 (current-write-waiter suspend-write-to-actor))
    (thunk)))

(define (actor-spawn-fiber thunk . args)
  "Spawn a fiber from an actor but unset actor-machinery-specific
dynamic context."
  (apply spawn-fiber
         (lambda ()
           (*current-actor* #f)
           (*resume-io-channel* #f)
           (*actor-prompt* #f)
           (thunk))
         args))



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

;; TODO: Make this a srfi-9 record type
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

(define *hive-id* (make-parameter #f))
(define *hive-channel* (make-parameter #f))

;; @@: Should we halt the hive either at the end of spawn-hive or run-hive?
(define* (spawn-hive proc #:key (hive (make-hive)))
  "Spawn a hive and run PROC, passing it the fresh hive and establishing
a dynamic context surrounding the hive."
  (spawn-fiber (lambda () (hive-main-loop hive)))
  (parameterize ((*hive-id* (hive-id hive))
                 (*hive-channel* (hive-channel hive)))
    (proc hive)))

(define (run-hive proc . args)
  "Spawn a hive and run it in run-fibers.  Takes a PROC as would be passed
to spawn-hive... all remaining arguments passed to run-fibers."
  (apply run-fibers
         (lambda ()
           (spawn-hive proc))
         args))

(define (%create-actor actor-class init-args id-cookie send-init?)
  (let* ((hive-channel (*hive-channel*))
         (hive-id (*hive-id*))
         (actor-id (gen-actor-id id-cookie))
         (dead? (make-condition))
         (inbox-enq (make-channel))
         (address (make-address actor-id hive-id
                                inbox-enq dead?))
         (actor (apply make actor-class
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

(define* (create-actor actor-class #:rest init-args)
  "Create an instance of actor-class.  Return the new actor's id.

This is the method actors should call directly (unless they want
to supply an id-cookie, in which case they should use
create-actor*)."
  (%create-actor actor-class init-args #f #t))


(define* (create-actor* actor-class id-cookie #:rest init-args)
  "Create an instance of actor-class.  Return the new actor's id.

Like create-actor, but permits supplying an id-cookie."
  (%create-actor actor-class init-args id-cookie #t))

(define* (self-destruct actor #:key (cleanup #t))
  "Remove an actor from the hive.

Unless #:cleanup is set to #f, this will first have the actor handle
its '*cleanup* action handler."
  (signal-condition! (address-dead? (actor-id actor)))
  (put-message (*hive-channel*) (list 'remove-actor (actor-id-actor actor)))
  ;; Set *actor-prompt* to nothing to prevent actor-cleanup! from sending
  ;; a message with <-wait
  (*actor-prompt* #f)
  (actor-cleanup! actor))


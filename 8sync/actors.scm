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
  #:use-module (ice-9 pretty-print)
  #:use-module (8sync agenda)
  #:use-module (8sync rmeta-slot)
  #:export (;; utilities... ought to go in their own module
            big-random-number
            big-random-number-string
            simple-message-id-generator

            <actor>
            actor-id
            actor-message-handler

            %current-actor

            ;;; Commenting out the <address> type for now;
            ;;; it may be back when we have better serializers
            ;; <address>
            make-address address?
            address-actor-id address-hive-id

            address->string
            actor-id-actor
            actor-id-hive
            actor-id-string

            actor-alive?

            build-actions

            define-actor

            <hive>
            make-hive
            ;; There are more methods for the hive, but there's
            ;; no reason for the outside world to look at them maybe?
            hive-id
            bootstrap-actor bootstrap-actor*

            create-actor create-actor*
            self-destruct

            <message>
            make-message message?
            message-to message-action message-from
            message-id message-body message-in-reply-to
            message-wants-reply

            message-auto-reply?

            <- <-* <-wait <-wait* <-reply <-reply* <-reply-wait <-reply-wait*

            call-with-message mbody-receive mbody-val

            run-hive
            bootstrap-message

            serialize-message write-message
            serialize-message-pretty pprint-message
            read-message read-message-from-string))

;; For ids
(define %random-state
  (make-parameter (random-state-from-platform)))

;; Same size as a uuid4 I think...
(define random-number-size (expt 2 128))

(define (big-random-number)
  (random random-number-size (%random-state)))

;; Would be great to get this base64 encoded instead.
(define (big-random-number-string)
  ;; @@: This is slow.  Using format here is wasteful.
  (format #f "~x" (big-random-number)))

;; @@: This is slow.  A mere ~275k / second on my (old) machine.
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


;; @@: We may want to add a deferred-reply to the below, similar to
;;   what we had in XUDD, for actors which do their own response
;;   queueing.... ie, that might receive messages but need to shelve
;;   them to be acted upon after something else is taken care of.

(define-record-type <message>
  (make-message-intern id to from action
                       body in-reply-to wants-reply
                       replied)
  message?
  (id message-id)                    ; id of this message
  (to message-to)                    ; actor id this is going to
  (from message-from)                ; actor id of sender
  (action message-action)            ; action (a symbol) to be handled
  (body message-body)                ; argument list "body" of message
  (in-reply-to message-in-reply-to)  ; message id this is in reply to, if any
  (wants-reply message-wants-reply)  ; whether caller is waiting for reply
  (replied message-replied           ; was this message replied to?
           set-message-replied!))


(define* (make-message id to from action body
                       #:key in-reply-to wants-reply
                       replied)
  (make-message-intern id to from action body
                       in-reply-to wants-reply replied))

(define (message-auto-reply? message)
  (eq? (message-action message) '*auto-reply*))

(define (message-needs-reply? message)
  "See if this message needs a reply still"
  (and (message-wants-reply message)
       (not (message-replied message))))


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

;; @@: Could we get rid of some of the conditional checks through
;;   some macro-foo?

(define-inlinable (send-message send-options from-actor to-id action
                                replying-to-message wants-reply?
                                message-body-args)
  (if replying-to-message
      (set-message-replied! replying-to-message #t))
  (let* ((hive (actor-hive from-actor))
         (new-message
          (make-message (hive-gen-message-id hive) to-id
                        (actor-id from-actor) action
                        message-body-args
                        #:wants-reply wants-reply?
                        #:in-reply-to
                        (if replying-to-message
                            (message-id replying-to-message)
                            #f))))
    (if wants-reply?
        (abort-to-prompt (hive-prompt (actor-hive from-actor))
                         from-actor new-message send-options)
        ;; @@: It might be that eventually we pass in send-options
        ;;   here too.  Since <-wait and <-reply-wait are the only ones
        ;;   that use it yet, for now it kind of just makes things
        ;;   confusing.
        (8sync (hive-process-message hive new-message)))))

(define (<- to-id action . message-body-args)
  "Send a message from an actor to another actor"
  (send-message '() (%current-actor) to-id action
                #f #f message-body-args))

(define (<-* send-options to-id action . message-body-args)
  "Like <-*, but allows extra parameters via send-options"
  (define* (really-send #:key (actor (%current-actor))
                        #:allow-other-keys)
    (send-message send-options actor to-id action
                  #f #f message-body-args))
  (apply really-send send-options))

(define (<-wait to-id action . message-body-args)
  "Send a message from an actor to another, but wait until we get a response"
  (wait-maybe-handle-errors
   (send-message '() (%current-actor) to-id action
                 #f #t message-body-args)))

(define (<-wait* send-options to-id action . message-body-args)
  "Like <-wait, but allows extra parameters, for example whether to
#:accept-errors"
  (define* (really-send #:key (actor (%current-actor))
                        #:allow-other-keys)
    (apply wait-maybe-handle-errors
           (send-message send-options actor to-id action
                         #f #t message-body-args)
           send-options))
  (apply really-send send-options))

;; TODO: Intelligently ~propagate(ish) errors on -wait functions.
;;   We might have `send-message-wait-brazen' to allow callers to
;;   not have an exception thrown and instead just have a message with
;;   the appropriate '*error* message returned.

(define (<-reply original-message . message-body-args)
  "Reply to a message"
  (when (message-needs-reply? original-message)
    (send-message '() (%current-actor) (message-from original-message) '*reply*
                  original-message #f message-body-args)))

(define (<-reply* send-options original-message . message-body-args)
  "Like <-reply, but allows extra parameters via send-options"
  (define* (really-send #:key (actor (%current-actor))
                        #:allow-other-keys)
    (send-message send-options actor
                  (message-from original-message) '*reply*
                  original-message #f message-body-args))
  (when (message-needs-reply? original-message)
    (apply really-send send-options)))

(define (<-auto-reply actor original-message)
  "Auto-reply to a message.  Internal use only!"
  (send-message '() actor (message-from original-message) '*auto-reply*
                original-message #f '()))

(define (<-reply-wait original-message . message-body-args)
  "Reply to a messsage, but wait until we get a response"
  (if (message-needs-reply? original-message)
      (wait-maybe-handle-errors
       (send-message '() (%current-actor)
                     (message-from original-message) '*reply*
                     original-message #t message-body-args))
      #f))

(define (<-reply-wait* send-options original-message
                       . message-body-args)
  "Like <-reply-wait, but allows extra parameters via send-options"
  (define* (really-send #:key (actor (%current-actor))
                        #:allow-other-keys)
    (apply wait-maybe-handle-errors
           (send-message send-options actor
                         (message-from original-message) '*reply*
                         original-message #t message-body-args)
           send-options))
  (when (message-needs-reply? original-message)
    (apply really-send send-options)))

(define* (wait-maybe-handle-errors message
                                   #:key accept-errors
                                   #:allow-other-keys)
  "Before returning a message to a waiting caller, see if we need to
raise an exception if an error."
  (define action (message-action message))
  (cond ((and (eq? action '*error*)
              (not accept-errors))
         (throw 'hive-unresumable-coroutine
                "Won't resume coroutine; got an *error* as a reply"
                #:message message))
        (else message)))



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

(define-syntax-rule (build-actions (symbol method) ...)
  "Construct an alist of (symbol . method), where the method is wrapped
with wrap-apply to facilitate live hacking and allow the method definition
to come after class definition."
  (wrap-rmeta-slot
   (list (cons (quote symbol)
               (wrap-apply method)) ...)))

(define-class <actor> ()
  ;; An address object
  (id #:init-keyword #:id
      #:getter actor-id)
  ;; The hive we're connected to.
  ;; We need this to be able to send messages.
  (hive #:init-keyword #:hive
        #:accessor actor-hive)
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
               #:allocation #:each-subclass)

  ;; This is the default, "simple" way to inherit and process messages.
  (actions #:init-thunk (build-actions
                         ;; Default init method is to do nothing.
                         (*init* (const #f))
                         ;; Default cleanup method is to do nothing.
                         (*cleanup* (const #f)))
           #:allocation #:each-subclass))

;;; Addresses are vectors where the first part is the actor-id and
;;; the second part is the hive-id.  This works well enough... they
;;; look decent being pretty-printed.

(define (make-address actor-id hive-id)
  (vector actor-id hive-id))

(define (address-actor-id address)
  (vector-ref address 0))

(define (address-hive-id address)
  (vector-ref address 1))

(define (address->string address)
  (string-append (address-actor-id address) "@"
                 (address-hive-id address)))

(define-method (actor-id-actor (actor <actor>))
  "Get the actor id component of the actor-id"
  (address-actor-id (actor-id actor)))

(define-method (actor-id-hive (actor <actor>))
  "Get the hive id component of the actor-id"
  (address-hive-id (actor-id actor)))

(define-method (actor-id-string (actor <actor>))
  "Render the full actor id as a human-readable string"
  (address->string (actor-id actor)))

(define %current-actor
  (make-parameter #f))

(define (actor-alive? actor)
  (hive-resolve-local-actor (actor-hive actor) (actor-id actor)))



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
;;;   Every actor has a hive.  The hive is a kind of "meta-actor"
;;;   which routes all the rest of the actors in a system.

(define-generic hive-handle-failed-forward)

(define-class <hive> (<actor>)
  (actor-registry #:init-thunk make-hash-table
                  #:getter hive-actor-registry)
  (msg-id-generator #:init-thunk simple-message-id-generator
                    #:getter hive-msg-id-generator)
  ;; Ambassadors are used (or will be) for inter-hive communication.
  ;; These are special actors that know how to route messages to other hives.
  (ambassadors #:init-thunk make-weak-key-hash-table
               #:getter hive-ambassadors)
  ;; Waiting coroutines
  ;; This is a map from cons cell of message-id
  ;;   to a cons cell of (actor-id . coroutine)
  ;; @@: Should we have a <waiting-coroutine> record type?
  ;; @@: Should there be any way to clear out "old" coroutines?
  (waiting-coroutines #:init-thunk make-hash-table
                      #:getter hive-waiting-coroutines)
  ;; Message prompt
  ;; When actors send messages to each other they abort to this prompt
  ;; to send the message, then carry on their way
  (prompt #:init-thunk make-prompt-tag
          #:getter hive-prompt)
  (actions #:allocation #:each-subclass
           #:init-thunk
           (build-actions
            ;; This is in the case of an ambassador failing to forward a
            ;; message... it reports it back to the hive
            (*failed-forward* hive-handle-failed-forward)
            ;; These are called at start and end of run-hive
            (*init-all* hive-handle-init-all)
            (*cleanup-all* hive-handle-cleanup-all))))

(define-method (hive-handle-init-all (hive <hive>) message)
  "Run *init* method on all actors in registry"
  ;; We have to do this hack and run over the list
  ;; twice, because hash-for-each would result in an unrewindable
  ;; continuation, and to avoid the hash-map changing during the
  ;; middle of this.
  (define actor-ids
    (hash-map->list (lambda (actor-id actor) actor-id)
                    (hive-actor-registry hive)))
  (for-each (lambda (actor-id)
              (let* ((actor (hash-ref (hive-actor-registry hive)
                                      actor-id)))
                (match (slot-ref actor 'should-init)
                  (#f #f)
                  ('wait
                   (<-wait actor-id '*init*))
                  (_
                   (<- actor-id '*init*)))))
            actor-ids))

(define-method (hive-handle-failed-forward (hive <hive>) message)
  "Handle an ambassador failing to forward a message"
  'TODO)

(define-method (hive-handle-cleanup-all (hive <hive>) message)
  "Send a message to all actors in our registry to clean themselves up."
  ;; We have to do this hack and run over the list
  ;; twice, because hash-for-each would result in an unrewindable
  ;; continuation, and to avoid the hash-map changing during the
  ;; middle of this.
  (define actor-ids
    (hash-map->list (lambda (actor-id actor) actor-id)
                    (hive-actor-registry hive)))
  (for-each (lambda (actor-id)
              (<- actor-id '*cleanup*))
            actor-ids))

(define* (make-hive #:key hive-id)
  (let ((hive (make <hive>
                #:id (make-address
                      "hive" (or hive-id
                                 (big-random-number-string))))))
    ;; Set the hive's actor reference to itself
    (set! (actor-hive hive) hive)
    ;; Register the actor with itself
    (hive-register-actor! hive hive)
    hive))

(define-method (hive-id (hive <hive>))
  (actor-id-hive hive))

(define-method (hive-gen-actor-id (hive <hive>) cookie)
  (make-address (if cookie
                    (string-append cookie ":" (big-random-number-string))
                    (big-random-number-string))
                (hive-id hive)))

(define-method (hive-gen-message-id (hive <hive>))
  "Generate a message id using HIVE's message id generator"
  ((hive-msg-id-generator hive)))

(define-method (hive-resolve-local-actor (hive <hive>) actor-address)
  (hash-ref (hive-actor-registry hive) actor-address))

(define-method (hive-resolve-ambassador (hive <hive>) ambassador-address)
  (hash-ref (hive-ambassadors hive) ambassador-address))

(define-method (make-forward-request (hive <hive>) (ambassador <actor>) message)
  (make-message (hive-gen-message-id hive) (actor-id ambassador)
                ;; If we make the hive not an actor, we could either switch this
                ;; to #f or to the original actor...?
                ;; Maybe some more thinking should be done on what should
                ;; happen in case of failure to forward?  Handling ambassador failures
                ;; seems like the primary motivation for the hive remaining an actor.
                (actor-id hive)
                '*forward*
                `((original . ,message))))

(define-method (hive-reply-with-error (hive <hive>) original-message
                                      error-key error-args)
  ;; We only supply the error-args if the original sender is on the same hive
  (define (orig-actor-on-same-hive?)
    (equal? (hive-id hive)
            (address-hive-id (message-from original-message))))
  (set-message-replied! original-message #t)
  (let* ((new-message-body
          (if (orig-actor-on-same-hive?)
              `(#:original-message ,original-message
                #:error-key ,error-key
                #:error-args ,error-args)
              `(#:original-message ,original-message
                #:error-key ,error-key)))
         (new-message (make-message (hive-gen-message-id hive)
                                    (message-from original-message)
                                    (actor-id hive) '*error*
                                    new-message-body
                                    #:in-reply-to (message-id original-message))))
    ;; We only return a thunk, rather than run 8sync here, because if
    ;; we ran 8sync in the middle of a catch we'd end up with an
    ;; unresumable continuation.
    (lambda () (hive-process-message hive new-message))))

(define-record-type <waiting-on-reply>
  (make-waiting-on-reply actor-id kont send-options)
  waiting-on-reply?
  (actor-id waiting-on-reply-actor-id)
  (kont waiting-on-reply-kont)
  (send-options waiting-on-reply-send-options))


(define-method (hive-process-message (hive <hive>) message)
  "Handle one message, or forward it via an ambassador"
  (define (maybe-autoreply actor)
    ;; Possibly autoreply
    (if (message-needs-reply? message)
        (<-auto-reply actor message)))

  (define (resolve-actor-to)
    "Get the actor the message was aimed at"
    (let ((actor (hive-resolve-local-actor hive (message-to message))))
      (if (not actor)
          (throw 'actor-not-found
                 (format #f "Message ~a from ~a directed to nonexistant actor ~a"
                         (message-id message)
                         (address->string (message-from message))
                         (address->string (message-to message)))
                 message))
      actor))

  (define (call-catching-coroutine thunk)
    (define queued-error-handling-thunk #f)
    (define (call-catching-errors)
      ;; TODO: maybe parameterize (or attach to hive) and use
      ;;   maybe-catch-all from agenda.scm
      ;; @@: Why not just use with-throw-handler and let the catch
      ;;   happen at the agenda?  That's what we used to do, but
      ;;   it ended up with a SIGABRT.  See:
      ;;     http://lists.gnu.org/archive/html/bug-guile/2016-05/msg00003.html
      (catch #t
        thunk
        ;; In the actor model, we don't totally crash on errors.
        (lambda _ #f)
        ;; If an error happens, we raise it
        (lambda (key . args)
          (if (message-needs-reply? message)
              ;; If the message is waiting on a reply, let them know
              ;; something went wrong.
              ;; However, we have to do it outside of this catch
              ;; routine, or we'll end up in an unrewindable continuation
              ;; situation.
              (set! queued-error-handling-thunk
                    (hive-reply-with-error hive message key args)))
          ;; print error message
          (apply print-error-and-continue key args)))
      ;; @@: This is a kludge.  See above for why.
      (if queued-error-handling-thunk
          (8sync (queued-error-handling-thunk))))
    (call-with-prompt (hive-prompt hive)
      call-catching-errors
      (lambda (kont actor message send-options)
        ;; Register the coroutine
        (hash-set! (hive-waiting-coroutines hive)
                   (message-id message)
                   (make-waiting-on-reply
                    (actor-id actor) kont send-options))
        ;; Send off the message
        (8sync (hive-process-message hive message)))))

  (define (process-local-message)
    (let ((actor (resolve-actor-to)))
      (call-catching-coroutine
       (lambda ()
         (define message-handler (actor-message-handler actor))
         ;; @@: Should a more general error handling happen here?
         (parameterize ((%current-actor actor))
           (let ((result
                  (message-handler actor message)))
             (maybe-autoreply actor)
             ;; Returning result allows actors to possibly make a run-request
             ;; at the end of handling a message.
             ;; ... We do want that, right?
             result))))))

  (define (resume-waiting-coroutine)
    (case (message-action message)
      ;; standard reply / auto-reply
      ((*reply* *auto-reply* *error*)
       (call-catching-coroutine
        (lambda ()
          (match (hash-remove! (hive-waiting-coroutines hive)
                               (message-in-reply-to message))
            ((_ . waiting)
             (if (not (equal? (message-to message)
                              (waiting-on-reply-actor-id waiting)))
                 (throw 'resuming-to-wrong-actor
                        "Attempted to resume a coroutine to the wrong actor!"
                        #:expected-actor-id (message-to message)
                        #:got-actor-id (waiting-on-reply-actor-id waiting)
                        #:message message))
             (let* (;; @@: How should we resolve resuming coroutines to actors who are
                    ;;   now gone?
                    (actor (resolve-actor-to))
                    (kont (waiting-on-reply-kont waiting))
                    (result (kont message)))
               (maybe-autoreply actor)
               result))
            (#f (throw 'no-waiting-coroutine
                       "message in-reply-to tries to resume nonexistent coroutine"
                       message))))))
      ;; Unhandled action for a reply!
      (else
       (throw 'hive-unresumable-coroutine
              "Won't resume coroutine, nonsense action on reply message"
              #:action (message-action message)
              #:message message))))

  (define (process-remote-message)
    ;; Find the ambassador
    (let* ((remote-hive-id (hive-id (message-to message)))
           (ambassador (hive-resolve-ambassador remote-hive-id))
           (message-handler (actor-message-handler ambassador))
           (forward-request (make-forward-request hive ambassador message)))
      (message-handler ambassador forward-request)))

  (let ((to (message-to message)))
    ;; This seems to be an easy mistake to make, so check that addressing
    ;; is correct here
    (if (not to)
        (throw 'missing-addressee
               "`to' field is missing on message"
               #:message message))
    (if (hive-actor-local? hive to)
        (if (message-in-reply-to message)
            (resume-waiting-coroutine)
            (process-local-message))
        (process-remote-message))))

(define-method (hive-actor-local? (hive <hive>) address)
  (equal? (hive-id hive) (address-hive-id address)))

(define-method (hive-register-actor! (hive <hive>) (actor <actor>))
  (hash-set! (hive-actor-registry hive) (actor-id actor) actor))

(define-method (%hive-create-actor (hive <hive>) actor-class
                                   init-args id-cookie send-init?)
  "Actual method called by bootstrap-actor / create-actor.

Since this is a define-method it can't accept fancy define* arguments,
so this gets called from the nicer bootstrap-actor interface.  See
that method for documentation."
  (let* ((actor-id (hive-gen-actor-id hive id-cookie))
         (actor (apply make actor-class
                       #:hive hive
                       #:id actor-id
                       init-args))
         (actor-should-init (slot-ref actor 'should-init)))
    (hive-register-actor! hive actor)
    ;; Maybe run actor init method
    (when (and send-init? actor-should-init)
      (let ((send-method
             (if (eq? actor-should-init 'wait)
                 <-wait <-)))
        (send-method actor-id '*init*)))
    ;; return the actor id
    actor-id))

(define* (bootstrap-actor hive actor-class #:rest init-args)
  "Create an actor on HIVE using ACTOR-CLASS passing in INIT-ARGS args"
  (%hive-create-actor hive actor-class
                    init-args (symbol->string (class-name actor-class))
                    #f))

(define* (bootstrap-actor* hive actor-class id-cookie #:rest init-args)
  "Create an actor, but also allow customizing a 'cookie' added to the id
for debugging"
  (%hive-create-actor hive actor-class
                    init-args id-cookie
                    #f))

(define (call-with-message message proc)
  "Applies message body arguments into procedure, with message as first
argument.  Similar to call-with-values in concept."
  (apply proc message (message-body message)))

;; (mbody-receive (<- bar baz)
;;     (baz)
;;   basil)

;; Emacs: (put 'mbody-receive 'scheme-indent-function 2)

;; @@: Or receive-msg or receieve-message or??
(define-syntax-rule (mbody-receive arglist message body ...)
  "Call body with arglist (which can accept arguments like lambda*)
applied from the message-body of message."
  (call-with-message message
                     (lambda* arglist
                       body ...)))

(define (mbody-val message)
  "Retrieve the first value from the message-body of message.
Like single value return from a procedure call.  Probably the most
common case when waiting on a reply from some action invocation."
  (call-with-message message
                     (lambda (_ val) val)))


;;; Various API methods for actors to interact with the system
;;; ==========================================================

;; TODO: move send-message and friends here...?

(define* (create-actor from-actor actor-class #:rest init-args)
  "Create an instance of actor-class.  Return the new actor's id.

This is the method actors should call directly (unless they want
to supply an id-cookie, in which case they should use
create-actor*)."
  (%hive-create-actor (actor-hive from-actor) actor-class
                      init-args #f #t))


(define* (create-actor* from-actor actor-class id-cookie #:rest init-args)
  "Create an instance of actor-class.  Return the new actor's id.

Like create-actor, but permits supplying an id-cookie."
  (%hive-create-actor (actor-hive from-actor) actor-class
                      init-args id-cookie #t))


(define* (self-destruct actor #:key (cleanup #t))
  "Remove an actor from the hive.

Unless #:cleanup is set to #f, this will first have the actor handle
its '*cleanup* action handler."
  (when cleanup
    (<-wait (actor-id actor) '*cleanup*))
  (hash-remove! (hive-actor-registry (actor-hive actor))
                (actor-id actor)))



;;; 8sync bootstrap utilities
;;; =========================

(define* (run-hive hive initial-tasks
                   #:key (cleanup #t)
                   (handle-signals (list SIGINT SIGTERM)))
  "Start up an agenda and run HIVE in it with INITIAL-TASKS.

Keyword arguments:
 - #:cleanup: Whether to run *cleanup* on all actors.
 - #:handle-sigactions: a list of signals to set up interrupt
   handlers for, so cleanup sill still happen as expected.
   Defaults to a list of SIGINT and SIGTERM."
  (dynamic-wind
    (const #f)
    (lambda ()
      (define (run-it escape)
        (define (handle-signal signum)
          (restore-signals)
          (escape signum))
        (for-each (lambda (signum)
                    (sigaction signum handle-signal))
                  handle-signals)
        (let* ((queue (list->q
                       (cons (bootstrap-message hive (actor-id hive) '*init-all*)
                             initial-tasks)))
               (agenda (make-agenda #:pre-unwind-handler print-error-and-continue
                                    #:queue queue)))
          (run-agenda agenda)))
      (call/ec run-it))
    ;; Run cleanup
    (lambda ()
      (when cleanup
        (run-hive-cleanup hive)))))

(define (run-hive-cleanup hive)
  (let ((queue (list->q (list (bootstrap-message hive (actor-id hive)
                                                 '*cleanup-all*)))))
    (run-agenda
     (make-agenda #:queue queue))))

(define (bootstrap-message hive to-id action . message-body-args)
  (wrap
   (apply <-* `(#:actor ,hive) to-id action message-body-args)))



;;; Basic readers / writers
;;; =======================

(define (serialize-message message)
  "Serialize a message for read/write"
  (list
   (message-id message)
   (message-to message)
   (message-from message)
   (message-action message)
   (message-body message)
   (message-in-reply-to message)
   (message-wants-reply message)
   (message-replied message)))

(define* (write-message message #:optional (port (current-output-port)))
  "Write out a message to a port for easy reading later.

Note that if a sub-value can't be easily written to something
Guile's `read' procedure knows how to read, this doesn't do anything
to improve that.  You'll need a better serializer for that.."
  (write (serialize-message message) port))

(define (serialize-message-pretty message)
  "Serialize a message in a way that's easy for humans to read."
  `(*message*
    (id ,(message-id message))
    (to ,(message-to message))
    (from ,(message-from message))
    (action ,(message-action message))
    (body ,(message-body message))
    (in-reply-to ,(message-in-reply-to message))
    (wants-reply ,(message-wants-reply message))
    (replied ,(message-replied message))))

(define (pprint-message message)
  "Pretty print a message."
  (pretty-print (serialize-message-pretty message)))

(define* (read-message #:optional (port (current-input-port)))
  "Read a message serialized via serialize-message from PORT"
  (match (read port)
    ((id to from action body in-reply-to wants-reply replied)
     (make-message-intern
      id to from action body
      in-reply-to wants-reply replied))
    (anything-else
     (throw 'message-read-bad-structure
            "Could not read message from structure"
            anything-else))))

(define (read-message-from-string message-str)
  "Read message from MESSAGE-STR"
  (with-input-from-string message-str
    (lambda ()
      (read-message (current-input-port)))))

;;; 8sync --- Asynchronous programming for Guile
;;; Copyright (C) 2016 Christopher Allan Webber <cwebber@dustycloud.org>
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

;; XUDD inspired actor system

(define-module (8sync systems actors)
  #:use-module (oop goops)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (8sync agenda)
  #:use-module (8sync repl)
  #:export (;; utilities... ought to go in their own module
            big-random-number
            big-random-number-string
            simple-message-id-generator
            require-slot

            <actor>
            actor-id
            actor-hive
            actor-message-handler

            ;;; Commenting out the <address> type for now;
            ;;; it may be back when we have better serializers
            ;; <address>
            make-address address?
            address-actor-id address-hive-id

            address->string
            actor-id-actor
            actor-id-hive
            actor-id-string

            make-action-dispatch
            define-simple-actor

            <hive>
            make-hive
            ;; There are more methods for the hive, but there's
            ;; no reason for the outside world to look at them maybe?
            hive-id
            hive-create-actor hive-create-actor*

            create-actor create-actor*
            self-destruct

            <message>
            make-message message?
            message-to message-action message-from
            message-id message-body message-in-reply-to
            message-wants-reply
            message-ref

            send-message send-message-wait
            reply-message reply-message-wait

            ez-run-hive
            hive-bootstrap-message

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

(define (require-slot slot-name)
  "Generate something for #:init-thunk to complain about unfilled slot"
  (lambda ()
    (throw 'required-slot
           (format #f "Slot ~s not filled" slot-name)
           slot-name)))



;;; Messages
;;; ========


(define-record-type <message>
  (make-message-intern id to from action
                       body in-reply-to wants-reply   ; do we need hive-proxy?
                       ;; Are these still needed?
                       replied deferred-reply)
  message?
  (id message-id)
  (to message-to)
  (from message-from)
  (action message-action)
  (body message-body)
  (in-reply-to message-in-reply-to)
  (wants-reply message-wants-reply)

  ;; See XUDD source for these.  Not use yet, maybe eventually will be?
  ;; XUDD uses them for autoreply.
  ;; Requiring mutation on message objects is clearly not great,
  ;; but it may be worth it...?  Investigate!
  (replied message-replied set-message-replied!)
  (deferred-reply message-deferred-reply set-message-deferred-reply!))


(define* (make-message id to from action body
                       #:key in-reply-to wants-reply
                       replied deferred-reply)
  (make-message-intern id to from action body
                       in-reply-to wants-reply replied
                       deferred-reply))

;; Note: the body of messages is currently an alist, but it's created
;;   from a keyword based property list (see the following two functions).
;;   But, that's an extra conversion step, and maybe totally unnecessary:
;;   we already have message-ref, and this could just pull a keyword
;;   from a property list.
;;   The main ways this might be useful are error checking,
;;   serialization across the wire (but even that might require some
;;   change), and using existing tooling (though adding new tooling
;;   would be negligible in implementation effort.)

;; This cons cell is immutable and unique (for eq? tests)
(define %nothing-provided (cons 'nothing 'provided))

(define* (message-ref message key #:optional (dflt %nothing-provided))
  "Extract KEY from body of MESSAGE.

Optionally set default with [DFLT]
If key not found and DFLT not provided, throw an error."
  (let ((result (assoc key (message-body message))))
    (if result (cdr result)
        (if (eq? dflt %nothing-provided)
            (throw 'message-missing-key
                   "Message body does not contain key and no default provided"
                   #:key key
                   #:message message)
            dflt))))


(define (message-needs-reply message)
  "See if this message needs a reply still"
  (and (message-wants-reply message)
       (not (or (message-replied message)
                (message-deferred-reply message)))))


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


(define (send-message from-actor to-id action . message-body-args)
  "Send a message from an actor to another actor"
  (let* ((hive (actor-hive from-actor))
         (message (make-message (hive-gen-message-id hive) to-id
                                (actor-id from-actor) action
                                (kwarg-list-to-alist message-body-args))))
    (8sync-nowait (hive-process-message hive message))))

(define (send-message-wait from-actor to-id action . message-body-args)
  "Send a message from an actor to another, but wait until we get a response"
  (let* ((hive (actor-hive from-actor))
         (abort-to (hive-prompt (actor-hive from-actor)))
         (message (make-message (hive-gen-message-id hive) to-id
                                (actor-id from-actor) action
                                (kwarg-list-to-alist message-body-args)
                                #:wants-reply #t)))
    (abort-to-prompt abort-to from-actor message)))

;; TODO: Intelligently ~propagate(ish) errors on -wait functions.
;;   We might have `send-message-wait-brazen' to allow callers to
;;   not have an exception thrown and instead just have a message with
;;   the appropriate '*error* message returned.

(define (reply-message from-actor original-message
                       . message-body-args)
  "Reply to a message"
  (set-message-replied! original-message #t)
  (let* ((hive (actor-hive from-actor))
         (new-message (make-message (hive-gen-message-id hive)
                                    (message-from original-message)
                                    (actor-id from-actor) '*reply*
                                    (kwarg-list-to-alist message-body-args)
                                    #:in-reply-to (message-id original-message))))
    (8sync-nowait (hive-process-message hive new-message))))

(define (reply-message-wait from-actor original-message
                            . message-body-args)
  "Reply to a messsage, but wait until we get a response"
  (set-message-replied! original-message #t)
  (let* ((hive (actor-hive from-actor))
         (abort-to (hive-prompt (actor-hive from-actor)))
         (new-message (make-message (hive-gen-message-id hive)
                                    (message-from original-message)
                                    (actor-id from-actor) '*reply*
                                    (kwarg-list-to-alist message-body-args)
                                    #:wants-reply #t
                                    #:in-reply-to (message-id original-message))))
    (abort-to-prompt abort-to from-actor new-message)))



;;; Main actor implementation
;;; =========================

(define-class <actor> ()
  ;; An address object
  (id #:init-thunk (require-slot "id")
      #:init-keyword #:id
      #:getter actor-id)
  ;; The hive we're connected to.
  ;; We need this to be able to send messages.
  (hive #:init-thunk (require-slot "hive")
        #:init-keyword #:hive
        #:accessor actor-hive)
  ;; How we receive and process new messages
  (message-handler #:init-thunk (require-slot "message-handler")
                   #:allocation #:each-subclass))

(define-method (actor-message-handler (actor <actor>))
  (slot-ref actor 'message-handler))

;;; So these are the nicer representations of addresses.
;;; However, they don't serialize so easily with scheme read/write, so we're
;;; using the simpler cons cell version below for now.

;; (define-record-type <address>
;;   (make-address actor-id hive-id)  ; @@: Do we want the trailing -id?
;;   address?
;;   (actor-id address-actor-id)
;;   (hive-id address-hive-id))
;;
;; (set-record-type-printer!
;;  <address>
;;  (lambda (record port)
;;    (format port "<address: ~s@~s>"
;;            (address-actor-id record) (address-hive-id record))))
;;

(define (make-address actor-id hive-id)
  (cons actor-id hive-id))

(define (address-actor-id address)
  (car address))

(define (address-hive-id address)
  (cdr address))

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



;;; Actor utilities
;;; ===============

(define (simple-dispatcher action-map)
  (lambda (actor message)
    (let* ((action (message-action message))
           (method (assoc-ref action-map action)))
      (if (not method)
          (throw 'action-not-found
                 "No appropriate action handler found for actor"
                 #:action action
                 #:actor actor
                 #:message message
                 #:available-actions (map car action-map)))
      (method actor message))))

(define-syntax %expand-action-item
  (syntax-rules ()
    ((_ ((action-name action-args ...) body ...))
     (cons (quote action-name)
           (lambda (action-args ...)
             body ...)))
    ((_ (action-name handler))
     (cons (quote action-name) handler))))

(define-syntax make-action-dispatch
  (syntax-rules ()
    "Expand a list of action names and actions into an alist

You can use this like the following:
  (make-action-dispatch
   (cookies
    (lambda (actor message)
      (display \"I love cookies!\n\")))
   (party
    (lambda (actor message)
      (display \"Life of the party!\"))))

Alternately, if you'd like to skip the lambda, you could use the slightly
more compact following syntax:
  (make-action-dispatch
   ((cookies actor message)
     (display \"I love cookies!\n\"))
   ((party actor message)
     (display \"Life of the party!\")))"
    ((make-action-dispatch action-item ...)
     (simple-dispatcher
      (list (%expand-action-item action-item) ...)))))

(define-syntax-rule (define-simple-actor class actions ...)
  (define-class class (<actor>)
    (message-handler
     #:init-value (make-action-dispatch actions ...)
     #:allocation #:each-subclass)))


;;; The Hive
;;; ========
;;;   Every actor has a hive.  The hive is a kind of "meta-actor"
;;;   which routes all the rest of the actors in a system.

(define-generic hive-handle-failed-forward)

(define-class <hive> (<actor>)
  ;; This gets set to itself immediately after being created
  (hive #:init-value #f)
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
  (message-handler
   #:init-value
   (make-action-dispatch
    ;; This is in the case of an ambassador failing to forward a message...
    ;; it reports it back to the hive
    (*failed-forward* hive-handle-failed-forward))))

(define-method (hive-handle-failed-forward (hive <hive>) message)
  "Handle an ambassador failing to forward a message"
  'TODO)

(define* (make-hive #:key hive-id)
  (let ((hive (make <hive>
                #:id (make-address
                      "hive" (or hive-id
                                 (big-random-number-string))))))
    ;; Set the hive's actor reference to itself
    (set! (actor-hive hive) hive)
    hive))

(define-method (hive-id (hive <hive>))
  (actor-id-hive hive))

(define-method (hive-gen-actor-id (hive <hive>) cookie)
  (make-address (if cookie
                    (string-append cookie "-" (big-random-number-string))
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

(define-method (hive-process-message (hive <hive>) message)
  "Handle one message, or forward it via an ambassador"
  (define (maybe-autoreply actor)
    ;; Possibly autoreply
    (if (message-needs-reply message)
        ;; @@: Should we give *autoreply* as the action instead of *reply*?
        (reply-message actor message
                       #:*auto-reply* #t)))

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
    (call-with-prompt (hive-prompt hive)
      thunk
      (lambda (kont actor message)
        (let ((hive (actor-hive actor)))
          ;; Register the coroutine
          (hash-set! (hive-waiting-coroutines hive)
                     (message-id message)
                     (cons (actor-id actor) kont))
          ;; Send off the message
          (8sync (hive-process-message hive message))))))

  (define (process-local-message)
    (let ((actor (resolve-actor-to)))
      (call-catching-coroutine
       (lambda ()
         (define message-handler (actor-message-handler actor))
         ;; @@: Should a more general error handling happen here?
         (let ((result
                (message-handler actor message)))
           (maybe-autoreply actor)
           ;; Returning result allows actors to possibly make a run-request
           ;; at the end of handling a message.
           ;; ... We do want that, right?
           result)))))

  (define (resume-waiting-coroutine)
    (call-catching-coroutine
     (lambda ()
       (match (hash-remove! (hive-waiting-coroutines hive)
                            (message-in-reply-to message))
         ((_ . (resume-actor-id . kont))
          (if (not (equal? (message-to message)
                           resume-actor-id))
              (throw 'resuming-to-wrong-actor
                     "Attempted to resume a coroutine to the wrong actor!"
                     #:expected-actor-id (message-to message)
                     #:got-actor-id resume-actor-id
                     #:message message))
          (let (;; @@: How should we resolve resuming coroutines to actors who are
                ;;   now gone?
                (actor (resolve-actor-to))
                (result (kont message)))
            (maybe-autoreply actor)
            result))
         (#f (throw 'no-waiting-coroutine
                    "message in-reply-to tries to resume nonexistent coroutine"
                    message))))))

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
  (hash-ref (hive-actor-registry hive) address))

(define-method (hive-register-actor! (hive <hive>) (actor <actor>))
  (hash-set! (hive-actor-registry hive) (actor-id actor) actor))

(define-method (%hive-create-actor (hive <hive>) actor-class
                                   init id-cookie)
  "Actual method called by hive-create-actor.

Since this is a define-method it can't accept fancy define* arguments,
so this gets called from the nicer hive-create-actor interface.  See
that method for documentation."
  (let* ((actor-id (hive-gen-actor-id hive id-cookie))
         (actor (apply make actor-class
                       ;; @@: If we switch to a hive-proxy, do it here
                       #:hive hive
                       #:id actor-id
                       init)))
    (hive-register-actor! hive actor)
    ;; return the actor id
    actor-id))

(define* (hive-create-actor hive actor-class #:rest init)
  (%hive-create-actor hive actor-class
                      init #f))

(define* (hive-create-actor* hive actor-class id-cookie #:rest init)
  (%hive-create-actor hive actor-class
                      init id-cookie))


;; TODO: Give actors this instead of the actual hive reference
(define-class <hive-proxy> ()
  (send-message #:getter proxy-send-message
                #:init-keyword #:send-message)
  (create-actor #:getter proxy-create-actor
                #:init-keyword #:create-actor))

;; Live the hive proxy, but has access to the hive itself...
(define-class <debug-hive-proxy> (<hive-proxy>)
  (hive #:init-keyword #:hive))



;;; Various API methods for actors to interact with the system
;;; ==========================================================

;; TODO: move send-message and friends here...?

;; TODO: Rewrite this inside of a <hive-proxy> ?
(define* (create-actor from-actor actor-class #:rest init)
  "Create an instance of actor-class.  Return the new actor's id.

This is the method actors should call directly (unless they want
to supply an id-cookie, in which case they should use
create-actor*)."
  (8sync (%hive-create-actor (actor-hive from-actor) actor-class
                             init #f)))


(define* (create-actor* from-actor actor-class id-cookie #:rest init)
  "Create an instance of actor-class.  Return the new actor's id.

Like create-actor, but permits supplying an id-cookie."
  (8sync (%hive-create-actor (actor-hive from-actor) actor-class
                             init id-cookie)))


(define (self-destruct actor)
  "Remove an actor from the hive."
  (hash-remove! (hive-actor-registry (actor-hive actor))
                (actor-id actor)))



;;; 8sync bootstrap utilities
;;; =========================

(define* (ez-run-hive hive initial-tasks #:key repl-server)
  "Start up an agenda and run HIVE in it with INITIAL-TASKS.

Should we start up a cooperative REPL for live hacking?  REPL-SERVER
wants to know!  You can pass it #t or #f, or if you want to specify a port,
an integer."
  (let* ((queue (list->q initial-tasks))
         (agenda (make-agenda #:pre-unwind-handler print-error-and-continue
                              #:queue queue)))
    (cond
     ;; If repl-server is an integer, we'll use that as the port
     ((integer? repl-server)
      (spawn-and-queue-repl-server! agenda repl-server))
     (repl-server
      (spawn-and-queue-repl-server! agenda)))
    (start-agenda agenda)))

(define (hive-bootstrap-message hive to-id action . message-body-args)
  (wrap
   (apply send-message hive to-id action message-body-args)))



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
   (message-replied message)
   (message-deferred-reply message)))

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
    (replied ,(message-replied message))
    (deferred-reply ,(message-deferred-reply message))))

(define (pprint-message message)
  "Pretty print a message."
  (pretty-print (serialize-message-pretty message)))

(define* (read-message #:optional (port (current-input-port)))
  "Read a message serialized via serialize-message from PORT"
  (match (read port)
    ((id to from action body in-reply-to wants-reply replied deferred-reply)
     (make-message-intern
      id to from action body
      in-reply-to wants-reply replied deferred-reply))
    (anything-else
     (throw 'message-read-bad-structure
            "Could not read message from structure"
            anything-else))))

(define (read-message-from-string message-str)
  "Read message from MESSAGE-STR"
  (with-input-from-string message-str
    (lambda ()
      (read-message (current-input-port)))))

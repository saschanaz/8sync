;;; 8sync --- Asynchronous programming for Guile
;;; Copyright (C) 2015 Christopher Allan Webber <cwebber@dustycloud.org>
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

(define-module (8sync agenda)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 q)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:export (<agenda>
            make-agenda agenda?
            agenda-queue agenda-prompt-tag
            agenda-read-port-map agenda-write-port-map agenda-except-port-map
            agenda-schedule
            
            make-async-prompt-tag

            list->q make-q*

            <time-segment>
            make-time-segment time-segment?
            time-segment-time time-segment-queue

            time< time= time<= time-delta+
            time-minus time-plus

            <time-delta>
            make-time-delta tdelta time-delta?
            time-delta-sec time-delta-usec

            <schedule>
            make-schedule schedule?
            schedule-add! schedule-empty?
            schedule-segments
            schedule-soonest-time

            schedule-segments-split schedule-extract-until!
            add-segments-contents-to-queue!

            <run-request>
            make-run-request run-request?
            run-request-proc run-request-when

            <port-request>
            make-port-request port-request port-request?
            port-request-port
            port-request-read port-request-write port-request-except

            <port-remove-request>
            make-port-remove-request port-remove-request port-remove-request?
            port-remove-request-port

            run-it wrap wrap-apply run run-at run-delay

            8sync 8sync-delay
            8sync-run 8sync-run-at 8sync-run-delay
            8sync-port 8sync-port-remove
            
            catch-8sync

            ;; used for introspecting the error, but a method for making
            ;; is not exposed
            wrapped-exception?
            wrapped-exception-key wrapped-exception-args
            wrapped-exception-stacks

            print-error-and-continue

            stop-on-nothing-to-do

            %current-agenda
            start-agenda agenda-run-once))

;; @@: Using immutable agendas here, so wouldn't it make sense to
;;   replace this queue stuff with using pfds based immutable queues?


;;; Agenda definition
;;; =================

;;; The agenda consists of:
;;;  - a queue of immediate items to handle
;;;  - sheduled future events to be added to a future queue
;;;  - a tag by which running processes can escape for some asynchronous
;;;    operation (from which they can be returned later)
;;;  - a mapping of ports to various handler procedures
;;;
;;; The goal, eventually, is for this all to be immutable and functional.
;;; However, we aren't there yet.  Some tricky things:
;;;  - The schedule needs to be immutable, yet reasonably efficient.
;;;  - Need to use immutable queues (ijp's pfds library?)
;;;  - Modeling reading from ports as something repeatable,
;;;    and with reasonable separation from functional components?

(define-immutable-record-type <agenda>
  (make-agenda-intern queue prompt-tag
                      read-port-map write-port-map except-port-map
                      schedule time catch-handler pre-unwind-handler)
  agenda?
  (queue agenda-queue)
  (prompt-tag agenda-prompt-tag)
  (read-port-map agenda-read-port-map)
  (write-port-map agenda-write-port-map)
  (except-port-map agenda-except-port-map)
  (schedule agenda-schedule)
  (time agenda-time)
  (catch-handler agenda-catch-handler)
  (pre-unwind-handler agenda-pre-unwind-handler))

(define (make-async-prompt-tag)
  "Make an async prompt tag for an agenda.

Generally done automatically for the user through (make-agenda)."
  (make-prompt-tag "prompt"))

(define* (make-agenda #:key
                      (queue (make-q))
                      (prompt (make-prompt-tag))
                      (read-port-map (make-hash-table))
                      (write-port-map (make-hash-table))
                      (except-port-map (make-hash-table))
                      (schedule (make-schedule))
                      (time (gettimeofday))
                      (catch-handler #f)
                      (pre-unwind-handler #f))
  ;; TODO: document arguments
  "Make a fresh agenda."
  (make-agenda-intern queue prompt
                      read-port-map write-port-map except-port-map
                      schedule time
                      catch-handler pre-unwind-handler))

(define (current-agenda-prompt)
  "Get the prompt for the current agenda; signal an error if there isn't one."
  (let ((current-agenda (%current-agenda)))
    (if (not current-agenda)
        (throw
         'no-current-agenda
         "Can't get current agenda prompt if there's no current agenda!")
        (agenda-prompt-tag current-agenda))))

;; helper for making queues for an agenda
(define (list->q lst)
  "Makes a queue composed of LST items"
  (let ((q (make-q)))
    (for-each
     (lambda (x)
       (enq! q x))
     lst)
    q))

(define (make-q* . args)
  "Makes a queue and populates it with this invocation's ARGS"
  (list->q args))


;;; Schedule
;;; ========

;;; This is where we handle timed events for the future

;; This section totally borrows from the ideas in SICP
;; <3 <3 <3

;; NOTE: time is a cons of (seconds . microseconds)

(define-record-type <time-segment>
  (make-time-segment-intern time queue)
  time-segment?
  (time time-segment-time)
  (queue time-segment-queue))

;; @@: This seems to be the same as srfi-18's seconds->time procedure?
;;   Maybe double check and switch to that?  (Thanks amz3!)

(define (time-from-float-or-fraction time)
  "Produce a (sec . usec) pair from TIME, a float or fraction"
  (let* ((mixed-whole (floor time))
         (mixed-rest (- time mixed-whole))  ; float or fraction component
         (sec mixed-whole)
         (usec (floor (* 1000000 mixed-rest))))
    (cons (inexact->exact sec) (inexact->exact usec))))

(define (time-segment-right-format time)
  "Ensure TIME is in the right format.

The right format means (second . microsecond).
If an integer, will convert appropriately."
  ;; TODO: add floating point / rational number support.
  (match time
    ;; time is already a cons of second and microsecnd
    (((? integer? s) . (? integer? u)) time)
    ;; time was just an integer (just the second)
    ((? integer? _) (cons time 0))
    ((or (? rational? _) (? inexact? _))
     (time-from-float-or-fraction time))
    (_ (throw 'invalid-time "Invalid time" time))))

(define* (make-time-segment time #:optional (queue (make-q)))
  "Make a time segment of TIME and QUEUE

No automatic conversion is done, so you might have to
run (time-segment-right-format) first."
  (make-time-segment-intern time queue))

(define (time< time1 time2)
  "Check if TIME1 is less than TIME2"
  (cond ((< (car time1)
            (car time2))
         #t)
        ((and (= (car time1)
                 (car time2))
              (< (cdr time1)
                 (cdr time2)))
         #t)
        (else #f)))

(define (time= time1 time2)
  "Check whether TIME1 and TIME2 are equivalent"
  (and (= (car time1) (car time2))
       (= (cdr time1) (cdr time2))))

(define (time<= time1 time2)
  "Check if TIME1 is less than or equal to TIME2"
  (or (time< time1 time2)
      (time= time1 time2)))


(define-record-type <time-delta>
  (make-time-delta-intern sec usec)
  time-delta?
  (sec time-delta-sec)
  (usec time-delta-usec))

(define* (make-time-delta time)
  "Make a <time-delta> of SEC seconds and USEC microseconds.

This is used primarily so the agenda can recognize RUN-REQUEST objects
which are meant to delay computation"
  (match (time-segment-right-format time)
    ((sec . usec)
     (make-time-delta-intern sec usec))))

(define tdelta make-time-delta)

(define (time-carry-correct time)
  "Corrects/handles time microsecond carry.
Will produce (0 . 0) instead of a negative number, if needed."
  (cond ((>= (cdr time) 1000000)
         (cons
          (+ (car time) 1)
          (- (cdr time) 1000000)))
        ((< (cdr time) 0)
         (if (= (car time) 0)
             '(0 0)
             (cons
              (- (car time) 1)
              (+ (cdr time) 1000000))))
        (else time)))

(define (time-delta+ time time-delta)
  "Increment a TIME by the value of TIME-DELTA"
  (time-carry-correct
   (cons (+ (car time) (time-delta-sec time-delta))
         (+ (cdr time) (time-delta-usec time-delta)))))

(define (time-minus time1 time2)
  "Subtract TIME2 from TIME1"
  (time-carry-correct
   (cons (- (car time1) (car time2))
         (- (cdr time1) (cdr time2)))))

(define (time-plus time1 time2)
  "Add TIME1 and TIME2"
  (time-carry-correct
   (cons (+ (car time1) (car time2))
         (+ (cdr time1) (cdr time2)))))


(define-record-type <schedule>
  (make-schedule-intern segments)
  schedule?
  (segments schedule-segments set-schedule-segments!))

(define* (make-schedule #:optional segments)
  "Make a schedule, optionally pre-composed of SEGMENTS"
  (make-schedule-intern (or segments '())))

(define (schedule-soonest-time schedule)
  "Return a cons of (sec . usec) for next time segement, or #f if none"
  (let ((segments (schedule-segments schedule)))
    (if (eq? segments '())
        #f
        (time-segment-time (car segments)))))

;; TODO: This code is reasonably easy to read but it
;;   mutates AND is worst case of O(n) in both space and time :(
;;   but at least it'll be reasonably easy to refactor to
;;   a more functional setup?
(define (schedule-add! schedule time proc)
  "Mutate SCHEDULE, adding PROC at an appropriate time segment for TIME"
  (let ((time (time-segment-right-format time)))
    (define (new-time-segment)
      (let ((new-segment
             (make-time-segment time)))
        (enq! (time-segment-queue new-segment) proc)
        new-segment))
    (define (loop segments)
      (define (segment-equals-time? segment)
        (time= time (time-segment-time segment)))

      (define (segment-more-than-time? segment)
        (time< time (time-segment-time segment)))

      ;; We could switch this out to be more mutate'y
      ;; and avoid the O(n) of space... is that over-optimizing?
      (match segments
        ;; If we're at the end of the list, time to make a new
        ;; segment...
        ('() (cons (new-time-segment) '()))
        ;; If the segment's time is exactly our time, good news
        ;; everyone!  Let's append our stuff to its queue
        (((? segment-equals-time? first) rest ...)
         (enq! (time-segment-queue first) proc)
         segments)
        ;; If the first segment is more than our time,
        ;; ours belongs before this one, so add it and
        ;; start consing our way back
        (((? segment-more-than-time? first) rest ...)
         (cons (new-time-segment) segments))
        ;; Otherwise, build up recursive result
        ((first rest ... )
         (cons first (loop rest)))))
    (set-schedule-segments!
     schedule
     (loop (schedule-segments schedule)))))

(define (schedule-empty? schedule)
  "Check if the SCHEDULE is currently empty"
  (eq? (schedule-segments schedule) '()))

(define (schedule-segments-split schedule time)
  "Does a multiple value return of time segments before/at and after TIME"
  (let ((time (time-segment-right-format time)))
    (define (segment-is-now? segment)
      (time= (time-segment-time segment) time))
    (define (segment-is-before-now? segment)
      (time< (time-segment-time segment) time))

    (let loop ((segments-before '())
               (segments-left (schedule-segments schedule)))
      (match segments-left
        ;; end of the line, return
        ('()
         (values (reverse segments-before) '()))

        ;; It's right now, so time to stop, but include this one in before
        ;; but otherwise return
        (((? segment-is-now? first) rest ...)
         (values (reverse (cons first segments-before)) rest))

        ;; This is prior or at now, so add it and keep going
        (((? segment-is-before-now? first) rest ...)
         (loop (cons first segments-before) rest))

        ;; Otherwise it's past now, just return what we have
        (segments-after
         (values segments-before segments-after))))))

(define (schedule-extract-until! schedule time)
  "Extract all segments until TIME from SCHEDULE, and pop old segments off"
  (receive (segments-before segments-after)
      (schedule-segments-split schedule time)
    (set-schedule-segments! schedule segments-after)
    segments-before))

(define (add-segments-contents-to-queue! segments queue)
  (for-each
   (lambda (segment)
     (let ((seg-queue (time-segment-queue segment)))
       (while (not (q-empty? seg-queue))
         (enq! queue (deq! seg-queue)))))
   segments))



;;; Request to run stuff
;;; ====================

(define-record-type <run-request>
  (make-run-request proc when)
  run-request?
  (proc run-request-proc)
  (when run-request-when))

(define* (run-it proc #:optional when)
  "Make a request to run PROC (possibly at WHEN)"
  (make-run-request proc when))

(define-syntax-rule (wrap body ...)
  "Wrap contents in a procedure"
  (lambda ()
    body ...))

(define-syntax-rule (wrap-apply body)
  "Wrap possibly multi-value function in a procedure, applies all arguments"
  (lambda args
    (apply body args)))


;; @@: Do we really want `body ...' here?
;;   what about just `body'?
(define-syntax-rule (run body ...)
  "Run everything in BODY but wrap in a convenient procedure"
  (make-run-request (wrap body ...) #f))

(define-syntax-rule (run-at body ... when)
  "Run BODY at WHEN"
  (make-run-request (wrap body ...) when))

;; @@: Is it okay to overload the term "delay" like this?
;;   Would `run-in' be better?
(define-syntax-rule (run-delay body ... delay-time)
  "Run BODY at DELAY-TIME time from now"
  (make-run-request (wrap body ...) (tdelta delay-time)))


;; A request to set up a port with at least one of read, write, except
;; handling processes

(define-record-type <port-request>
  (make-port-request-intern port read write except)
  port-request?
  (port port-request-port)
  (read port-request-read)
  (write port-request-write)
  (except port-request-except))

(define* (make-port-request port #:key read write except)
  (if (not (or read write except))
      (throw 'no-port-handler-given "No port handler given.\n"))
  (make-port-request-intern port read write except))

(define port-request make-port-request)

(define-record-type <port-remove-request>
  (make-port-remove-request port)
  port-remove-request?
  (port port-remove-request-port))

(define port-remove-request make-port-remove-request)



;;; Asynchronous escape to run things
;;; =================================

(define-syntax-rule (8sync-abort-to-prompt async-request)
  (abort-to-prompt (current-agenda-prompt)
                   async-request))

;; Async port request and run-request meta-requests
(define (make-async-request proc)
  "Wrap PROC in an async-request

The purpose of this is to make sure that users don't accidentally
return the wrong thing via (8sync) and trip themselves up."
  (cons '*async-request* proc))

(define (setup-async-request resume-kont async-request)
  "Complete an async request for agenda-run-once's continuation handling"
  (match async-request
    (('*async-request* . async-setup-proc)
     (async-setup-proc resume-kont))
    ;; TODO: deliver more helpful errors depending on what the user
    ;;   returned
    (_ (throw 'invalid-async-request
              "Invalid request passed back via an (8sync) procedure."
              async-request))))

(define-record-type <wrapped-exception>
  (make-wrapped-exception key args stacks)
  wrapped-exception?
  (key wrapped-exception-key)
  (args wrapped-exception-args)
  (stacks wrapped-exception-stacks))

(define-syntax-rule (propagate-%async-exceptions body)
  (let ((body-result body))
    (if (wrapped-exception? body-result)
        (throw '8sync-caught-error
               (wrapped-exception-key body-result)
               (wrapped-exception-args body-result)
               (wrapped-exception-stacks body-result))
        body-result)))

(define-syntax 8sync
  (syntax-rules ()
    "Run BODY asynchronously (8synchronously?) at a prompt, then return.

Possibly specify WHEN as the second argument."
    ((8sync body)
     (8sync-run body))
    ((8sync body when)
     (8sync-run-at body when))))

(define-syntax-rule (8sync-run body ...)
  (8sync-run-at body ... #f))

(define-syntax-rule (8sync-run-at body ... when)
  (propagate-%async-exceptions
   (8sync-abort-to-prompt
    ;; Send an asynchronous request to apply a continuation to the
    ;; following function, then handle that as a request to the agenda
    (make-async-request
     (lambda (kont)
       ;; We're making a run request
       (make-run-request
        ;; Wrapping the following execution to run...
        (wrap
         ;; Once we get the result from the inner part, we'll resume
         ;; this continuation, but first
         ;; @@: Is this running immediately, or queueing the result
         ;;   after evaluation for the next agenda tick?  It looks
         ;;   like evaluating immediately.  Is that what we want?
         (kont
          ;; Any unhandled errors are caught
          (let ((exception-stack #f))
            (catch #t
              ;; Run the actual code the user requested
              (lambda ()
                body ...)
              ;; If something bad happened and we didn't catch it,
              ;; we'll wrap it up in such a way that the continuation
              ;; can address it
              (lambda (key . args)
                (cond
                 ((eq? key '8sync-caught-error)
                  (match args
                    ((orig-key orig-args orig-stacks)
                     (make-wrapped-exception
                      orig-key orig-args
                      (cons exception-stack orig-stacks)))))
                 (else
                  (make-wrapped-exception key args
                                          (list exception-stack)))))
              (lambda _
                (set! exception-stack (make-stack #t 1 0)))))))
        when))))))

(define-syntax-rule (8sync-run-delay body ... delay-time)
  (8sync-run-at body ... (tdelta delay-time)))

(define-syntax-rule (8sync-delay args ...)
  (8sync-run-delay args ...))

(define-syntax-rule (8sync-port port port-request-args ...)
  (8sync-abort-to-prompt
   (make-async-request
    (lambda (kont)
      (list (make-port-request port port-request-args ...)
            (make-run-request
             ;; What's with returning #f to kont?
             ;; Otherwise we sometimes get errors like
             ;; "Zero values returned to single-valued continuation""
             (wrap (kont #f)) #f))))))

(define-syntax-rule (8sync-port-remove port)
  (8sync-abort-to-prompt
   (make-async-request
    (lambda (kont)
      (list (make-port-remove-request port)
            (make-run-request
             ;; See comment in 8sync-port
             (wrap (kont #f)) #f))))))


;; TODO: Write (%run-immediately)

(define-syntax-rule (8sync-immediate body)
  "Run body asynchronously but ignore its result...
forge ahead in our current function!"
  (8sync-abort-to-prompt
   (make-async-request
    (lambda (kont)
      (list (make-run-request
             ;; See comment in 8sync-port
             (wrap (kont #f)) #f)
            (make-run-request body #f))))))

(define-syntax-rule (catch-8sync exp (handler-key handler) ...)
  (catch '8sync-caught-error
    (lambda ()
      exp)
    (lambda (_ orig-key orig-args orig-stacks)
      (cond
       ((or (eq? handler-key #t)
            (eq? orig-key handler-key))
        (apply handler orig-stacks orig-args)) ...
       (else (raise '8sync-caught-error
                    orig-key orig-args orig-stacks))))))



;;; Execution of agenda, and current agenda
;;; =======================================

(define %current-agenda (make-parameter #f))

(define (update-agenda-from-select! agenda)
  "Potentially (select) on ports specified in agenda, adding items to queue.

Also handles sleeping when all we have to do is wait on the schedule."
  (define (hash-keys hash)
    (hash-map->list (lambda (k v) k) hash))
  (define (get-wait-time)
    ;; TODO: we need to figure this out based on whether there's anything
    ;;   in the queue, and if not, how long till the next scheduled item
    (let ((soonest-time (schedule-soonest-time (agenda-schedule agenda))))
      (cond 
       ((not (q-empty? (agenda-queue agenda)))
        (cons 0 0))
       (soonest-time    ; ie, the agenda is non-empty
        (let* ((current-time (agenda-time agenda)))
          (if (time<= soonest-time current-time)
              ;; Well there's something due so let's select
              ;; (this avoids a (possible?) race condition chance)
              (cons 0 0)
              (time-minus soonest-time current-time))))
       (else
        (cons #f #f)))))
  (define (do-select)
    ;; TODO: support usecond wait time too
    (match (get-wait-time)
      ((sec . usec)
       (catch 'system-error
         (lambda ()
           (select (hash-keys (agenda-read-port-map agenda))
                   (hash-keys (agenda-write-port-map agenda))
                   (hash-keys (agenda-except-port-map agenda))
                   sec usec))
         (lambda (key . rest-args)
           (match rest-args
             ((_ _ _ (EINTR))
              '(() () ()))
             (_ (error "Unhandled error in select!" key rest-args))))))))
  (define (get-procs-to-run)
    (define (ports->procs ports port-map)
      (lambda (initial-procs)
        (fold
         (lambda (port prev)
           (cons (lambda ()
                   ((hash-ref port-map port) port))
                 prev))
         initial-procs
         ports)))
    (match (do-select)
      ((read-ports write-ports except-ports)
       ;; @@: Come on, we can do better than append ;P
       ((compose (ports->procs
                  read-ports
                  (agenda-read-port-map agenda))
                 (ports->procs
                  write-ports
                  (agenda-write-port-map agenda))
                 (ports->procs
                  except-ports
                  (agenda-except-port-map agenda)))
        '()))))
  (define (update-agenda)
    (let ((procs-to-run (get-procs-to-run))
          (q (agenda-queue agenda)))
      (for-each
       (lambda (proc)
         (enq! q proc))
       procs-to-run))
    agenda)
  (define (ports-to-select?)
    (define (has-items? selector)
      ;; @@: O(n)
      ;;    ... we could use hash-for-each and a continuation to jump
      ;;    out with a #t at first indication of an item
      (not (= (hash-count (const #t)
                          (selector agenda))
              0)))
    (or (has-items? agenda-read-port-map)
        (has-items? agenda-write-port-map)
        (has-items? agenda-except-port-map)))

  (if (or (ports-to-select?)
          ;; select doubles as sleep...
          (not (schedule-empty? (agenda-schedule agenda)))) 
      (update-agenda)
      agenda))

(define (agenda-handle-port-request! agenda port-request)
  "Update an agenda for a port-request"
  (define (handle-selector request-selector port-map-selector)
    (if (request-selector port-request)
        ;; @@: Should we remove if #f?
        (hash-set! (port-map-selector agenda)
                   (port-request-port port-request)
                   (request-selector port-request))))
  (handle-selector port-request-read agenda-read-port-map)
  (handle-selector port-request-write agenda-write-port-map)
  (handle-selector port-request-except agenda-except-port-map))


(define (agenda-handle-port-remove-request! agenda port-remove-request)
  "Update an agenda for a port-remove-request"
  (let ((port (port-remove-request-port port-remove-request)))
    (hash-remove! (agenda-read-port-map agenda) port)
    (hash-remove! (agenda-write-port-map agenda) port)
    (hash-remove! (agenda-except-port-map agenda) port)))


(define (stop-on-nothing-to-do agenda)
  (and (q-empty? (agenda-queue agenda))
       (schedule-empty? (agenda-schedule agenda))
       (= 0 (hash-count (const #t) (agenda-read-port-map agenda)))
       (= 0 (hash-count (const #t) (agenda-write-port-map agenda)))
       (= 0 (hash-count (const #t) (agenda-except-port-map agenda)))))


(define* (start-agenda agenda
                       #:key
                       ;; @@: Should we make stop-on-nothing-to-do
                       ;;   the default stop-condition?
                       stop-condition
                       (get-time gettimeofday)
                       (handle-ports update-agenda-from-select!))
  ;; TODO: Document fields
  "Start up the AGENDA"
  (let loop ((agenda agenda))
    (let ((agenda   
           ;; @@: Hm, maybe here would be a great place to handle
           ;;   select'ing on ports.
           ;;   We could compose over agenda-run-once and agenda-read-ports
           (agenda-run-once agenda)))
      (if (and stop-condition (stop-condition agenda))
          'done
          (let* ((agenda
                  ;; We have to update the time after ports handled, too
                  ;; because it may have changed after a select
                  (set-field
                   (handle-ports
                    ;; Adjust the agenda's time just in time
                    ;; We do this here rather than in agenda-run-once to make
                    ;; agenda-run-once's behavior fairly predictable
                    (set-field agenda (agenda-time) (get-time)))
                   (agenda-time) (get-time))))
            ;; Update the agenda's current queue based on
            ;; currently applicable time segments
            (add-segments-contents-to-queue!
             (schedule-extract-until! (agenda-schedule agenda) (agenda-time agenda))
             (agenda-queue agenda))
            (loop agenda))))))


(define (print-error-and-continue key . args)
  "Frequently used as pre-unwind-handler for agenda"
  (cond
   ((eq? key '8sync-caught-error)
    (match args
      ((orig-key orig-args stacks)
       (display "\n*** Caught async exception. ***\n")
       (format (current-error-port)
               "* Original key '~s and arguments: ~s *\n"
               orig-key orig-args)
       (display "* Caught stacks below (ending with original) *\n\n")
       (for-each
        (lambda (s)
          (display-backtrace s (current-error-port))
          (newline (current-error-port)))
        stacks))))
   (else
    (format (current-error-port)
            "\n*** Caught exception with key '~s and arguments: ~s ***\n"
            key args)
    (display-backtrace (make-stack #t 1 0)
                       (current-error-port))
    (newline (current-error-port)))))

(define-syntax-rule (maybe-catch-all (catch-handler pre-unwind-handler)
                                     body ...)
  (if (or catch-handler pre-unwind-handler)
      (catch
        #t
        (lambda ()
          body ...)
        (or catch-handler (lambda _ #f))
        (or pre-unwind-handler (lambda _ #f)))
      (begin body ...)))

(define (agenda-run-once agenda)
  "Run once through the agenda, and produce a new agenda
based on the results"
  (define (call-proc proc)
    (call-with-prompt
     (agenda-prompt-tag agenda)
     (lambda ()
       (parameterize ((%current-agenda agenda))
         (maybe-catch-all
          ((agenda-catch-handler agenda)
           (agenda-pre-unwind-handler agenda))
          (proc))))
     (lambda (kont async-request)
       (setup-async-request kont async-request))))

  (let ((queue (agenda-queue agenda))
        (next-queue (make-q)))
    (while (not (q-empty? queue))
      (let* ((proc (q-pop! queue))
             (proc-result (call-proc proc))
             (enqueue
              (lambda (run-request)
                (define (schedule-at! time proc)
                  (schedule-add! (agenda-schedule agenda) time proc))
                (let ((request-time (run-request-when run-request)))
                  (match request-time
                    ((? time-delta? time-delta)
                     (let ((time (time-delta+ (agenda-time agenda)
                                              time-delta)))
                       (schedule-at! time (run-request-proc run-request))))
                    ((? integer? sec)
                     (let ((time (cons sec 0)))
                       (schedule-at! time (run-request-proc run-request))))
                    (((? integer? sec) . (? integer? usec))
                     (schedule-at! request-time (run-request-proc run-request)))
                    (#f
                     (enq! next-queue (run-request-proc run-request))))))))
        (define (handle-individual result)
          ;; @@: Could maybe optimize by switching to an explicit cond...
          (match result
            ((? run-request? new-proc)
             (enqueue new-proc))
            ((? port-request? port-request)
             (agenda-handle-port-request! agenda port-request))
            ((? port-remove-request? port-remove-request)
             (agenda-handle-port-remove-request! agenda port-remove-request))
            ;; do nothing
            (_ #f)))
        ;; @@: We might support delay-wrapped procedures here
        (match proc-result
          ((results ...)
           (for-each handle-individual results))
          (one-result (handle-individual one-result)))))
    ;; TODO: Alternately, we could return the next-queue
    ;;   along with changes to be added to the schedule here?
    ;; Return new agenda, with next queue set
    (set-field agenda (agenda-queue) next-queue)))

;;; 8sync --- Asynchronous programming for Guile
;;; Copyright (C) 2015, 2016 Christopher Allan Webber <cwebber@dustycloud.org>
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
  #:use-module (ice-9 q)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 suspendable-ports)
  #:export (<agenda>
            make-agenda agenda?
            agenda-queue agenda-prompt-tag
            agenda-read-port-map agenda-write-port-map
            agenda-schedule
            
            make-async-prompt-tag

            list->q make-q*

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

            run-it wrap wrap-apply run run-at run-delay

            8sync
            8sleep 8yield
            
            ;; used for introspecting the error, but a method for making
            ;; is not exposed
            wrapped-exception?
            wrapped-exception-key wrapped-exception-args
            wrapped-exception-stacks

            print-error-and-continue

            stop-on-nothing-to-do

            %current-agenda-prompt
            run-agenda agenda-run-once!))


;;; Agenda definition
;;; =================

;;; The agenda consists of:
;;;  - a queue of immediate items to handle
;;;  - sheduled future events to be added to a future queue
;;;  - a tag by which running processes can escape for some asynchronous
;;;    operation (from which they can be returned later)
;;;  - a mapping of ports to various handler procedures
;;;
;;; @@: Is this next part deprecated?
;;; The goal, maybe eventually, is for this all to be immutable and functional.
;;; However, we aren't there yet.  Some tricky things:
;;;  - The schedule needs to be immutable, yet reasonably efficient.
;;;  - Need to use immutable queues (ijp's pfds library?)
;;;  - Modeling reading from ports as something repeatable,
;;;    and with reasonable separation from functional components?

;; TODO: Tear out the immutable agenda aspect until we're actually ready
;;   to use it.
(define-record-type <agenda>
  (make-agenda-intern queue prompt-tag
                      read-port-map write-port-map
                      schedule catch-handler pre-unwind-handler)
  agenda?
  (queue agenda-queue set-agenda-queue!)
  (prompt-tag agenda-prompt-tag)
  (read-port-map agenda-read-port-map)
  (write-port-map agenda-write-port-map)
  (schedule agenda-schedule)
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
                      (schedule (make-schedule))
                      (catch-handler #f)
                      (pre-unwind-handler print-error-and-continue))
  ;; TODO: document arguments
  "Make a fresh agenda."
  (make-agenda-intern queue prompt
                      read-port-map write-port-map
                      schedule catch-handler pre-unwind-handler))

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

;; @@: Maybe we should use floor/ here?
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

(define (time-minus time1 time2)
  "Subtract TIME2 from TIME1"
  (time-carry-correct
   (cons (- (car time1) (car time2))
         (- (cdr time1) (cdr time2)))))

;; @@: Unused?
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
   (loop (schedule-segments schedule))))

(define (schedule-empty? schedule)
  "Check if the SCHEDULE is currently empty"
  (eq? (schedule-segments schedule) '()))

(define (schedule-segments-split schedule time)
  "Does a multiple value return of time segments before/at and after TIME"
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
       (values segments-before segments-after)))))

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

(define-syntax-rule (run-delay body ... delay-time)
  "Run BODY at DELAY-TIME time from now"
  (make-run-request (wrap body ...) (delayed-time delay-time)))

;;; Procedures that are delimited continuations being resumed are
;;; wrapped in a <kontinue>.  This is so that we don't accidentally
;;; wrap in another catch statement every time we resume them, which
;;; would be a drag.

(define-record-type <kontinue>
  (kontinue kont)
  kontinue?
  (kont kontinue-kont))



;;; Asynchronous escape to run things
;;; =================================

(define-syntax-rule (8sync-abort-to-prompt async-request)
  (abort-to-prompt (%current-agenda-prompt)
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

(define-syntax-rule (8sync body ...)
  "Run body asynchronously but ignore its result...
forge ahead in our current function!"
  (8sync-abort-to-prompt
   (make-async-request
    (lambda (kont)
      (list (make-run-request
             (kontinue kont) #f)
            (make-run-request (lambda () body ...) #f))))))

(define (delayed-time in-secs)
  "Calculate a cons of '(sec . usec) of IN-SECS from current time"
  (define cur-time (gettimeofday))
  (define cur-secs (car cur-time))
  (define cur-usecs (cdr cur-time))
  (define (convert-non-integer)
    (define next-time-in-usecs
      (+ (* (+ in-secs cur-secs) ; add our seconds to current seconds
            1000000)             ; and turn into usecs
         cur-usecs))             ; then add in current usecs
    ;; convert into sec / usec pair
    (receive (secs usecs)
        (floor/ next-time-in-usecs 1000000)
      (cons secs (floor usecs))))
  (define (convert-integer)
    (cons (+ in-secs cur-secs) cur-usecs))
  (if (integer? in-secs)
      (convert-integer)
      (convert-non-integer)))

;; TODO: Rewrite when we move to this being just `sleep'.
(define (8sleep secs)
  "Like sleep, but asynchronous."
  (8sync-abort-to-prompt
   (make-async-request
    (lambda (kont)
      (make-run-request (kontinue kont) (delayed-time secs))))))

;; Voluntarily yield execution
(define (8yield)
  "Voluntarily yield execution to the scheduler."
  (8sync-abort-to-prompt
   (make-async-request
    (lambda (kont)
      (make-run-request (kontinue kont) #f)))))


;;; Execution of agenda, and current agenda
;;; =======================================

(define %current-agenda-prompt (make-parameter #f))

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
        (let* ((current-time (gettimeofday)))
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
                   '()
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
           (define proc (hashq-ref port-map port))
           ;; Now that we've selected on this port, it can be removed
           (hashq-remove! port-map port)
           (cons proc prev))
         initial-procs
         ports)))
    (match (do-select)
      ((read-ports write-ports except-ports) ; except-ports not used
       ((compose (ports->procs
                  read-ports
                  (agenda-read-port-map agenda))
                 (ports->procs
                  write-ports
                  (agenda-write-port-map agenda)))
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
        (has-items? agenda-write-port-map)))

  (if (or (ports-to-select?)
          ;; select doubles as sleep...
          (not (schedule-empty? (agenda-schedule agenda)))) 
      (update-agenda)
      agenda))

(define-record-type <read-request>
  (make-read-request port proc)
  read-request?
  (port read-request-port)
  (proc read-request-proc))

(define-record-type <write-request>
  (make-write-request port proc)
  write-request?
  (port write-request-port)
  (proc write-request-proc))

(define (agenda-handle-read-request! agenda read-request)
  "Handle <read-request>, which is a request to add this port to the poll/select
on suspendable ports."
  (hashq-set! (agenda-read-port-map agenda)
              (read-request-port read-request)
              (read-request-proc read-request)))

(define (agenda-handle-write-request! agenda write-request)
  (hashq-set! (agenda-write-port-map agenda)
              (write-request-port write-request)
              (write-request-proc write-request)))

(define (stop-on-nothing-to-do agenda)
  (and (q-empty? (agenda-queue agenda))
       (schedule-empty? (agenda-schedule agenda))
       (= 0 (hash-count (const #t) (agenda-read-port-map agenda)))
       (= 0 (hash-count (const #t) (agenda-write-port-map agenda)))))


(define* (run-agenda agenda
                     #:key (stop-condition stop-on-nothing-to-do)
                     ;; For live hacking madness, etc
                     (post-run-hook #f))
  ;; TODO: Document fields
  "Start up the AGENDA"
  (install-suspendable-ports!)
  (parameterize ((%current-agenda-prompt (agenda-prompt-tag agenda))
                 (current-read-waiter wait-for-readable)
                 (current-write-waiter wait-for-writable))
    (while (not (stop-condition agenda))
      (agenda-run-once! agenda)
      (update-agenda-from-select! agenda)
      (add-segments-contents-to-queue!
       (schedule-extract-until! (agenda-schedule agenda) (gettimeofday))
       (agenda-queue agenda))
      (if post-run-hook
          (post-run-hook agenda))))
  'done)

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

(define (wait-for-readable port)
  (8sync-abort-to-prompt
   (make-async-request
    (lambda (kont)
      (make-read-request port (wrap (kont #f)))))))

(define (wait-for-writable port)
  (8sync-abort-to-prompt
   (make-async-request
    (lambda (kont)
      (make-write-request port (wrap (kont #f)))))))

(define (agenda-run-once! agenda)
  "Run once through the agenda, and produce a new agenda
based on the results"
  ;; @@: Maybe rename proc -> run-this ?
  (define (call-proc proc)
    (call-with-prompt
     (agenda-prompt-tag agenda)
     (lambda ()
       (if (kontinue? proc)
           ;; Resume continuation.
           ;; We need to pass in #f, otherwise we sometimes get errors like
           ;; "Zero values returned to single-valued continuation""
           ((kontinue-kont proc) #f)
           ;; Otherwise call the procedure and set up error catching.
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
                (let ((request-time (run-request-when run-request)))
                  (if request-time
                      (schedule-add! (agenda-schedule agenda) request-time
                                     (run-request-proc run-request))
                      (enq! next-queue (run-request-proc run-request)))))))
        (define (handle-individual result)
          ;; @@: Could maybe optimize by switching to an explicit cond...
          (match result
            ((? run-request? new-proc)
             (enqueue new-proc))
            ((? read-request? read-request)
             (agenda-handle-read-request! agenda read-request))
            ((? write-request? write-request)
             (agenda-handle-write-request! agenda write-request))
            ;; do nothing
            ;; Remember, we don't throw an error here because procedures can
            ;; return a run request, eg with run-it, at the end of their
            ;; evaluation to keep looping.
            ;; @@: Though is this really a useful feature?
            (_ #f)))
        ;; @@: We might support delay-wrapped procedures here
        (match proc-result
          ((results ...)
           (for-each handle-individual results))
          (one-result (handle-individual one-result)))))
    (set-agenda-queue! agenda next-queue)))

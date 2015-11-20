(define-module (loopy agenda)
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

            run-it wrap run run-at delay

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
                      schedule time)
  agenda?
  (queue agenda-queue)
  (prompt-tag agenda-prompt-tag)
  (read-port-map agenda-read-port-map)
  (write-port-map agenda-write-port-map)
  (except-port-map agenda-except-port-map)
  (schedule agenda-schedule)
  (time agenda-time))

(define (make-async-prompt-tag)
  (make-prompt-tag "prompt"))

(define* (make-agenda #:key
                      (queue (make-q))
                      (prompt (make-prompt-tag))
                      (read-port-map (make-hash-table))
                      (write-port-map (make-hash-table))
                      (except-port-map (make-hash-table))
                      (schedule (make-schedule))
                      (time (gettimeofday)))
  ;; TODO: document arguments
  "Make a fresh agenda."
  (make-agenda-intern queue prompt
                      read-port-map write-port-map except-port-map
                      schedule time))



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

(define* (make-time-delta sec #:optional (usec 0))
  "Make a <time-delta> of SEC seconds and USEC microseconds.

This is used primarily so the agenda can recognize RUN-REQUEST objects
which are meant "
  (make-time-delta-intern sec usec))

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
         (- (cdr time2) (cdr time2)))))

(define (time-plus time1 time2)
  "Add TIME1 and TIME2"
  (time-carry-correct
   (cons (+ (car time1) (car time2))
         (+ (cdr time2) (cdr time2)))))


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

(define-immutable-record-type <run-request>
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

(define-syntax-rule (run body ...)
  "Run everything in BODY but wrap in a convenient procedure"
  (make-run-request (wrap body ...) #f))

(define-syntax-rule (run-at body ... when)
  "Run BODY at WHEN"
  (make-run-request (wrap body ...) when))

(define-syntax-rule (run-delay body ... delay-time)
  "Run BODY at DELAY-TIME time from now"
  (make-run-request (wrap body ...) (tdelta delay-time)))

(define (delay run-request delay-time)
  "Delay a RUN-REQUEST by DELAY-TIME"
  (set-field run-request
             (run-request-when)
             (tdelta delay-time)))


;;; Execution of agenda, and current agenda
;;; =======================================

(define %current-agenda (make-parameter #f))

(define (update-agenda-from-select! agenda)
  "Potentially (select) on ports specified in agenda, adding items to queue"
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
       (select (hash-keys (agenda-read-port-map agenda))
               (hash-keys (agenda-write-port-map agenda))
               (hash-keys (agenda-except-port-map agenda))
               sec usec))))
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

  (if (ports-to-select?)
      (update-agenda)
      agenda))


(define* (start-agenda agenda
                       #:key stop-condition
                       (get-time gettimeofday)
                       (handle-ports update-agenda-from-select!))
  ;; TODO: Document fields
  "Start up the AGENDA"
  (let loop ((agenda agenda))
    (let ((agenda   
           ;; @@: Hm, maybe here would be a great place to handle
           ;;   select'ing on ports.
           ;;   We could compose over agenda-run-once and agenda-read-ports
           (parameterize ((%current-agenda agenda))
             (agenda-run-once agenda))))
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

(define (agenda-run-once agenda)
  "Run once through the agenda, and produce a new agenda
based on the results"
  (define (call-proc proc)
    (call-with-prompt
        (agenda-prompt-tag agenda)
      (lambda ()
        (proc))
      ;; TODO
      (lambda (k) k)))

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
        ;; @@: We might support delay-wrapped procedures here
        (match proc-result
          ;; TODO: replace procedure with something that indicates
          ;;   intent to run.  Use a (run foo) procedure
          ((? run-request? new-proc)
           (enqueue new-proc))
          (((? run-request? new-procs) ...)
           (for-each
            (lambda (new-proc)
              (enqueue new-proc))
            new-procs))
          ;; do nothing
          (_ #f))))
    ;; TODO: Alternately, we could return the next-queue
    ;;   along with changes to be added to the schedule here?
    ;; Return new agenda, with next queue set
    (set-field agenda (agenda-queue) next-queue)))

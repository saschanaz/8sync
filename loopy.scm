(define-module (loopy agenda)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 q)
  #:use-module (ice-9 match)
  #:export (make-agenda
            agenda?
            agenda-queue agenda-prompt-tag
            agenda-port-pmapping agenda-schedule
            
            make-async-prompt-tag

            make-time-segment
            time-segment?
            time-segment-time time-segment-queue

            time-< time-=

            make-schedule
            schedule-add! schedule-empty?

            make-port-mapping
            port-mapping-set! port-mapping-remove!
            port-mapping-empty? port-mapping-non-empty?

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
  (make-agenda-intern queue prompt-tag port-mapping schedule)
  agenda?
  (queue agenda-queue)
  (prompt-tag agenda-prompt-tag)
  (port-mapping agenda-port-mapping)
  (schedule agenda-schedule))

(define (make-async-prompt-tag)
  (make-prompt-tag "prompt"))

(define* (make-agenda #:key
                      (queue (make-q))
                      (prompt (make-prompt-tag))
                      (port-mapping (make-port-mapping))
                      (schedule (make-schedule)))
  (make-agenda-intern queue prompt port-mapping schedule))



;;; Schedule
;;; ========

;;; This is where we handle timed events for the future

;; This section totally borrows from SICP
;; <3 <3 <3

;; NOTE: time is a cons of (seconds . microseconds)

(define-record-type <time-segment>
  (make-time-segment-intern time queue)
  time-segment?
  (time time-segment-time)
  (queue time-segment-queue))

(define (time-segment-right-format time)
  (match time
    ;; time is already a cons of second and microsecnd
    (((? integer? s) (? integer? u)) time)
    ;; time was just an integer (just the second)
    ((? integer? _) (cons time 0))
    (_ (throw 'invalid-time "Invalid time" time))))

(define* (make-time-segment time #:optional (queue (make-q)))
  (make-time-segment-intern time queue))

(define (time-< time1 time2)
  (cond ((< (car time1)
            (car time2))
         #t)
        ((and (= (car time1)
                 (car time2))
              (< (cdr time1)
                 (cdr time2)))
         #t)
        (else #f)))

(define (time-= time1 time2)
  (and (= (car time1) (car time2))
       (= (cdr time1) (cdr time2))))

(define (make-schedule)
  '())

(define (schedule-add! time proc schedule)
  (let ((time (time-segment-right-format time)))
    (define (belongs-before? segments)
      (or (null? segments)
          (error))
    )

  ;; Find and add a schedule segment
  (error)))

(define (schedule-empty? schedule)
  (eq? schedule '()))



;;; Port handling
;;; =============

(define (make-port-mapping)
  (make-hash-table))

(define* (port-mapping-set! port-mapping port #:optional read write except)
  "Sets port-mapping for reader / writer / exception handlers"
  (if (not (or read write except))
      (throw 'no-handlers-given "No handlers given for port" port))
  (hashq-set! port-mapping port
              `#(,read ,write ,except)))

(define (port-mapping-remove! port-mapping port)
  (hashq-remove! port-mapping port))

;; TODO: This is O(n), I'm pretty sure :\
;; ... it might be worthwhile for us to have a
;;   port-mapping record that keeps a count of how many
;;   handlers (maybe via a promise?)
(define (port-mapping-empty? port-mapping)
  "Is this port mapping empty?"
  (eq? (hash-count (const #t) port-mapping) 0))

(define (port-mapping-non-empty? port-mapping)
  "Whether this port-mapping contains any elements"
  (not (port-mapping-empty? port-mapping)))



;;; Execution of agenda, and current agenda
;;; =======================================

(define %current-agenda (make-parameter #f))

(define* (start-agenda agenda #:optional stop-condition)
  (let loop ((agenda agenda))
    (let ((new-agenda   
           ;; @@: Hm, maybe here would be a great place to handle
           ;;   select'ing on ports.
           ;;   We could compose over agenda-run-once and agenda-read-ports
           (parameterize ((%current-agenda agenda))
             (agenda-run-once agenda))))
      (if (and stop-condition (stop-condition agenda))
          'done
          (loop new-agenda)))))

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
              (lambda (new-proc)
                (enq! next-queue new-proc))))
        ;; @@: We might support delay-wrapped procedures here
        (match proc-result
          ((? procedure? new-proc)
           (enqueue new-proc))
          (((? procedure? new-procs) ...)
           (for-each
            (lambda (new-proc)
              (enqueue new-proc))
            new-procs))
          ;; do nothing
          (_ #f))))
    ;; TODO: Selecting on ports would happen here?
    ;; Return new agenda, with next queue set
    (set-field agenda (agenda-queue) next-queue)))

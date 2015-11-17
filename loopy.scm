(use-modules (srfi srfi-9)
             (srfi srfi-9 gnu)
             (ice-9 q)
             (ice-9 match))

;; @@: Using immutable agendas here, so wouldn't it make sense to
;;   replace this

(define-immutable-record-type <agenda>
  (make-agenda-intern queue)
  agenda?
  (queue agenda-queue))

(define* (make-agenda #:key (queue (make-q)))
  (make-agenda-intern queue))

(define %current-agenda (make-parameter #f))

(define* (start-agenda agenda #:optional stop-condition)
  (let loop ((agenda agenda))
    (let ((new-agenda
           (parameterize ((%current-agenda agenda))
             (agenda-run-once agenda))))
      (if (and stop-condition (stop-condition agenda))
          'done
          (loop new-agenda)))))

(define (agenda-run-once agenda)
  "Run once through the agenda, and produce a new agenda
based on the results"
  (let ((queue (agenda-queue agenda))
        (next-queue (make-q)))
    (while (not (q-empty? queue))
      (let* ((proc (q-pop! queue))
             (proc-result (proc))
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
            new-procs)))))
    ;; TODO: Selecting on ports would happen here?
    ;; Return new agenda, with next queue set
    (set-field agenda (agenda-queue) next-queue)))

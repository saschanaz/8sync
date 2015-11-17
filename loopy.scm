(use-modules (srfi srfi-9)
             (srfi srfi-9 gnu)
             (ice-9 q)
             (ice-9 match))

;; @@: Using immutable agendas here, so wouldn't it make sense to
;;   replace this queue stuff with using pfds based immutable queues?

(define-immutable-record-type <agenda>
  (make-agenda-intern queue prompt-tag)
  agenda?
  (queue agenda-queue)
  (prompt-tag agenda-prompt-tag))

(define (make-async-prompt-tag)
  (make-prompt-tag "prompt"))

(define* (make-agenda #:key
                      (queue (make-q))
                      (prompt (make-prompt-tag)))
  (make-agenda-intern queue prompt))

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
            new-procs)))))
    ;; TODO: Selecting on ports would happen here?
    ;; Return new agenda, with next queue set
    (set-field agenda (agenda-queue) next-queue)))

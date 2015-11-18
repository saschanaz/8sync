(use-modules (srfi srfi-9)
             (srfi srfi-9 gnu)
             (ice-9 q)
             (ice-9 match))

;; @@: Using immutable agendas here, so wouldn't it make sense to
;;   replace this queue stuff with using pfds based immutable queues?

(define-immutable-record-type <agenda>
  (make-agenda-intern queue prompt-tag port-mapping)
  agenda?
  (queue agenda-queue)
  (prompt-tag agenda-prompt-tag)
  (port-mapping agenda-port-mapping))

(define (make-async-prompt-tag)
  (make-prompt-tag "prompt"))

(define* (make-agenda #:key
                      (queue (make-q))
                      (prompt (make-prompt-tag))
                      (port-mapping (make-port-mapping)))
  (make-agenda-intern queue prompt port-mapping))

(define (make-port-mapping)
  (make-hash-table))

(define (port-mapping-set! port-mapping port #:optional read write except)
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

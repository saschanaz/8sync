;;; 8sync --- Asynchronous programming for Guile
;;; Copyright © 2017 Christopher Allan Webber <cwebber@dustycloud.org>
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

(define-module (8sync inbox)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 atomic)
  #:export (delivery-agent))

;; TODO: Add backpressure limit
(define (delivery-agent inbox-enq inbox-deq stop?)
  "This starts up a loop doing delivery receiving from INBOX-ENQ and
delivering to INBOX-DEQ, actually managing an internal object QUEUE.
Condidtion variable STOP? can be set to indicate that this agent
should stop."
  (define queue
    (make-q))
  (define get-or-stop
    (choice-operation
     (wrap-operation (get-operation inbox-enq)
                     (lambda (message)
                       (enq! queue message)
                       'got-one))
     (wrap-operation (wait-operation stop?)
                     (const 'stop))))
  (let main-lp ()
    (cond
     ;; No items to deliver?  We need to get one first...
     ((q-empty? queue)
      (match (perform-operation get-or-stop)
        ;; keep looping
        ('got-one (main-lp))
        ;; halt!
        ('stop 'done)))
     (else
      ;; Pull an item off the queue for delivery...
      (let ((this-one (deq! queue)))
        ;; But we need to start looping!  
        (let deliver-this-one ()
          (match (perform-operation
                  (choice-operation
                   ;; get a new message and keep trying to deliver
                   ;; this one, or stop
                   get-or-stop
                   ;; deliver this one and get the next one to deliver
                   (wrap-operation (put-operation inbox-deq this-one)
                                   (const 'delivered))))
            ;; We're dispatching based on which one succeeds.
            ;; Maybe this isn't necessary, but I'm not convinced
            ;; that looping within the choice-operation would be
            ;; properly tail recursive.
            ('got-one (deliver-this-one))
            ('delivered (main-lp))
            ('stop 'done))))))))

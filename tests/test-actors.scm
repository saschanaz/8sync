;;; 8sync --- Asynchronous programming for Guile
;;; Copyright (C) 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright (C) 2021 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (tests test-actors)
  #:use-module (srfi srfi-64)
  #:use-module (8sync actors)
  #:use-module (fibers)
  #:use-module (fibers conditions)
  #:use-module (fibers operations)
  #:use-module (fibers timers)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (tests utils))

(test-begin "test-actors")


;;; Test waiting on a response

(define-actor <Responder> (<actor>)
  ((ping-pong
    (lambda (actor message ping-val)
      ;; Returns two values to its continuation: its ping-val, and the
      ;; value of its own response-val slot.
      (values (.response-val actor) ping-val))))
  (response-val #:init-keyword #:response-val
                #:accessor .response-val))

(define-actor <Requester> (<actor>)
  ((run requester-run))
  (done #:init-keyword #:done
        #:accessor .done)
  (test-box #:init-keyword #:test-box
            #:accessor .test-box))

(define (requester-run requester m)
  (define beeper
    (create-actor <Responder> #:response-val 'beep))
  (define booper
    (create-actor <Responder> #:response-val 'boop))
  (define bopper
    (create-actor <Responder> #:response-val 'bop))
  (define borker
    (create-actor <Responder> #:response-val 'bork))
  (receive (v1 v2)
      (<-wait beeper 'ping-pong 1)
    (test-equal v1 'beep)
    (test-equal v2 1))
  (receive (v1 v2)
      (<-wait booper 'ping-pong 2)
    (test-equal v1 'boop)
    (test-equal v2 2))
  (receive (v1 v2)
      (<-wait bopper 'ping-pong 3)
    (test-equal v1 'bop)
    (test-equal v2 3))
  (receive (v1 v2)
      (<-wait borker 'ping-pong 4)
    (test-equal v1 'bork)
    (test-equal v2 4))
  (atomic-box-set! (.test-box requester) 'we-did-it)
  (signal-condition! (.done requester)))

(let ((test-box (make-atomic-box 'not-yet))
      (done (make-condition)))
  (run-hive
   (lambda (hive)
     (define requester (create-actor <Requester>
                                     #:done done
                                     #:test-box test-box))
     (<- requester 'run)
     (perform-operation
      (choice-operation (wait-operation done)
                        ;; if somehow this times out after 5 seconds,
                        ;; something is deeply wrong
                        (sleep-operation 5)))
     (test-equal (atomic-box-ref test-box)
       'we-did-it))))

(test-exit)
(test-end "test-actors")

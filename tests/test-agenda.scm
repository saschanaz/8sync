;;; 8sync --- Asynchronous programming for Guile
;;; Copyright (C) 2015 Christopher Allan Webber <cwebber@dustycloud.org>
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA

(define-module (tests test-agenda)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 q)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (8sync agenda)
  #:use-module (tests utils))

(test-begin "test-agenda")

(define-syntax-rule (%import var)
  (define var
    (@@ (8sync agenda) var)))


;;; queue helpers
;;; =============

(define test-q (list->q '(1 2 3)))
(test-equal (deq! test-q) 1)
(test-equal (deq! test-q) 2)
(test-equal (deq! test-q) 3)
(test-assert (q-empty? test-q))

(define test-q (make-q* 'apple 'banana 'carrot))
(test-equal (deq! test-q) 'apple)
(test-equal (deq! test-q) 'banana)
(test-equal (deq! test-q) 'carrot)
(test-assert (q-empty? test-q))



;;; Timer tests
;;; ===========

(%import time=)
(%import time<)
(%import time-minus)
(%import time-plus)

(test-assert (time= '(1 . 1) '(1 . 1)))
(test-assert (not (time= '(1 . 1) '(1 . 0))))
(test-assert (not (time= '(0 . 1) '(1 . 1))))

(test-assert (time< '(1 . 1) '(1 . 2)))
(test-assert (time< '(7 . 2) '(8 . 2)))
(test-assert (not (time< '(7 . 2) '(7 . 2))))
(test-assert (not (time< '(7 . 8) '(7 . 2))))
(test-assert (not (time< '(8 . 2) '(7 . 2))))

(test-equal (time-minus '(100 . 100) '(50 . 66))
            '(50 . 34))
(test-equal (time-minus '(2 . 0) '(0 . 1))
            '(1 . 999999))

(test-equal (time-plus '(50 . 34) '(50 . 66))
            '(100 . 100))
(test-equal (time-plus '(1 . 999999) '(1 . 2))
            '(3 . 1))



;;; Schedule tests
;;; ==============

(%import time-segment-time)
(%import time-segment-queue)

;; helpers
(define (assert-times-expected time-segments expected-times)
  (test-equal (map time-segment-time time-segments)
    expected-times))

(define a-proc (const 'a))
(define b-proc (const 'b))
(define c-proc (const 'c))
(define d-proc (const 'd))
(define e-proc (const 'e))
(define f-proc (const 'f))

(define sched (make-schedule))
(test-assert (schedule-empty? sched))

;; Add a segment at (10 . 0)
(schedule-add! sched '(10 . 0) a-proc)
(test-assert (not (schedule-empty? sched)))
(test-equal (length (schedule-segments sched)) 1)
(test-equal (time-segment-time (car (schedule-segments sched)))
  '(10 . 0))
(test-equal (q-length (time-segment-queue (car (schedule-segments sched))))
  1)
(test-eq (q-front (time-segment-queue (car (schedule-segments sched))))
  a-proc)
(test-eq (q-rear (time-segment-queue (car (schedule-segments sched))))
  a-proc)
(test-eq ((q-front (time-segment-queue (car (schedule-segments sched)))))
  'a) ;; why not
(assert-times-expected (schedule-segments sched)
                       '((10 . 0)))

;; Add another segment at (10 . 0)
(schedule-add! sched '(10 . 0) b-proc)
(test-assert (not (schedule-empty? sched)))
(test-equal (length (schedule-segments sched)) 1)
(test-equal (time-segment-time (car (schedule-segments sched)))
  '(10 . 0))
(test-equal (q-length (time-segment-queue (car (schedule-segments sched))))
  2)
(test-eq (q-front (time-segment-queue (car (schedule-segments sched))))
  a-proc)
(test-eq (q-rear (time-segment-queue (car (schedule-segments sched))))
  b-proc)
(assert-times-expected (schedule-segments sched)
                       '((10 . 0)))

;; Add a segment to (11 . 0), (8 . 1) and (10 . 10)
(schedule-add! sched '(11 . 0) c-proc)
(schedule-add! sched '(8 . 1) d-proc)
(schedule-add! sched '(10 . 10) e-proc)
(test-assert (not (schedule-empty? sched)))
(test-equal (length (schedule-segments sched)) 4)
(assert-times-expected (schedule-segments sched)
                       '((8 . 1) (10 . 0) (10 . 10) (11 . 0)))

;; Splitting 
(define (test-split-at schedule time expected-before expected-after)
  (receive (segments-before segments-after)
      (schedule-segments-split schedule time)
    (assert-times-expected segments-before expected-before)
    (assert-times-expected segments-after expected-after)))

(test-split-at sched '(0 . 0)
               '()
               '((8 . 1) (10 . 0) (10 . 10) (11 . 0)))
(test-split-at sched '(8 . 0)
               '()
               '((8 . 1) (10 . 0) (10 . 10) (11 . 0)))
(test-split-at sched '(8 . 1)
               '((8 . 1))
               '((10 . 0) (10 . 10) (11 . 0)))
(test-split-at sched '(9 . 0)
               '((8 . 1))
               '((10 . 0) (10 . 10) (11 . 0)))
(test-split-at sched '(10 . 0)
               '((8 . 1) (10 . 0))
               '((10 . 10) (11 . 0)))
(test-split-at sched '(9000 . 0)
               '((8 . 1) (10 . 0) (10 . 10) (11 . 0))
               '())
(test-split-at sched '(9000 . 1)    ; over nine thousaaaaaaand
               '((8 . 1) (10 . 0) (10 . 10) (11 . 0))
               '())

;; Break off half of those and do some tests on them
(define some-extracted
  (schedule-extract-until! sched '(10 . 0)))
(assert-times-expected some-extracted '((8 . 1) (10 . 0)))
(assert-times-expected (schedule-segments sched) '((10 . 10) (11 . 0)))
(define first-extracted-queue
  (time-segment-queue (car some-extracted)))
(define second-extracted-queue
  (time-segment-queue (cadr some-extracted)))
(test-assert (not (q-empty? first-extracted-queue)))
(test-equal ((deq! first-extracted-queue)) 'd)
(test-assert (q-empty? first-extracted-queue))

(test-assert (not (q-empty? second-extracted-queue)))
(test-equal ((deq! second-extracted-queue)) 'a)
(test-equal ((deq! second-extracted-queue)) 'b)
(test-assert (q-empty? second-extracted-queue))

;; Add one more and test flattening to a queue
(test-assert (not (schedule-empty? sched)))
(schedule-add! sched '(10 . 10) f-proc)
(define remaining-segments
  (schedule-extract-until! sched '(9000 . 1)))
(test-assert (schedule-empty? sched))
(define some-queue (make-q))
(enq! some-queue (const 'ho-ho))
(enq! some-queue (const 'ha-ha))
(add-segments-contents-to-queue! remaining-segments some-queue)
(test-assert (not (q-empty? some-queue)))
(test-equal 'ho-ho ((deq! some-queue)))
(test-equal 'ha-ha ((deq! some-queue)))
(test-equal 'e ((deq! some-queue)))
(test-equal 'f ((deq! some-queue)))
(test-equal 'c ((deq! some-queue)))
(test-assert (q-empty? some-queue))

;; ... whew!

;;; Run/wrap request stuff
;;; ======================

(let ((wrapped (wrap (+ 1 2))))
  (test-assert (procedure? wrapped))
  (test-equal (wrapped) 3))

(let ((run-two-squared (run-it (lambda () (* 2 2)))))
  (test-assert (run-request? run-two-squared))
  (test-assert (procedure? (run-request-proc run-two-squared)))
  (test-equal ((run-request-proc run-two-squared)) 4)
  (test-eq (run-request-when run-two-squared) #f))

(let ((run-two-squared (run-it (lambda () (* 2 2)) '(88 . 0))))
  (test-assert (run-request? run-two-squared))
  (test-assert (procedure? (run-request-proc run-two-squared)))
  (test-equal ((run-request-proc run-two-squared)) 4)
  (test-equal (run-request-when run-two-squared) '(88 . 0)))

(let ((run-two-squared (run (* 2 2))))
  (test-assert (run-request? run-two-squared))
  (test-assert (procedure? (run-request-proc run-two-squared)))
  (test-equal ((run-request-proc run-two-squared)) 4)
  (test-eq (run-request-when run-two-squared) #f))

(let ((run-two-squared (run-at (* 2 2) '(88 . 0))))
  (test-assert (run-request? run-two-squared))
  (test-assert (procedure? (run-request-proc run-two-squared)))
  (test-equal ((run-request-proc run-two-squared)) 4)
  (test-equal (run-request-when run-two-squared) '(88 . 0)))


;;; %run, 8sync and friends tests
;;; ==============================

;; TODO: We need to rewrite the whole lot here...

;;; Agenda tests
;;; ============

;; helpers

(define (true-after-n-times n)
  (let ((count 0))
    (lambda _
      (define ans
        (if (>= count n) #t #f))
      (set! count (+ count 1))
      ans)))

;; the dummy test

(define (dummy-func)
  (speak "I'm a dummy\n"))

(define (run-dummy)
  (speak "I bet I can make you say you're a dummy!\n")
  (run-it dummy-func))

(with-fresh-speaker
 (run-agenda (make-agenda #:queue (make-q* run-dummy))
             #:stop-condition (true-after-n-times 2))
 (test-equal (get-spoken)
   '("I bet I can make you say you're a dummy!\n"
     "I'm a dummy\n")))

;; should only do the first one after one round though
(with-fresh-speaker
 (run-agenda (make-agenda #:queue (make-q* run-dummy))
             #:stop-condition (true-after-n-times 1))
 (test-equal (get-spoken)
   '("I bet I can make you say you're a dummy!\n")))


;; End tests

(test-end "test-agenda")

;; @@: A better way to handle this at the repl?
(test-exit)


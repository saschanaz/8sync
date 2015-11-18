#!/usr/bin/guile \
-s
!#

(define-module (tests test-core)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 q)
  #:use-module (loopy agenda))

(test-begin "tests")


;; Timer tests
;; ===========

(test-assert (time-= '(1 . 1) '(1 . 1)))
(test-assert (not (time-= '(1 . 1) '(1 . 0))))
(test-assert (not (time-= '(0 . 1) '(1 . 1))))

(test-assert (time-< '(1 . 1) '(1 . 2)))
(test-assert (time-< '(7 . 2) '(8 . 2)))
(test-assert (not (time-< '(7 . 2) '(7 . 2))))
(test-assert (not (time-< '(7 . 8) '(7 . 2))))
(test-assert (not (time-< '(8 . 2) '(7 . 2))))



;;; Schedule tests
;;; ==============

;; helpers
(define a-proc (const 'a))
(define b-proc (const 'b))
(define c-proc (const 'c))

(define sched (make-schedule))
(test-assert (schedule-empty? sched))

;; Add a segment at (10 . 0)
(schedule-add! 10 a-proc sched)
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

;; Add another segment at (10 . 0)
(schedule-add! '(10 . 0) b-proc sched)
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

;; Add a segment to (11 . 0), (8 . 1) and (10 . 10)
(schedule-add! 11 a-proc sched)
(schedule-add! '(8 . 1) c-proc sched)
(schedule-add! '(10 . 10) c-proc sched)
(test-assert (not (schedule-empty? sched)))
(test-equal (length (schedule-segments sched)) 4)
(test-equal (time-segment-time (car (schedule-segments sched)))
  '(8 . 1))
(test-equal (time-segment-time (cadr (schedule-segments sched)))
  '(10 . 0))
(test-equal (time-segment-time (caddr (schedule-segments sched)))
  '(10 . 10))
(test-equal (time-segment-time (cadddr (schedule-segments sched)))
  '(11 . 0))


;; End tests

(test-end "tests")
;; (test-exit)


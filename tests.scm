#!/usr/bin/guile \
-s
!#

(define-module (tests test-core)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 q)
  #:use-module (ice-9 receive)
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
(test-eq ((q-front (time-segment-queue (car (schedule-segments sched)))))
  'a) ;; why not
(assert-times-expected (schedule-segments sched)
                       '((10 . 0)))

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
(assert-times-expected (schedule-segments sched)
                       '((10 . 0)))

;; Add a segment to (11 . 0), (8 . 1) and (10 . 10)
(schedule-add! 11 c-proc sched)
(schedule-add! '(8 . 1) d-proc sched)
(schedule-add! '(10 . 10) e-proc sched)
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

(test-split-at sched 0
               '()
               '((8 . 1) (10 . 0) (10 . 10) (11 . 0)))
(test-split-at sched '(8 . 0)
               '()
               '((8 . 1) (10 . 0) (10 . 10) (11 . 0)))
(test-split-at sched '(8 . 1)
               '((8 . 1))
               '((10 . 0) (10 . 10) (11 . 0)))
(test-split-at sched 9
               '((8 . 1))
               '((10 . 0) (10 . 10) (11 . 0)))
(test-split-at sched 10
               '((8 . 1) (10 . 0))
               '((10 . 10) (11 . 0)))
(test-split-at sched 9000
               '((8 . 1) (10 . 0) (10 . 10) (11 . 0))
               '())
(test-split-at sched '(9000 . 1)    ; over nine thousaaaaaaand
               '((8 . 1) (10 . 0) (10 . 10) (11 . 0))
               '())

;; End tests

(test-end "tests")
;; (test-exit)


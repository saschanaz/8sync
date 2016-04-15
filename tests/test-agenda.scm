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

#!/usr/bin/guile \
-s
!#

(define-module (tests test-agenda)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 q)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (8sync agenda)
  #:use-module (tests utils))

(test-begin "test-agenda")



;;; Helpers
;;; =======

(define (speak-it)
  (let ((messages '()))
    (lambda* (#:optional message)
      (if message (set! messages (append messages (list message))))
      messages)))


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

(test-assert (time= '(1 . 1) '(1 . 1)))
(test-assert (not (time= '(1 . 1) '(1 . 0))))
(test-assert (not (time= '(0 . 1) '(1 . 1))))

(test-assert (time< '(1 . 1) '(1 . 2)))
(test-assert (time< '(7 . 2) '(8 . 2)))
(test-assert (not (time< '(7 . 2) '(7 . 2))))
(test-assert (not (time< '(7 . 8) '(7 . 2))))
(test-assert (not (time< '(8 . 2) '(7 . 2))))

(let ((tdelta (make-time-delta 8)))
  (test-assert (time-delta? tdelta))
  (test-eqv (time-delta-sec tdelta) 8)
  (test-eqv (time-delta-usec tdelta) 0)
  (test-equal
      (time-delta+ '(2 . 3) tdelta)
    '(10 . 3)))

(let ((tdelta (make-time-delta '(10 . 1))))
  (test-assert (time-delta? tdelta))
  (test-eqv (time-delta-sec tdelta) 10)
  (test-eqv (time-delta-usec tdelta) 1)
  (test-equal
      (time-delta+ '(2 . 3) tdelta)
    '(12 . 4)))

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
(schedule-add! sched 10 a-proc)
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
(schedule-add! sched 11 c-proc)
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

;; Break off half of those and do some tests on them
(define some-extracted
  (schedule-extract-until! sched 10))
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

(define-syntax-rule (run-in-fake-agenda
                     code-to-run)
  (let ((agenda (make-agenda)))
    (parameterize ((%current-agenda agenda))
      (call-with-prompt
       (agenda-prompt-tag agenda)
       (lambda ()
         (list '*normal-result* code-to-run))
       (lambda (kont async-request)
         (list '*caught-kont*
               kont async-request
               ((@@ (8sync agenda) setup-async-request)
                kont async-request)))))))

(define (test-%run-and-friends run-result expected-when)
  (match run-result
    (('*caught-kont* kont async-request setup-request)
     (let* ((fake-kont (speak-it))
            (run-request ((@@ (8sync agenda) setup-async-request)
                          fake-kont async-request)))
       (test-equal (car async-request) '*async-request*)
       (test-equal (run-request-when run-request) expected-when)
       ;; we're using speaker as a fake continuation ;p
       ((run-request-proc run-request))
       (test-equal (fake-kont)
                   '("applesauce"))))))

(test-%run-and-friends (run-in-fake-agenda
                        (8sync (string-concatenate '("apple" "sauce"))))
                       #f)

(test-%run-and-friends (run-in-fake-agenda
                        (8sync (string-concatenate '("apple" "sauce"))
                                '(8 . 0)))
                       '(8 . 0))

(test-%run-and-friends (run-in-fake-agenda
                        (8sync-delay (string-concatenate '("apple" "sauce"))
                                      8))
                       ;; whoa, I'm surprised equal? can
                       ;; compare records like this
                       (tdelta 8))

;; TODO: test %port-request
;; TODO: test 8sync and friends!


;;; Agenda tests
;;; ============

;; helpers

(define (true-after-n-times n)
  (let ((count 0))
    (lambda _
      (set! count (+ count 1))
      (if (>= count n) #t #f))))

;; the dummy test

(define speaker (speak-it))

(define (dummy-func)
  (speaker "I'm a dummy\n"))

(define (run-dummy)
  (speaker "I bet I can make you say you're a dummy!\n")
  (run-it dummy-func))

(begin
  (set! speaker (speak-it))  ; reset the speaker
  (start-agenda (make-agenda #:queue (make-q* run-dummy))
                #:stop-condition (true-after-n-times 2))
  (test-equal (speaker)
    '("I bet I can make you say you're a dummy!\n"
      "I'm a dummy\n")))

;; should only do the first one after one round though
(begin
  (set! speaker (speak-it))  ; reset the speaker
  (start-agenda (make-agenda #:queue (make-q* run-dummy))
                #:stop-condition (true-after-n-times 1))
  (test-equal (speaker)
    '("I bet I can make you say you're a dummy!\n")))

;; delimited continuation tests

(define (return-monkey)
  (speaker "(Hint, it's a monkey...)\n")
  'monkey)

(define (talk-about-the-zoo)
  (speaker "Today I went to the zoo and I saw...\n")
  (speaker
   (string-concatenate
    `("A " ,(symbol->string (8sync (return-monkey))) "!\n"))))

(begin
  (set! speaker (speak-it))
  ;; (enq! q talk-about-the-zoo-but-wait)
  (start-agenda (make-agenda #:queue (make-q* talk-about-the-zoo))
                #:stop-condition (true-after-n-times 10))
  (test-equal (speaker)
              '("Today I went to the zoo and I saw...\n"
                "(Hint, it's a monkey...)\n"
                "A monkey!\n")))


;; Error handling tests
;; --------------------

(define (remote-func-breaks)
  (speaker "Here we go...\n")
  (+ 1 2 (/ 1 0))
  (speaker "SHOULD NOT HAPPEN\n"))

(define (indirection-remote-func-breaks)
  (speaker "bebop\n")
  (8sync (remote-func-breaks))
  (speaker "bidop\n"))

(define* (local-func-gets-break #:key with-indirection)
  (speaker "Time for exception fun!\n")
  (let ((caught-exception #f))
    (catch-8sync
     (8sync-run (if with-indirection
                         (indirection-remote-func-breaks)
                         (remote-func-breaks)))
      ('numerical-overflow
       (lambda (orig-stacks . orig-args)
         (set! caught-exception #t)
         (speaker "in here now!\n")
         (test-equal orig-args '("/" "Numerical overflow" #f #f))
         (test-assert (list? orig-stacks))
         (test-equal (length orig-stacks)
                     (if with-indirection 2 1))
         (for-each
          (lambda (x)
            (test-assert (stack? x)))
          orig-stacks))))
    (test-assert caught-exception))
  (speaker "Well that was fun :)\n"))


(begin
  (set! speaker (speak-it))
  (start-agenda (make-agenda #:queue (make-q* local-func-gets-break))
                #:stop-condition (true-after-n-times 10))
  (test-equal (speaker)
              '("Time for exception fun!\n"
                "Here we go...\n"
                "in here now!\n"
                "Well that was fun :)\n")))

(begin
  (set! speaker (speak-it))
  (start-agenda (make-agenda
                 #:queue (make-q* (wrap (local-func-gets-break
                                         #:with-indirection #t))))
                #:stop-condition (true-after-n-times 10))
  (test-equal (speaker)
              '("Time for exception fun!\n"
                "bebop\n"
                "Here we go...\n"
                "in here now!\n"
                "Well that was fun :)\n")))

;; Make sure catching tools work

(let ((speaker (speak-it))
      (catch-result #f))
  (catch-8sync
   (begin
     (speaker "hello")
     (throw '8sync-caught-error
            'my-orig-key '(apple orange banana) '(*fake-stack* *fake-stack* *fake-stack*))
     (speaker "no goodbyes"))
   ('some-key
    (lambda (stacks . rest)
      (speaker "should not happen")))
   ('my-orig-key
    (lambda (stacks fruit1 fruit2 fruit3)
      (set! catch-result
            `((fruit1 ,fruit1)
              (fruit2 ,fruit2)
              (fruit3 ,fruit3))))))
  (test-equal (speaker) '("hello"))
  (test-equal catch-result '((fruit1 apple)
                             (fruit2 orange)
                             (fruit3 banana))))

;; End tests

(test-end "test-agenda")

;; @@: A better way to handle this at the repl?
(test-exit)


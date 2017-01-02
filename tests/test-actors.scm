;;; 8sync --- Asynchronous programming for Guile
;;; Copyright (C) 2016 Christopher Allan Webber <cwebber@dustycloud.org>
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
  #:use-module (8sync agenda)
  #:use-module (oop goops)
  #:use-module (tests utils))

(test-begin "test-actors")


;;; Test writing things to here
(define %record-out (make-parameter (open-output-string)))
(define (~display str)
  (display str (%record-out)))
(define-syntax-rule (~format args ...)
  (format (%record-out) args ...))

;;; Some test dummy values
;;; ======================

(define %fake-hive-id "the-main-hive")
;; Some fake ids for actors
(define %fake-emo-id (make-address "emo" %fake-hive-id))
(define %fake-proog-id (make-address "proog" %fake-hive-id))
(define %fake-hive-actor-id (make-address "hive" %fake-hive-id))

(define test-message
  (make-message ((simple-message-id-generator))
                %fake-emo-id
                %fake-hive-actor-id ; Bootstrap messages come from the hive
                'greet-proog `(#:target ,%fake-proog-id)))

;;; Actor utilities
;;; ===============

;;; Message tests
;;; =============

;; Make sure our test message serializes and deserializes okay

(let ((reread-message
       (read-message-from-string
        (with-output-to-string
          (lambda () (write-message test-message))))))
  (test-assert (message? reread-message))
  ;; Make sure that all the properties are the same from
  ;; the original message to the re-read message
  (for-each
   (lambda (getter)
     (test-equal (getter test-message) (getter reread-message)))
   (list message-id message-to message-from message-action message-body
         message-in-reply-to message-wants-reply
         (@@ (8sync actors) message-replied))))


;;; Test reply / autoreply
;;; ======================

(define-simple-actor <antsy-caller>
  (pester-rep (wrap-apply antsy-caller-pester-rep)))

(define* (antsy-caller-pester-rep actor message #:key who-to-call)
  (~display "customer> I'm calling customer service about this!\n")
  (msg-receive (first-reply #:key msg)
      (<-wait actor who-to-call 'field-call)
    (if (message-auto-reply? first-reply)
        (~display "customer> Whaaaaat?  I can't believe I got voice mail!\n")
        (begin
          (~format "*customer hears*: ~a\n" msg)
          (msg-receive (second-reply #:key *auto-reply*)
              (<-reply-wait actor first-reply
                            #:msg "Yes, it didn't work, I'm VERY ANGRY!")
            (if (message-auto-reply? second-reply)
                (~display "customer> Well then!  Harumph.\n")
                (error "Not an autoreply?  What's going on here...")))))))

(define-simple-actor <diligent-rep>
  (field-call (wrap-apply rep-field-call)))

(define (rep-field-call actor message)
  (~display "good-rep> Hm, another call from a customer...\n")
  (msg-receive (reply #:key msg)
      (<-reply-wait
       actor message
       #:msg "Have you tried turning it off and on?")
    (~format "*rep hears*: ~a\n" msg)
    (~display "good-rep> I'm sorry, that's all I can do for you.\n")))

(define-simple-actor <lazy-rep>
  (field-call
   (lambda (actor message)
     (~display "lazy-rep> I'm not answering that.\n"))))

(let* ((hive (make-hive))
       (customer (hive-create-actor* hive <antsy-caller> "antsy-caller"))
       (diligent-rep (hive-create-actor* hive <diligent-rep> "diligent-rep"))
       (lazy-rep (hive-create-actor* hive <lazy-rep> "lazy-rep")))
  ;; * Playing a tape of a diligent service rep *
  (parameterize ((%record-out (open-output-string)))
    (let* ((result (run-hive
                    hive
                    (list (bootstrap-message hive customer 'pester-rep
                                             #:who-to-call diligent-rep))))
           (displayed-text (get-output-string (%record-out))))
      (test-equal "customer> I'm calling customer service about this!
good-rep> Hm, another call from a customer...
*customer hears*: Have you tried turning it off and on?
*rep hears*: Yes, it didn't work, I'm VERY ANGRY!
good-rep> I'm sorry, that's all I can do for you.
customer> Well then!  Harumph.\n"
        displayed-text)))
  ;; * Playing a tape of a lazy service rep *
  (parameterize ((%record-out (open-output-string)))
    (let* ((result (run-hive
                    hive
                    (list (bootstrap-message hive customer 'pester-rep
                                                  #:who-to-call lazy-rep))))
           (displayed-text (get-output-string (%record-out))))
      (test-equal "customer> I'm calling customer service about this!
lazy-rep> I'm not answering that.
customer> Whaaaaat?  I can't believe I got voice mail!\n"
          displayed-text))))


;;; Cleanup tests

(define-simple-actor <cleanly>
  (*clean-up* test-call-clean-up))

(define (test-call-clean-up actor message)
  (speak "Hey, I'm cleanin' up here!\n"))

(with-fresh-speaker
 (let ((hive (make-hive)))
   (hive-create-actor hive <cleanly>)
   (run-hive hive '()))
 (test-equal '("Hey, I'm cleanin' up here!\n")
   (get-spoken)))

;; won't work if we turn off #:clean-up though

(with-fresh-speaker
 (let ((hive (make-hive)))
   (hive-create-actor hive <cleanly>)
   (run-hive hive '() #:clean-up #f))
 (test-equal '("Hey, I'm cleanin' up here!\n")
   (get-spoken)))

;; The exploder self-destructs, even though run-hive has clean-up
;; disabled, because it cleans up on self-destruct.

(with-fresh-speaker
 (let ((hive (make-hive)))
   (define exploder (hive-create-actor hive <exploder>))
   (run-hive hive (list (bootstrap-message hive exploder 'explode))
             #:clean-up #f))
 (get-spoken))


(test-end "test-actors")
(test-exit)

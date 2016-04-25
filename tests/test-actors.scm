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
  #:use-module (8sync systems actors)
  #:use-module (tests utils))

(test-begin "test-actors")


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
                'greet-proog `((target . ,%fake-proog-id))))

;;; Actor utilities
;;; ===============

;;; Message tests
;;; =============

(let ((monkey-message
       (make-message 'id 'to 'from 'action
                     '((monkey . banana)))))
  ;; A key we have
  (test-equal (message-ref monkey-message 'monkey)
    'banana)

  ;; A key we don't have
  (let ((caught-error #f))
    (catch 'message-missing-key
      (lambda ()
        (message-ref monkey-message 'coo-coo))
      (lambda (. args)
        (set! caught-error #t)))
    (test-assert caught-error))

  ;; A key we don't have, with a default set
  (test-equal (message-ref monkey-message 'coo-coo 'danger-danger)
    'danger-danger))


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
         (@@ (8sync systems actors) message-replied)
         (@@ (8sync systems actors) message-deferred-reply))))

(test-end "test-actors")
(test-exit)

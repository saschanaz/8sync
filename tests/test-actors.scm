(define-module (tests test-actors)
  #:use-module (srfi srfi-64)
  #:use-module (8sync systems actors)
  #:use-module (tests utils))

(test-begin "test-actors")


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
  (test-equal (message-ref monkey-message 'coo-coo)
    #f)

  ;; A key we don't have, with a default set
  (test-equal (message-ref monkey-message 'coo-coo 'danger-danger)
    'danger-danger))


(test-end "test-agenda")
(test-exit)

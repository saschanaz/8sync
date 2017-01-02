;;; Copyright © 2015 David Thompson <davet@gnu.org>
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
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;; Thanks for giving permission to license this under LGPL for
;; consistency, David!


(define-module (tests utils)
  #:use-module (srfi srfi-64)
  #:export (test-exit

            speak get-spoken with-fresh-speaker))

(define (test-exit)
  (exit (= (test-runner-fail-count (test-runner-current)) 0)))




;;; display-like helpers
;;; ====================

(define (speak-it)
  (let ((messages '()))
    (lambda* (#:optional message)
      (if message (set! messages (append messages (list message))))
      messages)))

(define %speaker (make-parameter (speak-it)))

(define (speak message)
  "Speak a message into the %speaker parameter"
  ((%speaker) message))

(define (get-spoken)
  "Get what's been spoken in the %speaker parameter"
  ((%speaker)))

(define-syntax-rule (with-fresh-speaker body ...)
  "Run body with a fresh %speaker"
  (parameterize ((%speaker (speak-it)))
    body ...))

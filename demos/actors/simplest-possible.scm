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

(use-modules (8sync systems actors)
             (oop goops))

(define-simple-actor <emo>
  (greet-proog
   (lambda (actor message)
     (display "emo> What's next, Proog?\n")
     (send-message
      actor (message-ref message 'target)
      'greet-emo))))

(define-simple-actor <proog>
  (greet-emo
   (lambda (actor message)
     (display "proog> Listen, Emo!  Listen to the sounds of the machine!\n"))))

(define hive (make-hive))
(define our-emo (hive-create-actor hive <emo>))
(define our-proog (hive-create-actor hive <proog>))
(define (main . args)
  (ez-run-hive hive
               (list (hive-bootstrap-message hive our-emo 'greet-proog
                                             #:target our-proog))))

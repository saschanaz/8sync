;;; 8sync --- Asynchronous programming for Guile
;;; Copyright © 2016, 2017 Christopher Allan Webber <cwebber@dustycloud.org>
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

(use-modules (8sync actors)
             (oop goops))

(define-actor <emo> (<actor>)
  ((greet-proog
    (lambda (actor message target)
      (display "emo> What's next, Proog?\n")
      (<- target 'greet-emo)))))

(define-actor <proog> (<actor>)
  ((greet-emo
    (lambda (actor message)
      (display "proog> Listen, Emo!  Listen to the sounds of the machine!\n")))))

(define hive (make-hive))
(define our-emo (bootstrap-actor hive <emo>))
(define our-proog (bootstrap-actor hive <proog>))
(define (main . args)
  (run-hive hive
            (list (bootstrap-message hive our-emo 'greet-proog
                                     our-proog))))

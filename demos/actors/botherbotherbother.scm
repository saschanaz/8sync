#!/usr/bin/guile \
-e main -s
!#

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

;; Puppet show simulator.

(use-modules (8sync agenda)
             (8sync systems actors)
             (oop goops)
             (ice-9 hash-table)
             (ice-9 format))

(set! *random-state* (random-state-from-platform))
(define (random-choice lst)
  (list-ref lst (random (length lst))))


(define student-names
  '("Henry" "Harmony" "Rolf"))

(define (student-name-generator)
  ;; a hashmap of student names with their current count
  (define student-count (make-hash-table))
  (lambda ()
    (let* ((student (random-choice student-names))
           (current-number (hash-ref student-count student 1)))
      (hash-set! student-count student (1+ current-number))
      (format #f "~a-~a" student current-number))))


(define-class <student> (<actor>)
  (name #:init-keyword #:name)
  (dead #:init-value #f
        #:accessor student-dead)
  (message-handler
   #:init-value
   (make-action-dispatch
    (bother-professor
     (lambda (actor message)
       "Go bother a professor"
       (while (not (student-dead actor))
         (format #t "~a: Bother bother bother!\n"
                 (actor-id-actor actor))
         (send-message
          actor (message-ref message 'target)
          'be-bothered
          #:noise "Bother bother bother!\n"))))

    (be-lambda-consvardraed
     (lambda (actor message)
       "This kills the student."
       (format #t "~a says: AAAAAAAHHHH!!! I'm dead!\n"
               (actor-id-actor actor))
       (set! (student-dead actor) #t))))))

(define complaints
  '("Hey!" "Stop that!" "Oof!"))

(define (professor-be-bothered actor message)
  (define whos-bothering (professor-bothered-by actor))

  (hash-set! whos-bothering (message-from message) #t)

  ;; Oof!  Those kids!
  (display (string-append (random-choice complaints)))

  ;; More than one student is bothering us, lose our temper
  (if (> (hash-count (const #t) whos-bothering)
         1)
      (begin
        (format #t "~s: LAMBDA CONSVARDRA!\n"
                (actor-id actor))
        (hash-for-each
         (lambda (student _)
           (send-message
            actor student
            'be-lambda-consvardraed)
           ;; Remove student from bothering list
           (hash-remove! whos-bothering student))
         whos-bothering))
      ;; Otherwise, remove them from the list and carry on
      (hash-remove! whos-bothering (message-from message))))

(define-class <professor> (<actor>)
  ;; This value checks whether any other actor is currently
  ;; bothering this same character.
  ;; We'll use a hash table as a fake set.
  (bothered-by #:init-thunk make-hash-table
               #:getter professor-bothered-by)
  (message-handler
   #:init-value
   (make-action-dispatch
    (be-bothered professor-be-bothered))))

(define num-students 10)

(define (main . args)
  (define agenda (make-agenda))
  (define hive (make-hive))
  (define professor (hive-create-actor* hive <professor> "prof"))
  (define namegen (student-name-generator))
  (define students
    (map
     (lambda _
       (let ((name (namegen)))
         (hive-create-actor* hive <student> name
                             #:name name)))
     (iota num-students)))

  ;; Bootstrap each student into bothering-professor mode.
  (define start-bothering-tasks
    (map
     (lambda (student)
       (bootstrap-message hive student 'bother-professor
                               #:target professor))
     students))

  (ez-run-hive hive start-bothering-tasks))

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

(define-module (8sync systems actors debug)
  #:use-module (oop goops)
  #:use-module (8sync systems actors)
  #:export (hive-resolve-local-actor
            hive-create-actor-gimmie))

(define hive-resolve-local-actor
  (@@ (8sync systems actors) hive-resolve-local-actor))

(define (hive-create-actor-gimmie . args)
  "Create an actor on the hive, and give us that actor.
Uses hive-create-actor* arguments."
  (let ((actor-id (apply hive-create-actor args)))
    (hive-resolve-local-actor hive actor-id)))

(define-syntax-rule (hive-create-actor-gimmie* args ...)
  "Create an actor on the hive, and give us that actor.
Uses hive-create-actor* arguments."
  (let ((actor-id (hive-create-actor* args ...)))
    (hive-resolve-local-actor hive actor-id)))

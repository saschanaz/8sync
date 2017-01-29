;;; 8sync --- Asynchronous programming for Guile
;;; Copyright (C) 2017 Christopher Allan Webber <cwebber@dustycloud.org>
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

(define-module (tests test-rmeta-slot)
  #:use-module (srfi srfi-64)
  #:use-module (8sync rmeta-slot)
  #:use-module (oop goops)
  #:use-module (tests utils))

(test-begin "test-rmeta-slot")

;; Define a class
(define-class <kah-lassy> ()
  (entries #:allocation #:each-subclass
           #:init-thunk
           (build-rmeta-slot
            `((foo . "bar")
              (baz . "basil")))))

(test-equal "bar"
    (class-rmeta-ref <kah-lassy> 'entries 'foo))
(test-equal "basil"
    (class-rmeta-ref <kah-lassy> 'entries 'baz))

;; Define a subclass

(define-class <sub-lassy> (<kah-lassy>)
  (entries #:allocation #:each-subclass
           #:init-thunk
           (build-rmeta-slot
            `((foo . "foo2")
              (peanut . "gallery")))))

;; Access values, and inheritance is preserved
(test-equal "foo2"
  (class-rmeta-ref <sub-lassy> 'entries 'foo))
(test-equal "gallery"
  (class-rmeta-ref <sub-lassy> 'entries 'peanut))
(test-equal "basil"
  (class-rmeta-ref <sub-lassy> 'entries 'baz))

;; Not defined
(test-equal #f
  (class-rmeta-ref <sub-lassy> 'entries 'not-defined))
;; Not defined, with default
(test-equal "no-way"
  (class-rmeta-ref <sub-lassy> 'entries 'not-defined
                   #:dflt "no-way"))

(test-end "test-rmeta-slot")
(test-exit)

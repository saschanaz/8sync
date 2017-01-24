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

(define-module (8sync rmeta-slot)
  #:use-module (oop goops)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)

  #:export (make-rmeta-slot
            maybe-build-rmeta-slot-cache!
            class-rmeta-ref))

;;; This module is for rmeta-slots, aka a recursive-meta-slot.
;;;
;;; Recursive meta-slots are recursive because we walk down the
;;; inheritance list until we find a match, and meta because they
;;; are sort of "slots" of their own... alists, rather, where you
;;; are searching for the right key.
;;;
;;; Recursive meta-slots have their own cache so access is
;;; reasonably fast.
;;;
;;; A recursive meta-slot definition looks something like this:
;;;
;;;   ;; Define a class with a meta-slot
;;;   (define-class <kah-lassy> ()
;;;     (entries #:allocation #:each-subclass
;;;              #:init-value
;;;              (make-rmeta-slot
;;;               `((foo . "bar")
;;;                 (baz . "basil")))))
;;;
;;;   ;; Access values
;;;   (class-rmeta-ref <kah-lassy> 'entries 'foo) => "bar"
;;;   (class-rmeta-ref <kah-lassy> 'entries 'baz) => "basil"
;;;
;;;   ;; Define a subclass
;;;   (define-class <sub-lassy> (<kah-lassy>)
;;;     (entries #:allocation #:each-subclass
;;;              #:init-value
;;;              (make-rmeta-slot
;;;               `((foo . "foo2")
;;;                 (peanut . "gallery")))))
;;;
;;;   ;; Access values, and inheritance is preserved
;;;   (class-rmeta-ref <sub-lassy> 'entries 'foo) => "foo2"
;;;   (class-rmeta-ref <sub-lassy> 'entries 'peanut) => "gallery"
;;;   (class-rmeta-ref <sub-lassy> 'entries 'baz) => "basil"

(define-record-type <rmeta-slot>
  (%make-rmeta-slot table cache)
  rmeta-slot?
  (table rmeta-slot-table)
  (cache rmeta-slot-cache set-rmeta-slot-cache!))

(define (make-rmeta-slot table)
  (%make-rmeta-slot table #f))

;; Immutable and unique
(define %the-nothing (cons '*the* '*nothing*))

(define (maybe-build-rmeta-slot-cache! class slot-name
                                       equals? cache-set! cache-ref)
  "Build the rmeta slot cache, if it isn't built already."
  (define rmeta-slot
    (class-slot-ref class slot-name))
  (define (build-cache)
    (define cache (make-hash-table))
    (for-each
     (lambda (this-class)
       (and (class-slot-definition this-class slot-name)
            (class-slot-ref this-class slot-name)
            (let ((this-rmeta (class-slot-ref this-class slot-name)))
              (for-each (match-lambda
                          ((key . val)
                           ;; Add this value to the list if we haven't yet seen
                           ;; such a definition before
                           (when (eq? (cache-ref cache key %the-nothing)
                                      %the-nothing)
                             (cache-set! cache key val))))
                        (rmeta-slot-table this-rmeta)))))
     (class-precedence-list class))
    cache)
  ;; If it's alreayd built, this is a no-op.
  (when (not (rmeta-slot-cache rmeta-slot))
    (set-rmeta-slot-cache! rmeta-slot (build-cache))))

(define* (class-rmeta-ref class slot-name key
                          #:key (equals? equal?)
                          (cache-set! hash-set!)
                          (cache-ref hash-ref)
                          dflt)
  "Search heirarchy of CLASS through the rmeta-slot named SLOT-NAME for
value matching KEY.  This also calls maybe-build-rmeta-slot-cache! as a side
effect."
  (maybe-build-rmeta-slot-cache! class slot-name
                                 equals? cache-set! cache-ref)
  (cache-ref (rmeta-slot-cache (class-slot-ref class slot-name)) key dflt))

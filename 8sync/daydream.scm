;;; 8sync --- Asynchronous programming for Guile
;;; Copyright © 2017 Christopher Allan Webber <cwebber@dustycloud.org>
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

(define-module (8sync daydream)
  #:use-module (8sync)
  #:use-module (fibers)
  #:export (daydream))

;;; Daydreaming is a lot like sleeping, except it doesn't block other
;;; message correspondence.

(define-actor <daydreamer> (<actor>)
  ((dream daydreamer-dream)))

(define (daydreamer-dream daydreamer message
                          secs)
  (sleep secs)
  ;; We're a single use actor, so we blow up
  (self-destruct daydreamer)
  ;; Now reply to the actor's continuation
  'wake-up)

;; TODO: don't require the actor here
(define (daydream secs)
  (define dreamer
    (create-actor ((@@ (8sync actors) *current-actor*))
                  <daydreamer>))
  (<-wait dreamer 'dream secs))

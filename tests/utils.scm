;;; Totes taken from dave

;;; srt2vtt --- SRT to WebVTT converter
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;;
;;; srt2vtt is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; srt2vtt is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with srt2vtt.  If not, see <http://www.gnu.org/licenses/>.

(define-module (tests utils)
  #:use-module (srfi srfi-64)
  #:export (test-exit))

(define (test-exit)
  (exit (= (test-runner-fail-count (test-runner-current)) 0)))

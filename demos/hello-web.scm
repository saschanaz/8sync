#!/usr/bin/guile \
-e main -s
!#

;;; 8sync --- Asynchronous programming for Guile
;;; Copyright (C) 2015 Christopher Allan Webber <cwebber@dustycloud.org>
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

(use-modules (8sync systems web)
             (8sync agenda)
             (web request)
             (web response)
             (web uri)
             (ice-9 match))


;;; Utilities
;;; =========

(define (simple-404 request request-body)
  (values (build-response
           #:code 404
           #:headers '((content-type . (text/plain))))
          "Not found.  Dag, yo."))



;;; Views
;;; =====

(define (hello-world-view request request-body)
  (values '((content-type . (text/plain)))
          "Hello world!"))



;;; Dispatch / routing
;;; ==================

(define (web-dispatch request request-body)
  (define (call-view view)
    (view request request-body))
  ;; URI routing here
  (match (split-and-decode-uri-path (uri-path (request-uri request)))
    ;; This is for the `/' root
    (() (call-view hello-world-view))
    ;; An example of an inline view at `/pants/'
    (("pants")
     (values '((content-type . (text/plain)))
             "Hello pants!"))
    (_
     (call-view simple-404))))



;;; CLI
;;; ===

(define main (make-web-demo-cli #:handler (wrap-apply web-dispatch)))

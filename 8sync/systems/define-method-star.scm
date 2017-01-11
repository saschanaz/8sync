;;; 8sync --- Asynchronous programming for Guile
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
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


(define-module (8sync define-method-star)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:export (define-method*))

(define-syntax define-method*
  (lambda (x)
    (syntax-case x ()
      ((_ (generic arg-spec ... . tail) body ...)
       (let-values (((required-arg-specs other-arg-specs)
                     (break (compose keyword? syntax->datum)
                            #'(arg-spec ...))))
         #`(define-method (generic #,@required-arg-specs . rest)
             (apply (lambda* (#,@other-arg-specs . tail)
                      body ...)
                    rest)))))))

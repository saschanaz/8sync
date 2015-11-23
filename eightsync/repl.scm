;; Copyright (C) 2015 Christopher Allan Webber <cwebber@dustycloud.org>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

(define-module (eightsync repl)
  #:use-module (ice-9 q)
  #:use-module (eightsync agenda)
  #:use-module (system repl coop-server)
  #:export (make-coop-server-handler
            spawn-and-queue-repl-server!))

(define (make-coop-server-handler coop-server)
  (define (run-self)
    (poll-coop-repl-server coop-server)
    ;; queue ourselves again
    (run-delay (run-self) (/ 1 60)))
  run-self)

(define* (spawn-and-queue-repl-server! agenda #:optional port)
  (let ((coop-server
         (if port
             (spawn-coop-repl-server port)
             (spawn-coop-repl-server))))
    (enq! (agenda-queue agenda)
          (make-coop-server-handler coop-server))))

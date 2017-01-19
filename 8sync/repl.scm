;;; 8sync --- Asynchronous programming for Guile
;;; Copyright © 2015, 2016, 2017 Christopher Allan Webber <cwebber@dustycloud.org>
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

(define-module (8sync repl)
  #:use-module (oop goops)
  #:use-module (8sync)
  #:use-module (srfi srfi-1)
  #:use-module (system repl server)
  #:use-module (system repl coop-server)
  #:export (<repl-manager>))

(define-actor <repl-manager> (<actor>)
  ((*cleanup* repl-manager-cleanup)
   (*init* repl-manager-init)
   (add-subscriber repl-manager-add-subscriber)
   (remove-subscriber repl-manager-remove-subscriber))
  (path #:init-keyword #:path
        #:init-value "/tmp/8sync-socket"
        #:getter .path)
  (socket #:init-value #f
          #:accessor .socket)
  (poll-every #:init-keyword #:poll-every
              #:init-value (/ 1 30)
              #:getter .poll-every)
  (subscribers #:init-keyword #:subscribers
               #:init-value '()
               #:accessor .subscribers))

(define (repl-manager-cleanup repl-manager message)
  ;; Close the socket, if open
  (and=> (.socket repl-manager)
         close)
  ;; Delete the file, if it exists
  (when (file-exists? (.path repl-manager))
    (delete-file (.path repl-manager))))

(define (repl-manager-init repl-manager message)
  (define socket
    (make-unix-domain-server-socket #:path (.path repl-manager)))
  (define server
    (spawn-coop-repl-server socket))
  (define (inform-subscribers)
    (for-each
     (lambda (subscriber)
       (<- subscriber 'repl-update))
     (.subscribers repl-manager)))
  (set! (.socket repl-manager) socket)
  (while (actor-alive? repl-manager)
    (poll-coop-repl-server server)
    (inform-subscribers)
    (8sleep (.poll-every repl-manager))))

(define (repl-manager-add-subscriber repl-manager message)
  (define from (message-from message))
  (unless (member from (.subscribers repl-manager))
    (set! (.subscribers repl-manager)
          (cons from (.subscribers repl-manager)))))

(define (repl-manager-remove-subscriber repl-manager message)
  (define from (message-from message))
  (set! (.subscribers repl-manager)
        (remove (lambda (x) (equal? x (message-from message)))
                (.subscribers repl-manager))))

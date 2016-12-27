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

(define-module (8sync repl)
  #:use-module (oop goops)
  #:use-module (8sync)
  #:use-module (system repl server)
  #:use-module (system repl coop-server)
  #:export (<repl-manager>))

(define-class <repl-manager> (<actor>)
  (path #:init-keyword #:path
        #:init-value "/tmp/8sync-socket"
        #:getter repl-manager-path)
  (socket #:init-value #f
          #:accessor repl-manager-socket)
  (poll-every #:init-keyword #:poll-every
              #:init-value (/ 1 30)
              #:getter repl-manager-poll-every)
  (actions #:allocation #:each-subclass
           ;; @@: Should we add a stop action?
           #:init-value (build-actions
                         (*cleanup* repl-manager-cleanup)
                         (init repl-manager-init))))

(define (repl-manager-cleanup repl-manager message)
  ;; Close the socket, if open
  (and=> (repl-manager-socket repl-manager)
         close)
  ;; Delete the file, if it exists
  (when (file-exists? (repl-manager-path repl-manager))
    (delete-file (repl-manager-path repl-manager))))

(define (repl-manager-init repl-manager message)
  (define socket
    (make-unix-domain-server-socket #:path (repl-manager-path repl-manager)))
  (define server
    (spawn-coop-repl-server socket))
  (set! (repl-manager-socket repl-manager) socket)
  (while (actor-am-i-alive? repl-manager)
    (poll-coop-repl-server server)
    (8sleep (repl-manager-poll-every repl-manager))))


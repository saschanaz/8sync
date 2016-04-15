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

;; This is a very preliminary web module.

(define-module (8sync systems web)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (8sync agenda)
  #:use-module (8sync repl)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:export (http-server-socket
            receive-http-conn
            simple-webdemo
            make-web-demo-cli))

(define (hello-world-handler request request-body)
  (values '((content-type . (text/plain)))
          "Hello world!"))

(define* (http-server-socket #:key
                             (host #f)
                             (family AF_INET)
                             (addr (if host
                                       (inet-pton family host)
                                       INADDR_LOOPBACK))
                             (port 8080)
                             (socket
                              ((@@ (web server http) make-default-socket)
                               family addr port)))
  (listen socket 128)
  ;;; This is used in Guile's http server under the commit:
  ;;; commit a0ad8ad16c14adbf13e0ead3dafd833fb3c8f0d3
  ;;; Author: Andy Wingo <wingo@pobox.com>
  ;;; Date:   Mon Nov 29 13:00:43 2010 +0100
  ;;;
  ;;;     http web server impl ignores SIGPIPE
  ;;;
  ;;;     * module/web/server/http.scm (http-open): Ignore SIGPIPE. Keeps the
  ;;;       server from dying in some circumstances.
  ;; (sigaction SIGPIPE SIG_IGN)
  ;;; Will this break other things that use pipes for us though?

  ;; Throw a system-error rather than block on an (accept)
  ;; that has nothing to do
  (fcntl socket F_SETFL
         (logior O_NONBLOCK
                 (fcntl socket F_GETFL)))
  socket)


(define* (accept-connection-gracefully socket #:key complain)
  "Try to accept a connection, but if there's nothing there,
don't totally failboat"
  (catch 'system-error
    (lambda () (accept socket))
    ;; Maybe should catch the more specific error:
    ;;   (system-error "accept" "~A" ("Resource temporarily unavailable") (11))
    (lambda args
      (if complain
          (display "Gracefully handled no connection to accept\n"
                   (current-error-port)))
      #f)))

(define (receive-http-conn handler)
  (lambda (server-socket)
    (let ((conn-pair (accept-connection-gracefully
                      server-socket #:complain #t)))
      (match conn-pair
        ((client-conn . socket-address)
         (define (close-and-dequeue)
           (close client-conn)
           (8sync-port-remove client-conn))

         (catch
           #t
           (lambda ()
             (let* ((request (read-request client-conn))
                    (request-body (read-request-body request)))
               (call-with-values
                   (lambda ()
                     (call-with-values
                         (lambda ()
                           ;; @@: Is it useful to wrap this in 8sync-run?
                           ;;  It's more indirection but might give breathing
                           ;;  room to other requests...
                           (handler request request-body))
                       (lambda return-values
                         (match return-values
                           ((response response-body)
                            (sanitize-response request response response-body))
                           (_ (throw 'invalid-http-response
                                     "Expected values of (response response-body) but didn't get 'em"
                                     return-values))))))
                 (lambda (response response-body)
                   (begin
                     (write-response-body
                      (write-response response client-conn)
                      response-body)
                     ;; TODO: We might NOT want to close here!
                     ;;   Eg, keep-alive requests.  See keep-alive? in guile's
                     ;;   (@@ (web server http) http-write)
                     (close-and-dequeue))))))
           (lambda (key . args)
             (display ":(\n")
             (close-and-dequeue)
             ;; re-raise exception
             (apply throw key args))))
        ;; If we get a #f back, there was nothing to do
        (#f #f)))))

(define (install-http-socket socket handler)
  (make-port-request socket #:read (receive-http-conn handler)))

;; Kinda junky since dynamic-wind and delimited continuations
;; seem to not really get along...

(define-syntax-rule (with-socket-as make-socket socket-name
                                    body ...)
  (let ((socket-name make-socket))
    (dynamic-wind
      (const #f)
      (lambda ()
        body ...)
      (lambda ()
        (close socket-name)))))

(define default-handler (wrap-apply hello-world-handler))

(define* (run-simple-webserver #:key
                               (handler default-handler)
                               (coop-repl #f))
  (display "Running on http://127.0.0.1:8080/\n")
  (with-socket-as
   (http-server-socket) server-socket
   (let* ((q (make-q*
              (wrap (install-http-socket server-socket
                                         handler))))
          (agenda (make-agenda
                   #:queue q
                   #:pre-unwind-handler print-error-and-continue)))
     (if coop-repl
         (spawn-and-queue-repl-server! agenda))
     (start-agenda agenda))))

(define* (make-web-demo-cli #:key
                            (listen #t)
                            (handler default-handler))
  (define (main args)
    (run-simple-webserver #:coop-repl listen
                          #:handler handler))
  main)

(define main (make-web-demo-cli))

#!/usr/bin/guile \
-e main -s
!#

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

(use-modules (eightsync repl)
             (eightsync agenda)
             (ice-9 getopt-long)
             (ice-9 format)
             (ice-9 q)
             (ice-9 match))


;;; Network stuff
;;; =============

(define default-irc-port 6665)

(define* (irc-socket-setup hostname #:optional (inet-port default-irc-port))
  (let ((s (socket PF_INET SOCK_STREAM 0))
        (ip-address (inet-ntoa (car (hostent:addr-list (gethost hostname))))))
    (connect s AF_INET
             (inet-pton AF_INET ip-address)
             inet-port)
    s))

(define (install-socket socket handler)
  (display "Installing socket...\n")   ; debugging :)
  (make-port-request socket #:read handler))

(define irc-eol "\r\n")

(define (irc-line line)
  (string-concatenate (list line irc-eol)))

(define-syntax-rule (irc-format dest format-string rest ...)
  (let ((line (string-concatenate
               (list (format #f format-string rest ...)
                     irc-eol))))
    (match dest
      (#f line)
      (#t (display line))
      (else
       (display line dest)))))

(define* (irc-display line #:optional dest)
  (if dest
      (display (irc-line line) dest)
      (display (irc-line dest))))

(define* (handle-login socket username
                       #:optional
                       (hostname "*")
                       (servername "*")
                       (realname username)
                       (channels '()))
  (irc-format socket "USER ~a ~a ~a :~a"
              username hostname servername realname)
  (irc-format socket "NICK ~a" username)
  (for-each
   (lambda (channel)
     (irc-format socket "JOIN ~a" channel))
   channels))

(define (handle-line socket line)
  (display line)
  (newline))

(define (make-simple-irc-handler handle-line)
  (let ((buffer '()))
    (define (reset-buffer)
      (set! buffer '()))
    (define (should-read-char socket)
      (and (char-ready? socket) (not (eof-object? (peek-char socket)))))
    (define (irc-handler socket)
      (while (should-read-char socket)
        (set! buffer (cons (read-char socket) buffer))
        (match buffer
          ((#\newline #\return (? char? line-chars) ...)
           (%sync (%run (handle-line
                         socket
                         (list->string (reverse line-chars)))))
           ;; reset buffer
           (set! buffer '()))
          (_ #f))))
    irc-handler))

(define* (queue-and-start-irc-agenda! agenda socket #:key
                                      (username "syncbot")
                                      (inet-port default-irc-port)
                                      (handler (make-simple-irc-handler handle-line))
                                      (channels '()))
  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (enq! (agenda-queue agenda) (wrap (install-socket socket handler)))
      (enq! (agenda-queue agenda) (wrap (handle-login socket username
                                                      #:channels channels)))
      (start-agenda agenda))
    (lambda ()
      (display "Cleaning up...\n")
      (close socket))))



;;; CLI
;;; ===

(define option-spec
  `((server (single-char #\s) (required? #t) (value #t))
    (port (single-char #\p)
          (value #t)
          (predicate
           ,(lambda (s)
              (if (string->number s) #t #f))))
    (username (single-char #\u) (required? #t) (value #t))
    (listen)))

(define (main args)
  (let* ((options (getopt-long args option-spec))
         (hostname (option-ref options 'server #f))
         (port (or (option-ref options 'port #f)
                   default-irc-port))
         (username (option-ref options 'username #f))
         (listen (option-ref options 'listen #f)))
    (display `((server ,hostname) (port ,port) (username ,username)
               (listen ,listen)))
    (newline)
    (queue-and-start-irc-agenda!
     (make-agenda)
     (irc-socket-setup hostname port)
     #:inet-port port
     #:username username
     #:handler (make-simple-irc-handler handle-line))))

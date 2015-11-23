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
             (srfi srfi-9)
             (ice-9 getopt-long)
             (ice-9 format)
             (ice-9 receive)
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
                       #:key
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

(define (startswith-colon? str)
  (and (> (string-length str) 0)
       (eq? (string-ref str 0)
            #\:)))

(define-record-type <irc-line>
  (make-irc-line prefix command params)
  irc-line?
  (prefix irc-line-prefix)
  (command irc-line-command)
  (params irc-line-params))


(define (parse-line line)
  (define (parse-params pre-params)
    ;; This is stupid and imperative but I can't wrap my brain around
    ;; the right way to do it in a functional way :\
    (let ((param-list '())
          (currently-building '()))
      (for-each
       (lambda (param-item)
         (cond
          ((startswith-colon? param-item)
           (if (not (eq? currently-building '()))
               (set! param-list
                     (cons
                      (reverse currently-building)
                      param-list)))
           (set! currently-building (list param-item)))
          (else
           (set! currently-building (cons param-item currently-building)))))
       pre-params)
      ;; We're still building something, so tack that on there
      (if (not (eq? currently-building '()))
          (set! param-list
                (cons (reverse currently-building) param-list)))
      ;; return the reverse of the param list
      (reverse param-list)))

  (match (string-split line #\space)
    (((? startswith-colon? prefix)
      command
      pre-params ...)
     (make-irc-line prefix command
                    (parse-params pre-params)))
    ((command pre-params ...)
     (make-irc-line #f command
                    (parse-params pre-params)))))

(define (strip-colon-if-necessary string)
  (if (and (> (string-length string) 0)
           (string-ref string 0))
      (substring/copy string 1)
      string))

;; @@: Not sure if this works in all cases, like what about in a non-privmsg one?
(define (irc-line-username irc-line)
  (let* ((prefix-name (strip-colon-if-necessary (irc-line-prefix irc-line)))
         (exclaim-index (string-index prefix-name #\!)))
    (if exclaim-index
        (substring/copy prefix-name 0 exclaim-index)
        prefix-name)))

(define (condense-privmsg-line line)
  "Condense message line and do multiple value return of
  (channel message is-action)"
  (define (strip-last-char string)
    (substring/copy string 0 (- (string-length string) 1)))
  (let* ((channel-name (caar line))
         (rest-params (apply append (cdr line))))
    (match rest-params
      (((or "\x01ACTION" ":\x01ACTION") middle-words ... (= strip-last-char last-word))
       (values channel-name
               (string-join
                (append middle-words (list last-word))
                " ")
               #t))
      (((= strip-colon-if-necessary first-word) rest-message ...)
       (values channel-name
               (string-join (cons first-word rest-message) " ")
               #f)))))

(define (echo-back-message my-name speaker
                           channel-name message is-action)
  (if is-action
      (format #t "~a emoted ~s in channel ~a\n"
              speaker message channel-name)
      (format #t "~a said ~s in channel ~a\n"
              speaker message channel-name)))

(define default-handle-privmsg echo-back-message)

(define* (make-handle-line #:key
                           (handle-privmsg default-handle-privmsg))
  (define (handle-line socket line my-username)
    (let ((parsed-line (parse-line line)))
      (match (irc-line-command parsed-line)
        ("PING"
         (irc-display "PONG" socket))
        ("PRIVMSG"
         (receive (channel-name message is-action)
             (condense-privmsg-line (irc-line-params parsed-line))
           (let ((username (irc-line-username parsed-line)))
             (handle-privmsg my-username username channel-name message is-action))))
        (_
         (display line)
         (newline)))))
  handle-line)

(define (make-simple-irc-handler handle-line username)
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
                         (list->string (reverse line-chars))
                         username)))
           ;; reset buffer
           (set! buffer '()))
          (_ #f))))
    irc-handler))

(define* (queue-and-start-irc-agenda! agenda socket #:key
                                      (username "syncbot")
                                      (inet-port default-irc-port)
                                      (handler (make-simple-irc-handler
                                                (lambda args
                                                  (apply (make-handle-line) args))
                                                username))
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
    (channels (value #t))
    (listen)))

(define (main args)
  (let* ((options (getopt-long args option-spec))
         (hostname (option-ref options 'server #f))
         (port (or (option-ref options 'port #f)
                   default-irc-port))
         (username (option-ref options 'username #f))
         (listen (option-ref options 'listen #f))
         (channels (option-ref options 'channels ""))
         (agenda (make-agenda)))
    (display `((server ,hostname) (port ,port) (username ,username)
               (listen ,listen) (channels-split ,(string-split channels #\space))))
    (newline)
    (if listen
        (spawn-and-queue-repl-server! agenda))
    (queue-and-start-irc-agenda!
     agenda
     (irc-socket-setup hostname port)
     #:inet-port port
     #:username username
     #:channels (string-split channels #\space))))

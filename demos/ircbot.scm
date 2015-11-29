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

(use-modules (8sync systems irc)
             (8sync agenda)
             (ice-9 match))

(define (handle-message socket my-name speaker
                        channel message is-action)
  (define (looks-like-me? str)
    (or (equal? str my-name)
        (equal? str (string-concatenate (list my-name ":")))))
  (match (string-split message #\space)
    (((? looks-like-me? _) action action-args ...)
     (match action
       ("botsnack"
        (irc-format socket "PRIVMSG ~a :Yippie! *does a dance!*" channel))
       ((or "hello" "hello!" "hello." "greetings" "greetings." "greetings!"
            "hei" "hei." "hei!")
        (irc-format socket "PRIVMSG ~a :Oh hi ~a!" channel speaker))
       ;; Add yours here
       (_
        (irc-format socket "PRIVMSG ~a :*stupid puppy look*" channel))))
    (_
     (cond
      (is-action
       (format #t "~a emoted ~s in channel ~a\n"
               speaker message channel))
      (else
       (format #t "~a said ~s in channel ~a\n"
               speaker message channel))))))

(define main
  (make-irc-bot-cli (make-handle-line
                     #:handle-privmsg (wrap-apply handle-message))))


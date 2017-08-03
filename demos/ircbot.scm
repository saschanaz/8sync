#!/usr/bin/guile \
-e main -s
!#

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

(use-modules (8sync)
             (8sync systems irc)
             (8sync repl)
             (oop goops)
             (srfi srfi-37)
             (ice-9 format)
             (ice-9 match)
             (fibers conditions))

(define-class <my-irc-bot> (<irc-bot>))

(define-method (handle-line (irc-bot <my-irc-bot>) message
                            speaker channel line emote?)
  (define my-name (irc-bot-username irc-bot))
  (define (looks-like-me? str)
    (or (equal? str my-name)
        (equal? str (string-concatenate (list my-name ":")))))
  (match (string-split line #\space)
    (((? looks-like-me? _) action action-args ...)
     (match action
       ;; The classic botsnack!
       ("botsnack"
        (<- (actor-id irc-bot) 'send-line channel
            "Yippie! *does a dance!*"))
       ;; Return greeting
       ((or "hello" "hello!" "hello." "greetings" "greetings." "greetings!"
            "hei" "hei." "hei!" "hi" "hi!")
        (<- (actor-id irc-bot) 'send-line channel
            (format #f "Oh hi ~a!" speaker)))

       ;; --->  Add yours here <---

       ;; Default
       (_
        (<- (actor-id irc-bot) 'send-line channel
            "*stupid puppy look*"))))
    ;; Otherwise... just spit the output to current-output-port or whatever
    (_
     (if emote?
         (format #t "~a emoted ~s in channel ~a\n"
                 speaker line channel)
         (format #t "~a said ~s in channel ~a\n"
                 speaker line channel)))))


(define (display-help scriptname)
  (format #t "Usage: ~a [OPTION] username" scriptname)
  (display "
  -h, --help                  display this text
      --server=SERVER-NAME    connect to SERVER-NAME
                                defaults to \"irc.freenode.net\"
      --channels=CHANNEL1,CHANNEL2
                              join comma-separated list of channels on connect
                                defaults to \"##botchat\"")
  (newline))

(define (parse-args scriptname args)
  (args-fold (cdr args)
             (list (option '(#\h "help") #f #f
                           (lambda _
                             (display-help scriptname)
                             (exit 0)))
                   (option '("server") #t #f
                           (lambda (opt name arg result)
                             `(#:server ,arg ,@result)))
                   (option '("channels") #t #f
                           (lambda (opt name arg result)
                             `(#:channels ,(string-split arg #\,)
                               ,@result)))
                   (option '("repl") #f #t
                           (lambda (opt name arg result)
                             `(#:repl ,(or arg #t) ,@result))))
             (lambda (opt name arg result)
               (format #t "Unrecognized option `~a'\n" name)
               (exit 1))
             (lambda (option result)
               `(#:username ,option ,@result))
             '()))

(define* (run-bot #:key (username "examplebot")
                  (server "irc.freenode.net")
                  (channels '("##botchat"))
                  (repl #f))
  (run-hive
   (lambda (hive)
     (define irc-bot
       (create-actor* <my-irc-bot> "irc-bot"
                      #:username username
                      #:server server
                      #:channels channels))
     (define repl-manager
       (cond
        ((string? repl)
         (create-actor* <repl-manager> "repl"
                        #:path repl))
        (repl
         (create-actor* <repl-manager> "repl"))))

     (wait (make-condition)))))

(define (main args)
  (define parsed-args (parse-args "ircbot.scm" args))
  (apply (lambda* (#:key username #:allow-other-keys)
           (when (not username)
             (display "Error: username not specified!")
             (newline) (newline)
             (display-help "ircbot.scm")
             (exit 1)))
         parsed-args)
  (apply run-bot parsed-args))

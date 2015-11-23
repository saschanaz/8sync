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

(use-modules (eightsync systems irc))

(define (handle-message socket my-name speaker
                        channel-name message is-action)
  (if is-action
      (format #t "~a emoted ~s in channel ~a\n"
              speaker message channel-name)
      (format #t "~a said ~s in channel ~a\n"
              speaker message channel-name)))

(define main
  (make-irc-bot-cli))

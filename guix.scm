;;; 8sync --- Asynchronous programming for Guile
;;; Copyright (C) 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright (C) 2017 Christopher Allan Webber <cwebber@dustycloud.org>
;;;
;;; This file is part of 8sync.
;;; However, unlike most of 8sync, which is under the LGPLv3+, this
;;; file in particular is licensed under GPLv3+.
;;; Guix is also licensed under GPLv3+.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, run:
;;
;;   guix package -f guix.scm
;;
;; To build it, but not install it, run:
;;
;;   guix build -f guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment -l guix.scm
;;
;;; Code:

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 popen)
             (ice-9 match)
             (ice-9 rdelim)
             (guix packages)
             (guix licenses)
             (guix gexp)
             (guix git-download)
             (guix build-system gnu)
             ((guix build utils) #:select (with-directory-excursion))
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages texinfo))

(define %source-dir (dirname (current-filename)))

(package
  (name "guile-8sync")
  (version "git")
  (source (local-file %source-dir
                      #:recursive? #t
                      #:select? (git-predicate %source-dir)))
  (build-system gnu-build-system)
  (native-inputs `(("autoconf" ,autoconf)
                   ("automake" ,automake)
                   ("guile" ,guile-2.2)
                   ("pkg-config" ,pkg-config)
                   ("texinfo" ,texinfo)))
  (propagated-inputs `(("guile-fibers" ,guile-fibers)))
  (arguments
   `(#:phases (modify-phases %standard-phases
                (add-before 'configure 'bootstrap
                            (lambda _
                              (zero? (system* "./bootstrap.sh"))))
                (add-before 'configure 'setenv
                            (lambda _
                              (setenv "GUILE_AUTO_COMPILE" "0"))))))
  (home-page "https://gnu.org/s/8sync/")
  (synopsis "Asynchronous actor model library for Guile")
  (description
   "GNU 8sync (pronounced \"eight-sync\") is an asynchronous programming
library for GNU Guile based on the actor model.")
  (license lgpl3+))

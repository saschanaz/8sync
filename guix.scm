;;; 8sync --- Asynchronous programming for Guile
;;; Copyright (C) 2016 Jan Nieuwenhuizen <janneke@gnu.org>
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

(use-modules (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages texinfo))

(package
  (name "8sync")
  (version "0.4.0")
  (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/guile/8sync-" version
                                  ".tar.gz"))
              ;; This will be wrong by time of release.
              ;; Oh well... a better guix.scm at next release :)
              (sha256
               (base32
                "08w163k8qv28d8zixbl0rh98d4b3hk0ksh8nlr4xaj58291aijlh"))))
  (build-system gnu-build-system)
  (native-inputs `(("autoconf" ,autoconf)
                   ("automake" ,automake)
                   ("guile" ,guile-next)
                   ("pkg-config" ,pkg-config)
                   ("texinfo" ,texinfo)))
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

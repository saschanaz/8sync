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
;;   guix package -f package.scm
;;
;; To build it, but not install it, run:
;;
;;   guix build -f package.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment -l package.scm
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
  (version "0.0")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://notabug.org/cwebber/8sync")
                  (commit "b02ef57")))
            (sha256
             (base32
              "1sfy72q35dhqkfq2k3fi7a10grx3ll2kblpjivdai2jn61fki6wm"))
            (modules '((guix build utils)))))
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
  (home-page "https://notabug.org/cwebber/8sync")
  (synopsis "An asynchronous programming library for GNU Guile")
  (description
   "GNU 8sync (pronounced \"eight-sync\") is an asynchronous programming
library for GNU Guile.

Be warned: it is early days for the 8sync project.  New contributors and users
are more than welcome, but beware API instability.")
  (license lgpl3+))

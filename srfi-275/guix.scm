;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (gnu packages guile)
             (srfi srfi-13)
             (guix build-system gnu)
             (guix gexp)
             ((guix licenses) #:prefix license:)
             (guix search-paths)
             (guix packages))

(define source-directory
  (dirname (canonicalize-path (current-filename))))

(package
  (name "guile-srfi-275")
  (version "0-unstable-2026-10-30")
  (source (local-file source-directory "guile-srfi-275-source"
                      #:recursive? #t
                      #:select? (lambda (file stat)
                                  (not (or (string-suffix? "/.git" file)
                                           (string-suffix? "/.blue-store" file)
                                           (string-suffix? "/result" file))))))
  (build-system gnu-build-system)
  (arguments
   (list
    #:tests? #f
    #:phases
    #~(modify-phases %standard-phases
        (delete 'configure)
        (replace 'build
          (lambda _
            (invoke "make" "check")))
        (replace 'check
          (lambda _ #t))
        (replace 'install
          (lambda _
            (invoke "make" "install"
                    (string-append "PREFIX=" #$output)
                    (string-append "DESTDIR=")))))
    #:make-flags #~(list)))
  (native-inputs (list guile-3.0))
  (propagated-inputs (list guile-3.0))
  (native-search-paths
   (list (search-path-specification
          (variable "GUILE_LOAD_PATH")
          (files '("share/guile/site/3.0")))))
  (home-page "https://srfi.schemers.org/srfi-275/")
  (synopsis "Guile implementation of SRFI 275")
  (description
   "A standalone Guile implementation of SRFI 275: URIs, IRIs, and basic
paths.")
  (license license:expat))

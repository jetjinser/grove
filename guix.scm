;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Direct Guix package entry point:
;;;   guix time-machine -C channels.scm -- build -f guix.scm

(use-modules (guix profiles))

(define repository-root
  (dirname (canonicalize-path (current-filename))))

;; The development manifest owns the local dependency package definitions
;; consumed by the standalone Grove package factory.
(primitive-load (string-append repository-root "/manifest.scm"))
(primitive-load (string-append repository-root "/grove-package.scm"))

(make-grove blue guile faster-minikanren grove-highlights libunistring
            (specification->package "guile-gcrypt")
            (specification->package "guile-ssh")
            (specification->package "guile-ts")
            (specification->package "coreutils")
            guile-srfi-275)

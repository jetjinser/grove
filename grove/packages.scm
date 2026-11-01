;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove packages)
  #:use-module (guix packages)
  #:use-module (ice-9 ftw)
  #:export (grove))

(define grove-source-root
  (dirname (dirname (canonicalize-path (current-filename)))))

;; Keep the package factory and its pinned dependencies in the same source
;; tree as Grove, so a Guix channel can expose the exact package recipe.
(primitive-load (string-append grove-source-root "/guix/package-definitions.scm"))
(primitive-load (string-append grove-source-root "/grove-package.scm"))

(define-public grove
  (make-grove blue guile faster-minikanren grove-highlights libunistring
              (specification->package "guile-gcrypt")
              (specification->package "guile-ssh")
              (specification->package "guile-ts")
              (specification->package "coreutils")
              guile-srfi-275
              libidn2
              #:source-root grove-source-root))

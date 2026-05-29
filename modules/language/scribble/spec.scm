;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble spec)
  #:use-module (system base language)
  #:use-module (language scribble parser)
  #:use-module (sxml simple)
  #:export (scribble))

(define (compile-to-value exp env opts)
  (values `(quote ,(sxml->xml exp)) env env))

(define-language scribble
  #:title      "Scribble"
  #:reader     scribble-reader
  #:compilers  `((scheme . ,compile-to-value))
  #:printer    write)


;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;; SPDX-License-Identifier: MIT

(define-module (srfi :275 normalize)
  #:use-module (srfi :275 internal normalize)
  #:re-export (normalize-iri-case normalize-iri-escape normalize-iri-path
               normalize-iri normalize-uri-case normalize-uri-escape
               normalize-uri-path normalize-uri resolve-iri resolve-uri
               relativize-iri relativize-uri uri->iri iri->uri))

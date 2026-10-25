;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private entity-uri)
  #:use-module (web uri)
  #:export (entity-uri-href))

(define (entity-uri-href value)
  "Return a useful browser URL for an entity URI, or #f if unsupported."
  (catch #t
    (lambda ()
      (let* ((uri (string->uri value))
             (scheme (and (uri? uri) (uri-scheme uri)
                          (symbol->string (uri-scheme uri)))))
        (cond
          ((or (string-ci=? scheme "http")
               (string-ci=? scheme "https"))
           value)
          ((and (string-ci=? scheme "doi")
                (string? (uri-path uri))
                (not (string=? (uri-path uri) ""))
                (not (uri-query uri))
                (not (uri-fragment uri)))
           (string-append "https://doi.org/" (uri-path uri)))
          (else #f))))
    (lambda _ #f)))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private entity-uri)
  #:use-module (web uri)
  #:export (absolute-entity-uri? entity-uri-href))

(define (absolute-entity-uri? value)
  "Return true for an absolute URI, requiring an authority for HTTP(S)."
  (catch #t
    (lambda ()
      (let* ((uri (string->uri value))
             (scheme (and (uri? uri) (uri-scheme uri)
                          (symbol->string (uri-scheme uri)))))
        (and scheme
             (or (not (or (string-ci=? scheme "http")
                          (string-ci=? scheme "https")))
                 (and (string? (uri-host uri))
                      (not (string=? (uri-host uri) "")))))))
    (lambda _ #f)))

(define (entity-uri-href value)
  "Return a useful browser URL for an entity URI, or #f if unsupported."
  (catch #t
    (lambda ()
      (let* ((uri (string->uri value))
             (scheme (and (uri? uri) (uri-scheme uri)
                          (symbol->string (uri-scheme uri)))))
        (cond
          ((and (or (string-ci=? scheme "http")
                    (string-ci=? scheme "https"))
                (string? (uri-host uri))
                (not (string=? (uri-host uri) "")))
           value)
          ((and (string-ci=? scheme "doi")
                (string? (uri-path uri))
                (not (string=? (uri-path uri) ""))
                (not (uri-query uri))
                (not (uri-fragment uri)))
           (string-append "https://doi.org/" (uri-path uri)))
          (else #f))))
    (lambda _ #f)))

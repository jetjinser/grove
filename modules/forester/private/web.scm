;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private web)
  #:use-module (web uri)
  #:use-module (srfi srfi-13)
  #:export (web-origin?
            web-base-url
            web-absolute-route-url
            web-xml-escape))

(define (web-origin? url)
  (and (string? url)
       (catch #t
         (lambda ()
           (let* ((uri (string->uri url))
                  (scheme (and uri (uri? uri) (uri-scheme uri)))
                  (host (and uri (uri? uri) (uri-host uri)))
                  (path (and uri (uri? uri) (uri-path uri))))
             (and scheme
                  (member (string-downcase (symbol->string scheme))
                          '("http" "https"))
                  (and (string? host) (not (string-null? host)))
                  (not (uri-userinfo uri))
                  (or (not path) (string-null? path) (string=? path "/"))
                  (not (uri-query uri))
                  (not (uri-fragment uri)))))
         (lambda _ #f))))

(define (web-base-url url)
  (let ((end (string-length url)))
    (if (and (> end 0) (char=? (string-ref url (- end 1)) #\/))
        (web-base-url (substring url 0 (- end 1)))
        url)))

(define (web-absolute-route-url base route)
  (unless (and (string? route) (string-prefix? "/" route))
    (error "Web route URL must begin with /" route))
  (string-append base route))

(define (web-xml-escape text)
  (string-concatenate
    (map (lambda (character)
           (case character
             ((#\&) "&amp;")
             ((#\<) "&lt;")
             ((#\>) "&gt;")
             ((#\") "&quot;")
             ((#\') "&apos;")
             (else (string character))))
         (string->list text))))

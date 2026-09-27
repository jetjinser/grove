;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private web)
  #:use-module (srfi srfi-13)
  #:export (web-url?
            web-base-url
            web-absolute-url
            web-absolute-route-url
            web-xml-escape))

(define (web-url? url)
  (and (string? url)
       (or (string-prefix? "https://" url)
           (string-prefix? "http://" url))
       (> (string-length url)
          (if (string-prefix? "https://" url) 8 7))))

(define (web-base-url url)
  (let ((end (string-length url)))
    (if (and (> end 0) (char=? (string-ref url (- end 1)) #\/))
        (web-base-url (substring url 0 (- end 1)))
        url)))

(define (web-absolute-url base path)
  (string-append base "/" path))

(define (web-absolute-route-url base route)
  (unless (and (string? route) (string-prefix? "/" route))
    (error "Web route URL must begin with /" route))
  (web-absolute-url base (substring route 1)))

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

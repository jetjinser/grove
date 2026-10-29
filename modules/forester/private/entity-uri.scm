;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private entity-uri)
  #:use-module (grove private string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (web uri)
  #:export (absolute-entity-uri?
            valid-entity-uri-list?
            entity-uri-href))

(define (one-of? character characters)
  (and (string-index characters character) #t))

(define (unreserved? character)
  (or (ascii-alpha? character)
      (ascii-digit? character)
      (one-of? character "-._~")))

(define (sub-delimiter? character)
  (one-of? character "!$&'()*+,;="))

(define (pchar? character)
  (or (unreserved? character)
      (sub-delimiter? character)
      (one-of? character ":@")))

(define (valid-component? value allowed?)
  (or (not value)
      (let ((length (string-length value)))
        (let loop ((index 0))
          (or (= index length)
              (let ((character (string-ref value index)))
                (cond
                 ((char=? character #\%)
                  (and (< (+ index 2) length)
                       (ascii-hex? (string-ref value (+ index 1)))
                       (ascii-hex? (string-ref value (+ index 2)))
                       (loop (+ index 3))))
                 ((and (< (char->integer character) 128)
                       (allowed? character))
                  (loop (+ index 1)))
                 (else #f))))))))

(define (parse-absolute-uri value)
  (catch #t
    (lambda ()
      (let ((uri (and (string? value) (string->uri value))))
        (and uri
             (not (uri-userinfo uri))
             (valid-component?
               (uri-path uri)
               (lambda (character)
                 (or (pchar? character) (char=? character #\/))))
             (valid-component?
               (uri-query uri)
               (lambda (character)
                 (or (pchar? character)
                     (char=? character #\/)
                     (char=? character #\?))))
             (valid-component?
               (uri-fragment uri)
               (lambda (character)
                 (or (pchar? character)
                     (char=? character #\/)
                     (char=? character #\?))))
             (let ((scheme (symbol->string (uri-scheme uri))))
               (or (not (or (string-ci=? scheme "http")
                            (string-ci=? scheme "https")))
                   (and (string? (uri-host uri))
                        (not (string=? (uri-host uri) "")))))
             uri)))
    (lambda _ #f)))

(define (absolute-entity-uri? value)
  "Return true for a syntactically valid ASCII absolute URI."
  (and (parse-absolute-uri value) #t))

(define (valid-entity-uri-list? values)
  "Return true for a list of distinct, syntactically valid entity URIs."
  (and (list? values)
       (every absolute-entity-uri? values)
       (= (length values) (length (delete-duplicates values string=?)))))

(define (entity-uri-href value)
  "Return a useful browser URL for an entity URI, or #f if unsupported."
  (let ((uri (parse-absolute-uri value)))
    (and uri
         (let ((scheme (symbol->string (uri-scheme uri))))
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
            (else #f))))))

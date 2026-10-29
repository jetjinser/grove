;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private entity-iri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi :275 iri)
  #:use-module (srfi :275 path)
  #:export (absolute-entity-iri?
            distinct-entity-iri-list?
            valid-entity-iri-list?
            entity-iri-href))

(define (parse-absolute-iri value)
  (catch #t
    (lambda ()
      (let ((iri (and (string? value) (string->iri value))))
        (and iri
             (non-relative-iri? iri)
             (not (iri-user iri))
             (let ((scheme (iri-scheme iri)))
               (or (not (or (string-ci=? scheme "http")
                            (string-ci=? scheme "https")))
                   (and (string? (iri-host iri))
                        (not (string=? (iri-host iri) "")))))
             iri)))
    (lambda _ #f)))

(define (absolute-entity-iri? value)
  "Return true for a syntactically valid absolute entity IRI."
  (and (parse-absolute-iri value) #t))

(define (distinct-entity-iri-list? values)
  "Return true when VALUES is a list with no repeated string entries."
  (and (list? values)
       (every string? values)
       (= (length values) (length (delete-duplicates values string=?)))))

(define (valid-entity-iri-list? values)
  "Return true for a list of distinct, syntactically valid entity IRIs."
  (and (list? values)
       (every absolute-entity-iri? values)
       (distinct-entity-iri-list? values)))

(define (entity-iri-href value)
  "Return a useful browser link for an entity IRI, or #f if unsupported."
  (let ((iri (parse-absolute-iri value)))
    (and iri
         (let ((scheme (iri-scheme iri)))
           (cond
            ((or (string-ci=? scheme "http")
                 (string-ci=? scheme "https"))
             value)
            ((and (string-ci=? scheme "doi")
                  (string? (path->string (iri-path iri)))
                  (not (string=? (path->string (iri-path iri)) ""))
                  (not (iri-query iri))
                  (not (iri-fragment iri)))
             (string-append "https://doi.org/"
                            (path->string (iri-path iri))))
            (else #f))))))

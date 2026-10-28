;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester content-integration)
  #:use-module (srfi srfi-64)
  #:use-module (grove ir)
  #:use-module (grove render html)
  #:use-module (grove render html element)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (grove style html)
  #:use-module (forester render html)
  #:use-module (forester site))

(test-begin "forester content integration")

(let* ((definition (make-element #:tag "dfn" #:anchor "term-id"))
       (page (make-site-page "glossary" "Glossary"
               (make-content-document
                 (list (make-content-element
                         'paragraph
                         (list (definition "a term")))))))
       (forest (make-forest (make-site (list page)))))
  (test-assert "a Grove anchored custom element renders on a Forester page"
    (string-contains
      (assoc-ref (site->html forest
                             #:options
                             (make-html-options
                               #:routes (make-html-routes
                                          #:home-page "glossary")))
                 "glossary")
      "<dfn id=\"term-id\">a term</dfn>")))

(test-end "forester content integration")

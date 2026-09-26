;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (language scribble site)
  #:use-module (language scribble content)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:export (site-page?
            make-site-page
            site-page-id
            site-page-title
            site-page-document
            site->html))

(define-record-type <site-page>
  (%make-site-page id title document)
  site-page?
  (id site-page-id)
  (title site-page-title)
  (document site-page-document))

(define (make-site-page id title document)
  (unless (content-document? document)
    (error "site page requires a content document" document))
  (%make-site-page id title document))

(define (links-in content)
  (cond
    ((content-link? content)
     (cons (content-link-target content)
           (append-map links-in (content-link-children content))))
    ((content-element? content)
     (append-map links-in (content-element-children content)))
    ((content-document? content)
     (append-map links-in (content-document-children content)))
    (else '())))

(define (serialize sxml)
  (call-with-output-string (lambda (port) (sxml->xml sxml port))))

(define (page-url page)
  (string-append "/" (site-page-id page) ".html"))

(define (backlinks-sxml incoming)
  (if (null? incoming)
      '()
      (list
        (list 'aside '(@ (class "backlinks"))
              '(h2 "Linked from")
              (cons 'ul
                    (map (lambda (source)
                           (list 'li
                                 (list 'a
                                       (list '@ (list 'href (page-url source)))
                                       (site-page-title source))))
                         incoming))))))

(define (site->html pages)
  (let* ((ids (map site-page-id pages))
         (duplicate (find (lambda (id)
                            (> (count (lambda (candidate)
                                        (equal? id candidate))
                                      ids)
                               1))
                          ids)))
    (when duplicate (error "duplicate site page id" duplicate))
    (for-each
      (lambda (page)
        (for-each
          (lambda (target)
            (unless (member target ids equal?)
              (error "unresolved Scribble link"
                     (site-page-id page) target)))
          (links-in (site-page-document page))))
      pages)
    (map
      (lambda (page)
        (let* ((id (site-page-id page))
               (incoming
                 (filter (lambda (source)
                           (member id
                                   (links-in (site-page-document source))
                                   equal?))
                         pages))
               (body (content->sxml (site-page-document page)))
               (article (append body (backlinks-sxml incoming)))
               (html
                 `(html
                    (head
                      (meta (@ (charset "utf-8")))
                      (title ,(site-page-title page))
                      (script (@ (src "https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-chtml.js")
                                 (async "async"))
                              " "))
                    (body
                      (header (h1 ,(site-page-title page)))
                      ,article))))
          (cons id (serialize html))))
      pages)))

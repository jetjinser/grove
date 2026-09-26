;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module (sxml simple)
  #:export (content->sxml
            site->html))

(define (element->sxml element)
  (let* ((kind (content-element-kind element))
         (children (map content->sxml (content-element-children element))))
    (case kind
      ((title) (cons 'h1 children))
      ((bold) (cons 'strong children))
      ((paragraph) (cons 'p children))
      ((verbatim) (cons 'pre children))
      ((link)
       `(a (@ (href ,(string-append
                       "/" (content-element-property element 'target) ".html")))
           ,@children))
      ((math)
       (let ((display? (content-element-property element 'display?))
             (source-text
               (content-verbatim-string (content-element-children element))))
         `(,(if display? 'div 'span)
            (@ (class ,(if display? "math-display" "math-inline")))
            ,(string-append (if display? "\\[" "\\(")
                            source-text
                            (if display? "\\]" "\\)")))))
      (else
       `(span (@ (class ,(symbol->string kind))) ,@children)))))

(define (content->sxml content)
  (cond
    ((string? content) content)
    ((content-text? content) (content-text-value content))
    ((content-layout? content) (content-layout-raw content))
    ((content-element? content) (element->sxml content))
    ((content-document? content)
     (cons 'article
           (map content->sxml (content-document-children content))))
    (else (error "not a content node" content))))

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

(define (html-document title body math? home-title)
  `(html
     (head
       (meta (@ (charset "utf-8")))
       (title ,title)
       ,@(if math?
             '((script (@ (src "https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-chtml.js")
                          (async "async"))
                       " "))
             '()))
     (body
       (header
         ,@(if home-title
               (list `(nav (@ (aria-label "Site"))
                           (a (@ (href "/index.html")) ,home-title)))
               '())
         (h1 ,title))
       ,@body)))

(define (index-sxml site)
  (html-document
    (site-title site)
    (list
      `(main
         (nav (@ (aria-label "Pages"))
              (ul
                ,@(map (lambda (page)
                         `(li (a (@ (href ,(page-url page)))
                                 ,(site-page-title page))))
                       (site-pages site))))))
    #f
    #f))

(define (site->html site)
  (unless (site? site)
    (error "HTML renderer requires a Grove site" site))
  (when (member "index" (map site-page-id (site-pages site)))
    (error "HTML index path conflicts with site page" "index"))
  (cons
    (cons "index" (serialize (index-sxml site)))
    (map
      (lambda (page)
        (let* ((incoming (site-backlinks site (site-page-id page)))
               (body (content->sxml (site-page-document page)))
               (article (append body (backlinks-sxml incoming)))
               (html (html-document
                       (site-page-title page)
                       (list article)
                       #t
                       (site-title site))))
          (cons (site-page-id page) (serialize html))))
      (site-pages site))))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (grove style html)
  #:use-module (sxml simple)
  #:export (content->sxml
            site->html
            render-html
            make-html-renderer))

(define (element->sxml element routes)
  (let* ((kind (content-element-kind element))
         (children (map (lambda (child)
                          (content->sxml child routes))
                        (content-element-children element))))
    (case kind
      ((title) (cons 'h1 children))
      ((bold) (cons 'strong children))
      ((paragraph) (cons 'p children))
      ((verbatim) (cons 'pre children))
      ((link)
       `(a (@ (href ,(html-page-url
                       routes (content-element-property element 'target))))
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

(define* (content->sxml content #:optional (routes (make-html-routes)))
  (cond
    ((string? content) content)
    ((content-text? content) (content-text-value content))
    ((content-layout? content) (content-layout-raw content))
    ((content-element? content) (element->sxml content routes))
    ((content-document? content)
     (cons 'article
           (map (lambda (child)
                  (content->sxml child routes))
                (content-document-children content))))
    (else (error "not a content node" content))))

(define (serialize sxml)
  (call-with-output-string (lambda (port) (sxml->xml sxml port))))

(define (backlinks-sxml incoming routes)
  (if (null? incoming)
      '()
      (list
        (list 'aside '(@ (class "backlinks"))
              '(h2 "Linked from")
              (cons 'ul
                    (map (lambda (source)
                           (list 'li
                                 (list 'a
                                       (list '@ (list 'href
                                                      (html-page-url
                                                        routes
                                                        (site-page-id source))))
                                       (site-page-title source))))
                         incoming))))))

(define (html-document title body math? home-title style routes)
  `(html
     (head
       (meta (@ (charset "utf-8")))
       (title ,title)
       ,@(map (lambda (stylesheet)
               `(link (@ (rel "stylesheet")
                         (href ,(html-asset-url
                                  routes (artifact-path stylesheet))))))
             (html-style-stylesheets style))
       ,@(if math?
             '((script (@ (src "https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-chtml.js")
                          (async "async"))
                       " "))
             '()))
     (body
       (header
         ,@(if home-title
               (list `(nav (@ (aria-label "Site"))
                           (a (@ (href ,(html-index-url routes)))
                              ,home-title)))
               '())
         (h1 ,title))
       ,@body)))

(define (index-sxml site style routes)
  (html-document
    (site-title site)
    (list
      `(main
         (nav (@ (aria-label "Pages"))
              (ul
                ,@(map (lambda (page)
                         `(li (a (@ (href ,(html-page-url
                                               routes (site-page-id page))))
                                 ,(site-page-title page))))
                       (site-pages site))))))
    #f
    #f
    style
    routes))

(define (check-html-input site style routes)
  (unless (site? site)
    (error "HTML renderer requires a Grove site" site))
  (unless (html-style? style)
    (error "HTML renderer requires an HTML style" style))
  (unless (html-routes? routes)
    (error "HTML renderer requires HTML routes" routes)))

(define (index-html site style routes)
  (serialize (index-sxml site style routes)))

(define (page-html site page style routes)
  (let* ((incoming (site-backlinks site (site-page-id page)))
         (body (content->sxml (site-page-document page) routes))
         (article (append body (backlinks-sxml incoming routes)))
         (html (html-document
                 (site-page-title page)
                 (list article)
                 #t
                 (site-title site)
                 style
                 routes)))
    (serialize html)))

(define* (site->html site #:key
                     (style (make-html-style))
                     (routes (make-html-routes)))
  (check-html-input site style routes)
  (when (member "index" (map site-page-id (site-pages site)))
    (error "HTML alist key conflicts with site page" "index"))
  (cons
    (cons "index" (index-html site style routes))
    (map
      (lambda (page)
        (cons (site-page-id page)
              (page-html site page style routes)))
      (site-pages site))))

(define* (render-html site #:key
                      (style (make-html-style))
                      (routes (make-html-routes)))
  (check-html-input site style routes)
  (append
    (html-style-artifacts style)
    (cons
      (make-artifact (html-index-path routes)
                     (index-html site style routes))
      (map (lambda (page)
             (make-artifact
               (html-page-path routes (site-page-id page))
               (page-html site page style routes)))
           (site-pages site)))))

(define* (make-html-renderer #:key
                             (style (make-html-style))
                             (routes (make-html-routes)))
  (lambda (site)
    (render-html site #:style style #:routes routes)))

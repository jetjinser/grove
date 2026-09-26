;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render html)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render html)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (grove style html)
  #:export (forest->html
            render-forest-html
            make-forester-html-renderer))

(define (link-handlers routes)
  `((link . ,(lambda (element children)
               `(a (@ (href ,(html-page-url
                               routes
                               (content-element-property element 'target))))
                   ,@children)))))

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

(define (index-html forest style routes)
  (let ((site (forest-site forest)))
    (sxml->html
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
        routes))))

(define (page-html forest page style routes)
  (let* ((site (forest-site forest))
         (incoming (forest-backlinks forest (site-page-id page)))
         (body (content->sxml
                 (site-page-document page)
                 #:handlers (link-handlers routes)))
         (article (append body (backlinks-sxml incoming routes))))
    (sxml->html
      (html-document
        (site-page-title page)
        (list article)
        #t
        (site-title site)
        style
        routes))))

(define (check-html-input forest style routes)
  (unless (forest? forest)
    (error "Forester HTML renderer requires a forest" forest))
  (unless (html-style? style)
    (error "Forester HTML renderer requires an HTML style" style))
  (unless (html-routes? routes)
    (error "Forester HTML renderer requires HTML routes" routes)))

(define* (forest->html forest #:key
                       (style (make-html-style))
                       (routes (make-html-routes)))
  (check-html-input forest style routes)
  (let ((site (forest-site forest)))
    (when (member "index" (map site-page-id (site-pages site)))
      (error "HTML alist key conflicts with forest page" "index"))
    (cons
      (cons "index" (index-html forest style routes))
      (map (lambda (page)
             (cons (site-page-id page)
                   (page-html forest page style routes)))
           (site-pages site)))))

(define* (render-forest-html forest #:key
                             (style (make-html-style))
                             (routes (make-html-routes)))
  (check-html-input forest style routes)
  (append
    (html-style-artifacts style)
    (cons
      (make-artifact (html-index-path routes)
                     (index-html forest style routes))
      (map (lambda (page)
             (make-artifact
               (html-page-path routes (site-page-id page))
               (page-html forest page style routes)))
           (site-pages (forest-site forest))))))

(define* (make-forester-html-renderer #:key
                                      (style (make-html-style))
                                      (routes (make-html-routes)))
  (lambda (forest)
    (render-forest-html forest #:style style #:routes routes)))

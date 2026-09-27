;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render html)
  #:use-module (grove build)
  #:use-module (forester site)
  #:use-module ((grove output) #:select (artifact-path?))
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render html)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (grove style html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (forest->html
            render-forest-html
            make-forester-html-renderer))

(define (link-url routes reference)
  (string-append
    (html-page-url routes
                   (forest-reference-target-page-id reference))
    (if (forest-reference-target-fragment reference)
        (string-append "#"
                       (forest-reference-target-fragment reference))
        "")))

(define (page-content-handlers forest routes page)
  (let ((heading-ids (forest-heading-ids forest (site-page-id page)))
        (references (forest-outgoing-references
                      forest (site-page-id page))))
    `((link . ,(lambda (element children)
                 (when (null? references)
                   (error "Forester link rendering exceeded its index"
                          (site-page-id page)))
                 (let ((reference (car references)))
                   (set! references (cdr references))
                   `(a (@ (href ,(link-url routes reference)))
                     ,@children))))
      (heading . ,(lambda (_ children)
                    (when (null? heading-ids)
                      (error "Forester heading rendering exceeded its index"
                             (site-page-id page)))
                    (let ((id (car heading-ids)))
                      (set! heading-ids (cdr heading-ids))
                      `(h2 (@ (id ,id)) ,@children)))))))

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

(define (table-of-contents-sxml forest page)
  (let ((headings (forest-headings forest (site-page-id page))))
    (if (null? headings)
        '()
        (list
          `(nav (@ (class "table-of-contents") (aria-label "Contents"))
             (h2 "Contents")
             (ol
               ,@(map (lambda (heading)
                        `(li
                           (a (@ (href ,(string-append
                                          "#" (forest-heading-id heading))))
                              ,(forest-heading-label heading))))
                      headings)))))))

(define default-mathjax-src
  "https://cdn.jsdelivr.net/npm/mathjax@4/tex-chtml.js")

(define (external-url? value)
  (or (string-prefix? "https://" value)
      (string-prefix? "http://" value)
      (string-prefix? "//" value)))

(define (mathjax-script-url source routes)
  (and source
       (if (external-url? source)
           source
           (html-asset-url routes source))))

(define (content-has-math? content)
  (cond
    ((content-element? content)
     (or (eq? (content-element-kind content) 'math)
         (any content-has-math? (content-element-children content))))
    ((content-document? content)
     (any content-has-math? (content-document-children content)))
    (else #f)))

(define (page-description site page)
  (or (assq-ref (site-page-metadata page) 'summary)
      (site-description site)))

(define (html-document title description body math? home-title style routes
                       mathjax-src)
  `(html
     (head
       (meta (@ (charset "utf-8")))
       (title ,title)
       ,@(if description
             (list `(meta (@ (name "description") (content ,description))))
             '())
       ,@(map (lambda (stylesheet)
               `(link (@ (rel "stylesheet")
                         (href ,(html-asset-url
                                  routes (artifact-path stylesheet))))))
             (html-style-stylesheets style))
       ,@(if (and math? mathjax-src)
             `((script (@ (src ,(mathjax-script-url mathjax-src routes))
                         (defer "defer"))
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

(define (index-html forest style routes mathjax-src)
  (let ((site (forest-site forest)))
    (sxml->html
      (html-document
        (site-title site)
        (site-description site)
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
        routes
        mathjax-src))))

(define (page-html forest page style routes mathjax-src)
  (let* ((site (forest-site forest))
         (incoming (forest-backlinks forest (site-page-id page)))
         (body (content->sxml
                 (site-page-document page)
                 #:handlers (page-content-handlers forest routes page)))
         (article (append body (backlinks-sxml incoming routes))))
    (sxml->html
      (html-document
        (site-page-title page)
        (page-description site page)
        (append (table-of-contents-sxml forest page) (list article))
        (content-has-math? (site-page-document page))
        (site-title site)
        style
        routes
        mathjax-src))))

(define (find-artifact artifacts path)
  (find (lambda (artifact) (string=? (artifact-path artifact) path))
        artifacts))

(define (artifact-current? current previous changed)
  (let ((sources (artifact-sources current)))
    (and previous
         (equal? sources (artifact-sources previous))
         (not (and sources
                   (any (lambda (path) (member path sources string=?))
                        changed))))))

(define (render-forest-html-changed forest style routes mathjax-src
                                    changed old-artifacts)
  (check-html-input forest style routes mathjax-src)
  (let* ((site (forest-site forest))
         (index-sources (forest-source-paths forest))
         (index-path (html-index-path routes))
         (old-index (find-artifact old-artifacts index-path))
         (index-current
           (make-artifact index-path "" index-sources))
         (index-artifact
           (make-artifact index-path
                          (if (artifact-current? index-current old-index changed)
                              (artifact-content old-index)
                              (index-html forest style routes mathjax-src))
                          index-sources))
         (page-artifacts
           (map (lambda (page)
                  (let* ((path (html-page-path routes (site-page-id page)))
                         (sources (forest-page-source-paths forest page))
                         (old (find-artifact old-artifacts path))
                         (current (make-artifact path "" sources)))
                    (if (artifact-current? current old changed)
                        old
                        (make-artifact
                          path
                          (page-html forest page style routes mathjax-src)
                          sources))))
                (site-pages site))))
    (append (html-style-artifacts style)
            (cons index-artifact page-artifacts))))

(define (check-html-input forest style routes mathjax-src)
  (unless (forest? forest)
    (error "Forester HTML renderer requires a forest" forest))
  (unless (html-style? style)
    (error "Forester HTML renderer requires an HTML style" style))
  (unless (html-routes? routes)
    (error "Forester HTML renderer requires HTML routes" routes))
  (unless (or (not mathjax-src)
              (and (string? mathjax-src)
                   (not (string-null? mathjax-src))
                   (or (external-url? mathjax-src)
                       (artifact-path? mathjax-src))))
    (error "MathJax source must be a URL, relative artifact path, or #f"
           mathjax-src)))

(define* (forest->html forest #:key
                       (style (make-html-style))
                       (routes (make-html-routes))
                       (mathjax-src default-mathjax-src))
  (check-html-input forest style routes mathjax-src)
  (let ((site (forest-site forest)))
    (when (member "index" (map site-page-id (site-pages site)))
      (error "HTML alist key conflicts with forest page" "index"))
    (cons
      (cons "index" (index-html forest style routes mathjax-src))
      (map (lambda (page)
             (cons (site-page-id page)
                   (page-html forest page style routes mathjax-src)))
           (site-pages site)))))

(define* (render-forest-html forest #:key
                             (style (make-html-style))
                             (routes (make-html-routes))
                             (mathjax-src default-mathjax-src))
  (check-html-input forest style routes mathjax-src)
  (append
    (html-style-artifacts style)
    (cons
      (make-artifact (html-index-path routes)
                     (index-html forest style routes mathjax-src)
                     (forest-source-paths forest))
      (map (lambda (page)
             (make-artifact
               (html-page-path routes (site-page-id page))
               (page-html forest page style routes mathjax-src)
               (forest-page-source-paths forest page)))
           (site-pages (forest-site forest))))))

(define* (make-forester-html-renderer #:key
                                      (style (make-html-style))
                                      (routes (make-html-routes))
                                      (mathjax-src default-mathjax-src)
                                      (cache-key #f))
  (let ((render (lambda (forest)
                  (render-forest-html forest
                                      #:style style
                                      #:routes routes
                                      #:mathjax-src mathjax-src))))
    (if cache-key
        (make-cached-renderer
          render
          #:cache-key (string-append "forester-html-v2:" cache-key)
          #:render-changed
          (lambda (forest changed dirty-paths old-artifacts)
            (render-forest-html-changed forest style routes mathjax-src
                                        changed old-artifacts)))
        render)))

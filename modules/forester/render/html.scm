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

(define (reference-source-url routes reference)
  (string-append
    (html-page-url routes (forest-reference-source-page-id reference))
    "#" (forest-reference-source-id reference)))

(define* (page-content-handlers forest routes page #:optional (anchor-prefix ""))
  (let ((heading-ids (forest-heading-ids forest (site-page-id page)))
        (references (forest-outgoing-references
                      forest (site-page-id page))))
    `((link . ,(lambda (element children)
                 (when (null? references)
                   (error "Forester link rendering exceeded its index"
                          (site-page-id page)))
                 (let ((reference (car references)))
                   (set! references (cdr references))
                   `(a (@ (id ,(string-append anchor-prefix
                                             (forest-reference-source-id reference)))
                         (href ,(link-url routes reference)))
                     ,@children))))
      (heading . ,(lambda (_ children)
                    (when (null? heading-ids)
                      (error "Forester heading rendering exceeded its index"
                             (site-page-id page)))
                    (let ((id (car heading-ids)))
                      (set! heading-ids (cdr heading-ids))
                      `(h2 (@ (id ,(string-append anchor-prefix id)))
                           ,@children)))))))

(define* (subtree-content-sxml forest routes page #:optional (anchor-prefix ""))
  (let ((transclusions
          (forest-outgoing-transclusions forest (site-page-id page))))
   (content->sxml
    (site-page-document page)
    #:handlers (page-content-handlers
                 forest routes page
                 anchor-prefix)
    #:lazy-handlers
    (list (cons 'subtree (lambda (element _render)
                    (let* ((id (content-element-property element 'tree-id))
                           (target (forest-page forest id)))
                      (tree-block-sxml
                        "inline-subtree" id (site-page-title target)
                        (html-page-url routes id)
                        (subtree-content-sxml
                          forest routes target
                          (string-append anchor-prefix "tree-" id "--"))))))
          (cons 'transclude (lambda (element _render)
                       (let* ((id (content-element-property element 'target))
                              (transclusion (car transclusions))
                              (target (forest-page forest id)))
                         (set! transclusions (cdr transclusions))
                         (tree-block-sxml
                           "transclusion" id (site-page-title target)
                           (html-page-url routes id)
                           (subtree-content-sxml
                             forest routes target
                             (string-append anchor-prefix "transcluded-"
                                            (forest-transclusion-source-id
                                              transclusion)
                                            "--"))))))))))

(define (tree-block-sxml kind id title page-url content)
  `(section (@ (class ,(string-append "forest-tree-block " kind))
               (data-tree-id ,id))
     (details (@ (open "open"))
       (summary
         (span (@ (class "forest-tree-title")) ,title)
         ,@(if (string-prefix? "unstable-" id)
               '()
               (list
                 " "
                 `(a (@ (class "forest-tree-id")
                       (href ,page-url)
                       (aria-label ,(string-append
                                      "Open tree " id " as a page")))
                    "[" ,id "]"))))
       (div (@ (class "forest-tree-content")) ,@content))))

(define (backlink-groups forest page)
  (let* ((page-id (site-page-id page))
         (incoming (forest-incoming-references forest page-id))
         (page-references
           (filter (lambda (reference)
                     (not (forest-reference-target-fragment reference)))
                   incoming))
         (heading-groups
           (filter-map
             (lambda (heading)
               (let* ((fragment (forest-heading-id heading))
                      (references
                        (forest-incoming-references forest page-id fragment)))
                 (and (pair? references) (cons fragment references))))
             (forest-headings forest page-id))))
    (append (if (null? page-references) '() (list (cons #f page-references)))
            heading-groups)))

(define (backlink-reference-sxml forest routes reference)
  (let ((source
          (find (lambda (candidate)
                  (equal? (site-page-id candidate)
                          (forest-reference-source-page-id reference)))
                (forest-pages forest))))
    `(li
       (a (@ (href ,(reference-source-url routes reference)))
          ,(site-page-title source))
       " — "
       ,(forest-reference-label reference))))

(define (backlink-group-sxml forest page routes group)
  (let* ((fragment (car group))
         (target-heading
           (and fragment
                (find (lambda (heading)
                        (string=? fragment (forest-heading-id heading)))
                      (forest-headings forest (site-page-id page)))))
         (references (cdr group))
         (label (if target-heading
                    `(a (@ (href ,(string-append "#" fragment)))
                        ,(forest-heading-label target-heading))
                    "This page")))
    `(li ,label
         (ul ,@(map (lambda (reference)
                      (backlink-reference-sxml forest routes reference))
                    references)))))

(define (backlinks-sxml forest page routes)
  (let ((groups (backlink-groups forest page)))
    (if (null? groups)
        '()
        (list `(aside (@ (class "backlinks"))
                      (h2 "Linked from")
                      (ul ,@(map (lambda (group)
                                   (backlink-group-sxml forest page routes group))
                                 groups)))))))

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

(define tree-block-style
  ".forest-tree-block { margin: 1rem 0; border-radius: .3rem; transition: background-color .12s ease; }\n.forest-tree-block:hover, .forest-tree-block:focus-within { background-color: rgba(0, 100, 255, .04); }\n.forest-tree-block summary { cursor: pointer; list-style: none; }\n.forest-tree-block summary::marker { content: ''; }\n.forest-tree-block summary::-webkit-details-marker { display: none; }\n.forest-tree-title { font-weight: 600; }\n.forest-tree-id { margin-left: .45em; font-size: .75em; opacity: .65; }\n.forest-tree-id:hover { opacity: 1; }\n.forest-tree-content { padding: .25rem .85rem; }")

(define fragment-reveal-script
  "(function () {\n  function revealFragment() {\n    if (!location.hash) return;\n    var id = location.hash.slice(1);\n    try { id = decodeURIComponent(id); } catch (error) {}\n    var target = document.getElementById(id);\n    if (!target) return;\n    var node = target;\n    while (node) {\n      if (node.tagName === 'DETAILS') node.open = true;\n      node = node.parentElement;\n    }\n    window.requestAnimationFrame(function () { target.scrollIntoView(); });\n  }\n  window.addEventListener('hashchange', revealFragment);\n  revealFragment();\n}());")

(define (html-document title description body math? home-title style routes
                       mathjax-src)
  `(html
     (head
       (meta (@ (charset "utf-8")))
       (title ,title)
       ,@(if description
             (list `(meta (@ (name "description") (content ,description))))
             '())
       (style ,tree-block-style)
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
       ,@body
       (script ,fragment-reveal-script))))

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
         (body (subtree-content-sxml forest routes page))
         (article (append body (backlinks-sxml forest page routes))))
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
                (forest-pages forest))))
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
    (when (member "index" (map site-page-id (forest-pages forest)))
      (error "HTML alist key conflicts with forest page" "index"))
    (cons
      (cons "index" (index-html forest style routes mathjax-src))
      (map (lambda (page)
             (cons (site-page-id page)
                   (page-html forest page style routes mathjax-src)))
           (forest-pages forest)))))

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
           (forest-pages forest)))))

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
          #:cache-key (string-append "forester-html-v4:" cache-key)
          #:render-changed
          (lambda (forest changed dirty-paths old-artifacts)
            (render-forest-html-changed forest style routes mathjax-src
                                        changed old-artifacts)))
        render)))

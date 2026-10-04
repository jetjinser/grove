;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render html)
  #:use-module (grove build)
  #:use-module (grove build state)
  #:use-module (grove string-fun)
  #:use-module (forester render artifact)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render html)
  #:use-module (grove highlight)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (forester backmatter)
  #:use-module (grove style html)
  #:use-module (forester style html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (site->html
            render-html
            make-html-renderer))

(define (link-url routes reference)
  (html-page-url routes (reference-target-page-id reference)))

(define (reference-source-url routes reference)
  (string-append
    (html-page-url routes (reference-source-page-id reference))
    "#" (reference-source-id reference)))

(define* (page-content-handlers forest routes page #:optional (anchor-prefix ""))
  (let ((references (outgoing-references forest (site-page-id page))))
    `((tag . ,(lambda (_element _children) '()))
      (taxon . ,(lambda (_element _children) '()))
      (image . ,(lambda (element children)
                 (let* ((source (content-element-property element 'source))
                        (url (if (or (external-url? source)
                                     (string-prefix? "/" source))
                                 source
                                 (html-asset-url routes source)))
                        (alt (content-verbatim-string children)))
                   `(img (@ (src ,url) (alt ,alt))))))
      (link . ,(lambda (element children)
                 (when (null? references)
                   (error "Forester link rendering exceeded its index"
                          (site-page-id page)))
                 (let ((reference (car references)))
                   (set! references (cdr references))
                   `(a (@ (id ,(string-append anchor-prefix
                                             (reference-source-id reference)))
                     (href ,(link-url routes reference)))
                     ,@children))))
      (query-link . ,(lambda (element children)
                       `(a (@ (href ,(html-page-url routes
                                        (content-element-property element 'target))))
                           ,@children))))))

(define* (subtree-content-sxml forest routes page
                              #:optional (anchor-prefix "") (depth 1) (highlighters '()))
  (let ((position 0))
    (define (render-tree element _render)
      (set! position (1+ position))
      (let* ((id (tree-occurrence-target element))
             (target (find-page forest id))
             (anchor (format #f "~atree-~a" anchor-prefix position)))
        (tree-block-sxml
          (if (eq? (content-element-kind element) 'subtree)
              "inline-subtree" "transclusion")
          id (site-page-title target) (html-page-url routes id)
          (subtree-content-sxml forest routes target (string-append anchor "--") (1+ depth) highlighters)
          #:anchor anchor #:depth (1+ depth)
          #:open? (if (assq 'open? (content-element-properties element))
                      (content-element-property element 'open?) #t))))
    (content->sxml
      (site-page-document page)
      #:id-prefix anchor-prefix
      #:highlighters highlighters
      #:handlers (page-content-handlers forest routes page anchor-prefix)
      #:lazy-handlers (map (lambda (kind) (cons kind render-tree))
                          '(subtree transclude query-transclude)))))

(define (display-tree-id id)
  (if (string-prefix? "unstable-" id)
      (string-append "#" (substring id (string-length "unstable-")))
      id))

(define* (tree-block-sxml kind id title page-url content #:key (open? #t) anchor (depth 2))
  `(section (@ (class ,(string-append "forest-tree-block " kind))
               (data-tree-id ,id) (id ,anchor))
     (details ,@(if open? '((@ (open "open"))) '())
       (summary
         (,(string->symbol (format #f "h~a" (min depth 6)))
           (@ (class "forest-tree-title")) (span ,title)
           (a (@ (class "forest-tree-id")
               (href ,page-url)
               (aria-label ,(string-append
                              "Open tree " (display-tree-id id)
                              " as a page")))
              "[" ,(display-tree-id id) "]")))
       (div (@ (class "forest-tree-content"))
            ,@(if (and (pair? content) (eq? (car content) 'article))
                  (cdr content)
                  (list content))))))

(define (backlink-reference-sxml forest routes reference)
  (let ((source
          (find (lambda (candidate)
                  (equal? (site-page-id candidate)
                          (reference-source-page-id reference)))
                (pages forest))))
    `(li
       (a (@ (href ,(reference-source-url routes reference)))
          ,(site-page-title source))
       " — "
       ,(reference-label reference))))

(define (backlinks-sxml forest page routes label)
  (let ((references (incoming-references forest (site-page-id page))))
    (if (null? references)
        '()
        (list `(aside (@ (class "backmatter backlinks"))
                      (h2 ,label)
                      (ul ,@(map (lambda (reference)
                                   (backlink-reference-sxml forest routes reference))
                                 references)))))))

(define (context-sxml forest page routes label)
  (let* ((sources (transcluding-pages forest (site-page-id page)))
         (items
           (map (lambda (source)
                  (list 'li
                    (list 'a
                      (list '@
                        (list 'href
                          (html-page-url routes (site-page-id source))))
                      (site-page-title source))))
                sources)))
    (if (null? sources)
        '()
        (list `(aside (@ (class "backmatter context"))
                      (h2 ,label)
                      (ul ,@items))))))

(define (page-list-backmatter-sxml routes class label targets)
    (if (null? targets)
        '()
        (list `(aside (@ (class ,(string-append "backmatter " class)))
                      (h2 ,label)
                      (ul
                        ,@(map (lambda (target)
                                 (list 'li
                                   (list 'a
                                     (list '@
                                       (list 'href
                                         (html-page-url
                                           routes (site-page-id target))))
                                     (site-page-title target))))
                               targets))))))

(define (related-sxml forest page routes reference-taxon-id label)
  (page-list-backmatter-sxml
    routes "related" label
    (related-pages forest (site-page-id page) reference-taxon-id)))

(define (references-sxml forest page routes reference-taxon-id label)
  (page-list-backmatter-sxml
    routes "references" label
    (reference-pages forest (site-page-id page) reference-taxon-id)))

(define (backmatter-sxml forest page routes sections labels
                         reference-taxon-id)
  (append-map
    (lambda (section)
      (case section
        ((context) (context-sxml forest page routes
                                 (assq-ref labels 'context)))
        ((references) (references-sxml forest page routes reference-taxon-id
                                       (assq-ref labels 'references)))
        ((backlinks) (backlinks-sxml forest page routes
                                     (assq-ref labels 'backlinks)))
        ((related) (related-sxml forest page routes reference-taxon-id
                                 (assq-ref labels 'related)))))
    sections))

(define (table-of-contents-sxml forest page)
  (define (entries id prefix)
    (map (lambda (element position)
           (let* ((target-id (tree-occurrence-target element))
                  (target (find-page forest target-id))
                  (anchor (format #f "~atree-~a" prefix position))
                  (nested (entries target-id (string-append anchor "--"))))
             `(li (a (@ (href ,(string-append "#" anchor))) ,(site-page-title target))
                  ,@(if (null? nested) '() (list `(ol ,@nested))))))
         (tree-occurrences forest id)
         (iota (length (tree-occurrences forest id)) 1)))
  (let ((items (entries (site-page-id page) "")))
    (if (null? items) '()
        (list `(nav (@ (class "table-of-contents") (aria-label "Contents"))
                    (h2 "Contents") (ol ,@items))))))

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

(define (content-has-tex-math? content)
  (cond
    ((content-element? content)
     (or (and (eq? (content-element-kind content) 'math)
              (content-element-property content 'tex?))
         (any content-has-tex-math? (content-element-children content))))
    ((content-document? content)
     (any content-has-tex-math? (content-document-children content)))
    (else #f)))

(define (page-description site page)
  (or (assq-ref (site-page-metadata page) 'summary)
      (site-description site)))

(define fragment-reveal-script
  "(function () {\n  function revealFragment() {\n    if (!location.hash) return;\n    var id = location.hash.slice(1);\n    try { id = decodeURIComponent(id); } catch (error) {}\n    var target = document.getElementById(id);\n    if (!target) return;\n    var node = target;\n    while (node) {\n      if (node.tagName === 'DETAILS') node.open = true;\n      node = node.parentElement;\n    }\n    window.requestAnimationFrame(function () { target.scrollIntoView(); });\n  }\n  window.addEventListener('hashchange', revealFragment);\n  revealFragment();\n}());")

(define (html-document title title-id description body math? home-title style
                       routes mathjax-src inline-stylesheets?)
  `(html
     (head
       (meta (@ (charset "utf-8")))
       (title ,title)
       ,@(if description
             (list `(meta (@ (name "description") (content ,description))))
             '())
       ,@(if inline-stylesheets?
             (map (lambda (stylesheet)
                    `(style ,(artifact-content stylesheet)))
                  (html-style-stylesheets style))
             (map (lambda (stylesheet)
                    `(link (@ (rel "stylesheet")
                              (href ,(html-asset-url
                                       routes (artifact-path stylesheet))))))
                  (html-style-stylesheets style)))
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
       (h1 ,title
           ,@(if title-id
                 (list `(a (@ (class "forest-tree-id")
                             (href ,(html-page-url routes title-id))
                             (aria-label ,(string-append
                                            "Open tree "
                                            (display-tree-id title-id)
                                            " as a page")))
                          "[" ,(display-tree-id title-id) "]"))
                 '())))
       ,@body
       (script ,fragment-reveal-script))))

(define (serialize-html-document sxml)
  (string-append "<!DOCTYPE html>\n" (sxml->html sxml)))

(define (index-redirect-html routes page)
  (let ((url (html-page-url routes (site-page-id page))))
    (serialize-html-document
      `(html
         (head
           (meta (@ (charset "utf-8")))
           (meta (@ (http-equiv "refresh")
                    (content ,(string-append "0;url=" url))))
           (link (@ (rel "canonical") (href ,url))))
         (body
           (p "Redirecting to "
              (a (@ (href ,url)) ,(site-page-title page))
              "."))))))

(define (index-content forest routes)
  (let* ((home-id (html-home-page routes))
         (home (find-page forest home-id)))
    (unless home
      (error "configured HTML home page was not found" home-id))
    (when (string=? (html-index-path routes)
                    (html-page-path routes home-id))
      (error "HTML home page route conflicts with the index path"
             home-id (html-index-path routes)))
    (index-redirect-html routes home)))

(define* (page-html forest page style routes mathjax-src backmatter labels
                    reference-taxon-id
                    #:optional (inline-stylesheets? #f) (highlighters '()))
  (let* ((site (site forest))
         (body (subtree-content-sxml forest routes page "" 1 highlighters))
         (article (append body
                          (backmatter-sxml forest page routes backmatter
                                           labels reference-taxon-id))))
    (serialize-html-document
      (html-document
        (site-page-title page)
        (site-page-id page)
        (page-description site page)
        (append (table-of-contents-sxml forest page) (list article))
        (content-has-tex-math? (site-page-document page))
        (site-title site)
        style
        routes
        mathjax-src
        inline-stylesheets?))))

(define (render-html-changed forest style routes mathjax-src backmatter labels
                             reference-taxon-id
                             changed old-artifacts highlighters)
  (check-html-input forest style routes mathjax-src)
  (let* ((site (site forest))
         (index-sources (source-paths forest))
         (index-path (html-index-path routes))
         (index-artifact
           (reuse-artifact index-path index-sources old-artifacts changed
             (lambda () (index-content forest routes))))
         (page-artifacts
           (map (lambda (page)
                  (let* ((path (html-page-path routes (site-page-id page)))
                         (sources (page-source-paths forest page)))
                    (reuse-artifact path sources old-artifacts changed
                      (lambda ()
                        (page-html forest page style routes mathjax-src
                                   backmatter labels reference-taxon-id #f highlighters)))))
                (pages forest))))
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
              (and (non-empty-string? mathjax-src)
                   (or (external-url? mathjax-src)
                       (artifact-path? mathjax-src))))
    (error "MathJax source must be a URL, relative artifact path, or #f"
           mathjax-src)))

(define* (site->html forest #:key
                       (style default-html-style)
                       (highlighters (default-highlighters))
                       routes
                       (mathjax-src default-mathjax-src)
                       (backmatter default-backmatter-sections)
                       (backmatter-labels '())
                       (reference-taxon-id default-reference-taxon-id))
  (validate-backmatter-sections backmatter)
  (set! backmatter-labels (normalize-backmatter-labels backmatter-labels))
  (check-reference-taxon-id reference-taxon-id)
  (check-html-input forest style routes mathjax-src)
  (let ((site (site forest)))
    (when (member "index" (map site-page-id (pages forest)))
      (error "HTML alist key conflicts with forest page" "index"))
    (cons
      (cons "index" (index-content forest routes))
      (map (lambda (page)
             (cons (site-page-id page)
                   (page-html forest page style routes mathjax-src
                              backmatter backmatter-labels
                              reference-taxon-id #t highlighters)))
           (pages forest)))))

(define* (render-html forest #:key
                             (style default-html-style)
                             (highlighters (default-highlighters))
                             routes
                             (mathjax-src default-mathjax-src)
                             (backmatter default-backmatter-sections)
                             (backmatter-labels '())
                             (reference-taxon-id default-reference-taxon-id))
  (validate-backmatter-sections backmatter)
  (set! backmatter-labels (normalize-backmatter-labels backmatter-labels))
  (check-reference-taxon-id reference-taxon-id)
  (check-html-input forest style routes mathjax-src)
  (append
    (html-style-artifacts style)
    (cons
      (make-artifact (html-index-path routes)
                     (index-content forest routes)
                     (source-paths forest))
      (map (lambda (page)
             (make-artifact
               (html-page-path routes (site-page-id page))
               (page-html forest page style routes mathjax-src backmatter
                          backmatter-labels reference-taxon-id #f highlighters)
               (page-source-paths forest page)))
           (pages forest)))))

(define* (make-html-renderer #:key
                                      (style default-html-style)
                                      (highlighters (default-highlighters))
                                      routes
                                      (mathjax-src default-mathjax-src)
                                      (backmatter default-backmatter-sections)
                                      (backmatter-labels '())
                                      (reference-taxon-id default-reference-taxon-id)
                                      (cache? #t))
  (validate-backmatter-sections backmatter)
  (set! backmatter-labels (normalize-backmatter-labels backmatter-labels))
  (check-reference-taxon-id reference-taxon-id)
  (let ((render (lambda (forest)
                  (render-html forest
                                      #:style style
                                      #:highlighters highlighters
                                      #:routes routes
                                      #:mathjax-src mathjax-src
                                      #:backmatter backmatter
                                      #:backmatter-labels backmatter-labels
                                      #:reference-taxon-id
                                      reference-taxon-id))))
    (make-cached-renderer render #:cache? cache?
          #:configuration
          (lambda (forest)
            (list 'html (site-title (site forest)) (site-description (site forest))
                  (html-home-page routes) (html-index-path routes)
                  (html-index-url routes)
                  (html-page-url routes (html-home-page routes))
                  mathjax-src backmatter backmatter-labels reference-taxon-id
                  (map (lambda (entry)
                         (list (car entry) (highlighter-configuration (cdr entry))))
                       highlighters)
                  (map (lambda (artifact)
                         (list (artifact-path artifact)
                               (content-fingerprint (artifact-content artifact))))
                       (html-style-artifacts style))))
          #:inputs (lambda ()
                     (append
                       (append-map (lambda (entry) (highlighter-inputs (cdr entry))) highlighters)
                       (append-map (lambda (artifact) (or (artifact-sources artifact) '()))
                                   (html-style-artifacts style))))
          #:render-changed
          (lambda (forest changed dirty-paths old-artifacts)
            (if (any (lambda (entry)
                       (any (lambda (path) (member path changed string=?))
                            (highlighter-inputs (cdr entry)))) highlighters)
                (render forest)
                (render-html-changed forest style routes mathjax-src backmatter
                                     backmatter-labels reference-taxon-id
                                     changed old-artifacts highlighters))))))

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

(define (link-url routes reference element)
  (let ((fragment (and element (content-element-property element 'fragment))))
    (string-append (html-page-url routes (reference-target-page-id reference))
                   (if fragment (string-append "#" fragment) ""))))

(define (reference-source-url routes reference)
  (string-append
    (html-page-url routes (reference-source-page-id reference))
    "#" (reference-source-id reference)))

(define (occurrence-anchor element prefix position)
  "The canonical anchor of a tree occurrence: the author anchor when the
element declares one (explicitly or through the unique-occurrence pass),
otherwise the generated position anchor that backs repeated occurrences."
  (or (content-element-property element 'anchor)
      (format #f "~atree-~a" prefix position)))

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
                     (href ,(link-url routes reference element)))
                     ,@children))))
      (query-link . ,(lambda (element children)
                       (let ((fragment
                               (content-element-property element 'fragment)))
                         `(a (@ (href ,(string-append
                                         (html-page-url routes
                                           (content-element-property element 'target))
                                         (if fragment
                                             (string-append "#" fragment)
                                             ""))))
                             ,@children)))))))

(define* (subtree-content-sxml forest routes page
                              #:optional (anchor-prefix "") (depth 1) (highlighters '())
                              #:key (title-decoration #f))
  (let ((position 0))
    (define (render-tree element _render)
      (set! position (1+ position))
      (let* ((id (tree-occurrence-target element))
             (target (find-page forest id))
             (anchor (occurrence-anchor element anchor-prefix position)))
        (tree-block-sxml
          (if (eq? (content-element-kind element) 'subtree)
              "inline-subtree" "transclusion")
          id (site-page-title target) (html-page-url routes id)
          (subtree-content-sxml forest routes target (string-append anchor "--") (1+ depth) highlighters
                                #:title-decoration title-decoration)
          #:anchor anchor #:depth (1+ depth)
          #:style (content-element-style element)
          #:id-prefix anchor-prefix
          #:title-decoration (if title-decoration
                                 (title-decoration forest id)
                                 '())
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
(define* (tree-block-sxml kind id title page-url content
                          #:key (open? #t) anchor (depth 2)
                          (title-decoration '()) (style #f) (id-prefix ""))
  (let* ((style-attributes (style-attribute-assoc style id-prefix))
         (_ (when (assq 'id style-attributes)
              (error "tree container style cannot set an id; the occurrence anchor owns it"
                     (cdr (assq 'id style-attributes)))))
         (attributes
           (merge-html-attributes
             `((class . ,(string-append "forest-tree-block " kind))
               (data-tree-id . ,id)
               (id . ,anchor))
             style-attributes))
         (section-attributes
           (map (lambda (attribute)
                  (list (car attribute) (cdr attribute)))
                attributes)))
    `(section (@ ,@section-attributes)
       (details ,@(if open? '((@ (open "open"))) '())
       (summary
         (,(string->symbol (format #f "h~a" (min depth 6)))
           (@ (class "forest-tree-title"))
           ,@title-decoration
           (span ,title)
           (a (@ (class "forest-tree-id")
               (href ,page-url)
               (aria-label ,(string-append
                              "Open tree " (display-tree-id id)
                              " as a page")))
              "[" ,(display-tree-id id) "]")))
       (div (@ (class "forest-tree-content"))
            ,@(if (and (pair? content) (eq? (car content) 'article))
                  (cdr content)
                  (list content)))))))

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

(define (backmatter-section-sxml class label content details?)
  (if details?
      `(aside (@ (class ,(string-append "backmatter forest-tree-block " class)))
         (details (@ (open "open"))
           (summary (h2 (@ (class "forest-tree-title")) ,label))
           (div (@ (class "forest-tree-content")) ,@content)))
      `(aside (@ (class ,(string-append "backmatter " class)))
         (h2 ,label)
         ,@content)))

(define (backlinks-sxml forest page routes label details?)
  (let ((references (incoming-references forest (site-page-id page))))
    (if (null? references)
        '()
        (list (backmatter-section-sxml
                "backlinks" label
                (list `(ul ,@(map (lambda (reference)
                                    (backlink-reference-sxml forest routes reference))
                                  references)))
                details?)))))

(define (context-sxml forest page routes label details?)
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
        (list (backmatter-section-sxml "context" label (list `(ul ,@items)) details?)))))

(define (page-list-backmatter-sxml routes class label targets details?)
    (if (null? targets)
        '()
        (list (backmatter-section-sxml
                class label
                (list `(ul
                        ,@(map (lambda (target)
                                 (list 'li
                                   (list 'a
                                     (list '@
                                       (list 'href
                                         (html-page-url
                                           routes (site-page-id target))))
                                     (site-page-title target))))
                               targets)))
                details?))))

(define (related-sxml forest page routes reference-taxon-id label details?)
  (page-list-backmatter-sxml
    routes "related" label
    (related-pages forest (site-page-id page) reference-taxon-id)
    details?))

(define (references-sxml forest page routes reference-taxon-id label details?)
  (page-list-backmatter-sxml
    routes "references" label
    (reference-pages forest (site-page-id page) reference-taxon-id)
    details?))

(define (backmatter-sxml forest page routes sections labels
                         reference-taxon-id details?)
  (append-map
    (lambda (section)
      (case section
        ((context) (context-sxml forest page routes
                                 (assq-ref labels 'context) details?))
        ((references) (references-sxml forest page routes reference-taxon-id
                                       (assq-ref labels 'references) details?))
        ((backlinks) (backlinks-sxml forest page routes
                                     (assq-ref labels 'backlinks) details?))
        ((related) (related-sxml forest page routes reference-taxon-id
                                 (assq-ref labels 'related) details?))))
    sections))

(define* (table-of-contents-sxml forest page #:key (title-decoration #f))
  (define (entries id prefix)
    (map (lambda (element position)
           (let* ((target-id (tree-occurrence-target element))
                  (target (find-page forest target-id))
                  (anchor (occurrence-anchor element prefix position))
                  (decoration (if title-decoration
                                  (title-decoration forest target-id)
                                  '()))
                  (nested (entries target-id (string-append anchor "--"))))
             `(li (a (@ (href ,(string-append "#" anchor)))
                     ,@decoration
                     ,(site-page-title target))
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

(define* (html-document title description body math? home-title style
                       routes mathjax-src inline-stylesheets?
                       #:key (html-attributes '()) (head '()) (site-nav '())
                       (home-link #f))
  `(html
     ,@(if (null? html-attributes)
           '()
           (list (cons '@ (map (lambda (attribute)
                                 (list (car attribute) (cdr attribute)))
                               html-attributes))))
     (head
       (meta (@ (charset "utf-8")))
       (title ,title)
       ,@(if description
             (list `(meta (@ (name "description") (content ,description))))
             '())
       ,@head
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
         ,@(if (or home-title home-link (pair? site-nav))
               (list `(nav (@ (aria-label "Site"))
                           ,@(cond
                               (home-link (list (home-link (html-index-url routes))))
                               (home-title (list `(a (@ (href ,(html-index-url routes)))
                                                    ,home-title)))
                               (else '()))
                           ,@site-nav))
               '()))
       ,@body
       (script ,fragment-reveal-script))))

(define (serialize-html-document sxml)
  (string-append "<!DOCTYPE html>\n" (sxml->html sxml)))

(define* (page-html forest page style routes mathjax-src backmatter labels
                    reference-taxon-id
                    #:optional (inline-stylesheets? #f) (highlighters '())
                    #:key (title-decoration #f) (article? #f) (backmatter-details? #f)
                    (html-attributes '()) (head '()) (site-nav '()) (home-link #f))
  (let* ((site (site forest))
         (body (subtree-content-sxml forest routes page "" 1 highlighters
                                     #:title-decoration title-decoration))
         (tree (tree-block-sxml
                 "page-tree" (site-page-id page) (site-page-title page)
                 (html-page-url routes (site-page-id page)) body
                 #:anchor (string-append "tree-root-" (site-page-id page))
                 #:depth 1
                 #:title-decoration (if title-decoration
                                        (title-decoration forest (site-page-id page))
                                        '())))
         (article (append (list tree)
                          (backmatter-sxml forest page routes backmatter
                                           labels reference-taxon-id
                                           backmatter-details?))))
    (serialize-html-document
      (html-document
        (site-page-title page)
        (page-description site page)
        (append (table-of-contents-sxml forest page #:title-decoration title-decoration)
                (list (if article? (cons 'article article) article)))
        (content-has-tex-math? (site-page-document page))
        (site-title site)
        style
        routes
        mathjax-src
        inline-stylesheets?
        #:html-attributes html-attributes
        #:head head
        #:site-nav site-nav
        #:home-link home-link))))

(define* (render-html-changed forest style routes mathjax-src backmatter labels
                             reference-taxon-id
                             changed old-artifacts highlighters
                             #:key (title-decoration #f) (article? #f)
                             (backmatter-details? #f) (html-attributes '())
                             (head '()) (site-nav '()) (home-link #f))
  (set! routes (resolve-html-routes forest routes))
  (check-html-input forest style routes mathjax-src)
  (let* ((route-table
           (html-route-table routes (map site-page-id (pages forest))))
         (page-artifacts
           (map (lambda (page)
                  (let* ((path (cdr (assoc (site-page-id page) route-table)))
                         (sources (page-source-paths forest page)))
                    (reuse-artifact path sources old-artifacts changed
                      (lambda ()
                        (page-html forest page style routes mathjax-src
                                   backmatter labels reference-taxon-id #f highlighters
                                   #:title-decoration title-decoration
                                   #:article? article?
                                   #:backmatter-details? backmatter-details?
                                   #:html-attributes html-attributes
                                   #:head head
                                   #:site-nav site-nav
                                   #:home-link home-link)))))
                (pages forest))))
    (append (html-style-artifacts style) page-artifacts)))

(define (resolve-html-routes forest routes)
  (if (html-home-page routes)
      routes
      (let ((home (site-home (site forest))))
        (unless home
          (error "HTML routes need a home page; set it on the site or in the routes"))
        (html-routes-with-home-page routes home))))

(define (check-html-input forest style routes mathjax-src)
  (unless (forest? forest)
    (error "Forester HTML renderer requires a forest" forest))
  (unless (html-style? style)
    (error "Forester HTML renderer requires an HTML style" style))
  (unless (html-routes? routes)
    (error "Forester HTML renderer requires HTML routes" routes))
  (unless (find-page forest (html-home-page routes))
    (error "configured HTML home page was not found" (html-home-page routes)))
  (html-route-table routes (map site-page-id (pages forest)))
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
                       (reference-taxon-id default-reference-taxon-id)
                       (title-decoration #f)
                       (article? #f)
                       (backmatter-details? #f)
                       (html-attributes '())
                       (head '())
                       (site-nav '())
                       (home-link #f))
  (validate-backmatter-sections backmatter)
  (set! backmatter-labels (normalize-backmatter-labels backmatter-labels))
  (check-reference-taxon-id reference-taxon-id)
  (set! routes (resolve-html-routes forest routes))
  (check-html-input forest style routes mathjax-src)
  (map (lambda (page)
         (cons (site-page-id page)
               (page-html forest page style routes mathjax-src
                          backmatter backmatter-labels
                          reference-taxon-id #t highlighters
                          #:title-decoration title-decoration
                          #:article? article?
                          #:backmatter-details? backmatter-details?
                          #:html-attributes html-attributes
                          #:head head
                          #:site-nav site-nav
                          #:home-link home-link)))
       (pages forest)))

(define* (render-html forest #:key
                             (style default-html-style)
                             (highlighters (default-highlighters))
                             routes
                             (mathjax-src default-mathjax-src)
                             (backmatter default-backmatter-sections)
                             (backmatter-labels '())
                             (reference-taxon-id default-reference-taxon-id)
                             (title-decoration #f)
                             (article? #f)
                             (backmatter-details? #f)
                             (html-attributes '())
                             (head '())
                             (site-nav '())
                             (home-link #f))
  (validate-backmatter-sections backmatter)
  (set! backmatter-labels (normalize-backmatter-labels backmatter-labels))
  (check-reference-taxon-id reference-taxon-id)
  (set! routes (resolve-html-routes forest routes))
  (check-html-input forest style routes mathjax-src)
  (append
    (html-style-artifacts style)
    (let ((route-table (html-route-table routes
                                          (map site-page-id (pages forest)))))
      (map (lambda (page)
             (make-artifact
               (cdr (assoc (site-page-id page) route-table))
               (page-html forest page style routes mathjax-src backmatter
                          backmatter-labels reference-taxon-id #f highlighters
                          #:title-decoration title-decoration
                          #:article? article?
                          #:backmatter-details? backmatter-details?
                          #:html-attributes html-attributes
                          #:head head
                          #:site-nav site-nav
                          #:home-link home-link)
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
                                      (title-decoration #f)
                                      (article? #f)
                                      (backmatter-details? #f)
                                      (html-attributes '())
                                      (head '())
                                      (site-nav '())
                                      (home-link #f)
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
                                      reference-taxon-id
                                      #:title-decoration title-decoration
                                      #:article? article?
                                      #:backmatter-details? backmatter-details?
                                      #:html-attributes html-attributes
                                      #:head head
                                      #:site-nav site-nav
                                      #:home-link home-link))))
    (make-cached-renderer render #:cache? cache?
          #:configuration
          (lambda (forest)
            (list 'html (site-title (site forest)) (site-description (site forest))
                  (or (html-home-page routes) (site-home (site forest)))
                  (html-index-url routes)
                  mathjax-src backmatter backmatter-labels reference-taxon-id
                  article? backmatter-details? html-attributes head site-nav
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
                                     changed old-artifacts highlighters
                                     #:title-decoration title-decoration
                                     #:article? article?
                                     #:backmatter-details? backmatter-details?
                                     #:html-attributes html-attributes
                                     #:head head
                                     #:site-nav site-nav
                                     #:home-link home-link))))))

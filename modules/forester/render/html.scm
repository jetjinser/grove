;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render html)
  #:use-module (grove build)
  #:use-module ((forester composition) #:prefix composition:)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render html)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (forester backmatter)
  #:use-module (grove style html)
  #:use-module ((forester style html) #:prefix forester:)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (site->html
            render-html
            make-html-renderer))

(define (link-url routes reference)
  (string-append
    (html-page-url routes
                   (reference-target-page-id reference))
    (if (reference-target-fragment reference)
        (string-append "#"
                       (reference-target-fragment reference))
        "")))

(define (reference-source-url routes reference)
  (string-append
    (html-page-url routes (reference-source-page-id reference))
    "#" (reference-source-id reference)))

(define* (page-content-handlers forest routes page #:optional (anchor-prefix ""))
  (let ((heading-ids (heading-ids forest (site-page-id page)))
        (references (outgoing-references
                      forest (site-page-id page))))
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
                       (let ((target
                               (content-element-property element 'target)))
                         `(a (@ (href
                                  ,(string-append
                                     (html-page-url routes
                                       (page-id-from-link target))
                                     (if (fragment-from-link target)
                                         (string-append "#"
                                           (fragment-from-link target))
                                         ""))))
                            ,@children))))
      (heading . ,(lambda (_ children)
                    (when (null? heading-ids)
                      (error "Forester heading rendering exceeded its index"
                             (site-page-id page)))
                    (let ((id (car heading-ids)))
                      (set! heading-ids (cdr heading-ids))
                      `(h2 (@ (id ,(string-append anchor-prefix id)))
                           ,@children))))
      (query-heading . ,(lambda (_ children) `(h2 ,@children))))))

(define* (subtree-content-sxml forest routes page #:optional (anchor-prefix ""))
  (let ((transclusions
          (outgoing-transclusions forest (site-page-id page))))
   (content->sxml
    (site-page-document page)
    #:handlers (page-content-handlers
                 forest routes page
                 anchor-prefix)
    #:lazy-handlers
    (list (cons 'subtree (lambda (element _render)
                    (let* ((id (content-element-property element 'tree-id))
                           (target (find-page forest id)))
                      (tree-block-sxml
                        "inline-subtree" id (site-page-title target)
                        (html-page-url routes id)
                        (subtree-content-sxml
                          forest routes target
                          (string-append anchor-prefix "tree-" id "--"))
                        (if (assq 'open? (content-element-properties element))
                            (content-element-property element 'open?)
                            #t)))))
          (cons 'transclude (lambda (element _render)
                       (let* ((id (content-element-property element 'target))
                              (transclusion (car transclusions))
                              (target (find-page forest id)))
                         (set! transclusions (cdr transclusions))
                         (tree-block-sxml
                           "transclusion" id (site-page-title target)
                           (html-page-url routes id)
                           (subtree-content-sxml
                             forest routes target
                             (string-append anchor-prefix "transcluded-"
                                            (composition:transclusion-source-id
                                              transclusion)
                                            "--"))))))))))

(define (display-tree-id id)
  (if (string-prefix? "unstable-" id)
      (string-append "#" (substring id (string-length "unstable-")))
      id))

(define* (tree-block-sxml kind id title page-url content #:optional (open? #t))
  `(section (@ (class ,(string-append "forest-tree-block " kind))
               (data-tree-id ,id))
     (details ,@(if open? '((@ (open "open"))) '())
       (summary
         (span (@ (class "forest-tree-title")) ,title)
         (a (@ (class "forest-tree-id")
               (href ,page-url)
               (aria-label ,(string-append
                              "Open tree " (display-tree-id id)
                              " as a page")))
            "[" ,(display-tree-id id) "]"))
       (div (@ (class "forest-tree-content"))
            ,@(if (and (pair? content) (eq? (car content) 'article))
                  (cdr content)
                  (list content))))))

(define (backlink-groups forest page)
  (let* ((page-id (site-page-id page))
         (incoming (incoming-references forest page-id))
         (page-references
           (filter (lambda (reference)
                     (not (reference-target-fragment reference)))
                   incoming))
         (heading-groups
           (filter-map
             (lambda (heading)
               (let* ((fragment (heading-id heading))
                      (references
                        (incoming-references forest page-id fragment)))
                 (and (pair? references) (cons fragment references))))
             (headings forest page-id))))
    (append (if (null? page-references) '() (list (cons #f page-references)))
            heading-groups)))

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

(define (backlink-group-sxml forest page routes group)
  (let* ((fragment (car group))
         (target-heading
           (and fragment
                (find (lambda (heading)
                        (string=? fragment (heading-id heading)))
                      (headings forest (site-page-id page)))))
         (references (cdr group))
         (label (if target-heading
                    `(a (@ (href ,(string-append "#" fragment)))
                        ,(heading-label target-heading))
                    "This page")))
    `(li ,label
         (ul ,@(map (lambda (reference)
                      (backlink-reference-sxml forest routes reference))
                    references)))))

(define (backlinks-sxml forest page routes label)
  (let ((groups (backlink-groups forest page)))
    (if (null? groups)
        '()
        (list `(aside (@ (class "backmatter backlinks"))
                      (h2 ,label)
                      (ul ,@(map (lambda (group)
                                   (backlink-group-sxml forest page routes group))
                                 groups)))))))

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
  (let ((headings (headings forest (site-page-id page))))
    (if (null? headings)
        '()
        (list
          `(nav (@ (class "table-of-contents") (aria-label "Contents"))
             (h2 "Contents")
             (ol
               ,@(map (lambda (heading)
                        `(li
                           (a (@ (href ,(string-append
                                          "#" (heading-id heading))))
                              ,(heading-label heading))))
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
                    #:optional (inline-stylesheets? #f))
  (let* ((site (site forest))
         (body (subtree-content-sxml forest routes page))
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

(define (render-html-changed forest style routes mathjax-src backmatter labels
                             reference-taxon-id
                                    changed old-artifacts)
  (check-html-input forest style routes mathjax-src)
  (let* ((site (site forest))
         (index-sources (source-paths forest))
         (index-path (html-index-path routes))
         (old-index (find-artifact old-artifacts index-path))
         (index-current
           (make-artifact index-path "" index-sources))
         (index-artifact
           (make-artifact index-path
                          (if (artifact-current? index-current old-index changed)
                              (artifact-content old-index)
                              (index-content forest routes))
                          index-sources))
         (page-artifacts
           (map (lambda (page)
                  (let* ((path (html-page-path routes (site-page-id page)))
                         (sources (page-source-paths forest page))
                         (old (find-artifact old-artifacts path))
                         (current (make-artifact path "" sources)))
                    (if (artifact-current? current old changed)
                        old
                        (make-artifact
                          path
                          (page-html forest page style routes mathjax-src
                                     backmatter labels reference-taxon-id)
                          sources))))
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
              (and (string? mathjax-src)
                   (not (string-null? mathjax-src))
                   (or (external-url? mathjax-src)
                       (artifact-path? mathjax-src))))
    (error "MathJax source must be a URL, relative artifact path, or #f"
           mathjax-src)))

(define* (site->html forest #:key
                       (style (make-html-style))
                       routes
                       (mathjax-src default-mathjax-src)
                       (backmatter default-backmatter-sections)
                       (backmatter-labels '())
                       (reference-taxon-id default-reference-taxon-id))
  (set! style (forester:make-html-style #:style style))
  (check-backmatter-sections backmatter)
  (set! backmatter-labels (check-backmatter-labels backmatter-labels))
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
                              reference-taxon-id #t)))
           (pages forest)))))

(define* (render-html forest #:key
                             (style (make-html-style))
                             routes
                             (mathjax-src default-mathjax-src)
                             (backmatter default-backmatter-sections)
                             (backmatter-labels '())
                             (reference-taxon-id default-reference-taxon-id))
  (set! style (forester:make-html-style #:style style))
  (check-backmatter-sections backmatter)
  (set! backmatter-labels (check-backmatter-labels backmatter-labels))
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
                          backmatter-labels reference-taxon-id)
               (page-source-paths forest page)))
           (pages forest)))))

(define* (make-html-renderer #:key
                                      (style (make-html-style))
                                      routes
                                      (mathjax-src default-mathjax-src)
                                      (backmatter default-backmatter-sections)
                                      (backmatter-labels '())
                                      (reference-taxon-id default-reference-taxon-id)
                                      (cache-key #f))
  (check-backmatter-sections backmatter)
  (set! backmatter-labels (check-backmatter-labels backmatter-labels))
  (check-reference-taxon-id reference-taxon-id)
  (let* ((style (forester:make-html-style #:style style))
         (render (lambda (forest)
                  (render-html forest
                                      #:style style
                                      #:routes routes
                                      #:mathjax-src mathjax-src
                                      #:backmatter backmatter
                                      #:backmatter-labels backmatter-labels
                                      #:reference-taxon-id
                                      reference-taxon-id))))
    (if cache-key
        (make-cached-renderer
          render
          #:cache-key (string-append "forester-html-v16:" cache-key ":home="
                                     (html-home-page routes) ":backmatter="
                                     (format #f "~s" backmatter)
                                     ":labels="
                                     (format #f "~s" backmatter-labels)
                                     ":reference-taxon="
                                     reference-taxon-id)
          #:render-changed
          (lambda (forest changed dirty-paths old-artifacts)
            (render-html-changed forest style routes mathjax-src backmatter
                                        backmatter-labels
                                        reference-taxon-id
                                        changed old-artifacts)))
        render)))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render html)
  #:use-module (grove build)
  #:use-module (grove build state)
  #:use-module (grove i18n)
  #:use-module (grove string-fun)
  #:use-module (forester render artifact)
  #:use-module (forester render labels)
  #:use-module (forester render context)
  #:use-module (forester render site)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render html)
  #:use-module (grove render html rules)
  #:use-module (grove render context)
  #:use-module (grove render html presentation)
  #:use-module (grove highlight)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module ((forester private content-index) #:prefix content-index:)
  #:use-module (forester backmatter)
  #:use-module (forester relations)
  #:use-module (grove style html)
  #:use-module (forester style html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (site->html
            site-nav-link
            site-nav
            split-into-chunks
            render-html
            default-metadata-items
            make-html-options
            html-options?
            make-html-renderer))

(define* (chrome-date text #:optional context)
  (let* ((context (or context (current-render-context)))
         (locale (and context
                      (render-context-ref context 'locale-object))))
    (if locale (format-date locale text #:style 'long) text)))

(define (page-locale page)
  (assq-ref (site-page-metadata page) 'locale))

(define* (translation-group forest page #:optional context)
  (let ((key (assq-ref (site-page-metadata page) 'translation)))
    (and key
         (reverse (hash-ref (or (and context
                                (render-context-ref context 'translation-groups))
                                (translation-groups (pages forest)))
                            key
                            '())))))

(define (hreflang-links forest page routes context)
  (let ((group (translation-group forest page context)))
    (if (or (not group) (< (length group) 2))
        '()
        (append
          (map (lambda (other)
                 `(link (@ (rel "alternate")
                           (hreflang ,(or (page-locale other) "und"))
                           (href ,(html-page-url routes (site-page-id other))))))
               group)
          (let* ((default-locale
                   (render-context-ref context 'default-locale))
                 (default
                   (and default-locale
                        (find (lambda (other)
                                (equal? (page-locale other) default-locale))
                              group))))
            (if default
                (list `(link (@ (rel "alternate")
                                (hreflang "x-default")
                                (href ,(html-page-url
                                         routes (site-page-id default))))))
                '()))))))

(define (alternate-og-locales forest page context)
  (let ((group (translation-group forest page context)))
    (if (not group)
        '()
        (let loop ((rest group) (acc '()))
          (if (null? rest)
              (reverse acc)
              (let ((other (car rest)))
                (if (and (not (string=? (site-page-id other)
                                        (site-page-id page)))
                         (page-locale other))
                    (loop (cdr rest) (cons (page-locale other) acc))
                    (loop (cdr rest) acc))))))))

(define (language-switcher-sxml forest page routes context)
  (let ((group (translation-group forest page context)))
    (if (or (not group) (< (length group) 2))
        '()
        `(nav (@ (class "language-switcher")
                 (aria-label ,(chrome-message 'language context)))
              ,@(map (lambda (other)
                       (let ((tag (or (page-locale other) "und")))
                         `(a (@ (href ,(html-page-url
                                         routes (site-page-id other)))
                                (hreflang ,tag)
                                (lang ,tag)
                                ,@(if (string=? (site-page-id other)
                                                (site-page-id page))
                                      '((aria-current "true"))
                                      '()))
                             ,tag)))
                     group)))))

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

(define (term-mention-sxml forest routes element children)
  "Render an element marked with a term property: an anchor to the
canonical defining occurrence - the owning page's own rendering keeps
unprefixed anchor ids - with the definition's display text when the
author supplies no body.  A term nothing defines is not a dead link:
the body (or the term id) stays inline under missing-term, and the
backlog report collects the mention."
  (let* ((term-id (content-element-property element 'term))
         (rows (term-definition-rows
                 (term-definition-relation forest) term-id)))
    (if (null? rows)
        (if (null? children)
            `(span (@ (class "missing-term")) ,term-id)
            `(span (@ (class "missing-term")) ,@children))
        (let* ((canonical (car rows))
               (page (car canonical))
               (anchor (cadr canonical))
               (display (caddr canonical))
               (label (if (null? children) (list display) children)))
          `(a (@ (href ,(string-append (html-page-url routes page)
                                       "#" anchor)))
              ,@label)))))

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
                 (if (content-element-property element 'term)
                     (term-mention-sxml forest routes element children)
                     (begin
                       (when (null? references)
                         (error "Forester link rendering exceeded its index"
                                (site-page-id page)))
                       (let ((reference (car references)))
                         (set! references (cdr references))
                         `(a (@ (id ,(string-append anchor-prefix
                                                   (reference-source-id reference)))
                                (href ,(link-url routes reference element)))
                             ,@children))))))
      (query-link . ,(lambda (element children)
                       (if (content-element-property element 'term)
                           (term-mention-sxml forest routes element children)
                           (let ((fragment
                                   (content-element-property element 'fragment)))
                             `(a (@ (href ,(string-append
                                             (html-page-url routes
                                               (content-element-property element 'target))
                                             (if fragment
                                                 (string-append "#" fragment)
                                                 ""))))
                                 ,@children))))))))

(define* (subtree-content-sxml forest routes page
                              #:optional (anchor-prefix "") (depth 1) (highlighters '())
                              #:key (title-decoration #f)
                              (metadata-items default-metadata-items)
                              (extra-handlers '())
                              (rules empty-html-rules)
                    (context #f))
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
                                #:title-decoration title-decoration
                                #:metadata-items metadata-items
                                #:extra-handlers extra-handlers
                                #:rules rules
                                #:context (if context
                                             (render-context-with context
                                               #:forest forest
                                               #:routes routes
                                               #:page target
                                               #:anchor-prefix (string-append anchor "--"))
                                             context))
          #:anchor anchor #:depth (1+ depth)
          #:context context
          #:style (content-element-style element)
          #:id-prefix anchor-prefix
          #:metadata-items (metadata-items-for metadata-items target context)
          #:title-decoration (if title-decoration
                                 (call-hook title-decoration
                                            (list forest target) context)
                                 '())
          #:open? (if (assq 'open? (content-element-properties element))
                      (content-element-property element 'open?) #t))))
    (content->sxml
      (site-page-document page)
      #:id-prefix anchor-prefix
      #:highlighters highlighters
      #:rules rules
      #:context (if context
                    context
                    (make-render-context
                      #:forest forest
                      #:routes routes
                      #:page page
                      #:anchor-prefix anchor-prefix))
      #:handlers (append
                   extra-handlers
                   (page-content-handlers forest routes page anchor-prefix))
      #:lazy-handlers (map (lambda (kind) (cons kind render-tree))
                          '(subtree transclude query-transclude)))))

(define (display-tree-id id)
  (if (string-prefix? "unstable-" id)
      (string-append "#" (substring id (string-length "unstable-")))
      id))
(define* (default-metadata-items page #:optional context)
  "The stock metadata items: the page's publication dates. Render
hooks replace this wholesale; compose with it to extend."
  (let ((published (assq-ref (site-page-metadata page) 'published))
        (updated (assq-ref (site-page-metadata page) 'updated)))
    `(,@(if published
            `((li (@ (class "meta-item"))
                  (time (@ (datetime ,published)) ,(chrome-date published context))))
            '())
      ,@(if updated
            `((li (@ (class "meta-item"))
                  (time (@ (datetime ,updated))
                        ,(chrome-message-format
                           'metadata-updated (list (chrome-date updated context))
                           context))))
            '()))))

(define (call-metadata-items metadata-items page context)
  "Call a metadata hook with CONTEXT when it accepts two arguments.

One-argument hooks run with CONTEXT as a scoped fallback so chrome helpers
inside them still localize."
  (let ((arity (procedure-minimum-arity metadata-items)))
    (if (and arity
             (let ((required (car arity))
                   (optional (cadr arity))
                   (rest? (caddr arity)))
               (or rest? (>= (+ required optional) 2))))
        (metadata-items page context)
        (if context
            (with-render-context context
              (lambda () (metadata-items page)))
            (metadata-items page)))))

(define (metadata-items-for metadata-items page context)
  (if (eq? metadata-items default-metadata-items)
      (metadata-items page context)
      (call-metadata-items metadata-items page context)))

(define (call-hook hook arguments context)
  "Call HOOK with ARGUMENTS plus CONTEXT when it accepts one more argument."
  (let ((arity (procedure-minimum-arity hook)))
    (if (and context arity
             (let ((required (car arity))
                   (optional (cadr arity))
                   (rest? (caddr arity)))
               (or rest? (>= (+ required optional)
                             (1+ (length arguments))))))
        (apply hook (append arguments (list context)))
        (apply hook arguments))))

(define (metadata-sxml items)
  (if (null? items)
      '()
      (list `(div (@ (class "metadata")) (ul ,@items)))))

(define* (tree-block-sxml kind id title page-url content
                          #:key (open? #t) anchor (depth 2)
                          (title-decoration '()) (style #f) (id-prefix "")
                          (metadata-items '()) (context #f))
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
           (span ,title))
         ,@(metadata-sxml metadata-items))
       (a (@ (class "forest-tree-id")
           (href ,page-url)
           (aria-label ,(chrome-message-format
                          'tree-open-page (list (display-tree-id id)) context)))
          "[" ,(display-tree-id id) "]")
       (div (@ (class "forest-tree-content"))
            ,@(if (and (pair? content) (eq? (car content) 'article))
                  (cdr content)
                  (list content)))))))

(define (taxon-entry-sxml page)
  "Leading taxon span for a backmatter entry, from the page's own
top-level taxon element; pages without one contribute nothing."
  (let ((taxon (page-taxon page)))
    (if taxon
        (let ((text (content-index:content->plain-text taxon)))
          (if (string-null? text)
              '()
              (let ((id (content-element-property taxon 'id)))
                `((span (@ (class "taxon")
                            ,@(if id `((data-taxon ,id)) '()))
                        ,text " ")))))
        '())))
(define* (backmatter-entry-sxml forest routes class page metadata-items
                                #:key
                                (url (html-page-url routes
                                                 (site-page-id page)))
                                (context #f))
  ;;     Forester-style: a backmatter entry is a tree in its own
  ;;     right.  The summary identifies the tree - taxon, title, [id],
  ;;     metadata - and the disclosure folds open like any tree.  What
  ;;     folds open is the tree's outline, linking into the real
  ;;     page's sections: embedding full content statically made
  ;;     citation-heavy pages explode (a 292-entry page rendered to
  ;;     14 MiB).  The fetch script swaps the outline for the real
  ;;     content on first expand.  The alt text of a reference stays
  ;;     in the citing body, where it was authored.
  (let ((outline (tree-outline-sxml
                   forest (site-page-id page)
                   #:base-url (html-page-url routes (site-page-id page))
                   #:context context)))
    (tree-block-sxml
      "backmatter"
      (site-page-id page) (site-page-title page) url
      (if (null? outline)
          '()
          (list `(ol (@ (class "tree-outline")) ,@outline)))
      #:anchor (string-append "backmatter-" class "-" (site-page-id page))
      #:depth 3
      #:metadata-items (metadata-items-for metadata-items page context)
      #:title-decoration (taxon-entry-sxml page)
      #:open? #f)))

(define (backlink-reference-sxml forest routes class reference
                                 metadata-items context)
  (let ((source (lookup-page forest (reference-source-page-id reference))))
    (backmatter-entry-sxml forest routes class source metadata-items
                           #:url (reference-source-url routes reference)
                           #:context context)))

(define (backmatter-section-sxml class label content details?)
  (if details?
      `(aside (@ (class ,(string-append "backmatter forest-tree-block " class)))
         (details (@ (open "open"))
           (summary (h2 (@ (class "forest-tree-title")) ,label))
           (div (@ (class "forest-tree-content")) ,@content)))
      `(aside (@ (class ,(string-append "backmatter " class)))
         (h2 ,label)
         ,@content)))

(define (backlinks-sxml forest page routes label details? metadata-items context)
  (let ((references (incoming-references forest (site-page-id page))))
    (if (null? references)
        '()
        (list (backmatter-section-sxml
                "backlinks" label
                (map (lambda (reference)
                       (backlink-reference-sxml forest routes "backlinks"
                                                reference metadata-items context))
                     references)
                details?)))))

(define (context-sxml forest page routes label details? metadata-items context)
  (let ((sources (context-pages forest (site-page-id page))))
    (if (null? sources)
        '()
        (list (backmatter-section-sxml
                "context" label
                (map (lambda (source)
                       (backmatter-entry-sxml forest routes "context" source
                                              metadata-items #:context context))
                     sources)
                details?)))))

(define (page-list-backmatter-sxml forest routes class label targets
                                   details? metadata-items context)
    (if (null? targets)
        '()
        (list (backmatter-section-sxml
                class label
                (map (lambda (target)
                       (backmatter-entry-sxml forest routes class target
                                              metadata-items #:context context))
                     targets)
                details?))))

(define (related-sxml forest page routes reference-taxon-id label details?
                      metadata-items context)
  (page-list-backmatter-sxml
    forest routes "related" label
    (related-pages forest (site-page-id page) reference-taxon-id)
    details? metadata-items context))

(define (references-sxml forest page routes reference-taxon-id label details?
                         metadata-items context)
  (page-list-backmatter-sxml
    forest routes "references" label
    (reference-pages forest (site-page-id page) reference-taxon-id)
    details? metadata-items context))

(define* (backmatter-sxml forest page routes sections labels
                          reference-taxon-id details?
                          #:key (metadata-items default-metadata-items)
                          (context #f))
  (append-map
    (lambda (section)
      (case section
        ((context) (context-sxml forest page routes
                                 (assq-ref labels 'context) details?
                                 metadata-items context))
        ((references) (references-sxml forest page routes reference-taxon-id
                                       (assq-ref labels 'references) details?
                                       metadata-items context))
        ((backlinks) (backlinks-sxml forest page routes
                                     (assq-ref labels 'backlinks) details?
                                     metadata-items context))
        ((related) (related-sxml forest page routes reference-taxon-id
                                 (assq-ref labels 'related) details?
                                 metadata-items context))))
    sections))

(define* (tree-outline-sxml forest id
                            #:key (base-url "") (title-decoration #f)
                            (context #f))
  ;;     The tree's own subtree/transclusion outline as nested list
  ;;     items.  With the default BASE-URL the links are same-page
  ;;     fragments (the page's table of contents); pass a page URL to
  ;;     point them at that page's sections from anywhere, e.g. from a
  ;;     backmatter entry on another page.  The visited set keeps
  ;;     transclusion cycles from recursing forever.
  (letrec ((entries
             (lambda (id prefix visited)
               (map (lambda (element position)
                      (let* ((target-id (tree-occurrence-target element))
                             (target (find-page forest target-id))
                             (anchor (occurrence-anchor element prefix
                                                        position))
                             (decoration
                               (if title-decoration
                                   (call-hook title-decoration
                                            (list forest target) context)
                                   '()))
                             (nested
                               (if (member target-id visited)
                                   '()
                                   (entries target-id
                                            (string-append anchor "--")
                                            (cons target-id visited)))))
                        `(li (a (@ (href ,(string-append base-url "#"
                                                         anchor)))
                                ,@decoration
                                ,(site-page-title target))
                             ,@(if (null? nested) '()
                                   (list `(ol ,@nested))))))
                    (tree-occurrences forest id)
                    (iota (length (tree-occurrences forest id)) 1)))))
    (entries id "" (list id))))

(define* (table-of-contents-sxml forest page #:key (title-decoration #f) (context #f))
  (let ((items (tree-outline-sxml forest (site-page-id page)
                                  #:title-decoration title-decoration
                                  #:context context)))
    (if (null? items) '()
        (list `(nav (@ (class "table-of-contents")
                       (aria-label ,(chrome-message 'nav-contents context)))
                    (h2 ,(chrome-message 'nav-contents context))
                    (ol ,@items))))))

(define* (site-nav-link routes id label #:key (current-page #f))
  "A site navigation link to page ID, marked as current when it is
CURRENT-PAGE.  #:site-nav may be a procedure of the current page ID so
that navigation links can use this helper."
  `(a (@ (href ,(html-page-url routes id))
         ,@(if (and current-page (string=? id current-page))
               '((aria-current "page"))
               '()))
      ,label))

(define* (site-nav routes entries #:key (current-page #f))
  "Build site navigation links from ENTRIES, an alist of page id to label.

The result is stable SXML, so it participates in the renderer cache key
better than a procedure recreated on every project load."
  (map (lambda (entry)
         (site-nav-link routes (car entry) (cdr entry)
                        #:current-page current-page))
       entries))

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

(define backmatter-fetch-script
  ;;     On first expand, replace a backmatter entry's static outline
  ;;     with the cited page's real content: the entry fetches the
  ;;     page (which exists anyway), takes its tree root's content,
  ;;     and swaps it in.  Zero duplication in the static output; a
  ;;     failed fetch quietly keeps the outline.
  "(function () {\n  function loadFull(entry) {\n    var details = entry.querySelector(':scope > details');\n    if (!details || details.dataset.full) return;\n    var id = entry.getAttribute('data-tree-id');\n    var slug = entry.querySelector('a.forest-tree-id');\n    if (!id || !slug) return;\n    var url = slug.getAttribute('href').split('#')[0];\n    if (!url) return;\n    details.dataset.full = 'pending';\n    fetch(url).then(function (response) { return response.text(); })\n      .then(function (html) {\n        var doc = new DOMParser().parseFromString(html, 'text/html');\n        var root = doc.getElementById('tree-root-' + id);\n        var content = root && root.querySelector('.forest-tree-content');\n        if (!content) throw new Error('tree content missing');\n        var panel = details.querySelector('.forest-tree-content');\n        panel.innerHTML = content.innerHTML;\n        panel.classList.add('backmatter-full');\n        details.dataset.full = 'done';\n      })\n      .catch(function () { delete details.dataset.full; });\n  }\n  document.querySelectorAll('section.forest-tree-block.backmatter')\n    .forEach(function (entry) {\n      var details = entry.querySelector(':scope > details');\n      if (details) details.addEventListener('toggle', function () {\n        if (details.open) loadFull(entry);\n      });\n    });\n}());")

(define (head-attribute node attribute)
  (let ((attrs (and (pair? node) (pair? (cdr node))
                    (eq? (caadr node) '@)
                    (cdadr node))))
    (and attrs
         (let ((entry (assq attribute attrs)))
           (and entry (cadr entry))))))

(define (head-key node)
  (let ((tag (and (pair? node) (car node))))
    (cond
      ((eq? tag 'meta)
       (cond ((head-attribute node 'name)
              => (lambda (value) (cons 'meta-name value)))
             ((head-attribute node 'property)
              => (lambda (value) (cons 'meta-property value)))
             (else #f)))
      ((eq? tag 'link)
       (let ((rel (head-attribute node 'rel))
             (hreflang (head-attribute node 'hreflang)))
         (cond ((and rel hreflang (string=? rel "alternate"))
                (cons 'link-alternate hreflang))
               (rel (cons 'link-rel rel))
               (else #f))))
      ((eq? tag 'title) 'title)
      (else #f))))

(define (head-keys nodes)
  (let loop ((rest nodes) (keys '()))
    (if (null? rest)
        (reverse keys)
        (let ((key (head-key (car rest))))
          (loop (cdr rest) (if key (cons key keys) keys))))))

(define (merge-head defaults overrides)
  "Append OVERRIDES to DEFAULTS, dropping defaults they replace by
name, property, rel, or title."
  (let ((keys (head-keys overrides)))
    (append (filter (lambda (node)
                      (let ((key (head-key node)))
                        (or (not key) (not (member key keys)))))
                    defaults)
            overrides)))

(define* (html-document title description body math? home-title style
                       routes mathjax-src inline-stylesheets?
                       #:key (html-attributes '()) (head '()) (site-nav '())
                       (home-link #f) (scripts '()) (current-page #f)
                       (language-nav '()) (article? #f) (published #f)
                       (og-locales '()) (updated #f) (og-image #f)
                       (og-image-alt #f) (context #f))
  (unless (render-context? context)
    (error "html-document requires a render context" context))
  (let* ((site-nav (if (procedure? site-nav)
                       (call-hook site-nav (list current-page) context)
                       site-nav))
         (locale (render-context-ref context 'locale-object))
         (default-locale
           (render-context-ref context 'default-locale))
         (html-attributes
           (cond
             (locale
              (merge-html-attributes (locale-html-attributes locale)
                                     html-attributes))
             (default-locale
              (merge-html-attributes
                (locale-html-attributes (make-locale default-locale))
                html-attributes))
             (else
              (merge-html-attributes '((lang . "en")) html-attributes))))
         (site-url (render-context-ref context 'site-url))
         (page-url (and site-url current-page
                        (html-page-url routes current-page)))
         (absolute-url (and page-url
                            (string-append (string-trim-right site-url #\/)
                                           page-url)))
         (og-image-url
           (and og-image
                (cond ((or (string-prefix? "http://" og-image)
                           (string-prefix? "https://" og-image))
                       og-image)
                      ((and site-url (string-prefix? "/" og-image))
                       (string-append (string-trim-right site-url #\/)
                                      og-image))
                      (site-url
                       (string-append (string-trim-right site-url #\/)
                                      (html-asset-url routes og-image)))
                      (else #f)))))
    `(html
     ,@(if (null? html-attributes)
           '()
           (list (cons '@ (map (lambda (attribute)
                                 (list (car attribute) (cdr attribute)))
                               html-attributes))))
     (head
       ,@(merge-head
           (append
             (list `(meta (@ (charset "utf-8")))
                   `(meta (@ (name "viewport")
                             (content "width=device-width, initial-scale=1")))
                   `(title ,title)
                   `(meta (@ (property "og:title") (content ,title)))
                   `(meta (@ (property "og:type")
                             (content ,(if article? "article" "website")))))
             (if (and home-title (not (string-null? home-title)))
                 (list `(meta (@ (property "og:site_name")
                                 (content ,home-title))))
                 '())
             (if description
                 (list `(meta (@ (name "description") (content ,description)))
                       `(meta (@ (property "og:description")
                                 (content ,description))))
                 '())
             (if locale
                 (list `(meta (@ (property "og:locale")
                                  (content ,(og-locale-tag
                                               (render-context-ref context 'locale))))))
                 '())
             (map (lambda (tag)
                    `(meta (@ (property "og:locale:alternate")
                              (content ,(og-locale-tag tag)))))
                  og-locales)
             (if og-image-url
                 (append
                   (list `(meta (@ (property "og:image")
                                   (content ,og-image-url))))
                   (if og-image-alt
                       (list `(meta (@ (property "og:image:alt")
                                       (content ,og-image-alt))))
                       '()))
                 '())
             (if absolute-url
                 (list `(link (@ (rel "canonical") (href ,absolute-url)))
                       `(meta (@ (property "og:url") (content ,absolute-url))))
                 '())
             (if (and article? published)
                 (list `(meta (@ (property "article:published_time")
                                 (content ,published))))
                 '())
             (if (and article? updated)
                 (list `(meta (@ (property "article:modified_time")
                                 (content ,updated))))
                 '()))
           head)
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
       (a (@ (class "skip-link") (href "#main"))
          ,(chrome-message 'skip-to-content context))
       (header
         ,@(if (or home-title home-link (pair? site-nav) (pair? language-nav))
               (list `(nav (@ (aria-label ,(chrome-message 'nav-site context)))
                           ,@(cond
                               (home-link (list (call-hook home-link
                                                           (list (html-index-url routes))
                                                           context)))
                               (home-title (list `(a (@ (href ,(html-index-url routes)))
                                                    ,home-title)))
                               (else '()))
                           ,@site-nav)
                     language-nav)
               '()))
       (main (@ (id "main") (tabindex "-1"))
         ,@body)
       ,@(map (lambda (script) `(script ,script)) scripts)
       (script ,fragment-reveal-script)))))
(define (make-side-lane-collector)
  ;;     Accumulates lifted elements in document order; the in-place
  ;;     marker and the lane item share the sequence number.
  (let ((count 0) (items '()))
    (case-lambda
      ((message element children)
       (case message
         ((put!)
          (set! count (+ count 1))
          (set! items (cons (list count children) items))
          count)))
      ((message)
       (case message
         ((items) (reverse items)))))))

(define (side-lane-handlers kinds collector)
  "Handlers for the SIDE-LANE kinds: the element's rendered body moves
to the lane, and an in-place numbered marker links to it."
  (map (lambda (kind)
         (cons kind
               (lambda (element children)
                 (let ((n (collector 'put! element children)))
                   `(sup (@ (class "margin-ref")
                            (id ,(string-append "margin-ref-"
                                                (number->string n))))
                      (a (@ (href ,(string-append "#margin-"
                                                  (number->string n))))
                         ,(number->string n)))))))
       kinds))

(define (side-lane-sxml collector)
  (let ((items (collector 'items)))
    (if (null? items)
        '()
        (list `(div (@ (class "margin-lane"))
                  ,@(map (lambda (entry)
                           (let ((n (car entry))
                                 (children (cadr entry)))
                             `(div (@ (class "margin-item")
                                      (id ,(string-append "margin-"
                                                          (number->string n))))
                                (a (@ (class "margin-backref")
                                      (href ,(string-append "#margin-ref-"
                                                            (number->string n))))
                                   "↩")
                                ,@children)))
                         items))))))

(define (serialize-html-document sxml)
  (string-append "<!DOCTYPE html>\n" (sxml->html sxml)))

(define* (page-html forest page style routes mathjax-src backmatter labels
                    reference-taxon-id
                    #:optional (inline-stylesheets? #f) (highlighters '())
                    #:key (title-decoration #f) (article? #f) (backmatter-details? #f)
                    (html-attributes '()) (head '()) (site-nav '()) (home-link #f)
                    (metadata-items default-metadata-items) (side-lane '())
                    (rules empty-html-rules)
                    (context #f))
  (unless (render-context? context)
    (error "page-html requires a render context" context))
  (let* ((page-locale (page-locale page))
         (site (site forest))
         (context
           (render-context-with context
             #:forest forest
             #:routes routes
             #:page page
             #:anchor-prefix ""
             #:locale (or page-locale (render-context-ref context 'locale))
             #:locale-object (if page-locale
                                 (make-locale page-locale)
                                 (render-context-ref context 'locale-object))
             #:messages (render-context-ref context 'messages)))
         (labels
           (normalize-backmatter-labels
             labels
             #:defaults (localized-backmatter-labels context)))
         (head (if (render-context-ref context 'hreflang?)
                   (append head (hreflang-links forest page routes context))
                   head))
         (language-nav (if (render-context-ref context 'language-switcher?)
                           (language-switcher-sxml forest page routes context)
                           '()))
         (og-locales (if (render-context-ref context 'hreflang?)
                         (alternate-og-locales forest page context)
                         '()))
         (page-image (assq-ref (site-page-metadata page) 'og-image))
         (page-image-alt (assq-ref (site-page-metadata page) 'og-image-alt))
         (og-image (or page-image (render-context-ref context 'og-image)))
         (og-image-alt (cond (page-image-alt page-image-alt)
                             (page-image (site-page-title page))
                             (og-image (or (render-context-ref context 'og-image-alt)
                                           (site-page-title page)))
                             (else #f)))
         (lane (and (pair? side-lane) (make-side-lane-collector)))
         (body (subtree-content-sxml forest routes page "" 1 highlighters
                                     #:title-decoration title-decoration
                                     #:metadata-items metadata-items
                                     #:extra-handlers
                                     (if lane
                                         (side-lane-handlers side-lane lane)
                                         '())
                                     #:rules rules
                                     #:context context))
         (tree (tree-block-sxml
                 "page-tree" (site-page-id page) (site-page-title page)
                 (html-page-url routes (site-page-id page)) body
                 #:anchor (string-append "tree-root-" (site-page-id page))
                 #:depth 1
                 #:context context
                 #:metadata-items (metadata-items-for metadata-items page context)
                 #:title-decoration (if title-decoration
                                        (call-hook title-decoration
                                                   (list forest page) context)
                                        '())))
         (article (append (list tree)
                          (if lane (side-lane-sxml lane) '())
                          (backmatter-sxml forest page routes backmatter
                                           labels reference-taxon-id
                                           backmatter-details?
                                           #:metadata-items metadata-items
                                           #:context context))))
    (serialize-html-document
      (html-document
        (site-page-title page)
        (page-description site page)
        (append (table-of-contents-sxml forest page
                                        #:title-decoration title-decoration
                                        #:context context)
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
        #:language-nav language-nav
        #:home-link home-link
        #:current-page (site-page-id page)
        #:article? article?
        #:published (assq-ref (site-page-metadata page) 'published)
        #:updated (assq-ref (site-page-metadata page) 'updated)
        #:og-locales og-locales
        #:og-image og-image
        #:og-image-alt og-image-alt
        #:context context
        #:scripts (if (null? backmatter)
                      '()
                      (list backmatter-fetch-script))))))

(define* (render-html-changed forest style routes mathjax-src backmatter labels
                             reference-taxon-id
                             changed old-artifacts highlighters
                             #:key (title-decoration #f) (article? #f)
                             (backmatter-details? #f) (html-attributes '())
                             (head '()) (site-nav '()) (home-link #f)
                             (metadata-items default-metadata-items)
                             (side-lane '())
                             (rules empty-html-rules)
                             (presentation #f)
                             (context #f)
                             (dirty-paths '()))
  (when presentation
    (set! style (html-presentation-style presentation))
    (set! rules (html-presentation-rules presentation)))
  (set! routes (resolve-html-routes forest routes))
  (check-html-input forest style routes mathjax-src)
  (let ((style-artifacts (html-style-artifacts style)))
    (let ((style-paths (map artifact-path style-artifacts)))
      (let ((old-page-artifacts
             (filter (lambda (artifact)
                       (not (member (artifact-path artifact) style-paths string=?)))
                     old-artifacts)))
        (if (and (every (lambda (path)
                          ;;     Only load-time style artifacts are
                          ;;     dirty: their bytes feed the @font-face
                          ;;     and <link> chrome, never a page body,
                          ;;     so every page artifact keeps its
                          ;;     previous bytes under the retained
                          ;;     forest and the page walk is skipped -
                          ;;     the difference between a seconds-long
                          ;;     asset-only preview cycle and a full
                          ;;     re-render.
                          (member path style-paths string=?))
                        dirty-paths)
                 ;;     Same page count: no page appeared or vanished,
                 ;;     so a changed count falls back to the walk and
                 ;;     new and removed pages still render.
                 (= (length (pages forest)) (length old-page-artifacts)))
            (append style-artifacts old-page-artifacts)
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
                                             #:home-link home-link
                                             #:metadata-items metadata-items
                                             #:side-lane side-lane
                                             #:rules rules
                                             #:context context)))))
                          (pages forest))))
              (append style-artifacts page-artifacts)))))))

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

(define-record-type <html-options>
  (%make-html-options style highlighters routes mathjax-src backmatter
                      backmatter-labels reference-taxon-id title-decoration
                      article? backmatter-details? html-attributes head
                      site-nav home-link metadata-items side-lane rules
                      presentation locale messages settings default-locale
                      site-url og-image og-image-alt hreflang?
                      language-switcher? cache?)
  html-options?
  (style html-options-style)
  (highlighters html-options-highlighters)
  (routes html-options-routes)
  (mathjax-src html-options-mathjax-src)
  (backmatter html-options-backmatter)
  (backmatter-labels html-options-backmatter-labels)
  (reference-taxon-id html-options-reference-taxon-id)
  (title-decoration html-options-title-decoration)
  (article? html-options-article?)
  (backmatter-details? html-options-backmatter-details?)
  (html-attributes html-options-html-attributes)
  (head html-options-head)
  (site-nav html-options-site-nav)
  (home-link html-options-home-link)
  (metadata-items html-options-metadata-items)
  (side-lane html-options-side-lane)
  (rules html-options-rules)
  (presentation html-options-presentation)
  (locale html-options-locale)
  (messages html-options-messages)
  (settings html-options-settings)
  (default-locale html-options-default-locale)
  (site-url html-options-site-url)
  (og-image html-options-og-image)
  (og-image-alt html-options-og-image-alt)
  (hreflang? html-options-hreflang?)
  (language-switcher? html-options-language-switcher?)
  (cache? html-options-cache?))

(define* (make-html-options #:key
                            (style default-html-style)
                            (highlighters (default-highlighters))
                            (routes #f)
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
                            (metadata-items default-metadata-items)
                            (side-lane '())
                            (rules empty-html-rules)
                            (presentation #f)
                            (locale #f)
                            (messages default-message-catalog)
                            (settings #f)
                            (default-locale
                              (and settings
                                   (site-settings-default-locale settings)))
                            (site-url
                              (and settings
                                   (site-settings-site-url settings)))
                            (og-image #f)
                            (og-image-alt #f)
                            (hreflang? #t)
                            (language-switcher? #t)
                            (cache? #t))
  "Build the immutable HTML renderer configuration.

PRESENTATION, when given, replaces STYLE and RULES.  Entry points accept this
value with #:options; the legacy keyword arguments remain as a compatibility
shim."
  (let ((style (or (and presentation
                        (html-presentation-style presentation))
                   style))
        (rules (or (and presentation
                        (html-presentation-rules presentation))
                   rules)))
    (%make-html-options style highlighters routes mathjax-src backmatter
                        backmatter-labels reference-taxon-id title-decoration
                        article? backmatter-details? html-attributes head
                        site-nav home-link metadata-items side-lane rules
                        presentation locale messages settings default-locale
                        site-url og-image og-image-alt hreflang?
                        language-switcher? cache?)))

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
                       (home-link #f)
                       (metadata-items default-metadata-items)
                       (side-lane '())
                       (rules empty-html-rules)
                       (presentation #f)
                       (locale #f)
                       (messages default-message-catalog)
                       (settings #f)
                       (default-locale
                         (and settings (site-settings-default-locale settings)))
                       (site-url
                         (and settings (site-settings-site-url settings)))
                       (og-image #f)
                       (og-image-alt #f)
                       (hreflang? #t)
                       (language-switcher? #t)
                       (options #f))
  (let* ((options
           (or options
               (make-html-options
                 #:style style
                 #:highlighters highlighters
                 #:routes routes
                 #:mathjax-src mathjax-src
                 #:backmatter backmatter
                 #:backmatter-labels backmatter-labels
                 #:reference-taxon-id reference-taxon-id
                 #:title-decoration title-decoration
                 #:article? article?
                 #:backmatter-details? backmatter-details?
                 #:html-attributes html-attributes
                 #:head head
                 #:site-nav site-nav
                 #:home-link home-link
                 #:metadata-items metadata-items
                 #:side-lane side-lane
                 #:rules rules
                 #:presentation presentation
                 #:locale locale
                 #:messages messages
                 #:settings settings
                 #:default-locale default-locale
                 #:site-url site-url
                 #:og-image og-image
                 #:og-image-alt og-image-alt
                 #:hreflang? hreflang?
                 #:language-switcher? language-switcher?)))
         (style (html-options-style options))
         (highlighters (html-options-highlighters options))
         (routes (html-options-routes options))
         (mathjax-src (html-options-mathjax-src options))
         (backmatter (html-options-backmatter options))
         (backmatter-labels (html-options-backmatter-labels options))
         (reference-taxon-id (html-options-reference-taxon-id options))
         (title-decoration (html-options-title-decoration options))
         (article? (html-options-article? options))
         (backmatter-details? (html-options-backmatter-details? options))
         (html-attributes (html-options-html-attributes options))
         (head (html-options-head options))
         (site-nav (html-options-site-nav options))
         (home-link (html-options-home-link options))
         (metadata-items (html-options-metadata-items options))
         (side-lane (html-options-side-lane options))
         (rules (html-options-rules options))
         (locale (html-options-locale options))
         (messages (html-options-messages options))
         (default-locale (html-options-default-locale options))
         (site-url (html-options-site-url options))
         (og-image (html-options-og-image options))
         (og-image-alt (html-options-og-image-alt options))
         (hreflang? (html-options-hreflang? options))
         (language-switcher? (html-options-language-switcher? options)))
    (let ((context
          (make-site-render-context forest
                                    #:routes routes
                                    #:site-url site-url
                                    #:default-locale default-locale
                                    #:hreflang? hreflang?
                                    #:language-switcher? language-switcher?
                                    #:og-image og-image
                                    #:og-image-alt og-image-alt
                                    #:locale locale
                                    #:messages messages
                                    #:translation-groups
                                    (translation-groups (pages forest)))))
    (validate-backmatter-sections backmatter)
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
                            #:home-link home-link
                            #:metadata-items metadata-items
                            #:side-lane side-lane
                            #:rules rules
                            #:context context)))
         (pages forest)))))
(define (split-into-chunks entries chunk-size)
  "Split ENTRIES into consecutive groups of at most CHUNK-SIZE entries,
preserving order.  The tail group may be shorter, so the tail walk uses
the remaining length instead of the chunk size: this Guile's list-tail
errors when the count outlives the list."
  (let loop ((rest entries) (chunks '()))
    (if (null? rest)
        (reverse chunks)
        (let ((size (min chunk-size (length rest))))
          (loop (list-tail rest size)
                (cons (list-head rest size) chunks))))))

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
                             (home-link #f)
                             (metadata-items default-metadata-items)
                             (side-lane '())
                             (rules empty-html-rules)
                             (presentation #f)
                             (locale #f)
                             (messages default-message-catalog)
                             (settings #f)
                             (default-locale
                               (and settings (site-settings-default-locale settings)))
                             (site-url
                               (and settings (site-settings-site-url settings)))
                             (og-image #f)
                             (og-image-alt #f)
                             (hreflang? #t)
                             (language-switcher? #t)
                             (options #f))
  (let ((options
          (or options
              (make-html-options
                #:style style
                #:highlighters highlighters
                #:routes routes
                #:mathjax-src mathjax-src
                #:backmatter backmatter
                #:backmatter-labels backmatter-labels
                #:reference-taxon-id reference-taxon-id
                #:title-decoration title-decoration
                #:article? article?
                #:backmatter-details? backmatter-details?
                #:html-attributes html-attributes
                #:head head
                #:site-nav site-nav
                #:home-link home-link
                #:metadata-items metadata-items
                #:side-lane side-lane
                #:rules rules
                #:presentation presentation
                #:locale locale
                #:messages messages
                #:settings settings
                #:default-locale default-locale
                #:site-url site-url
                #:og-image og-image
                #:og-image-alt og-image-alt
                #:hreflang? hreflang?
                #:language-switcher? language-switcher?))))
    (let ((style (html-options-style options))
          (highlighters (html-options-highlighters options))
          (routes (html-options-routes options))
          (mathjax-src (html-options-mathjax-src options))
          (backmatter (html-options-backmatter options))
          (backmatter-labels (html-options-backmatter-labels options))
          (reference-taxon-id (html-options-reference-taxon-id options))
          (title-decoration (html-options-title-decoration options))
          (article? (html-options-article? options))
          (backmatter-details? (html-options-backmatter-details? options))
          (html-attributes (html-options-html-attributes options))
          (head (html-options-head options))
          (site-nav (html-options-site-nav options))
          (home-link (html-options-home-link options))
          (metadata-items (html-options-metadata-items options))
          (side-lane (html-options-side-lane options))
          (rules (html-options-rules options))
          (locale (html-options-locale options))
          (messages (html-options-messages options))
          (default-locale (html-options-default-locale options))
          (site-url (html-options-site-url options))
          (og-image (html-options-og-image options))
          (og-image-alt (html-options-og-image-alt options))
          (hreflang? (html-options-hreflang? options))
          (language-switcher? (html-options-language-switcher? options)))
      (let ((context
          (make-site-render-context forest
                                    #:routes routes
                                    #:site-url site-url
                                    #:default-locale default-locale
                                    #:hreflang? hreflang?
                                    #:language-switcher? language-switcher?
                                    #:og-image og-image
                                    #:og-image-alt og-image-alt
                                    #:locale locale
                                    #:messages messages
                                    #:translation-groups
                                    (translation-groups (pages forest)))))
    (validate-backmatter-sections backmatter)
    (check-reference-taxon-id reference-taxon-id)
    (set! routes (resolve-html-routes forest routes))
    (check-html-input forest style routes mathjax-src)
    (append
      (html-style-artifacts style)
      (let* ((forest-pages (pages forest))
             (total (length forest-pages))
             (route-table (html-route-table routes
                                            (map site-page-id forest-pages))))
        (map (lambda (page index)
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
                            #:home-link home-link
                            #:metadata-items metadata-items
                            #:side-lane side-lane
                            #:rules rules
                            #:context context)
                 (page-source-paths forest page)))
             forest-pages
             (iota total 1))))))))
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
                                      (metadata-items default-metadata-items)
                                      (side-lane '())
                                      (rules empty-html-rules)
                                      (presentation #f)
                                      (locale #f)
                                      (messages default-message-catalog)
                                      (settings #f)
                                      (default-locale
                                        (and settings (site-settings-default-locale settings)))
                                      (site-url
                                        (and settings (site-settings-site-url settings)))
                                      (og-image #f)
                                      (og-image-alt #f)
                                      (hreflang? #t)
                                      (language-switcher? #t)
                                      (cache? #t)
                                      (options #f))
  (let ((options
          (or options
              (make-html-options
                #:style style
                #:highlighters highlighters
                #:routes routes
                #:mathjax-src mathjax-src
                #:backmatter backmatter
                #:backmatter-labels backmatter-labels
                #:reference-taxon-id reference-taxon-id
                #:title-decoration title-decoration
                #:article? article?
                #:backmatter-details? backmatter-details?
                #:html-attributes html-attributes
                #:head head
                #:site-nav site-nav
                #:home-link home-link
                #:metadata-items metadata-items
                #:side-lane side-lane
                #:rules rules
                #:presentation presentation
                #:locale locale
                #:messages messages
                #:settings settings
                #:default-locale default-locale
                #:site-url site-url
                #:og-image og-image
                #:og-image-alt og-image-alt
                #:hreflang? hreflang?
                #:language-switcher? language-switcher?
                #:cache? cache?))))
    (let ((style (html-options-style options))
          (highlighters (html-options-highlighters options))
          (routes (html-options-routes options))
          (mathjax-src (html-options-mathjax-src options))
          (backmatter (html-options-backmatter options))
          (backmatter-labels (html-options-backmatter-labels options))
          (reference-taxon-id (html-options-reference-taxon-id options))
          (title-decoration (html-options-title-decoration options))
          (article? (html-options-article? options))
          (backmatter-details? (html-options-backmatter-details? options))
          (html-attributes (html-options-html-attributes options))
          (head (html-options-head options))
          (site-nav (html-options-site-nav options))
          (home-link (html-options-home-link options))
          (metadata-items (html-options-metadata-items options))
          (side-lane (html-options-side-lane options))
          (rules (html-options-rules options))
          (locale (html-options-locale options))
          (messages (html-options-messages options))
          (default-locale (html-options-default-locale options))
          (site-url (html-options-site-url options))
          (og-image (html-options-og-image options))
          (og-image-alt (html-options-og-image-alt options))
          (hreflang? (html-options-hreflang? options))
          (language-switcher? (html-options-language-switcher? options))
          (cache? (html-options-cache? options)))
      (validate-backmatter-sections backmatter)
      (check-reference-taxon-id reference-taxon-id)
      (let ((render (lambda (forest)
                      (render-html forest #:options options))))
    (make-cached-renderer render #:cache? cache?
          #:configuration
          (lambda (forest)
            (list 'html (site-title (site forest)) (site-description (site forest))
                  (or (html-home-page routes) (site-home (site forest)))
                  (html-index-url routes)
                  mathjax-src backmatter backmatter-labels reference-taxon-id
                  article? backmatter-details? html-attributes head site-nav
                  side-lane (resolve-chrome-locale locale (resolve-chrome-catalog messages))
                  default-locale site-url og-image og-image-alt hreflang? language-switcher?
                  (message-catalog-default (resolve-chrome-catalog messages))
                  (message-catalog-entries (resolve-chrome-catalog messages))
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
            (let ((context
                    (make-site-render-context forest
                                              #:routes routes
                                              #:site-url site-url
                                              #:default-locale default-locale
                                              #:hreflang? hreflang?
                                              #:language-switcher? language-switcher?
                                              #:og-image og-image
                                              #:og-image-alt og-image-alt
                                              #:locale locale
                                              #:messages messages
                                              #:translation-groups
                                              (translation-groups
                                                (pages forest)))))
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
                                         #:home-link home-link
                                         #:metadata-items metadata-items
                                         #:side-lane side-lane
                                         #:rules rules
                                         #:context context
                                         #:dirty-paths dirty-paths)))))))))

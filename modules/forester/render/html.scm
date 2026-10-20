(define-module (forester render html)
  #:use-module (grove build)
  #:use-module (grove build state)
  #:use-module (grove i18n)
  #:use-module (grove string-fun)
  #:use-module (forester render artifact)
  #:use-module (forester render labels)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render html)
  #:use-module (grove highlight)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module ((forester private content-index) #:prefix content-index:)
  #:use-module (forester backmatter)
  #:use-module (forester relations)
  #:use-module (grove style html)
  #:use-module (forester style html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (site->html
            site-nav-link
            split-into-chunks
            render-html
            default-metadata-items
            make-html-renderer))

(define (chrome-date text)
  (let ((locale (current-chrome-locale-object)))
    (if locale (format-date locale text #:style 'long) text)))

(define current-hreflang? (make-parameter #f))
(define current-default-locale (make-parameter #f))
(define current-language-switcher? (make-parameter #f))

(define (page-locale page)
  (assq-ref (site-page-metadata page) 'locale))

(define current-translation-groups (make-parameter #f))

(define (translation-groups forest)
  (let ((groups (make-hash-table)))
    (for-each
      (lambda (page)
        (let ((key (assq-ref (site-page-metadata page) 'translation)))
          (when key
            (hash-set! groups key
                       (cons page (or (hash-ref groups key) '()))))))
      (pages forest))
    groups))

(define (translation-group forest page)
  (let ((key (assq-ref (site-page-metadata page) 'translation)))
    (and key
         (reverse (hash-ref (or (current-translation-groups)
                                (translation-groups forest))
                            key
                            '())))))

(define (hreflang-links forest page routes)
  (let ((group (translation-group forest page)))
    (if (or (not group) (< (length group) 2))
        '()
        (append
          (map (lambda (other)
                 `(link (@ (rel "alternate")
                           (hreflang ,(or (page-locale other) "und"))
                           (href ,(html-page-url routes (site-page-id other))))))
               group)
          (let ((default (and (current-default-locale)
                              (find (lambda (other)
                                      (equal? (page-locale other)
                                              (current-default-locale)))
                                    group))))
            (if default
                (list `(link (@ (rel "alternate")
                                (hreflang "x-default")
                                (href ,(html-page-url
                                         routes (site-page-id default))))))
                '()))))))

(define (language-switcher-sxml forest page routes)
  (let ((group (translation-group forest page)))
    (if (or (not group) (< (length group) 2))
        '()
        `(nav (@ (class "language-switcher")
                 (aria-label ,(chrome-message 'language)))
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
                              (extra-handlers '()))
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
                                #:extra-handlers extra-handlers)
          #:anchor anchor #:depth (1+ depth)
          #:style (content-element-style element)
          #:id-prefix anchor-prefix
          #:metadata-items (metadata-items target)
          #:title-decoration (if title-decoration
                                 (title-decoration forest target)
                                 '())
          #:open? (if (assq 'open? (content-element-properties element))
                      (content-element-property element 'open?) #t))))
    (content->sxml
      (site-page-document page)
      #:id-prefix anchor-prefix
      #:highlighters highlighters
      #:handlers (append extra-handlers
                         (page-content-handlers forest routes page anchor-prefix))
      #:lazy-handlers (map (lambda (kind) (cons kind render-tree))
                          '(subtree transclude query-transclude)))))

(define (display-tree-id id)
  (if (string-prefix? "unstable-" id)
      (string-append "#" (substring id (string-length "unstable-")))
      id))
(define (default-metadata-items page)
  "The stock metadata items: the page's publication dates. Render
hooks replace this wholesale; compose with it to extend."
  (let ((published (assq-ref (site-page-metadata page) 'published))
        (updated (assq-ref (site-page-metadata page) 'updated)))
    `(,@(if published
            `((li (@ (class "meta-item"))
                  (time (@ (datetime ,published)) ,(chrome-date published))))
            '())
      ,@(if updated
            `((li (@ (class "meta-item"))
                  (time (@ (datetime ,updated))
                        ,(chrome-message-format
                           'metadata-updated (list (chrome-date updated))))))
            '()))))

(define (metadata-sxml items)
  (if (null? items)
      '()
      (list `(div (@ (class "metadata")) (ul ,@items)))))

(define* (tree-block-sxml kind id title page-url content
                          #:key (open? #t) anchor (depth 2)
                          (title-decoration '()) (style #f) (id-prefix "")
                          (metadata-items '()))
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
               (aria-label ,(chrome-message-format
                              'tree-open-page (list (display-tree-id id)))))
              "[" ,(display-tree-id id) "]"))
         ,@(metadata-sxml metadata-items))
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
                                                 (site-page-id page))))
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
                   #:base-url (html-page-url routes (site-page-id page)))))
    (tree-block-sxml
      "backmatter"
      (site-page-id page) (site-page-title page) url
      (if (null? outline)
          '()
          (list `(ol (@ (class "tree-outline")) ,@outline)))
      #:anchor (string-append "backmatter-" class "-" (site-page-id page))
      #:depth 3
      #:metadata-items (metadata-items page)
      #:title-decoration (taxon-entry-sxml page)
      #:open? #f)))

(define (backlink-reference-sxml forest routes class reference
                                 metadata-items)
  (let ((source (lookup-page forest (reference-source-page-id reference))))
    (backmatter-entry-sxml forest routes class source metadata-items
                           #:url (reference-source-url routes reference))))

(define (backmatter-section-sxml class label content details?)
  (if details?
      `(aside (@ (class ,(string-append "backmatter forest-tree-block " class)))
         (details (@ (open "open"))
           (summary (h2 (@ (class "forest-tree-title")) ,label))
           (div (@ (class "forest-tree-content")) ,@content)))
      `(aside (@ (class ,(string-append "backmatter " class)))
         (h2 ,label)
         ,@content)))

(define (backlinks-sxml forest page routes label details? metadata-items)
  (let ((references (incoming-references forest (site-page-id page))))
    (if (null? references)
        '()
        (list (backmatter-section-sxml
                "backlinks" label
                (map (lambda (reference)
                       (backlink-reference-sxml forest routes "backlinks"
                                                reference metadata-items))
                     references)
                details?)))))

(define (context-sxml forest page routes label details? metadata-items)
  (let ((sources (context-pages forest (site-page-id page))))
    (if (null? sources)
        '()
        (list (backmatter-section-sxml
                "context" label
                (map (lambda (source)
                       (backmatter-entry-sxml forest routes "context" source
                                              metadata-items))
                     sources)
                details?)))))

(define (page-list-backmatter-sxml forest routes class label targets
                                   details? metadata-items)
    (if (null? targets)
        '()
        (list (backmatter-section-sxml
                class label
                (map (lambda (target)
                       (backmatter-entry-sxml forest routes class target
                                              metadata-items))
                     targets)
                details?))))

(define (related-sxml forest page routes reference-taxon-id label details?
                      metadata-items)
  (page-list-backmatter-sxml
    forest routes "related" label
    (related-pages forest (site-page-id page) reference-taxon-id)
    details? metadata-items))

(define (references-sxml forest page routes reference-taxon-id label details?
                         metadata-items)
  (page-list-backmatter-sxml
    forest routes "references" label
    (reference-pages forest (site-page-id page) reference-taxon-id)
    details? metadata-items))

(define* (backmatter-sxml forest page routes sections labels
                         reference-taxon-id details?
                         #:key (metadata-items default-metadata-items))
  (append-map
    (lambda (section)
      (case section
        ((context) (context-sxml forest page routes
                                 (assq-ref labels 'context) details?
                                 metadata-items))
        ((references) (references-sxml forest page routes reference-taxon-id
                                       (assq-ref labels 'references) details?
                                       metadata-items))
        ((backlinks) (backlinks-sxml forest page routes
                                     (assq-ref labels 'backlinks) details?
                                     metadata-items))
        ((related) (related-sxml forest page routes reference-taxon-id
                                 (assq-ref labels 'related) details?
                                 metadata-items))))
    sections))

(define* (tree-outline-sxml forest id
                            #:key (base-url "") (title-decoration #f)
                            (current-page #f))
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
                                   (title-decoration forest target)
                                   '()))
                             (nested
                               (if (member target-id visited)
                                   '()
                                   (entries target-id
                                            (string-append anchor "--")
                                            (cons target-id visited)))))
                        `(li (a (@ (href ,(string-append base-url "#"
                                                         anchor))
                                   ,@(if (and current-page
                                              (string=? target-id current-page))
                                         '((aria-current "page"))
                                         '()))
                                ,@decoration
                                ,(site-page-title target))
                             ,@(if (null? nested) '()
                                   (list `(ol ,@nested))))))
                    (tree-occurrences forest id)
                    (iota (length (tree-occurrences forest id)) 1)))))
    (entries id "" (list id))))

(define* (table-of-contents-sxml forest page #:key (title-decoration #f))
  (let ((items (tree-outline-sxml forest (site-page-id page)
                                  #:title-decoration title-decoration
                                  #:current-page (site-page-id page))))
    (if (null? items) '()
        (list `(nav (@ (class "table-of-contents")
                       (aria-label ,(chrome-message 'nav-contents)))
                    (h2 ,(chrome-message 'nav-contents))
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

(define* (html-document title description body math? home-title style
                       routes mathjax-src inline-stylesheets?
                       #:key (html-attributes '()) (head '()) (site-nav '())
                       (home-link #f) (scripts '()) (current-page #f)
                       (language-nav '()))
  (let* ((site-nav (if (procedure? site-nav)
                       (site-nav current-page)
                       site-nav))
         (locale (current-chrome-locale-object))
         (html-attributes
           (if locale
               (merge-html-attributes (locale-html-attributes locale)
                                      html-attributes)
               html-attributes)))
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
       (a (@ (class "skip-link") (href "#main"))
          ,(chrome-message 'skip-to-content))
       (header
         ,@(if (or home-title home-link (pair? site-nav) (pair? language-nav))
               (list `(nav (@ (aria-label ,(chrome-message 'nav-site)))
                           ,@(cond
                               (home-link (list (home-link (html-index-url routes))))
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

(define (serialize-html-document sxml)
  (string-append "<!DOCTYPE html>\n" (sxml->html sxml)))

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

(define* (page-html forest page style routes mathjax-src backmatter labels
                    reference-taxon-id
                    #:optional (inline-stylesheets? #f) (highlighters '())
                    #:key (title-decoration #f) (article? #f) (backmatter-details? #f)
                    (html-attributes '()) (head '()) (site-nav '()) (home-link #f)
                    (metadata-items default-metadata-items) (side-lane '()))
  (let* ((site (site forest))
         (head (if (current-hreflang?)
                   (append head (hreflang-links forest page routes))
                   head))
         (language-nav (if (current-language-switcher?)
                           (language-switcher-sxml forest page routes)
                           '()))
         (lane (and (pair? side-lane) (make-side-lane-collector)))
         (body (subtree-content-sxml forest routes page "" 1 highlighters
                                     #:title-decoration title-decoration
                                     #:metadata-items metadata-items
                                     #:extra-handlers
                                     (if lane
                                         (side-lane-handlers side-lane lane)
                                         '())))
         (tree (tree-block-sxml
                 "page-tree" (site-page-id page) (site-page-title page)
                 (html-page-url routes (site-page-id page)) body
                 #:anchor (string-append "tree-root-" (site-page-id page))
                 #:depth 1
                 #:metadata-items (metadata-items page)
                 #:title-decoration (if title-decoration
                                        (title-decoration forest page)
                                        '())))
         (article (append (list tree)
                          (if lane (side-lane-sxml lane) '())
                          (backmatter-sxml forest page routes backmatter
                                           labels reference-taxon-id
                                           backmatter-details?
                                           #:metadata-items metadata-items))))
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
        #:language-nav language-nav
        #:home-link home-link
        #:current-page (site-page-id page)
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
                             (dirty-paths '()))
  (set! labels
    (normalize-backmatter-labels
      labels
      #:defaults (localized-backmatter-labels)))
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
                                             #:side-lane side-lane)))))
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
                       (locale #f)
                       (messages default-message-catalog)
                       (default-locale #f)
                       (hreflang? #f)
                       (language-switcher? #f))
  (parameterize ((current-hreflang? hreflang?)
                 (current-default-locale default-locale)
                 (current-language-switcher? language-switcher?)
                 (current-translation-groups (translation-groups forest)))
    (with-chrome-locale (locale messages)
      (validate-backmatter-sections backmatter)
      (set! backmatter-labels
        (normalize-backmatter-labels
          backmatter-labels
          #:defaults (localized-backmatter-labels)))
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
                              #:side-lane side-lane)))
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
                             (locale #f)
                             (messages default-message-catalog)
                             (default-locale #f)
                             (hreflang? #f)
                             (language-switcher? #f))
  (parameterize ((current-hreflang? hreflang?)
                 (current-default-locale default-locale)
                 (current-language-switcher? language-switcher?)
                 (current-translation-groups (translation-groups forest)))
    (with-chrome-locale (locale messages)
      (validate-backmatter-sections backmatter)
      (set! backmatter-labels
        (normalize-backmatter-labels
          backmatter-labels
          #:defaults (localized-backmatter-labels)))
      (check-reference-taxon-id reference-taxon-id)
      (set! routes (resolve-html-routes forest routes))
      (check-html-input forest style routes mathjax-src)
      (append
        (html-style-artifacts style)
        (let* ((report (current-render-progress))
               (forest-pages (pages forest))
               (total (length forest-pages))
               (route-table (html-route-table routes
                                              (map site-page-id forest-pages))))
          ;;     Pages render independently against the shared forest. The
          ;;     entry list is zipped with an explicit named let: the
          ;;     multi-list map on load paths that pull in
          ;;     faster-minikanren's rnrs override is unreliable.
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
                              #:side-lane side-lane)
                   (page-source-paths forest page)))
               forest-pages
               (iota total 1)))))))
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
                                      (locale #f)
                                      (messages default-message-catalog)
                                      (default-locale #f)
                                      (hreflang? #f)
                                      (language-switcher? #f)
                                      (cache? #t))
  (validate-backmatter-sections backmatter)
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
                                      #:home-link home-link
                                      #:metadata-items metadata-items
                                      #:side-lane side-lane
                                      #:locale locale
                                      #:messages messages
                                      #:default-locale default-locale
                                      #:hreflang? hreflang?
                                      #:language-switcher? language-switcher?))))
    (make-cached-renderer render #:cache? cache?
          #:configuration
          (lambda (forest)
            (list 'html (site-title (site forest)) (site-description (site forest))
                  (or (html-home-page routes) (site-home (site forest)))
                  (html-index-url routes)
                  mathjax-src backmatter backmatter-labels reference-taxon-id
                  article? backmatter-details? html-attributes head site-nav
                  side-lane (resolve-chrome-locale locale (resolve-chrome-catalog messages))
                  default-locale hreflang? language-switcher?
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
            (parameterize ((current-hreflang? hreflang?)
                           (current-default-locale default-locale)
                           (current-language-switcher? language-switcher?)
                           (current-translation-groups
                             (translation-groups forest)))
              (with-chrome-locale (locale messages)
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
                                         #:dirty-paths dirty-paths))))))))

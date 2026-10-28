;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render html)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove build)
  #:use-module (grove private fingerprint)
  #:use-module (grove private html-attributes)
  #:use-module (grove i18n)
  #:use-module (grove private string-fun)
  #:use-module (forester private render artifact)
  #:use-module (forester private render anchors)
  #:use-module (forester private render labels)
  #:use-module (forester private translations)
  #:use-module (forester render context)
  #:use-module (forester render site)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove private render html)
  #:use-module (grove private snapshot)
  #:use-module (grove private uri)
  #:use-module (grove render html)
  #:use-module (grove render html attributes)
  #:use-module (grove render rules)
  #:use-module (grove render context)
  #:use-module (grove render html presentation)
  #:use-module (grove render plain-text)
  #:use-module (grove highlight)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (forester private content-index)
  #:use-module (forester private backmatter)
  #:use-module (forester private entity-uri)
  #:use-module (forester private render-routes)
  #:use-module (forester private web)
  #:use-module (forester relations)
  #:use-module (grove style html)
  #:use-module (forester style html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (site->html
            render-html
            make-html-options
            html-options?
            make-html-renderer))

;; Forester's tree traversal uses the private content interpreter for lazy
;; handlers; those handlers are deliberately outside the public renderer API.
;; Open Graph uses language_TERRITORY, so language-only tags need a stable
;; site-facing territory default. These defaults belong to this output format;
;; BCP 47 tags with an explicit region always take precedence.
(define og-default-territories
  '(("en" . "US") ("fr" . "FR") ("de" . "DE") ("es" . "ES")
    ("it" . "IT") ("pt" . "PT") ("nl" . "NL") ("sv" . "SE")
    ("da" . "DK") ("nb" . "NO") ("fi" . "FI") ("pl" . "PL")
    ("cs" . "CZ") ("tr" . "TR") ("ru" . "RU") ("el" . "GR")
    ("ja" . "JP") ("ko" . "KR") ("zh" . "CN")))

(define (ascii-alpha? character)
  (or (char<=? #\A character #\Z)
      (char<=? #\a character #\z)))

(define (ascii-digit? character)
  (char<=? #\0 character #\9))

(define (og-region? part)
  (or (and (= (string-length part) 2)
           (string-every ascii-alpha? part))
      (and (= (string-length part) 3)
           (string-every ascii-digit? part))))

(define (og-script? part)
  (and (= (string-length part) 4)
       (string-every ascii-alpha? part)))

(define (open-graph-locale-tag locale)
  "Return the Open Graph language_TERRITORY form for LOCALE."
  (let* ((tag (locale-tag (if (locale? locale) locale (make-locale locale))))
         (parts (string-split tag #\-))
         (language (car parts))
         (subtags (cdr parts)))
    (let scan ((subtags subtags) (script #f) (region #f))
      (cond
        ((or (null? subtags) (= (string-length (car subtags)) 1))
         (let ((territory
                 (or region
                     (and (string=? language "zh")
                          (if (equal? script "Hant") "TW" "CN"))
                     (assoc-ref og-default-territories language))))
           (if territory
               (string-append language "_" territory)
               language)))
        ((og-region? (car subtags))
         (scan (cdr subtags) script (car subtags)))
        ((and (not script) (og-script? (car subtags)))
         (scan (cdr subtags) (car subtags) region))
        (else (scan (cdr subtags) script region))))))

(define (format-rendered-date text context)
  (let ((locale (render-context-ref context 'locale-object)))
    (if locale (format-date locale text #:style 'long) text)))

(define (hreflang-links forest page routes context)
  (let ((group (page-translation-group
                 page (render-context-ref context 'translation-groups))))
    (if (or (not group) (< (length group) 2))
        '()
        (append
          (map (lambda (other)
                 `(link (@ (rel "alternate")
                           (hreflang ,(or (page-locale other) "und"))
                           (href ,(html-page-url routes (site-page-id other))))))
               group)
          (let ((default
                  (default-translation-page
                    group (render-context-ref context 'default-locale))))
            (if default
                (list `(link (@ (rel "alternate")
                                (hreflang "x-default")
                                (href ,(html-page-url
                                         routes (site-page-id default))))))
                '()))))))

(define (alternate-og-locales forest page context)
  (let ((group (page-translation-group
                 page (render-context-ref context 'translation-groups))))
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
  (let ((group (page-translation-group
                 page (render-context-ref context 'translation-groups))))
    (if (or (not group) (< (length group) 2))
        '()
        `(nav (@ (class "language-switcher")
                 (aria-label ,(interface-message 'language context)))
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
  (let ((fragment (and element (ir:content-element-property/raw element 'fragment))))
    (string-append (html-page-url routes (reference-target-page-id reference))
                   (if fragment (string-append "#" fragment) ""))))

(define (reference-source-url routes reference)
  (string-append
    (html-page-url routes (reference-source-page-id reference))
    "#" (reference-source-id reference)))

(define (term-mention-sxml forest routes element children)
  "Render an element marked with a term property: an anchor to the
canonical defining occurrence - the owning page's own rendering keeps
unprefixed anchor ids - with the definition's display text when the
author supplies no body.  A term nothing defines is not a dead link:
the body (or the term id) stays inline under missing-term, and the
backlog report collects the mention."
  (let* ((term-id (ir:content-element-property/raw element 'term))
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
                 (let* ((source (ir:content-element-property/raw element 'source))
                        (url (if (external-url? source)
                                 source
                                 (local-asset-url routes source)))
                        (alt (content-verbatim-string children)))
                   `(img (@ (src ,url) (alt ,alt))))))
      (link . ,(lambda (element children)
                 (if (ir:content-element-property/raw element 'term)
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
                       (if (ir:content-element-property/raw element 'term)
                           (term-mention-sxml forest routes element children)
                           (let ((fragment
                                   (ir:content-element-property/raw element 'fragment)))
                             `(a (@ (href ,(string-append
                                             (html-page-url routes
                                               (ir:content-element-property/raw element 'target))
                                             (if fragment
                                                 (string-append "#" fragment)
                                                 ""))))
                                 ,@children))))))))

(define* (subtree-content-sxml forest routes page
                              #:optional (anchor-prefix "") (depth 1) (highlighters '())
                              #:key (title-decoration #f)
                              (metadata-items default-metadata-items)
                              (extra-handlers '())
                              (rules empty-render-rules)
                    (context #f))
  (let ((position 0))
    (define (render-tree element _render)
      (set! position (1+ position))
      (let* ((id (tree-occurrence-target element))
             (target (require-page forest id))
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
          #:metadata-items (metadata-items target context)
          #:title-decoration (if title-decoration
                                 (title-decoration forest target)
                                 '())
          #:open? (if (assq 'open? (ir:content-element-properties/raw element))
                      (ir:content-element-property/raw element 'open?) #t))))
    (content->sxml/internal
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
                      #:anchor-prefix anchor-prefix
                      #:hreflang? #t
                      #:language-switcher? #t))
      #:internal-handlers (append
                   extra-handlers
                   (page-content-handlers forest routes page anchor-prefix))
      #:internal-lazy-handlers (map (lambda (kind) (cons kind render-tree))
                          '(subtree transclude query-transclude)))))

(define (display-tree-id id)
  (if (string-prefix? "unstable-" id)
      (string-append "#" (substring id (string-length "unstable-")))
      id))
(define (default-metadata-items page context)
  "The stock metadata items: the page's publication dates. Additional items
from the HTML presentation are appended after these."
  (let ((published (assq-ref (site-page-metadata page) 'published))
        (updated (assq-ref (site-page-metadata page) 'updated)))
    `(,@(if published
            `((li (@ (class "meta-item"))
                  (time (@ (datetime ,published)) ,(format-rendered-date published context))))
            '())
      ,@(if updated
            `((li (@ (class "meta-item"))
                  (time (@ (datetime ,updated))
                        ,(interface-message-format
                           'metadata-updated (list (format-rendered-date updated context))
                           context))))
            '()))))

(define (presentation-title-decoration presentation page)
  (html-presentation-title-decoration presentation (site-page-id page)))

(define (presentation-metadata-items presentation page context)
  (append (default-metadata-items page context)
          (html-presentation-metadata-items presentation (site-page-id page))))

(define (presentation-title-decoration-procedure presentation)
  (lambda (_forest page) (presentation-title-decoration presentation page)))

(define (presentation-metadata-items-procedure presentation)
  (lambda (page context) (presentation-metadata-items presentation page context)))

(define (presentation-cache-identity presentation)
  (let ((home-link (html-presentation-home-link presentation)))
    (list (html-presentation-head presentation)
          (and home-link
               (list (html-home-link-content home-link)
                     (html-home-link-attributes home-link)))
          (map (lambda (entry)
                 (if (html-nav-link? entry)
                     (list 'nav-link (html-nav-link-page-id entry)
                           (html-nav-link-label entry)
                           (html-nav-link-attributes entry))
                     entry))
               (html-presentation-site-nav presentation))
          (html-presentation-title-decorations presentation)
          (html-presentation-metadata-item-map presentation))))

(define (site-nav-for-page routes entries page)
  (map (lambda (entry)
         (if (html-nav-link? entry)
             (let* ((link (nav-link-sxml routes
                                         (html-nav-link-page-id entry)
                                         (html-nav-link-label entry)
                                         #:current-page (site-page-id page)))
                    (attributes (html-nav-link-attributes entry)))
               (if (null? attributes)
                   link
                   (cons 'a
                     (cons (cons '@
                             (append
                               (map (lambda (attribute)
                                      (list (car attribute) (cdr attribute)))
                                    attributes)
                               (cdadr link)))
                           (cddr link)))))
             entry))
       entries))

(define (home-link-sxml routes home-link)
  (and home-link
       `(a (@ ,@(map (lambda (attribute)
                       (list (car attribute) (cdr attribute)))
                     (html-home-link-attributes home-link))
              (href ,(html-index-url routes)))
          ,@(html-home-link-content home-link))))

(define (metadata-sxml items)
  (if (null? items)
      '()
      (list `(div (@ (class "metadata")) (ul ,@items)))))

(define* (tree-block-sxml kind id title page-url content
                          #:key (open? #t) anchor (depth 2)
                          (title-decoration '()) (style #f) (id-prefix "")
                          (metadata-items '()) (context #f))
  (let* ((style-attributes (content-style-html-attributes style id-prefix))
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
           (aria-label ,(interface-message-format
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
        (let ((text (content->plain-text taxon)))
          (if (string-null? text)
              '()
              (let ((id (ir:content-element-property/raw taxon 'id)))
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
      #:context context
      #:metadata-items (metadata-items page context)
      #:title-decoration (taxon-entry-sxml page)
      #:open? #f)))

(define (backlink-reference-sxml forest routes class reference
                                 metadata-items context)
  (let ((source (find-page forest (reference-source-page-id reference))))
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
               (let ((occurrences (tree-occurrences forest id)))
                 (map (lambda (element position)
                        (let* ((target-id (tree-occurrence-target element))
                               (target (require-page forest target-id))
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
                                                           anchor)))
                                  ,@decoration
                                  ,(site-page-title target))
                               ,@(if (null? nested) '()
                                     (list `(ol ,@nested))))))
                      occurrences
                      (iota (length occurrences) 1))))))
    (entries id "" (list id))))

(define* (table-of-contents-sxml forest page #:key (title-decoration #f) (context #f))
  (let ((items (tree-outline-sxml forest (site-page-id page)
                                  #:title-decoration title-decoration
                                  #:context context)))
    (if (null? items) '()
        (list `(nav (@ (class "table-of-contents")
                       (aria-label ,(interface-message 'nav-contents context)))
                    (h2 ,(interface-message 'nav-contents context))
                    (ol ,@items))))))

(define* (nav-link-sxml routes id label #:key (current-page #f))
  "Build route-aware SXML for a declarative navigation link."
  `(a (@ (href ,(html-page-url routes id))
         ,@(if (and current-page (string=? id current-page))
               '((aria-current "page"))
               '()))
      ,label))

(define default-mathjax-src
  "https://cdn.jsdelivr.net/npm/mathjax@4/tex-chtml.js")

(define (external-url? value)
  (and (string? value)
       (or (string-prefix-ci? "https://" value)
           (string-prefix-ci? "http://" value)
           (string-prefix? "//" value))))

(define (local-asset-url routes source)
  (call-with-values
    (lambda () (split-uri-suffix source))
    (lambda (path suffix)
      (string-append
        (if (string-prefix? "/" path)
            (uri-encode-path path)
            (html-asset-url routes path))
        suffix))))

(define (validate-mathjax-source source)
  (unless (or (not source)
              (and (non-empty-string? source)
                   (or (external-url? source)
                       (artifact-path? source))))
    (error "MathJax source must be a URL, relative artifact path, or #f"
           source))
  source)

(define (describes-sxml page context)
  (let ((uris (or (assq-ref (site-page-metadata page) 'describes) '())))
    (if (null? uris)
        '()
        (list
          `(section (@ (class "describes"))
             (h2 ,(interface-message 'describes context))
             (ul
               ,@(map (lambda (uri)
                        (let ((href (entity-uri-href uri)))
                          `(li ,(if href
                                    `(a (@ (href ,href)) (code ,uri))
                                    `(code ,uri)))))
                      uris)))))))

(define (mathjax-script-url source routes)
  (and source
       (if (external-url? source)
           source
           (html-asset-url routes source))))

(define (content-has-tex-math? content)
  (cond
    ((content-element? content)
     (or (and (eq? (content-element-kind content) 'math)
              (ir:content-element-property/raw content 'tex?))
         (any content-has-tex-math? (ir:content-element-children/raw content))))
    ((content-document? content)
     (any content-has-tex-math? (ir:content-document-children/raw content)))
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
  (let* ((site-nav site-nav)
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
                (cond ((or (string-prefix-ci? "http://" og-image)
                           (string-prefix-ci? "https://" og-image))
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
                                  (content ,(open-graph-locale-tag
                                               (render-context-ref context 'locale))))))
                 '())
             (map (lambda (tag)
                    `(meta (@ (property "og:locale:alternate")
                              (content ,(open-graph-locale-tag tag)))))
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
                    `(style ,(out:artifact-content/raw stylesheet)))
                  (html-style-stylesheets style))
             (map (lambda (stylesheet)
                    `(link (@ (rel "stylesheet")
                              (href ,(html-asset-url
                                       routes (out:artifact-path/raw stylesheet))))))
                  (html-style-stylesheets style)))
       ,@(if (and math? mathjax-src)
             `((script (@ (src ,(mathjax-script-url mathjax-src routes))
                         (defer "defer"))
                       " "))
             '()))
     (body
       (a (@ (class "skip-link") (href "#main"))
          ,(interface-message 'skip-to-content context))
       (header
         ,@(if (or home-title home-link (pair? site-nav) (pair? language-nav))
               (list `(nav (@ (aria-label ,(interface-message 'nav-site context)))
                           ,@(cond
                               (home-link (list home-link))
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

(define* (page-html forest page style routes mathjax-src backmatter
                    reference-taxon-id
                    #:optional (inline-stylesheets? #f) (highlighters '())
                    #:key (title-decoration #f) (article? #f) (backmatter-details? #f)
                    (html-attributes '()) (head '()) (site-nav '()) (home-link #f)
                    (metadata-items default-metadata-items) (side-lane '())
                    (rules empty-render-rules)
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
         (labels (localized-backmatter-labels context))
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
                 #:metadata-items (metadata-items page context)
                 #:title-decoration (if title-decoration
                                        (title-decoration forest page)
                                        '())))
         (article (append (list tree)
                          (describes-sxml page context)
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
        #:site-nav (site-nav-for-page routes site-nav page)
        #:language-nav language-nav
        #:home-link (home-link-sxml routes home-link)
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
  (validate-mathjax-source mathjax-src))

(define-record-type <html-options>
  (%make-html-options highlighters routes mathjax-src backmatter
                      reference-taxon-id
                      article? backmatter-details? html-attributes
                      side-lane presentation locale messages default-locale
                      site-url og-image og-image-alt hreflang?
                      language-switcher?)
  html-options?
  (highlighters html-options-highlighters)
  (routes html-options-routes)
  (mathjax-src html-options-mathjax-src)
  (backmatter html-options-backmatter)
  (reference-taxon-id html-options-reference-taxon-id)
  (article? html-options-article?)
  (backmatter-details? html-options-backmatter-details?)
  (html-attributes html-options-html-attributes)
  (side-lane html-options-side-lane)
  (presentation html-options-presentation)
  (locale html-options-locale)
  (messages html-options-messages)
  (default-locale html-options-default-locale)
  (site-url html-options-site-url)
  (og-image html-options-og-image)
  (og-image-alt html-options-og-image-alt)
  (hreflang? html-options-hreflang?)
  (language-switcher? html-options-language-switcher?))

(define (valid-side-lane? kinds)
  (and (list? kinds)
       (every symbol? kinds)
       (= (length kinds) (length (delete-duplicates kinds eq?)))))

(define (require-html-options options who)
  (unless (html-options? options)
    (error (string-append who " requires HTML options") options))
  options)

(define (make-html-page-renderer forest options routes)
  (let* ((presentation (html-options-presentation options))
         (style (html-presentation-style presentation))
         (highlighters (html-options-highlighters options))
         (mathjax-src (html-options-mathjax-src options))
         (backmatter (html-options-backmatter options))
         (reference-taxon-id (html-options-reference-taxon-id options))
         (title-decoration
           (presentation-title-decoration-procedure presentation))
         (article? (html-options-article? options))
         (backmatter-details? (html-options-backmatter-details? options))
         (html-attributes (html-options-html-attributes options))
         (head (html-presentation-head presentation))
         (site-nav (html-presentation-site-nav presentation))
         (home-link (html-presentation-home-link presentation))
         (metadata-items
           (presentation-metadata-items-procedure presentation))
         (side-lane (html-options-side-lane options))
         (rules (html-presentation-rules presentation))
         (context (make-html-render-context forest routes options)))
    (check-html-input forest style routes mathjax-src)
    (lambda (page inline-stylesheets?)
      (page-html forest page style routes mathjax-src backmatter
                 reference-taxon-id inline-stylesheets? highlighters
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
                 #:context context))))

(define (render-html-changed forest options changed old-artifacts dirty-paths)
  (let* ((presentation (html-options-presentation options))
         (style (html-presentation-style presentation))
         (routes (resolve-site-routes forest (html-options-routes options)))
         (render-page (make-html-page-renderer forest options routes))
         (style-artifacts (html-style-artifacts style))
         (style-paths (map out:artifact-path/raw style-artifacts))
         (old-page-artifacts
           (filter (lambda (artifact)
                     (not (member (out:artifact-path/raw artifact)
                                  style-paths string=?)))
                   old-artifacts)))
    (if (and (every (lambda (path)
                      ;;     Only load-time style artifacts are dirty: their
                      ;;     bytes feed the @font-face rules and stylesheet
                      ;;     links, never a page body, so every page artifact
                      ;;     keeps its previous bytes under the retained forest
                      ;;     and the page walk is skipped.
                      (member path style-paths string=?))
                    dirty-paths)
             ;;     Same page count: no page appeared or vanished, so a changed
             ;;     count falls back to the walk and new and removed pages
             ;;     still render.
             (= (length (pages forest)) (length old-page-artifacts)))
        (append style-artifacts old-page-artifacts)
        (let* ((route-table
                 (html-route-table routes (map site-page-id (pages forest))))
               (page-artifacts
                 (map (lambda (page)
                        (let* ((path (cdr (assoc (site-page-id page) route-table)))
                               (sources (page-source-paths forest page)))
                          (reuse-artifact path sources old-artifacts changed
                            (lambda () (render-page page #f)))))
                      (pages forest))))
          (append style-artifacts page-artifacts)))))

(define* (make-html-options #:key
                            (highlighters (default-highlighters))
                            (routes (make-html-routes))
                            (mathjax-src default-mathjax-src)
                            (backmatter default-backmatter-sections)
                            (reference-taxon-id default-reference-taxon-id)
                            (article? #f)
                            (backmatter-details? #f)
                            (html-attributes '())
                            (side-lane '())
                            (presentation #f)
                            (settings #f)
                            (locale
                              (and (site-settings? settings)
                                   (site-settings-default-locale settings)))
                            (messages default-message-catalog)
                            (default-locale
                              (and (site-settings? settings)
                                   (site-settings-default-locale settings)))
                            (site-url
                              (and (site-settings? settings)
                                   (site-settings-site-url settings)))
                            (og-image #f)
                            (og-image-alt #f)
                            (hreflang? #t)
                            (language-switcher? #t))
  "Build a validated HTML-renderer configuration snapshot.

PRESENTATION contains HTML style, element rules, and static site presentation data.
Entry points take this value with #:options."
  (let* ((highlighters (snapshot-data highlighters))
         (backmatter (snapshot-data backmatter))
         (reference-taxon-id (snapshot-data reference-taxon-id))
         (html-attributes (snapshot-data html-attributes))
         (side-lane (snapshot-data side-lane))
         (locale (snapshot-data locale))
         (default-locale (snapshot-data default-locale))
         (site-url (snapshot-data site-url))
         (mathjax-src (snapshot-data mathjax-src))
         (og-image (snapshot-data og-image))
         (og-image-alt (snapshot-data og-image-alt))
         (presentation
          (or presentation
              (make-html-presentation #:style default-html-style))))
    (unless (or (not settings) (site-settings? settings))
      (error "HTML settings must be site settings or #f" settings))
    (unless (html-routes? routes)
      (error "HTML options require HTML routes" routes))
    (validate-backmatter-sections backmatter)
    (check-reference-taxon-id reference-taxon-id)
    (set! locale (validate-interface-options locale messages))
    (unless (valid-html-attribute-list? html-attributes)
      (error "HTML root attributes must be unique symbol/string pairs"
             html-attributes))
    (unless (valid-side-lane? side-lane)
      (error "HTML side-lane kinds must be unique symbols" side-lane))
    (when default-locale
      (unless (non-empty-string? default-locale)
        (error "HTML default locale must be a non-empty tag or #f"
               default-locale))
      (set! default-locale (locale-tag (make-locale default-locale))))
    (unless (or (not site-url) (web-origin? site-url))
      (error "HTML site URL must be an absolute HTTP(S) origin or #f"
             site-url))
    (unless (or (not og-image) (non-empty-string? og-image))
      (error "HTML Open Graph image must be a non-empty string or #f"
             og-image))
    (unless (or (not og-image-alt) (string? og-image-alt))
      (error "HTML Open Graph image alt text must be a string or #f"
             og-image-alt))
    (unless (and (list? highlighters)
                 (every (lambda (entry)
                          (and (pair? entry)
                               (symbol? (car entry))
                               (highlighter? (cdr entry))))
                        highlighters)
                 (= (length highlighters)
                    (length (delete-duplicates (map car highlighters) eq?)))
                 (boolean? article?)
                 (boolean? backmatter-details?)
                 (boolean? hreflang?)
                 (boolean? language-switcher?))
      (error "HTML options contain invalid renderer values"))
    (validate-mathjax-source mathjax-src)
    (unless (html-presentation? presentation)
      (error "HTML options require an HTML presentation" presentation))
    (%make-html-options highlighters routes mathjax-src backmatter
                        reference-taxon-id
                        article? backmatter-details? html-attributes
                        side-lane presentation
                        locale messages
                        default-locale site-url og-image og-image-alt hreflang?
                        language-switcher?)))

(define (make-html-render-context forest routes options)
  (make-site-render-context forest
    #:routes routes
    #:site-url (html-options-site-url options)
    #:default-locale (html-options-default-locale options)
    #:hreflang? (html-options-hreflang? options)
    #:language-switcher? (html-options-language-switcher? options)
    #:og-image (html-options-og-image options)
    #:og-image-alt (html-options-og-image-alt options)
    #:locale (html-options-locale options)
    #:messages (html-options-messages options)))

(define* (site->html forest #:key (options (make-html-options)))
  (let* ((_ (require-html-options options "site->html"))
         (routes (resolve-site-routes forest (html-options-routes options)))
         (render-page (make-html-page-renderer forest options routes)))
    (map (lambda (page)
           (cons (site-page-id page)
                 (render-page page #t)))
         (pages forest))))
(define* (render-html forest #:key (options (make-html-options)))
  (let* ((_ (require-html-options options "render-html"))
         (presentation (html-options-presentation options))
         (style (html-presentation-style presentation))
         (routes (resolve-site-routes forest (html-options-routes options)))
         (render-page (make-html-page-renderer forest options routes)))
    (append
      (html-style-artifacts style)
      (let* ((forest-pages (pages forest))
             (total (length forest-pages))
             (route-table (html-route-table routes
                                            (map site-page-id forest-pages))))
        (map (lambda (page index)
               (make-artifact
                 (cdr (assoc (site-page-id page) route-table))
                 (render-page page #f)
                 (page-source-paths forest page)))
             forest-pages
             (iota total 1))))))
(define* (make-html-renderer #:key (options (make-html-options))
                            (rules-id #f)
                            (cache? #t))
    (let* ((_ (require-html-options options "make-html-renderer"))
          (presentation (html-options-presentation options))
          (presentation-data presentation)
          (style (html-presentation-style presentation))
          (highlighters (html-options-highlighters options))
          (routes (html-options-routes options))
          (mathjax-src (html-options-mathjax-src options))
          (backmatter (html-options-backmatter options))
          (reference-taxon-id (html-options-reference-taxon-id options))
          (article? (html-options-article? options))
          (backmatter-details? (html-options-backmatter-details? options))
          (html-attributes (html-options-html-attributes options))
          (side-lane (html-options-side-lane options))
          (rules (html-presentation-rules presentation))
          (locale (html-options-locale options))
          (messages (html-options-messages options))
          (default-locale (html-options-default-locale options))
          (site-url (html-options-site-url options))
          (og-image (html-options-og-image options))
          (og-image-alt (html-options-og-image-alt options))
          (hreflang? (html-options-hreflang? options))
          (language-switcher? (html-options-language-switcher? options))
          (effective-cache?
            (and cache?
                 (or (eq? rules empty-render-rules)
                     (and rules-id #t)))))
      (validate-rules-id rules-id)
      (set! rules-id (snapshot-data rules-id))
      (unless (boolean? cache?)
        (error "Forester HTML renderer cache option must be a boolean" cache?))
      (let ((render (lambda (forest)
                      (render-html forest #:options options))))
    (if (not effective-cache?)
        render
        (make-cached-renderer render
          #:configuration
          (lambda (forest)
            (let* ((routes (resolve-site-routes forest routes))
                   (interface-key (interface-cache-identity locale messages)))
              (list 'html (site-title (site forest))
                    (site-description (site forest))
                    (html-home-page routes)
                    (html-index-url routes)
                    (html-route-table routes
                                      (map site-page-id (pages forest)))
                    mathjax-src backmatter reference-taxon-id
                    article? backmatter-details? html-attributes
                    (presentation-cache-identity presentation-data)
                    side-lane (car interface-key)
                    default-locale site-url og-image og-image-alt
                    hreflang? language-switcher? rules-id
                    (cadr interface-key) (caddr interface-key)
                    (map (lambda (entry)
                           (list (car entry)
                                 (highlighter-configuration (cdr entry))))
                         highlighters)
                    (map (lambda (artifact)
                           (list (out:artifact-path/raw artifact)
                                 (content-fingerprint
                                   (out:artifact-content/raw artifact))))
                         (html-style-artifacts style)))))
          #:inputs (lambda ()
                     (append
                       (append-map (lambda (entry) (highlighter-inputs (cdr entry))) highlighters)
                       (append-map (lambda (artifact) (or (out:artifact-sources/raw artifact) '()))
                                   (html-style-artifacts style))))
          #:render-changed
          (lambda (forest changed dirty-paths old-artifacts)
            (if (any (lambda (entry)
                       (any (lambda (path) (member path changed string=?))
                            (highlighter-inputs (cdr entry)))) highlighters)
                (render forest)
                (render-html-changed forest options changed old-artifacts
                                     dirty-paths))))))))

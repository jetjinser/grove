;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private render html-renderer)
  #:use-module (grove private alist)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove render)
  #:use-module (grove private fingerprint)
  #:use-module (grove private html-attributes)
  #:use-module (grove i18n)
  #:use-module (grove private string-fun)
  #:use-module (forester private render artifact)
  #:use-module (forester private render anchors)
  #:use-module (forester private render labels)
  #:use-module (forester private render cache)
  #:use-module ((forester private render html-document) #:prefix document:)
  #:use-module (forester private translations)
  #:use-module (forester render context)
  #:use-module (forester render site)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove private render html)
  #:use-module (grove private snapshot)
  #:use-module (grove private uri)
  #:use-module (grove render html style)
  #:use-module (grove render rules)
  #:use-module (grove render context)
  #:use-module (grove render html presentation)
  #:use-module (grove ir plain-text)
  #:use-module (grove highlight)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (forester private backmatter)
  #:use-module (forester private entity-iri)
  #:use-module (forester private render-routes)
  #:use-module (forester private web)
  #:use-module (forester relations)
  #:use-module (grove style html)
  #:use-module (forester style html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (forester private render html-page)
  #:export (site->html
            render-html
            make-html-options
            html-options?
            make-html-renderer))

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

(define default-mathjax-src
  "https://cdn.jsdelivr.net/npm/mathjax@4/tex-chtml.js")

(define (validate-mathjax-source source)
  (unless (or (not source)
              (and (non-empty-string? source)
                   (or (external-url? source)
                       (artifact-path? source))))
    (error "MathJax source must be a URL, relative artifact path, or #f"
           source))
  source)

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
    (check-html-input forest style routes)
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
                               (sources (page-source-paths
                                          forest (site-page-id page))))
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

PRESENTATION contains HTML style, element rules, and static site presentation
data. SETTINGS supplies default locale and site URL values; explicit LOCALE,
DEFAULT-LOCALE, and SITE-URL keywords override those values. Entry points take
this value with #:options."
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
                 (unique-symbol-key-alist? highlighters)
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
  "Return an ordered alist mapping each page ID to its rendered HTML string.

OPTIONS configures the same site presentation used by RENDER-HTML. This
projection omits shared style artifacts; use RENDER-HTML for a complete
artifact list or MAKE-HTML-RENDERER in a project."
  (let* ((_ (require-html-options options "site->html"))
         (routes (resolve-site-routes forest (html-options-routes options)))
         (render-page (make-html-page-renderer forest options routes)))
    (map (lambda (page)
           (cons (site-page-id page)
                 (render-page page #t)))
         (pages forest))))
(define* (render-html forest #:key (options (make-html-options)))
  "Render FOREST to its complete HTML artifact list.

OPTIONS selects routes, presentation, styles, localization, and other HTML
behavior. Use MAKE-HTML-RENDERER to add the output to a project lifecycle."
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
                 #:sources (page-source-paths
                             forest (site-page-id page))))
             forest-pages
             (iota total 1))))))
(define* (make-html-renderer #:key (options (make-html-options))
                            (rules-id #f)
                            (cache? #t))
  "Construct a project renderer that produces HTML artifacts for a Forest.

OPTIONS supplies HTML behavior. CACHE? defaults to enabled; custom rendering
RULES need a stable RULES-ID for safe caching. The result is a plain procedure
when caching cannot be identified safely, otherwise a cached renderer."
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
          (effective-cache? (renderer-cacheable? rules rules-id cache?)))
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
                           (let ((sources
                                   (or (out:artifact-sources/raw artifact) '())))
                             ;;     Declared sources invalidate this output through
                             ;;     artifact dirtiness, where the incremental
                             ;;     callback can refresh it without discarding
                             ;;     every page. Inline artifacts have no such
                             ;;     dependency signal, so their bytes remain part
                             ;;     of renderer identity.
                             (list (out:artifact-path/raw artifact)
                                   sources
                                   (and (null? sources)
                                        (content-fingerprint
                                          (out:artifact-content/raw artifact))))))
                         (html-style-artifacts style)))))
          #:dependencies (lambda ()
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

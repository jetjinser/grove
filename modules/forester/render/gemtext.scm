;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render gemtext)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove render)
  #:use-module (grove ir)
  #:use-module (grove private snapshot)
  #:use-module (grove private string-fun)
  #:use-module (grove private render text)
  #:use-module (grove private uri)
  #:use-module (grove render rules)
  #:use-module (grove render context)
  #:use-module (forester render text routes)
  #:use-module (forester private backmatter)
  #:use-module (forester private render labels)
  #:use-module (forester private render cache)
  #:use-module (forester private render text-options)
  #:use-module (forester private render text-output)
  #:use-module (forester private entity-iri)
  #:use-module (forester private web)
  #:use-module (grove i18n)
  #:use-module (forester site)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (site->gemtext
            render-gemtext
            make-gemtext-routes
            make-gemtext-options
            gemtext-options?
            make-gemtext-renderer))

(define* (make-gemtext-routes #:key
                              (page-path (lambda (id)
                                           (string-append id ".gmi")))
                              (index-path "index.gmi"))
  "Construct Gemtext routes with `.gmi' defaults.

PAGE-PATH maps a page ID to its artifact path. INDEX-PATH names the aggregate
index artifact. Either can be overridden while keeping the other Gemtext
default. Paths are validated by MAKE-TEXT-ROUTES."
  (make-text-routes
    #:page-path page-path
    #:index-path index-path))

(define (line-text text)
  (string-join (string-tokenize text) " "))

(define (marked-line kind text)
  (string-append (string #\x1e) (string kind) text))

(define (link-line url label)
  (string-append
    (marked-line #\L
      (string-append "=> " url
        (if (string-null? label) "" (string-append " " label))))
    "\n"))

(define (describes-gemtext page context)
  (let ((iris (or (assq-ref (site-page-metadata page) 'describes) '())))
    (if (null? iris)
        ""
        (string-append
          "\n" (title-line (interface-message 'describes context))
          (string-concatenate
            (map (lambda (iri)
                   (let ((href (entity-iri-href iri)))
                     (if href (link-line (web-iri-uri href) iri)
                         (string-append iri "\n"))))
                 iris))))))

(define (uri-scheme? value)
  (let ((colon (string-index value #\:)))
    (and colon (> colon 0)
         (ascii-alpha? (string-ref value 0))
         (every (lambda (character)
                  (or (ascii-alpha? character)
                      (and (char>=? character #\0) (char<=? character #\9))
                      (memv character '(#\+ #\. #\-))))
                (string->list (substring value 1 colon))))))

(define (route-uri path)
  (uri-encode-path path))

(define (page-url routes from-id to-id)
  (route-uri
    (text-relative-path
      (text-page-path routes from-id)
      (text-page-path routes to-id))))

(define (asset-url routes page source)
  (if (uri-scheme? source)
      (web-iri-uri source)
      (call-with-values
        (lambda () (split-uri-suffix source))
        (lambda (path suffix)
          (let ((relative
                  (if (string-prefix? "/" path)
                      path
                      (text-relative-path
                        (text-page-path routes (site-page-id page)) path))))
            (string-append (route-uri relative) suffix))))))

(define (page-content-handlers forest routes page)
  (let ((references (outgoing-references forest (site-page-id page)))
        (pending-links '()))
    (define (queue-link url label)
      (set! pending-links (cons (link-line url label) pending-links)))
    (define (flush-links body)
      (let ((links (reverse pending-links)))
        (set! pending-links '())
        (string-append body
          (if (null? links) "" (string-append "\n" (string-concatenate links))))))
    `((tag . ,(lambda (_ body) ""))
      (taxon . ,(lambda (_ body) ""))
      (item . ,(lambda (_ body)
                 (flush-links
                   (string-append "\n" (marked-line #\M
                     (string-append "* " (line-text body))) "\n"))))
      (paragraph . ,(lambda (_ body)
                      (string-append (flush-links body) "\n")))
      (blockquote . ,(lambda (_ body)
                       (string-append
                         (string-join
                           (map (lambda (line)
                                  (marked-line #\Q (string-append "> " line)))
                                (string-split (string-trim-right body #\newline)
                                              #\newline))
                           "\n")
                         "\n")))
      (link . ,(lambda (element body)
                 (when (null? references)
                   (error "Gemtext link rendering exceeded its index"
                          (site-page-id page)))
                 (let* ((reference (car references))
                        (target (require-page forest
                                           (reference-target-page-id reference)))
                        (url (page-url routes (site-page-id page)
                                       (reference-target-page-id reference)))
                        (fragment (ir:content-element-property/raw element 'fragment))
                        (url (if fragment (string-append url "#" fragment) url))
                        (label (line-text body))
                        (label (if (string-null? label)
                                   (site-page-title target) label)))
                   (set! references (cdr references))
                   (queue-link url label)
                   label)))
      (query-link . ,(lambda (element body)
                       (let* ((target-id (ir:content-element-property/raw element 'target))
                              (fragment (ir:content-element-property/raw element 'fragment))
                              (target (require-page forest target-id))
                              (url (page-url routes (site-page-id page) target-id))
                              (url (if fragment (string-append url "#" fragment) url))
                              (label (line-text body)))
                         (set! label (if (string-null? label)
                                         (site-page-title target) label))
                         (queue-link url label)
                         label)))
      (image . ,(lambda (element body)
                  (let* ((source (ir:content-element-property/raw element 'source))
                         (url (asset-url routes page source))
                         (label (line-text body))
                         (label (if (string-null? label) "[image]" label)))
                    (queue-link url label)
                    label)))
      (code . ,(lambda (_ body) body))
      (code-block . ,(lambda (element body)
                       (when (any (lambda (line) (string-prefix? "```" line))
                                  (string-split body #\newline))
                         (error "Gemtext code block in page ~a contains a fence line"
                                (site-page-id page)))
                       (string-append "\n" (marked-line #\F "```")
                                      (let ((language
                                              (ir:content-element-property/raw
                                                element 'language)))
                                        (if language
                                            (string-append " " (symbol->string language))
                                            ""))
                                      "\n" body
                                      (if (string-suffix? "\n" body) "" "\n")
                                      (marked-line #\F "```") "\n"))))))

(define (page-body forest routes page rules context)
  (content->text/internal (site-page-document page)
    #:internal-handlers (page-content-handlers forest routes page)
    #:rules rules
    #:context (render-context-with context
               #:forest forest
               #:routes routes
               #:page page)
    #:internal-lazy-handlers
    (map (lambda (kind)
           (cons kind
             (lambda (element _render)
                 (let* ((id (tree-occurrence-target element))
                      (target (require-page forest id))
                      (url (page-url routes (site-page-id page) id)))
                 (string-append "\n" (link-line url (site-page-title target)))))))
         '(subtree transclude query-transclude))))

(define (page-backmatter forest page routes sections labels reference-taxon-id)
  (define (section heading targets)
    (if (null? targets)
        ""
        (string-append
          (marked-line #\H (string-append "## " heading)) "\n"
          (string-concatenate
            (map (lambda (target)
                   (link-line
                     (page-url routes (site-page-id page) (site-page-id target))
                     (site-page-title target)))
                 targets))
          "\n")))
  (string-concatenate
    (map (lambda (name)
           (let ((heading (assq-ref labels name)))
             (case name
               ((context)
                (section heading
                         (context-pages forest (site-page-id page))))
               ((references)
                (section heading
                         (reference-pages forest (site-page-id page)
                                          reference-taxon-id)))
               ((backlinks)
                (section heading
                         (backlinks forest (site-page-id page))))
               ((related)
                (section heading
                         (related-pages forest (site-page-id page)
                                        reference-taxon-id))))))
         sections)))

(define (title-line title)
  (string-append (marked-line #\H
                 (string-append "# " (line-text title))) "\n\n"))

(define (gemtext-syntax-line? line)
  (or (string-prefix? "=>" line)
      (string-prefix? "#" line)
      (string-prefix? "* " line)
      (string-prefix? ">" line)
      (string-prefix? "```" line)))

(define (finish-gemtext text)
  (let loop ((lines (string-split text #\newline))
             (preformatted? #f)
             (result '()))
    (if (null? lines)
        (string-join (reverse result) "\n")
        (let* ((line (car lines))
               (marked? (and (>= (string-length line) 2)
                             (char=? (string-ref line 0) #\x1e)))
               (kind (and marked? (string-ref line 1)))
               (body (if marked? (substring line 2) line))
               (fence? (and marked? (char=? kind #\F)))
               (next-preformatted? (if fence? (not preformatted?)
                                       preformatted?))
               (finished
                 (cond
                   ((and marked? (memv kind '(#\H #\L #\M #\Q #\F))) body)
                   (preformatted? body)
                   ((gemtext-syntax-line? body) (string-append " " body))
                   (else body))))
          (loop (cdr lines) next-preformatted? (cons finished result))))))

(define (index->gemtext forest routes)
  (finish-gemtext
   (string-append
    (title-line (site-title (site forest)))
    (string-concatenate
      (map (lambda (page)
             (string-append (marked-line #\L "=> ")
               (route-uri (text-relative-path (text-index-path routes)
                 (text-page-path routes (site-page-id page))))
               " " (line-text (site-page-title page)) "\n"))
           (pages forest))))))

(define (page->gemtext forest routes page backmatter backmatter-labels
                       reference-taxon-id rules context)
  (let ((body (page-body forest routes page rules context)))
    (finish-gemtext
      (string-append
        (title-line (site-page-title page))
        (link-line (route-uri
                     (text-relative-path
                       (text-page-path routes (site-page-id page))
                       (text-index-path routes)))
                   (interface-message 'home context))
        "\n" body
        (describes-gemtext page context)
        (page-backmatter forest page routes backmatter backmatter-labels
                         reference-taxon-id)
        (if (string-suffix? "\n" body) "" "\n")))))

(define-record-type <gemtext-options>
  (%make-gemtext-options prefix routes backmatter
                         reference-taxon-id locale messages rules)
  gemtext-options?
  (prefix gemtext-options-prefix)
  (routes gemtext-options-routes)
  (backmatter gemtext-options-backmatter)
  (reference-taxon-id gemtext-options-reference-taxon-id)
  (locale gemtext-options-locale)
  (messages gemtext-options-messages)
  (rules gemtext-options-rules))

(define (require-gemtext-options options who)
  (unless (gemtext-options? options)
    (error (string-append who " requires Gemtext options") options))
  options)

(define (make-gemtext-page-renderer forest options)
  (let ((routes (gemtext-options-routes options))
        (backmatter (gemtext-options-backmatter options))
        (reference-taxon-id (gemtext-options-reference-taxon-id options)))
    (make-forester-text-page-renderer
      "Gemtext" forest routes
      (gemtext-options-locale options)
      (gemtext-options-messages options)
      (gemtext-options-rules options)
      (lambda (page context backmatter-labels rules)
        (page->gemtext forest routes page backmatter backmatter-labels
                       reference-taxon-id rules context)))))

(define (render-gemtext-changed forest options changed old-artifacts)
  (let* ((prefix (gemtext-options-prefix options))
         (routes (gemtext-options-routes options))
         (render-page (make-gemtext-page-renderer forest options)))
    (render-text-output-changed
      forest prefix routes changed old-artifacts
      (lambda () (index->gemtext forest routes))
      render-page)))

(define* (make-gemtext-options #:key
                               (prefix #f)
                               (routes (make-gemtext-routes))
                               (backmatter default-backmatter-sections)
                               (reference-taxon-id default-reference-taxon-id)
                               (locale #f)
                               (messages default-message-catalog)
                               (rules empty-render-rules))
  "Build a validated Gemtext-renderer configuration snapshot."
  (call-with-values
    (lambda ()
      (normalize-text-renderer-options
        "Gemtext" prefix routes backmatter reference-taxon-id locale messages
        rules))
    %make-gemtext-options))

(define* (site->gemtext forest #:key (options (make-gemtext-options)))
  "Return Gemtext pages as an ordered alist of page IDs and strings.

The aggregate index is stored under the symbol INDEX. OPTIONS configures
routes and Gemtext rendering. Use RENDER-GEMTEXT for artifacts or
MAKE-GEMTEXT-RENDERER in a project."
  (let* ((_ (require-gemtext-options options "site->gemtext"))
         (routes (gemtext-options-routes options))
         (render-page (make-gemtext-page-renderer forest options)))
    (site->text-output forest routes
      (lambda () (index->gemtext forest routes))
      render-page)))

(define* (render-gemtext forest #:key (options (make-gemtext-options)))
  "Render FOREST to its complete Gemtext artifact list, including the index.

OPTIONS configures routes and Gemtext rendering. Use MAKE-GEMTEXT-RENDERER to
add the output to a project lifecycle."
  (let* ((_ (require-gemtext-options options "render-gemtext"))
         (prefix (gemtext-options-prefix options))
         (routes (gemtext-options-routes options))
         (render-page (make-gemtext-page-renderer forest options)))
    (render-text-output forest prefix routes
      (lambda () (index->gemtext forest routes))
      render-page)))

(define* (make-gemtext-renderer #:key (options (make-gemtext-options))
                               (rules-id #f)
                               (cache? #t))
  "Construct a project renderer that produces Gemtext artifacts for a Forest.

OPTIONS supplies Gemtext behavior. CACHE? defaults to enabled; custom rendering
RULES need a stable RULES-ID for safe caching. The result is a plain procedure
when caching cannot be identified safely, otherwise a cached renderer."
  (let* ((_ (require-gemtext-options options "make-gemtext-renderer"))
         (prefix (gemtext-options-prefix options))
         (routes (gemtext-options-routes options))
         (backmatter (gemtext-options-backmatter options))
         (reference-taxon-id (gemtext-options-reference-taxon-id options))
         (locale (gemtext-options-locale options))
         (messages (gemtext-options-messages options))
         (rules (gemtext-options-rules options))
         (effective-cache? (renderer-cacheable? rules rules-id cache?)))
  (validate-rules-id rules-id)
  (set! rules-id (snapshot-data rules-id))
  (unless (boolean? cache?)
    (error "Forester Gemtext renderer cache option must be a boolean" cache?))
  (let ((render (lambda (forest)
                  (render-gemtext forest #:options options))))
    (if (not effective-cache?)
        render
        (make-cached-renderer render
          #:configuration
          (lambda (forest)
            (text-renderer-cache-identity
              'gemtext forest prefix routes backmatter reference-taxon-id
              rules-id locale messages))
          #:render-changed
          (lambda (forest changed _dirty-paths old-artifacts)
            (render-gemtext-changed forest options changed old-artifacts)))))))

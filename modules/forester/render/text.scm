;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render text)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove render)
  #:use-module (forester private render anchors)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove private snapshot)
  #:use-module (grove private render text)
  #:use-module (grove render rules)
  #:use-module (grove render context)
  #:use-module (forester render text routes)
  #:use-module (forester private backmatter)
  #:use-module (forester private render labels)
  #:use-module (forester private render text-options)
  #:use-module (forester private render text-output)
  #:use-module (forester private entity-uri)
  #:use-module (grove i18n)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (site->text
            render-text
            make-text-options
            text-options?
            make-text-renderer))

;; Forester supplies the tree-aware lazy handlers; ordinary extensions use
;; render rules through the public (grove render text) entry point.
(define (page-content-handlers forest routes page)
  (let ((references (outgoing-references forest (site-page-id page))))
    `((tag . ,(lambda (_element _body) ""))
      (taxon . ,(lambda (_element _body) ""))
      (image . ,(lambda (element body)
                 (if (string-null? (string-trim-both body))
                     (string-append "[image: "
                                    (ir:content-element-property/raw element 'source)
                                    "]")
                     body)))
      (link . ,(lambda (element body)
                 (when (null? references)
                   (error "Forester link rendering exceeded its index" (site-page-id page)))
                 (let* ((reference (car references))
                        (path (text-relative-path
                                (text-page-path routes (site-page-id page))
                                (text-page-path routes (reference-target-page-id reference))))
                        (fragment (ir:content-element-property/raw element 'fragment))
                        (path (if fragment (string-append path "#" fragment) path)))
                   (set! references (cdr references))
                   (string-append body " [" path "] [#" (reference-source-id reference) "]"))))
      (query-link . ,(lambda (element body)
                      (let ((fragment (ir:content-element-property/raw element 'fragment)))
                        (string-append body " ["
                          (text-relative-path (text-page-path routes (site-page-id page))
                            (text-page-path routes (ir:content-element-property/raw element 'target)))
                          (if fragment (string-append "#" fragment) "")
                          "]")))))))

(define* (subtree-content-text forest routes page #:optional (prefix "") (context #f)
                               #:key (rules empty-render-rules))
  (let ((position 0))
    (define (render-tree element _render)
      (set! position (1+ position))
      (let* ((id (tree-occurrence-target element))
             (target (require-page forest id))
             (anchor (occurrence-anchor element prefix position))
             (child-prefix (string-append anchor "--"))
             (target-context
               (and context
                    (render-context-with context
                      #:forest forest
                      #:routes routes
                      #:page target
                      #:anchor-prefix child-prefix))))
        (string-append (title-text (site-page-title target)) "[#" anchor "]\n"
                       (subtree-content-text forest routes target child-prefix
                                             target-context
                                             #:rules rules))))
    (content->text/internal (site-page-document page)
      #:internal-handlers (page-content-handlers forest routes page)
      #:rules rules
      #:context context
      #:internal-lazy-handlers (map (lambda (kind) (cons kind render-tree))
                          '(subtree transclude query-transclude)))))

(define (title-text title)
  (string-append title "\n" (make-string (string-length title) #\=) "\n"))

(define (describes-text page context)
  (let ((uris (or (assq-ref (site-page-metadata page) 'describes) '())))
    (if (null? uris)
        ""
        (string-append
          "\n" (interface-message 'describes context) ":\n"
          (string-concatenate
            (map (lambda (uri)
                   (string-append "- " (or (entity-uri-href uri) uri) "\n"))
                 uris))))))

(define (page-reference routes from-path page)
  (string-append (site-page-title page)
                 " (" (text-relative-path
                        from-path
                        (text-page-path routes (site-page-id page))) ")"))

(define* (table-of-contents-text forest page #:optional context)
  (define (entries id prefix indent)
    (let ((occurrences (tree-occurrences forest id)))
      (string-concatenate
        (map (lambda (element position)
               (let* ((target-id (tree-occurrence-target element))
                      (target (require-page forest target-id))
                      (anchor (occurrence-anchor element prefix position)))
                 (string-append indent "- " (site-page-title target) " [#" anchor "]\n"
                   (entries target-id (string-append anchor "--")
                            (string-append indent "  ")))))
             occurrences (iota (length occurrences) 1)))))
  (let ((items (entries (site-page-id page) "" "")))
    (if (string-null? items)
        ""
        (string-append (title-text (interface-message 'nav-contents context))
                       "\n" items "\n"))))

(define (text-backlink-reference forest page-id routes reference indent)
  ;;     Title plus the anchored path; the citing context's alt text
  ;;     stays in the body where it was written.
  (let* ((source (find-page forest (reference-source-page-id reference)))
         (path (text-relative-path (text-page-path routes page-id)
                                   (text-page-path routes
                                                   (reference-source-page-id
                                                    reference)))))
    (string-append indent "- " (site-page-title source) " [" path "#"
                   (reference-source-id reference) "]\n")))



(define (backlinks-text forest page routes label)
  (let* ((page-id (site-page-id page))
         (references (incoming-references forest page-id)))
    (if (null? references) ""
        (string-append "\n\n" label ":\n"
          (string-concatenate
            (map (lambda (reference)
                   (text-backlink-reference forest page-id routes reference ""))
                 references))))))

(define (context-text forest page routes label)
  (let ((sources (context-pages forest (site-page-id page))))
    (if (null? sources)
        ""
        (string-append
          (string-append "\n\n" label ":\n")
          (string-concatenate
            (map (lambda (source)
                   (string-append "- "
                     (page-reference routes
                       (text-page-path routes (site-page-id page))
                       source)
                     "\n"))
                 sources))))))

(define (page-list-backmatter-text page routes label targets)
  (if (null? targets)
      ""
      (string-append
        (string-append "\n\n" label ":\n")
        (string-concatenate
          (map (lambda (target)
                 (string-append "- "
                   (page-reference routes
                     (text-page-path routes (site-page-id page))
                     target)
                   "\n"))
               targets)))))

(define (related-text forest page routes reference-taxon-id label)
  (page-list-backmatter-text
    page routes label
    (related-pages forest (site-page-id page) reference-taxon-id)))

(define (references-text forest page routes reference-taxon-id label)
  (page-list-backmatter-text
    page routes label
    (reference-pages forest (site-page-id page) reference-taxon-id)))

(define (backmatter-text forest page routes sections labels
                         reference-taxon-id)
  (string-concatenate
    (map (lambda (section)
           (case section
             ((context) (context-text forest page routes
                                      (assq-ref labels 'context)))
             ((references) (references-text forest page routes
                                            reference-taxon-id
                                            (assq-ref labels 'references)))
             ((backlinks) (backlinks-text forest page routes
                                          (assq-ref labels 'backlinks)))
             ((related) (related-text forest page routes reference-taxon-id
                                      (assq-ref labels 'related)))))
         sections)))

(define (index->text forest routes)
  (let* ((site (site forest))
         (index-path (text-index-path routes)))
    (string-append
      (title-text (site-title site))
      "\n"
      (string-concatenate
        (map (lambda (page)
               (string-append "- "
                              (page-reference routes index-path page)
                              "\n"))
             (site-pages site))))))

(define* (page->text forest page routes backmatter backmatter-labels
                     reference-taxon-id #:optional (context #f)
                     #:key (rules empty-render-rules))
  (let* ((page-context
           (and context
                (render-context-with context
                  #:forest forest
                  #:routes routes
                  #:page page
                  #:anchor-prefix "")))
         (page-path (text-page-path routes (site-page-id page)))
         (body (string-trim-both
                 (subtree-content-text forest routes page "" page-context
                                       #:rules rules)
                 (char-set #\newline))))
    (string-append
      (title-text (site-page-title page))
      "\n" (interface-message 'home context) ": "
      (text-relative-path page-path (text-index-path routes))
      "\n\n"
      (table-of-contents-text forest page context)
      body
      (describes-text page context)
      (backmatter-text forest page routes backmatter backmatter-labels
                       reference-taxon-id))))

(define-record-type <text-options>
  (%make-text-options prefix routes backmatter
                      reference-taxon-id locale messages rules)
  text-options?
  (prefix text-options-prefix)
  (routes text-options-routes)
  (backmatter text-options-backmatter)
  (reference-taxon-id text-options-reference-taxon-id)
  (locale text-options-locale)
  (messages text-options-messages)
  (rules text-options-rules))

(define (require-text-options options who)
  (unless (text-options? options)
    (error (string-append who " requires text options") options))
  options)

(define (make-text-page-renderer forest options)
  (let ((routes (text-options-routes options))
        (backmatter (text-options-backmatter options))
        (reference-taxon-id (text-options-reference-taxon-id options)))
    (make-forester-text-page-renderer
      "text" forest routes
      (text-options-locale options)
      (text-options-messages options)
      (text-options-rules options)
      (lambda (page context backmatter-labels rules)
        (page->text forest page routes backmatter backmatter-labels
                    reference-taxon-id context #:rules rules)))))

(define (render-text-changed forest options changed old-artifacts)
  (let* ((prefix (text-options-prefix options))
         (routes (text-options-routes options))
         (render-page (make-text-page-renderer forest options)))
    (render-text-output-changed
      forest prefix routes changed old-artifacts
      (lambda () (index->text forest routes))
      render-page)))

(define* (make-text-options #:key
                            (prefix #f)
                            (routes (make-text-routes))
                            (backmatter default-backmatter-sections)
                            (reference-taxon-id default-reference-taxon-id)
                            (locale #f)
                            (messages default-message-catalog)
                            (rules empty-render-rules))
  "Build a validated text-renderer configuration snapshot."
  (call-with-values
    (lambda ()
      (normalize-text-renderer-options
        "text" prefix routes backmatter reference-taxon-id locale messages
        rules))
    %make-text-options))

(define* (site->text forest #:key (options (make-text-options)))
  (let* ((_ (require-text-options options "site->text"))
         (routes (text-options-routes options))
         (render-page (make-text-page-renderer forest options)))
    (site->text-output forest routes
      (lambda () (index->text forest routes))
      render-page)))

(define* (render-text forest #:key (options (make-text-options)))
  (let* ((_ (require-text-options options "render-text"))
         (prefix (text-options-prefix options))
         (routes (text-options-routes options))
         (render-page (make-text-page-renderer forest options)))
    (render-text-output forest prefix routes
      (lambda () (index->text forest routes))
      render-page)))

(define* (make-text-renderer #:key (options (make-text-options))
                            (rules-id #f)
                            (cache? #t))
  (let* ((_ (require-text-options options "make-text-renderer"))
         (prefix (text-options-prefix options))
         (routes (text-options-routes options))
         (backmatter (text-options-backmatter options))
         (reference-taxon-id (text-options-reference-taxon-id options))
         (locale (text-options-locale options))
         (messages (text-options-messages options))
         (rules (text-options-rules options)))
  (validate-rules-id rules-id)
  (set! rules-id (snapshot-data rules-id))
  (unless (boolean? cache?)
    (error "Forester text renderer cache option must be a boolean" cache?))
  (let ((render (lambda (forest)
                  (render-text forest #:options options)))
        (effective-cache?
          (and cache?
               (or (eq? rules empty-render-rules)
                   (and rules-id #t)))))
    (if (not effective-cache?)
        render
        (make-cached-renderer render
          #:configuration
          (lambda (forest)
            (text-renderer-cache-identity
              'text forest prefix routes backmatter reference-taxon-id
              rules-id locale messages))
          #:render-changed
          (lambda (forest changed dirty-paths old-artifacts)
            (render-text-changed forest options changed old-artifacts)))))))

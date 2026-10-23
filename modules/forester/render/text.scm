;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render text)
  #:use-module (grove build)
  #:use-module (forester render artifact)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render text)
  #:use-module (grove render rules)
  #:use-module (grove render context)
  #:use-module (forester render text routes)
  #:use-module (forester backmatter)
  #:use-module (forester render labels)
  #:use-module (forester render context)
  #:use-module (grove i18n)
  #:use-module (forester i18n)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:export (site->text
            render-text
            make-text-options
            text-options?
            make-text-renderer))

(define (page-content-handlers forest routes page)
  (let ((references (outgoing-references forest (site-page-id page))))
    (append
     bilingual-text-handlers
     `((tag . ,(lambda (_element _body) ""))
      (taxon . ,(lambda (_element _body) ""))
      (image . ,(lambda (element body)
                 (if (string-null? (string-trim-both body))
                     (string-append "[image: "
                                    (content-element-property element 'source)
                                    "]")
                     body)))
      (link . ,(lambda (element body)
                 (when (null? references)
                   (error "Forester link rendering exceeded its index" (site-page-id page)))
                 (let* ((reference (car references))
                        (path (text-relative-path
                                (text-page-path routes (site-page-id page))
                                (text-page-path routes (reference-target-page-id reference))))
                        (fragment (content-element-property element 'fragment))
                        (path (if fragment (string-append path "#" fragment) path)))
                   (set! references (cdr references))
                   (string-append body " [" path "] [#" (reference-source-id reference) "]"))))
      (query-link . ,(lambda (element body)
                      (let ((fragment (content-element-property element 'fragment)))
                        (string-append body " ["
                          (text-relative-path (text-page-path routes (site-page-id page))
                            (text-page-path routes (content-element-property element 'target)))
                          (if fragment (string-append "#" fragment) "")
                          "]"))))))))

(define (occurrence-anchor element prefix position)
  "The canonical anchor of a tree occurrence: the author anchor when the
element declares one, otherwise the generated position anchor. Mirrors the
HTML renderer so text and HTML address occurrences identically."
  (or (content-element-property element 'anchor)
      (format #f "~atree-~a" prefix position)))

(define* (subtree-content-text forest routes page #:optional (prefix "") (context #f)
                               #:key (rules empty-render-rules))
  (let ((position 0))
    (define (render-tree element _render)
      (set! position (1+ position))
      (let* ((id (tree-occurrence-target element))
             (target (find-page forest id))
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
    (content->text (site-page-document page)
      #:handlers (page-content-handlers forest routes page)
      #:rules rules
      #:context context
      #:lazy-handlers (map (lambda (kind) (cons kind render-tree))
                          '(subtree transclude query-transclude)))))

(define (title-text title)
  (string-append title "\n" (make-string (string-length title) #\=) "\n"))

(define (page-reference routes from-path page)
  (string-append (site-page-title page)
                 " (" (text-relative-path
                        from-path
                        (text-page-path routes (site-page-id page))) ")"))

(define* (table-of-contents-text forest page #:optional context)
  (define (entries id prefix indent)
    (string-concatenate
      (map (lambda (element position)
             (let* ((target-id (tree-occurrence-target element))
                    (target (find-page forest target-id))
                    (anchor (occurrence-anchor element prefix position)))
               (string-append indent "- " (site-page-title target) " [#" anchor "]\n"
                 (entries target-id (string-append anchor "--") (string-append indent "  ")))))
           (tree-occurrences forest id) (iota (length (tree-occurrences forest id)) 1))))
  (let ((items (entries (site-page-id page) "" "")))
    (if (string-null? items)
        ""
        (string-append (title-text (chrome-message 'nav-contents context))
                       "\n" items "\n"))))

(define (text-backlink-reference forest page-id routes reference indent)
  ;;     Title plus the anchored path; the citing context's alt text
  ;;     stays in the body where it was written.
  (let* ((source (lookup-page forest (reference-source-page-id reference)))
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
      "\n" (chrome-message 'home context) ": "
      (text-relative-path page-path (text-index-path routes))
      "\n\n"
      (table-of-contents-text forest page context)
      body
      (backmatter-text forest page routes backmatter backmatter-labels
                       reference-taxon-id))))

(define (check-text-routes forest routes)
  (let* ((site (site forest))
         (paths (cons (text-index-path routes)
                      (map (lambda (page)
                             (text-page-path routes (site-page-id page)))
                           (pages forest)))))
    (unless (= (length paths) (length (delete-duplicates paths string=?)))
      (error "text routes produce duplicate output paths" paths))))

(define (output-path prefix name)
  (if prefix
      (string-append prefix "/" name)
      name))

(define (render-text-changed forest prefix routes backmatter backmatter-labels
                             reference-taxon-id rules context
                             changed old-artifacts)
  (set! backmatter-labels
    (normalize-backmatter-labels backmatter-labels
      #:defaults (localized-backmatter-labels context)))
  (check-text-routes forest routes)
  (let* ((site (site forest))
         (index-path (output-path prefix (text-index-path routes)))
         (index-sources (source-paths forest))
         (index-artifact
           (reuse-artifact index-path index-sources old-artifacts changed
             (lambda () (index->text forest routes))))
         (page-artifacts
           (map (lambda (page)
                  (let* ((path (output-path
                                 prefix
                                 (text-page-path routes
                                                 (site-page-id page))))
                         (sources (page-source-paths forest page)))
                    (reuse-artifact path sources old-artifacts changed
                      (lambda ()
                        (page->text forest page routes backmatter
                                    backmatter-labels
                                    reference-taxon-id
                                    context #:rules rules)))))
                (pages forest))))
    (cons index-artifact page-artifacts)))

(define-record-type <text-options>
  (%make-text-options prefix routes backmatter backmatter-labels
                      reference-taxon-id locale messages rules rules-id
                      cache?)
  text-options?
  (prefix text-options-prefix)
  (routes text-options-routes)
  (backmatter text-options-backmatter)
  (backmatter-labels text-options-backmatter-labels)
  (reference-taxon-id text-options-reference-taxon-id)
  (locale text-options-locale)
  (messages text-options-messages)
  (rules text-options-rules)
  (rules-id text-options-rules-id)
  (cache? text-options-cache?))

(define* (make-text-options #:key
                            (prefix #f)
                            (routes (make-text-routes))
                            (backmatter default-backmatter-sections)
                            (backmatter-labels '())
                            (reference-taxon-id default-reference-taxon-id)
                            (locale #f)
                            (messages default-message-catalog)
                            (rules empty-render-rules)
                            (rules-id #f)
                            (cache? #t))
  "Immutable configuration for the Forester text renderer."
  (%make-text-options prefix routes backmatter backmatter-labels
                      reference-taxon-id locale messages rules rules-id
                      cache?))

(define* (site->text forest #:key
                     (routes (make-text-routes))
                     (backmatter default-backmatter-sections)
                     (backmatter-labels '())
                     (reference-taxon-id default-reference-taxon-id)
                     (locale #f)
                     (messages default-message-catalog)
                     (rules empty-render-rules)
                     (options #f))
  (let* ((options
           (or options
               (make-text-options #:routes routes
                                  #:backmatter backmatter
                                  #:backmatter-labels backmatter-labels
                                  #:reference-taxon-id reference-taxon-id
                                  #:locale locale
                                  #:messages messages
                                  #:rules rules)))
         (routes (text-options-routes options))
         (backmatter (text-options-backmatter options))
         (backmatter-labels (text-options-backmatter-labels options))
         (reference-taxon-id (text-options-reference-taxon-id options))
         (locale (text-options-locale options))
         (messages (text-options-messages options))
         (rules (text-options-rules options)))
  (validate-backmatter-sections backmatter)
  (check-reference-taxon-id reference-taxon-id)
  (unless (forest? forest)
    (error "Forester text renderer requires a forest" forest))
  (check-text-routes forest routes)
  (let* ((context
           (make-site-render-context forest
                                     #:routes routes
                                     #:locale locale
                                     #:messages messages))
         (backmatter-labels
           (normalize-backmatter-labels backmatter-labels
             #:defaults (localized-backmatter-labels context)))
         (site (site forest)))
    (cons (cons "index" (index->text forest routes))
          (map (lambda (page)
                 (cons (site-page-id page)
                       (page->text forest page routes backmatter
                                   backmatter-labels reference-taxon-id
                                   context #:rules rules)))
               (pages forest))))))

(define* (render-text forest #:key
                      (routes (make-text-routes))
                      (backmatter default-backmatter-sections)
                      (backmatter-labels '())
                      (reference-taxon-id default-reference-taxon-id)
                      (locale #f)
                      (messages default-message-catalog)
                      (rules empty-render-rules)
                      (options #f))
  (let* ((options
           (or options
               (make-text-options #:routes routes
                                  #:backmatter backmatter
                                  #:backmatter-labels backmatter-labels
                                  #:reference-taxon-id reference-taxon-id
                                  #:locale locale
                                  #:messages messages
                                  #:rules rules)))
         (routes (text-options-routes options))
         (backmatter (text-options-backmatter options))
         (backmatter-labels (text-options-backmatter-labels options))
         (reference-taxon-id (text-options-reference-taxon-id options))
         (locale (text-options-locale options))
         (messages (text-options-messages options))
         (rules (text-options-rules options)))
  (validate-backmatter-sections backmatter)
  (check-reference-taxon-id reference-taxon-id)
  (let* ((context
           (make-site-render-context forest
                                     #:routes routes
                                     #:locale locale
                                     #:messages messages))
         (backmatter-labels
           (normalize-backmatter-labels backmatter-labels
             #:defaults (localized-backmatter-labels context)))
         (site (site forest))
         (sources (source-paths forest)))
    (check-text-routes forest routes)
    (cons
      (make-artifact (text-index-path routes)
                     (index->text forest routes)
                     sources)
      (map (lambda (page)
             (make-artifact
               (text-page-path routes (site-page-id page))
               (page->text forest page routes backmatter backmatter-labels
                           reference-taxon-id context #:rules rules)
               (page-source-paths forest page)))
           (pages forest))))))

(define* (make-text-renderer #:key (prefix #f)
                                      (routes (make-text-routes))
                                      (backmatter default-backmatter-sections)
                                      (backmatter-labels '())
                                      (reference-taxon-id default-reference-taxon-id)
                                      (locale #f)
                                      (messages default-message-catalog)
                                      (rules empty-render-rules)
                                      (rules-id #f)
                                      (cache? #t)
                                      (options #f))
  (let* ((options
           (or options
               (make-text-options #:prefix prefix
                                  #:routes routes
                                  #:backmatter backmatter
                                  #:backmatter-labels backmatter-labels
                                  #:reference-taxon-id reference-taxon-id
                                  #:locale locale
                                  #:messages messages
                                  #:rules rules
                                  #:rules-id rules-id
                                  #:cache? cache?)))
         (prefix (text-options-prefix options))
         (routes (text-options-routes options))
         (backmatter (text-options-backmatter options))
         (backmatter-labels (text-options-backmatter-labels options))
         (reference-taxon-id (text-options-reference-taxon-id options))
         (locale (text-options-locale options))
         (messages (text-options-messages options))
         (rules (text-options-rules options))
         (rules-id (text-options-rules-id options))
         (cache? (text-options-cache? options)))
  (validate-backmatter-sections backmatter)
  (check-reference-taxon-id reference-taxon-id)
  (unless (or (not prefix) (artifact-path? prefix))
    (error "Forester text output prefix must be a relative path or #f" prefix))
  (unless (text-routes? routes)
    (error "Forester text renderer requires text routes" routes))
  (let ((render (lambda (forest)
                  (let ((artifacts (render-text forest
                                                #:routes routes
                                                #:backmatter backmatter
                                                #:backmatter-labels
                                                backmatter-labels
                                                #:reference-taxon-id
                                                reference-taxon-id
                                                #:locale locale
                                                #:messages messages
                                                #:rules rules)))
                    (if prefix
                        (prefix-artifacts prefix artifacts)
                        artifacts))))
        (effective-cache?
          (and cache?
               (or (eq? rules empty-render-rules)
                   (and rules-id #t)))))
    (make-cached-renderer render #:cache? effective-cache?
          #:configuration
          (lambda (forest)
            (list 'text prefix (site-title (site forest)) (site-description (site forest))
                  (text-index-path routes)
                  backmatter backmatter-labels reference-taxon-id rules-id
                  (resolve-chrome-locale locale (resolve-chrome-catalog messages))
                  (message-catalog-default (resolve-chrome-catalog messages))
                  (message-catalog-entries (resolve-chrome-catalog messages))))
          #:render-changed
          (lambda (forest changed dirty-paths old-artifacts)
            (render-text-changed forest prefix routes backmatter
                                 backmatter-labels reference-taxon-id rules
                                 (make-site-render-context
                                   forest
                                   #:routes routes
                                   #:locale locale
                                   #:messages messages)
                                 changed old-artifacts))))))

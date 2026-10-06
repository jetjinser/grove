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
  #:use-module (forester render text routes)
  #:use-module (forester backmatter)
  #:use-module (forester i18n)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:export (site->text
            render-text
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

(define* (subtree-content-text forest routes page #:optional (prefix ""))
  (let ((position 0))
    (define (render-tree element _render)
      (set! position (1+ position))
      (let* ((id (tree-occurrence-target element))
             (target (find-page forest id))
             (anchor (occurrence-anchor element prefix position)))
        (string-append (title-text (site-page-title target)) "[#" anchor "]\n"
                       (subtree-content-text forest routes target (string-append anchor "--")))))
    (content->text (site-page-document page)
      #:handlers (page-content-handlers forest routes page)
      #:lazy-handlers (map (lambda (kind) (cons kind render-tree))
                          '(subtree transclude query-transclude)))))

(define (title-text title)
  (string-append title "\n" (make-string (string-length title) #\=) "\n"))

(define (page-reference routes from-path page)
  (string-append (site-page-title page)
                 " (" (text-relative-path
                        from-path
                        (text-page-path routes (site-page-id page))) ")"))

(define (table-of-contents-text forest page)
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
    (if (string-null? items) "" (string-append (title-text "Contents") "\n" items "\n"))))

(define (text-backlink-reference forest page-id routes reference indent)
  (let* ((source-id (reference-source-page-id reference))
         (source (find (lambda (candidate)
                         (equal? source-id (site-page-id candidate)))
                       (pages forest)))
         (path (text-relative-path (text-page-path routes page-id)
                                   (text-page-path routes source-id))))
    (string-append indent "- " (site-page-title source) " [" path "#"
                   (reference-source-id reference) "]: "
                   (reference-label reference) "\n")))



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
  (let ((sources (transcluding-pages forest (site-page-id page))))
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

(define (page->text forest page routes backmatter backmatter-labels
                    reference-taxon-id)
  (let ((page-path (text-page-path routes (site-page-id page)))
        (body (string-trim-both
                (subtree-content-text forest routes page)
                (char-set #\newline))))
    (string-append
      (title-text (site-page-title page))
      "\nHome: "
      (text-relative-path page-path (text-index-path routes))
      "\n\n"
      (table-of-contents-text forest page)
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
                             reference-taxon-id
                             changed old-artifacts)
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
                                    reference-taxon-id)))))
                (pages forest))))
    (cons index-artifact page-artifacts)))

(define* (site->text forest #:key
                     (routes (make-text-routes))
                     (backmatter default-backmatter-sections)
                     (backmatter-labels '())
                     (reference-taxon-id default-reference-taxon-id))
  (validate-backmatter-sections backmatter)
  (set! backmatter-labels (normalize-backmatter-labels backmatter-labels))
  (check-reference-taxon-id reference-taxon-id)
  (unless (forest? forest)
    (error "Forester text renderer requires a forest" forest))
  (check-text-routes forest routes)
  (let ((site (site forest)))
    (cons (cons "index" (index->text forest routes))
          (map (lambda (page)
                 (cons (site-page-id page)
                       (page->text forest page routes backmatter
                                   backmatter-labels reference-taxon-id)))
               (pages forest)))))

(define* (render-text forest #:key
                      (routes (make-text-routes))
                      (backmatter default-backmatter-sections)
                      (backmatter-labels '())
                      (reference-taxon-id default-reference-taxon-id))
  (validate-backmatter-sections backmatter)
  (set! backmatter-labels (normalize-backmatter-labels backmatter-labels))
  (check-reference-taxon-id reference-taxon-id)
  (let* ((site (site forest))
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
                           reference-taxon-id)
               (page-source-paths forest page)))
           (pages forest)))))

(define* (make-text-renderer #:key (prefix #f)
                                      (routes (make-text-routes))
                                      (backmatter default-backmatter-sections)
                                      (backmatter-labels '())
                                      (reference-taxon-id default-reference-taxon-id)
                                      (cache? #t))
  (validate-backmatter-sections backmatter)
  (set! backmatter-labels (normalize-backmatter-labels backmatter-labels))
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
                                                reference-taxon-id)))
                    (if prefix
                        (prefix-artifacts prefix artifacts)
                        artifacts)))))
    (make-cached-renderer render #:cache? cache?
          #:configuration
          (lambda (forest)
            (list 'text prefix (site-title (site forest)) (site-description (site forest))
                  (text-index-path routes)
                  backmatter backmatter-labels reference-taxon-id))
          #:render-changed
          (lambda (forest changed dirty-paths old-artifacts)
            (render-text-changed forest prefix routes backmatter
                                 backmatter-labels reference-taxon-id
                                 changed old-artifacts)))))

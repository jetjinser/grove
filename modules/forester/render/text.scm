;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render text)
  #:use-module (grove build)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render text)
  #:use-module (forester render text routes)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:export (site->text
            render-text
            make-text-renderer))

(define (page-content-handlers forest routes page)
  (let ((references (outgoing-references forest (site-page-id page)))
        (headings (headings forest (site-page-id page))))
    `((image . ,(lambda (element body)
                 (if (string-null? (string-trim-both body))
                     (string-append "[image: "
                                    (content-element-property element 'source)
                                    "]")
                     body)))
      (link . ,(lambda (_ body)
                 (when (null? references)
                   (error "Forester link rendering exceeded its index"
                          (site-page-id page)))
                 (let* ((reference (car references))
                        (path
                          (text-relative-path
                            (text-page-path routes (site-page-id page))
                            (text-page-path
                              routes
                              (reference-target-page-id reference))))
                        (fragment
                          (reference-target-fragment reference)))
                   (set! references (cdr references))
                   (string-append body " [" path
                                  (if fragment (string-append "#" fragment) "")
                                  "] [#"
                                  (reference-source-id reference) "]"))))
      (heading . ,(lambda (_ body)
                    (when (null? headings)
                      (error "Forester heading rendering exceeded its index"
                             (site-page-id page)))
                    (let* ((heading-record (car headings))
                           (id (heading-id heading-record)))
                      (set! headings (cdr headings))
                      (string-append body "\n"
                                     (make-string (string-length body) #\=)
                                     "\n[#" id "]\n")))))))

(define (subtree-content-text forest routes page)
  (content->text
    (site-page-document page)
    #:handlers (page-content-handlers forest routes page)
    #:lazy-handlers
    (list (cons 'subtree (lambda (element _render)
                    (let* ((id (content-element-property element 'tree-id))
                           (target (find-page forest id)))
                      (string-append
                        (heading (site-page-title target))
                        (subtree-content-text forest routes target)))))
          (cons 'transclude (lambda (element _render)
                       (let* ((id (content-element-property element 'target))
                              (target (find-page forest id)))
                         (string-append
                           (heading (site-page-title target))
                           (subtree-content-text forest routes target))))))))

(define (heading title)
  (string-append title "\n" (make-string (string-length title) #\=) "\n"))

(define (page-reference routes from-path page)
  (string-append (site-page-title page)
                 " (" (text-relative-path
                        from-path
                        (text-page-path routes (site-page-id page))) ")"))

(define (table-of-contents-text forest page)
  (let ((headings (headings forest (site-page-id page))))
    (if (null? headings)
        ""
        (string-append
          (heading "Contents") "\n"
          (string-concatenate
            (map (lambda (heading-record)
                   (string-append "- "
                                  (heading-label heading-record)
                                  " [#" (heading-id heading-record) "]\n"))
                 headings))
          "\n"))))

(define (text-backlink-reference forest page-id routes reference)
  (let* ((source-id (reference-source-page-id reference))
         (source (find (lambda (candidate)
                         (equal? source-id (site-page-id candidate)))
                       (pages forest)))
         (path (text-relative-path (text-page-path routes page-id)
                                   (text-page-path routes source-id))))
    (string-append "  - " (site-page-title source) " [" path "#"
                   (reference-source-id reference) "]: "
                   (reference-label reference) "\n")))

(define (text-backlink-group forest page routes group)
  (let ((heading-record (car group))
        (references (cdr group)))
    (string-append
      (if heading-record
          (string-append "- " (heading-label heading-record)
                         " [#" (heading-id heading-record) "]:\n")
          "- This page:\n")
      (string-concatenate
        (map (lambda (reference)
               (text-backlink-reference forest (site-page-id page) routes
                                        reference))
             references)))))

(define (linked-from-text forest page routes)
  (let* ((page-id (site-page-id page))
         (incoming (incoming-references forest page-id))
         (page-references
           (filter (lambda (reference)
                     (not (reference-target-fragment reference)))
                   incoming))
         (heading-groups
           (filter-map
             (lambda (heading-record)
               (let* ((fragment (heading-id heading-record))
                      (references
                        (incoming-references forest page-id fragment)))
                 (and (pair? references) (cons heading-record references))))
             (headings forest page-id)))
         (groups (append (if (null? page-references)
                             '()
                             (list (cons #f page-references)))
                         heading-groups)))
    (if (null? groups)
        "\n"
        (string-append
          "\n\nLinked from:\n"
          (string-concatenate
            (map (lambda (group)
                   (text-backlink-group forest page routes group))
                 groups))))))

(define (index->text forest routes)
  (let* ((site (site forest))
         (index-path (text-index-path routes)))
    (string-append
      (heading (site-title site))
      "\n"
      (string-concatenate
        (map (lambda (page)
               (string-append "- "
                              (page-reference routes index-path page)
                              "\n"))
             (site-pages site))))))

(define (page->text forest page routes)
  (let ((page-path (text-page-path routes (site-page-id page)))
        (body (string-trim-both
                (subtree-content-text forest routes page)
                (char-set #\newline))))
    (string-append
      (heading (site-page-title page))
      "\nHome: "
      (text-relative-path page-path (text-index-path routes))
      "\n\n"
      (table-of-contents-text forest page)
      body
      (linked-from-text forest page routes))))

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

(define (render-text-changed forest prefix routes changed old-artifacts)
  (check-text-routes forest routes)
  (let* ((site (site forest))
         (index-path (output-path prefix (text-index-path routes)))
         (index-sources (source-paths forest))
         (old-index (find-artifact old-artifacts index-path))
         (index-current (make-artifact index-path "" index-sources))
         (index-artifact
           (make-artifact index-path
                          (if (artifact-current? index-current old-index changed)
                              (artifact-content old-index)
                              (index->text forest routes))
                          index-sources))
         (page-artifacts
           (map (lambda (page)
                  (let* ((path (output-path
                                 prefix
                                 (text-page-path routes
                                                 (site-page-id page))))
                         (sources (page-source-paths forest page))
                         (old (find-artifact old-artifacts path))
                         (current (make-artifact path "" sources)))
                    (if (artifact-current? current old changed)
                        old
                        (make-artifact
                          path (page->text forest page routes) sources))))
                (pages forest))))
    (cons index-artifact page-artifacts)))

(define* (site->text forest #:key (routes (make-text-routes)))
  (unless (forest? forest)
    (error "Forester text renderer requires a forest" forest))
  (check-text-routes forest routes)
  (let ((site (site forest)))
    (cons (cons "index" (index->text forest routes))
          (map (lambda (page)
                 (cons (site-page-id page)
                       (page->text forest page routes)))
               (pages forest)))))

(define* (render-text forest #:key (routes (make-text-routes)))
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
               (page->text forest page routes)
               (page-source-paths forest page)))
           (pages forest)))))

(define* (make-text-renderer #:key (prefix #f)
                                      (routes (make-text-routes))
                                      (cache-key #f))
  (unless (or (not prefix) (artifact-path? prefix))
    (error "Forester text output prefix must be a relative path or #f" prefix))
  (unless (text-routes? routes)
    (error "Forester text renderer requires text routes" routes))
  (let ((render (lambda (forest)
                  (let ((artifacts (render-text forest
                                                       #:routes routes)))
                    (if prefix
                        (prefix-artifacts prefix artifacts)
                        artifacts)))))
    (if cache-key
        (make-cached-renderer
          render
          #:cache-key (string-append "forester-text-v2:" cache-key)
          #:render-changed
          (lambda (forest changed dirty-paths old-artifacts)
            (render-text-changed forest prefix routes changed
                                        old-artifacts)))
        render)))

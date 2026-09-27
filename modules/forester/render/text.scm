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
  #:export (forest->text
            render-forest-text
            make-forester-text-renderer))

(define (link-handlers forest routes page)
  (let ((references
          (forest-outgoing-references forest (site-page-id page))))
    `((link . ,(lambda (_ body)
                 (when (null? references)
                   (error "Forester link rendering exceeded its index"
                          (site-page-id page)))
                 (let* ((reference (car references))
                        (_ (set! references (cdr references)))
                        (path
                          (text-relative-path
                            (text-page-path routes (site-page-id page))
                            (text-page-path
                              routes
                              (forest-reference-target-page-id reference))))
                        (fragment
                          (forest-reference-target-fragment reference)))
                 (string-append body " [" path
                                (if fragment (string-append "#" fragment) "")
                                "]")))))))

(define (heading title)
  (string-append title "\n" (make-string (string-length title) #\=) "\n"))

(define (page-reference routes from-path page)
  (string-append (site-page-title page)
                 " (" (text-relative-path
                        from-path
                        (text-page-path routes (site-page-id page))) ")"))

(define (index->text forest routes)
  (let* ((site (forest-site forest))
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
  (let ((incoming (forest-backlinks forest (site-page-id page)))
        (page-path (text-page-path routes (site-page-id page)))
        (body (string-trim-both
                (content->text (site-page-document page)
                               #:handlers (link-handlers forest routes page))
                (char-set #\newline))))
    (string-append
      (heading (site-page-title page))
      "\nHome: "
      (text-relative-path page-path (text-index-path routes))
      "\n\n"
      body
      (if (null? incoming)
          "\n"
          (string-append
            "\n\nLinked from:\n"
            (string-concatenate
              (map (lambda (source)
                     (string-append "- "
                                    (page-reference routes page-path source)
                                    "\n"))
                   incoming)))))))

(define (check-text-routes forest routes)
  (let* ((site (forest-site forest))
         (paths (cons (text-index-path routes)
                      (map (lambda (page)
                             (text-page-path routes (site-page-id page)))
                           (site-pages site)))))
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

(define (render-forest-text-changed forest prefix routes changed old-artifacts)
  (check-text-routes forest routes)
  (let* ((site (forest-site forest))
         (index-path (output-path prefix (text-index-path routes)))
         (index-sources (forest-source-paths forest))
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
                         (sources (forest-page-source-paths forest page))
                         (old (find-artifact old-artifacts path))
                         (current (make-artifact path "" sources)))
                    (if (artifact-current? current old changed)
                        old
                        (make-artifact
                          path (page->text forest page routes) sources))))
                (site-pages site))))
    (cons index-artifact page-artifacts)))

(define* (forest->text forest #:key (routes (make-text-routes)))
  (unless (forest? forest)
    (error "Forester text renderer requires a forest" forest))
  (check-text-routes forest routes)
  (let ((site (forest-site forest)))
    (cons (cons "index" (index->text forest routes))
          (map (lambda (page)
                 (cons (site-page-id page)
                       (page->text forest page routes)))
               (site-pages site)))))

(define* (render-forest-text forest #:key (routes (make-text-routes)))
  (let* ((site (forest-site forest))
         (sources (forest-source-paths forest)))
    (check-text-routes forest routes)
    (cons
      (make-artifact (text-index-path routes)
                     (index->text forest routes)
                     sources)
      (map (lambda (page)
             (make-artifact
               (text-page-path routes (site-page-id page))
               (page->text forest page routes)
               (forest-page-source-paths forest page)))
           (site-pages site)))))

(define* (make-forester-text-renderer #:key (prefix #f)
                                      (routes (make-text-routes))
                                      (cache-key #f))
  (unless (or (not prefix) (artifact-path? prefix))
    (error "Forester text output prefix must be a relative path or #f" prefix))
  (unless (text-routes? routes)
    (error "Forester text renderer requires text routes" routes))
  (let ((render (lambda (forest)
                  (let ((artifacts (render-forest-text forest
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
            (render-forest-text-changed forest prefix routes changed
                                        old-artifacts)))
        render)))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render text)
  #:use-module (grove build)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render text)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:export (forest->text
            render-forest-text
            make-forester-text-renderer))

(define link-handlers
  `((link . ,(lambda (element body)
               (string-append body " ["
                              (content-element-property element 'target)
                              ".txt]")))))

(define (heading title)
  (string-append title "\n" (make-string (string-length title) #\=) "\n"))

(define (page-reference page)
  (string-append (site-page-title page)
                 " (" (site-page-id page) ".txt)"))

(define (index->text forest)
  (let ((site (forest-site forest)))
    (string-append
      (heading (site-title site))
      "\n"
      (string-concatenate
        (map (lambda (page)
               (string-append "- " (page-reference page) "\n"))
             (site-pages site))))))

(define (page->text forest page)
  (let ((incoming (forest-backlinks forest (site-page-id page)))
        (body (string-trim-both
                (content->text (site-page-document page)
                               #:handlers link-handlers)
                (char-set #\newline))))
    (string-append
      (heading (site-page-title page))
      "\nHome: index.txt\n\n"
      body
      (if (null? incoming)
          "\n"
          (string-append
            "\n\nLinked from:\n"
            (string-concatenate
              (map (lambda (source)
                     (string-append "- " (page-reference source) "\n"))
                   incoming)))))))

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

(define (render-forest-text-changed forest prefix changed old-artifacts)
  (let* ((site (forest-site forest))
         (index-path (output-path prefix "index.txt"))
         (index-sources (forest-source-paths forest))
         (old-index (find-artifact old-artifacts index-path))
         (index-current (make-artifact index-path "" index-sources))
         (index-artifact
           (make-artifact index-path
                          (if (artifact-current? index-current old-index changed)
                              (artifact-content old-index)
                              (index->text forest))
                          index-sources))
         (page-artifacts
           (map (lambda (page)
                  (let* ((path (output-path prefix
                                            (string-append
                                              (site-page-id page) ".txt")))
                         (sources (forest-page-source-paths forest page))
                         (old (find-artifact old-artifacts path))
                         (current (make-artifact path "" sources)))
                    (if (artifact-current? current old changed)
                        old
                        (make-artifact path (page->text forest page) sources))))
                (site-pages site))))
    (cons index-artifact page-artifacts)))

(define (forest->text forest)
  (unless (forest? forest)
    (error "Forester text renderer requires a forest" forest))
  (let ((site (forest-site forest)))
    (when (member "index" (map site-page-id (site-pages site)))
      (error "text index path conflicts with forest page" "index"))
    (cons (cons "index" (index->text forest))
          (map (lambda (page)
                 (cons (site-page-id page) (page->text forest page)))
               (site-pages site)))))

(define (render-forest-text forest)
  (let* ((site (forest-site forest))
         (sources (forest-source-paths forest)))
    (cons
      (make-artifact "index.txt" (index->text forest) sources)
      (map (lambda (page)
             (make-artifact
               (string-append (site-page-id page) ".txt")
               (page->text forest page)
               (forest-page-source-paths forest page)))
           (site-pages site)))))

(define* (make-forester-text-renderer #:key (prefix #f) (cache-key #f))
  (unless (or (not prefix) (artifact-path? prefix))
    (error "Forester text output prefix must be a relative path or #f" prefix))
  (let ((render (lambda (forest)
                  (let ((artifacts (render-forest-text forest)))
                    (if prefix
                        (prefix-artifacts prefix artifacts)
                        artifacts)))))
    (if cache-key
        (make-cached-renderer
          render
          #:cache-key cache-key
          #:render-changed
          (lambda (forest changed dirty-paths old-artifacts)
            (render-forest-text-changed forest prefix changed old-artifacts)))
        render)))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render gemtext)
  #:use-module (grove build)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render text)
  #:use-module (forester render text routes)
  #:use-module (forester site)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (site->gemtext
            render-gemtext
            make-gemtext-renderer))

(define (default-gemtext-routes)
  (make-text-routes
    #:page-path (lambda (id) (string-append id ".gmi"))
    #:index-path "index.gmi"))

(define (line-text text)
  (string-join (string-tokenize text) " "))

(define (page-content-handlers forest routes page)
  (let ((references (outgoing-references forest (site-page-id page))))
    `((tag . ,(lambda (_ body) ""))
      (taxon . ,(lambda (_ body) ""))
      (item . ,(lambda (_ body)
                 (string-append "\n* " (line-text body) "\n")))
      (link . ,(lambda (_ body)
                 (when (null? references)
                   (error "Gemtext link rendering exceeded its index"
                          (site-page-id page)))
                 (let* ((reference (car references))
                        (target (find-page forest
                                           (reference-target-page-id reference)))
                        (url (text-relative-path
                               (text-page-path routes (site-page-id page))
                               (text-page-path routes
                                               (reference-target-page-id reference))))
                        (label (line-text body)))
                   (set! references (cdr references))
                   (string-append "\n=> " url
                                  (if (string-null? label)
                                      (string-append " " (site-page-title target))
                                      (string-append " " label))
                                  "\n"))))
      (query-link . ,(lambda (element body)
                       (let* ((target-id (content-element-property element 'target))
                              (target (find-page forest target-id))
                              (url (text-relative-path
                                     (text-page-path routes (site-page-id page))
                                     (text-page-path routes target-id)))
                              (label (line-text body)))
                         (string-append "\n=> " url
                           (if (string-null? label)
                               (string-append " " (site-page-title target))
                               (string-append " " label))
                           "\n"))))
      (image . ,(lambda (element body)
                  (let* ((source (content-element-property element 'source))
                         (url (if (or (string-prefix? "https://" source)
                                      (string-prefix? "gemini://" source)
                                      (string-prefix? "/" source))
                                  source
                                  (text-relative-path
                                    (text-page-path routes (site-page-id page))
                                    source)))
                         (label (line-text body)))
                    (string-append "\n=> " url
                                   (if (string-null? label) ""
                                       (string-append " " label))
                                   "\n"))))
      (code-block . ,(lambda (element body)
                       (when (any (lambda (line) (string-prefix? "```" line))
                                  (string-split body #\newline))
                         (error "Gemtext code block contains a fence line" page))
                       (string-append "\n```"
                                      (let ((language
                                              (content-element-property
                                                element 'language)))
                                        (if language
                                            (string-append " " (symbol->string language))
                                            ""))
                                      "\n" body
                                      (if (string-suffix? "\n" body) "" "\n")
                                      "```\n"))))))

(define (page-body forest routes page)
  (content->text (site-page-document page)
    #:handlers (page-content-handlers forest routes page)
    #:lazy-handlers
    (map (lambda (kind)
           (cons kind
             (lambda (element _render)
               (let* ((id (tree-occurrence-target element))
                      (target (find-page forest id))
                      (url (text-relative-path
                             (text-page-path routes (site-page-id page))
                             (text-page-path routes id))))
                 (string-append "\n=> " url " "
                                (line-text (site-page-title target)) "\n")))))
         '(subtree transclude query-transclude))))

(define (title-line title)
  (string-append "# " (line-text title) "\n\n"))

(define (index->gemtext forest routes)
  (string-append
    (title-line (site-title (site forest)))
    (string-concatenate
      (map (lambda (page)
             (string-append "=> "
               (text-relative-path (text-index-path routes)
                 (text-page-path routes (site-page-id page)))
               " " (line-text (site-page-title page)) "\n"))
           (pages forest)))))

(define (page->gemtext forest routes page)
  (let ((body (page-body forest routes page)))
    (string-append (title-line (site-page-title page)) body
                   (if (string-suffix? "\n" body) "" "\n"))))

(define (check-routes forest routes)
  (let ((paths (cons (text-index-path routes)
                     (map (lambda (page)
                            (text-page-path routes (site-page-id page)))
                          (pages forest)))))
    (unless (= (length paths) (length (delete-duplicates paths string=?)))
      (error "Gemtext routes produce duplicate output paths" paths))))

(define* (site->gemtext forest #:key (routes (default-gemtext-routes)))
  (unless (forest? forest)
    (error "Forester Gemtext renderer requires a forest" forest))
  (check-routes forest routes)
  (cons (cons "index" (index->gemtext forest routes))
        (map (lambda (page)
               (cons (site-page-id page) (page->gemtext forest routes page)))
             (pages forest))))

(define* (render-gemtext forest #:key (routes (default-gemtext-routes)))
  (unless (forest? forest)
    (error "Forester Gemtext renderer requires a forest" forest))
  (check-routes forest routes)
  (cons (make-artifact (text-index-path routes)
                       (index->gemtext forest routes)
                       (source-paths forest))
        (map (lambda (page)
               (make-artifact
                 (text-page-path routes (site-page-id page))
                 (page->gemtext forest routes page)
                 (page-source-paths forest page)))
             (pages forest))))

(define* (make-gemtext-renderer #:key (prefix #f)
                                (routes (default-gemtext-routes))
                                (cache? #t))
  (unless (text-routes? routes)
    (error "Gemtext renderer requires text routes" routes))
  (unless (or (not prefix) (artifact-path? prefix))
    (error "Gemtext output prefix must be a relative path or #f" prefix))
  (let ((render (lambda (forest)
                  (let ((artifacts (render-gemtext forest #:routes routes)))
                    (if prefix (prefix-artifacts prefix artifacts) artifacts)))))
    (make-cached-renderer render #:cache? cache?
      #:configuration
      (lambda (forest)
        (list 'gemtext prefix
              (map (lambda (page)
                     (cons (site-page-id page)
                           (text-page-path routes (site-page-id page))))
                   (pages forest))
              (text-index-path routes)
              (site-title (site forest)) (site-description (site forest)))))))

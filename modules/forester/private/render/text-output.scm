;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private render text-output)
  #:use-module (forester private render artifact)
  #:use-module (forester private render labels)
  #:use-module (forester private render-routes)
  #:use-module (forester render context)
  #:use-module (forester render text routes)
  #:use-module (forester site)
  #:use-module (forester i18n)
  #:use-module (grove site)
  #:use-module (grove output)
  #:use-module (grove render rules)
  #:use-module (srfi srfi-1)
  #:export (make-forester-text-page-renderer
            site->text-output
            render-text-output
            render-text-output-changed))

(define (make-forester-text-page-renderer format forest routes locale messages
                                          rules render-page)
  (unless (forest? forest)
    (error (string-append "Forester " format " renderer requires a forest")
           forest))
  (resolve-text-route-table format forest routes)
  (let* ((rules (compose-render-rules bilingual-text-rules rules))
         (context (make-site-render-context forest
                                            #:routes routes
                                            #:locale locale
                                            #:messages messages))
         (labels (localized-backmatter-labels context)))
    (lambda (page)
      (render-page page context labels rules))))

(define (site->text-output forest routes render-index render-page)
  (cons (cons 'index (render-index))
        (map (lambda (page)
               (cons (site-page-id page) (render-page page)))
             (pages forest))))

(define (output-path prefix path)
  (if prefix (string-append prefix "/" path) path))

(define (render-text-output forest prefix routes render-index render-page)
  (let ((artifacts
          (cons (make-artifact (text-index-path routes)
                               (render-index)
                               #:sources (source-paths forest))
                (map (lambda (page)
                       (make-artifact
                         (text-page-path routes (site-page-id page))
                         (render-page page)
                         #:sources (page-source-paths
                                     forest (site-page-id page))))
                     (pages forest)))))
    (if prefix (prefix-artifacts prefix artifacts) artifacts)))

(define (render-text-output-changed forest prefix routes changed old-artifacts
                                    render-index render-page)
  (let* ((index-artifact
           (reuse-artifact
             (output-path prefix (text-index-path routes))
             (source-paths forest) old-artifacts changed render-index))
         (page-artifacts
           (map (lambda (page)
                  (let* ((path (output-path
                                 prefix
                                 (text-page-path routes
                                                 (site-page-id page))))
                         (sources (page-source-paths
                                    forest (site-page-id page))))
                    (reuse-artifact path sources old-artifacts changed
                      (lambda () (render-page page)))))
                (pages forest))))
    (cons index-artifact page-artifacts)))

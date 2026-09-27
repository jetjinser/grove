;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render graph)
  #:use-module (forester site)
  #:use-module (grove build)
  #:use-module (grove output)
  #:use-module (grove site)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (forest->graph
            render-forest-graph
            make-forester-graph-renderer))

(define (heading->graph heading)
  `((id . ,(forest-heading-id heading))
    (label . ,(forest-heading-label heading))))

(define (page->graph forest page)
  `((id . ,(site-page-id page))
    (title . ,(site-page-title page))
    (headings . ,(map heading->graph
                       (forest-headings forest (site-page-id page))))))

(define (reference->graph reference)
  `((source-page . ,(forest-reference-source-page-id reference))
    (source-id . ,(forest-reference-source-id reference))
    (target-page . ,(forest-reference-target-page-id reference))
    (target-fragment . ,(forest-reference-target-fragment reference))
    (label . ,(forest-reference-label reference))))

(define (forest->graph forest)
  (unless (forest? forest)
    (error "Forester graph renderer requires a forest" forest))
  (let ((site (forest-site forest)))
    `(forest-graph
       (pages ,(map (lambda (page) (page->graph forest page))
                    (site-pages site)))
       (references
         ,(append-map
            (lambda (page)
              (map reference->graph
                   (forest-outgoing-references forest (site-page-id page))))
            (site-pages site))))))

(define* (render-forest-graph forest #:key (path "forest-graph.scm"))
  (unless (forest? forest)
    (error "Forester graph renderer requires a forest" forest))
  (unless (artifact-path? path)
    (error "Forester graph path must be a relative artifact path" path))
  (let ((content
          (call-with-output-string
            (lambda (port)
              (write (forest->graph forest) port)
              (newline port)))))
    (list (make-artifact path content (forest-source-paths forest)))))

(define* (make-forester-graph-renderer #:key (path "forest-graph.scm")
                                      (cache-key #f))
  (unless (artifact-path? path)
    (error "Forester graph path must be a relative artifact path" path))
  (let ((render (lambda (forest) (render-forest-graph forest #:path path))))
    (if cache-key
        (make-cached-renderer
          render
          #:cache-key (string-append "forester-graph-v1:" cache-key))
        render)))

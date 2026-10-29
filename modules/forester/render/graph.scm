;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render graph)
  #:use-module (grove render)
  #:use-module (forester site)
  #:use-module ((forester composition) #:prefix composition:)
  #:use-module (grove output)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:export (site->graph
            render-graph
            make-graph-renderer))



(define (page->graph forest page)
  `((id . ,(site-page-id page)) (title . ,(site-page-title page))))

(define (reference->graph reference)
  `((source-page . ,(reference-source-page-id reference))
    (source-id . ,(reference-source-id reference))
    (target-page . ,(reference-target-page-id reference))
    (label . ,(reference-label reference))))

(define (transclusion->graph transclusion)
  `((source-tree . ,(composition:transclusion-source-tree-id transclusion))
    (source-id . ,(composition:transclusion-source-id transclusion))
    (target-tree . ,(composition:transclusion-target-tree-id transclusion))))

(define (site->graph forest)
  "Return FOREST's page, reference, subtree, and transclusion graph as data."
  (unless (forest? forest)
    (error "Forester graph renderer requires a forest" forest))
  (let ((pages (pages forest)))
    `(forest-graph
       (pages ,(map (lambda (page) (page->graph forest page)) pages))
       (references
         ,(append-map
            (lambda (page)
              (map reference->graph
                   (outgoing-references forest (site-page-id page))))
            pages))
       (subtrees
         ,(map (lambda (definition)
                 `((parent-tree . ,(composition:subtree-definition-parent
                                     definition))
                   (subtree . ,(composition:subtree-definition-child
                                 definition))))
               (subtree-definitions forest)))
       (transclusions
         ,(append-map
            (lambda (page)
              (map transclusion->graph
                   (outgoing-transclusions forest
                                                  (site-page-id page))))
            pages)))))

(define* (render-graph forest #:key (path "forest-graph.scm"))
  "Render FOREST's graph as one Scheme-datum artifact at PATH."
  (unless (forest? forest)
    (error "Forester graph renderer requires a forest" forest))
  (unless (artifact-path? path)
    (error "Forester graph path must be a relative artifact path" path))
  (let ((content
          (call-with-output-string
            (lambda (port)
              (write (site->graph forest) port)
              (newline port)))))
    (list (make-artifact path content #:sources (source-paths forest)))))

(define* (make-graph-renderer #:key (path "forest-graph.scm"))
  "Construct a cached project renderer that writes the graph artifact at PATH."
  (unless (artifact-path? path)
    (error "Forester graph path must be a relative artifact path" path))
  (set! path (string-copy path))
  (make-cached-renderer
    (lambda (forest) (render-graph forest #:path path))
    #:configuration (list 'graph path)))

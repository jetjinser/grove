;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester graph)
  #:use-module (forester render graph)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove site)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64))

(define target
  (make-site-page
    "target" "Target"
    (make-content-document
      (list (make-content-element 'heading '("Overview"))))
    "/notes/target.grv"))

(define source
  (make-site-page
    "source" "Source"
    (make-content-document
      (list (make-content-element
              'paragraph
              (list (make-content-element
                      'link '("the overview")
                      #:properties '((target . "target#overview")))))))
    "/notes/source.grv"))

(define forest
  (make-forest
    (make-site (list source target)
               #:sources '("/notes/modules/links.scm"))))

(test-begin "forester graph")

(let* ((graph (forest->graph forest))
       (pages (cadr (assoc 'pages (cdr graph))))
       (references (cadr (assoc 'references (cdr graph)))))
  (test-equal "graph records page headings in site order"
    '(((id . "source") (title . "Source") (headings . ()))
      ((id . "target") (title . "Target")
       (headings . (((id . "overview") (label . "Overview"))))))
    pages)
  (test-equal "graph records directed references and their occurrences"
    '(((source-page . "source")
       (source-id . "ref-1")
       (target-page . "target")
       (target-fragment . "overview")
       (label . "the overview")))
    references))

(let* ((artifact (car (render-forest-graph forest #:path "data/graph.scm")))
       (serialized (artifact-content artifact))
       (round-trip (read (open-input-string serialized))))
  (test-equal "graph renderer writes a readable Scheme datum"
    (forest->graph forest) round-trip)
  (test-equal "graph artifact uses its configured path"
    "data/graph.scm" (artifact-path artifact))
  (test-equal "graph artifact tracks every forest source"
    '("/notes/modules/links.scm" "/notes/source.grv" "/notes/target.grv")
    (artifact-sources artifact)))

(test-error "graph renderer rejects absolute output paths" #t
  (render-forest-graph forest #:path "/tmp/forest.scm"))

(test-error "graph renderer rejects paths with parent traversal" #t
  (make-forester-graph-renderer #:path "../forest.scm"))

(test-end "forester graph")

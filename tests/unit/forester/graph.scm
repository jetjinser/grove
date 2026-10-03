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
      '())
    "/notes/target.plant"))

(define source
  (make-site-page
    "source" "Source"
    (make-content-document
      (list (make-content-element
              'paragraph
              (list (make-content-element
                      'link '("the overview")
                      #:properties '((target . "target")))))))
    "/notes/source.plant"))

(define forest
  (make-forest
    (make-site (list source target)
               #:sources '("/notes/modules/links.scm"))))

(test-begin "forester graph")

(let* ((graph (site->graph forest))
       (pages (cadr (assoc 'pages (cdr graph))))
       (references (cadr (assoc 'references (cdr graph)))))
  (test-equal "graph records tree identities in site order"
    '(((id . "source") (title . "Source"))
      ((id . "target") (title . "Target")))
    pages)
  (test-equal "graph records directed references and their occurrences"
    '(((source-page . "source")
       (source-id . "ref-1")
       (target-page . "target")
       (label . "the overview")))
    references))

(let* ((artifact (car (render-graph forest #:path "data/graph.scm")))
       (serialized (artifact-content artifact))
       (round-trip (read (open-input-string serialized))))
  (test-equal "graph renderer writes a readable Scheme datum"
    (site->graph forest) round-trip)
  (test-equal "graph artifact uses its configured path"
    "data/graph.scm" (artifact-path artifact))
  (test-equal "graph artifact tracks every forest source"
    '("/notes/modules/links.scm" "/notes/source.plant" "/notes/target.plant")
    (artifact-sources artifact)))

(test-error "graph renderer rejects absolute output paths" #t
  (render-graph forest #:path "/tmp/forest.scm"))

(test-error "graph renderer rejects paths with parent traversal" #t
  (make-graph-renderer #:path "../forest.scm"))

(let* ((root
         (make-site-page
           "root" "Root"
           (make-content-document
             (list
               (make-content-element
                 'subtree
                 (list (make-content-element 'paragraph '("Inline")))
                 #:properties '((title . "Inline tree")))))))
       (host
         (make-site-page
           "host" "Host"
           (make-content-document
             (list (make-content-element 'transclude '()
                     #:properties '((target . "unstable-0")))))))
       (graph (site->graph (make-forest (make-site (list root host)))))
       (subtrees (cadr (assoc 'subtrees (cdr graph))))
       (transclusions (cadr (assoc 'transclusions (cdr graph)))))
  (test-equal "graph records containment and transclusion as distinct edges"
    '(((parent-tree . "root") (subtree . "unstable-0")))
    subtrees)
  (test-equal "graph preserves the transclusion source occurrence"
    '(((source-tree . "host")
       (source-id . "transclude-1")
       (target-tree . "unstable-0")))
    transclusions))

(test-end "forester graph")

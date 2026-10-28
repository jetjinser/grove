;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester site)
  #:use-module (forester private site model)
  #:use-module (forester private site builder)
  #:use-module (forester private site queries)
  #:re-export (forest?
               make-forest
               site
               pages
               page-relation
               described-entity-relation
               describing-pages
               content-node-relation
               content-node
               topic-pages
               related-pages
               reference-pages
               find-page
               require-page
               page-taxon
               page-tags
               links
               tree-occurrences
               tree-occurrence-target
               backlinks
               common-backlinks
               reachable-pages
               reference?
               reference-source-page-id
               reference-source-id
               reference-target-page-id
               reference-label
               outgoing-references
               incoming-references
               subtree-definitions
               outgoing-transclusions
               incoming-transclusions
               transcluding-pages
               context-pages
               used-codepoints
               source-paths
               page-source-paths
               term-definition-relation
               term-mention-relation
               term-definitions
               term-definition-conflicts
               undefined-term-mentions))

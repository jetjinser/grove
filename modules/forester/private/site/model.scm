;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private site model)
  #:use-module (grove private snapshot)
  #:use-module (srfi srfi-9)
  #:export (forest?
            site
            pages
            reference?
            reference-source-page-id
            reference-source-id
            reference-target-page-id
            reference-label
            ;; Internal forest state shared by the builder and query layer.
            %make-forest
            forest-pages/raw
            reference-relation
            composition
            page-index
            page-relation
            described-entity-relation
            content-node-relation
            content-node-index
            term-definition-relation
            term-mention-relation
            query-page-ids
            make-reference))

(define-record-type <reference>
  (%make-reference source-page-id source-id target-page-id label)
  reference?
  (source-page-id reference-source-page-id/raw)
  (source-id reference-source-id/raw)
  (target-page-id reference-target-page-id/raw)
  (label reference-label/raw))

(define (make-reference source-page-id source-id target-page-id label)
  (%make-reference (snapshot-data source-page-id)
                   (snapshot-data source-id)
                   (snapshot-data target-page-id)
                   (snapshot-data label)))

(define (reference-source-page-id reference)
  (snapshot-data (reference-source-page-id/raw reference)))

(define (reference-source-id reference)
  (snapshot-data (reference-source-id/raw reference)))

(define (reference-target-page-id reference)
  (snapshot-data (reference-target-page-id/raw reference)))

(define (reference-label reference)
  (snapshot-data (reference-label/raw reference)))

(define-record-type <forest>
  (%make-forest site pages reference-relation
                composition page-index page-relation described-entity-relation
                content-node-relation
                content-node-index term-definition-relation
                term-mention-relation query-page-ids)
  forest?
  (site site)
  (pages forest-pages/raw)
  (reference-relation reference-relation)
  (composition composition)
  (page-index page-index)
  (page-relation page-relation)
  (described-entity-relation described-entity-relation)
  (content-node-relation content-node-relation)
  (content-node-index content-node-index)
  (term-definition-relation term-definition-relation)
  (term-mention-relation term-mention-relation)
  (query-page-ids query-page-ids))

(define (pages forest)
  (snapshot-data (forest-pages/raw forest)))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester queries)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (forester relations)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove site))

(define (page-with-links id targets)
  (make-site-page
    id id
    (make-content-document
      (map (lambda (target)
             (make-content-element
               'link (list target)
               #:properties `((target . ,target))))
           targets))))

(define (bfs-reachable-page-ids forest source-id)
  (let visit ((pending (list source-id)) (visited '()))
    (if (null? pending)
        (map site-page-id
             (filter (lambda (page)
                       (and (not (equal? (site-page-id page) source-id))
                            (member (site-page-id page) visited equal?)))
                     (site-pages (site forest))))
        (let ((current (car pending)))
          (if (member current visited equal?)
              (visit (cdr pending) visited)
              (visit
                (append
                  (cdr pending)
                  (map reference-target-page-id
                       (outgoing-references forest current)))
                (cons current visited)))))))

(test-begin "forester site queries")

(let* ((destination (page-with-links "destination" '()))
       (source (page-with-links "source" '("destination" "destination")))
       (other-source (page-with-links "other-source" '("destination")))
       (unlinked (page-with-links "unlinked" '()))
       (forest
         (make-forest
           (make-site (list destination source other-source unlinked))))
       (backlinks (backlinks forest "destination")))
  (test-equal "backlinks return unique source pages in site order"
    '("source" "other-source")
    (map site-page-id backlinks))
  (test-equal "incoming references still preserve every link occurrence"
    3
    (length (incoming-references forest "destination"))))

(let* ((left (page-with-links "left" '()))
       (right (page-with-links "right" '()))
       (shared-late (page-with-links "shared-late" '("left" "right")))
       (left-only (page-with-links "left-only" '("left")))
       (shared-early
         (page-with-links "shared-early" '("left" "left" "right")))
       (right-only (page-with-links "right-only" '("right")))
       (forest
         (make-forest
           (make-site
             (list left shared-late left-only right shared-early right-only))))
       (common (common-backlinks forest "left" "right")))
  (test-equal "common backlinks preserve site order and ignore duplicate links"
    '("shared-late" "shared-early")
    (map site-page-id common))
  (test-equal "joining a target to itself gives its unique backlink pages"
    (map site-page-id (backlinks forest "left"))
    (map site-page-id (common-backlinks forest "left" "left"))))

(let* ((isolated (page-with-links "isolated" '()))
       (hub (page-with-links "hub" '("left" "right")))
       (left (page-with-links "left" '("merge")))
       (right (page-with-links "right" '("merge")))
       (merge (page-with-links "merge" '("hub")))
       (forest (make-forest (make-site (list isolated hub left right merge)))))
  (test-equal "reachability returns each transitive target in site order"
    '("left" "right" "merge")
    (map site-page-id (reachable-pages forest "hub")))
  (test-equal "reachability excludes the source when a cycle returns to it"
    '("hub" "left" "right")
    (map site-page-id (reachable-pages forest "merge")))
  (test-equal "isolated pages have no reachable pages"
    '()
    (reachable-pages forest "isolated"))
  (test-equal "relational reachability agrees with an independent breadth-first traversal"
    (map (lambda (page)
           (cons (site-page-id page)
                 (bfs-reachable-page-ids forest (site-page-id page))))
         (site-pages (site forest)))
    (map (lambda (page)
           (cons (site-page-id page)
                 (map site-page-id
                      (reachable-pages forest (site-page-id page)))))
         (site-pages (site forest)))))

(test-end "forester site queries")

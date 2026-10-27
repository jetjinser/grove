;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester queries)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (forester private query-output)
  #:use-module (forester relations)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove relation dsl)
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

(let* ((target (string-copy "reusable-tree"))
       (element (make-content-element 'transclude '()
                  #:properties (list (cons 'target target))))
       (exposed-target (tree-occurrence-target element)))
  (string-set! exposed-target 0 #\X)
  (test-equal "tree occurrence target accessors return independent strings"
    "reusable-tree" (tree-occurrence-target element)))

(let* ((source (page-with-links "source" '("target")))
       (target (page-with-links "target" '()))
       (forest (make-forest (make-site (list source target))))
       (exposed-pages (pages forest))
       (exposed-references (outgoing-references forest "source"))
       (label (reference-label (car exposed-references)))
       (exposed-backlinks (backlinks forest "target")))
  (set-car! exposed-pages #f)
  (set-car! exposed-references #f)
  (string-set! label 0 #\X)
  (set-car! exposed-backlinks #f)
  (test-equal "forest page accessors return independent collections"
    '("source" "target") (map site-page-id (pages forest)))
  (test-equal "reference query caches cannot be changed through returned lists"
    '("target") (map reference-target-page-id (outgoing-references forest "source")))
  (test-equal "reference records protect labels from accessor mutation"
    "target" (reference-label (car (outgoing-references forest "source"))))
  (test-equal "cached backlink queries return independent page lists"
    '("source") (map site-page-id (backlinks forest "target"))))

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

(test-assert "query links warn on unknown fragments"
  (let* ((port (open-output-string))
         (document
           (make-content-document
             (list (make-content-element
                     'link '("target")
                     #:properties '((target . "target") (fragment . "missing")))))))
    (parameterize ((current-warning-port port))
      (prepare-query-output
        document "queries/test" (lambda (target) 'page)
        (lambda (target fragment) #f)))
    (string-contains (get-output-string port) "missing")))

(test-assert "query links with declared fragments stay silent"
  (let* ((port (open-output-string))
         (document
           (make-content-document
             (list (make-content-element
                     'link '("target")
                     #:properties '((target . "target") (fragment . "eq-1")))))))
    (parameterize ((current-warning-port port))
      (prepare-query-output
        document "queries/test" (lambda (target) 'page)
        (lambda (target fragment) #t)))
    (string-null? (get-output-string port))))

(test-assert "query-returned links warn through forest resolution"
  (let* ((port (open-output-string))
         (host
           (make-site-page "host" "Host"
             (make-content-document
               (list (make-content-element
                       'forest-query '()
                       #:properties
                       `((query . ,(lambda (forest page-id)
                                     (make-content-document
                                       (list (make-content-element
                                               'link '("x")
                                               #:properties '((target . "host")
                                                              (fragment . "missing"))))))))
                       #:flow-kind 'block)))))
         (forest
           (parameterize ((current-warning-port port))
             (make-forest (make-site (list host)))))
         (message (get-output-string port)))
    (and (string-contains message "missing")
         (string-contains message "host"))))

(let* ((doi "doi:10.1146/annurev.fluid.29.1.123")
       (first (make-site-page "first" "First" (make-content-document '())
                #:metadata `((describes . (,doi)))))
       (second (make-site-page "second" "Second" (make-content-document '())
                 #:metadata `((describes . (,doi)))))
       (forest (make-forest (make-site (list first second)))))
  (test-equal "describing-pages returns all pages for one external entity"
    '("first" "second")
    (map site-page-id (describing-pages forest doi)))
  (test-equal "described entity relation is part of the forest"
    2
    (length (described-entity-page-ids
              (described-entity-relation forest) doi)))
  (test-equal "described entities compose with page facts by page ID"
    '(("first" "First") ("second" "Second"))
    (run* (id title)
      (described-entityo (described-entity-relation forest)
        (uri doi) (page-id id))
      (pageo (page-relation forest)
        (page-id id) (title title)))))

(test-end "forester site queries")

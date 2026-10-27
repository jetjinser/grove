;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private render-routes)
  #:use-module (forester site)
  #:use-module (forester render text routes)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:export (resolve-site-routes
            resolve-text-route-table))

(define (validate-unique-render-paths! renderer paths)
  (unless (= (length paths) (length (delete-duplicates paths string=?)))
    (error (string-append renderer " routes produce duplicate output paths")
           paths))
  paths)

(define (resolve-site-routes forest routes)
  "Use the analyzed site's home page when ROUTES leaves it unspecified."
  (if (html-home-page routes)
      routes
      (let ((home (site-home (site forest))))
        (if home
            (html-routes-with-home-page routes home)
            routes))))

(define (resolve-text-route-table renderer forest routes)
  "Resolve and validate ROUTES for every page in FOREST.

The result is an ordered page-ID to artifact-path association list. The
aggregate index participates in collision checks but is not included in the
returned page table."
  (let* ((table
           (map (lambda (page)
                  (cons (site-page-id page)
                        (text-page-path routes (site-page-id page))))
                (pages forest))))
    (validate-unique-render-paths!
      renderer
      (cons (text-index-path routes) (map cdr table)))
    table))

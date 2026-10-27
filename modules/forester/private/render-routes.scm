;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private render-routes)
  #:use-module (forester site)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:export (resolve-site-routes validate-unique-render-paths!))

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

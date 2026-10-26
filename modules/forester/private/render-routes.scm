;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private render-routes)
  #:use-module (forester site)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:export (resolve-site-routes))

(define (resolve-site-routes forest routes)
  "Use the analyzed site's home page when ROUTES leaves it unspecified."
  (if (html-home-page routes)
      routes
      (let ((home (site-home (site forest))))
        (if home
            (html-routes-with-home-page routes home)
            routes))))

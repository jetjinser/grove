;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove site build)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:export (build-site))

(define (read-page-with-context path read-page)
  (catch #t
    (lambda () (read-page path))
    (lambda (key . args)
      (error (format #f "failed to read project source ~a (~a): ~s"
                     path key args)))))

(define* (build-site sources read-page #:key
                     (title "Site")
                     (description #f)
                     (module-sources '())
                     (load-modules #f))
  (unless (null? module-sources)
    (unless (procedure? load-modules)
      (error "site module sources require a module loader" module-sources))
    (catch #t
      (lambda () (load-modules module-sources))
      (lambda (key . args)
        (error (format #f "failed to load project module sources ~s (~a): ~s"
                       module-sources key args)))))
  (let ((pages (map (lambda (path)
                      (read-page-with-context path read-page))
                    sources)))
    (make-site pages
               #:title title
               #:description description
               #:sources (delete-duplicates
                           (filter string?
                             (append module-sources sources
                               (filter-map site-page-source pages)))
                           string=?))))

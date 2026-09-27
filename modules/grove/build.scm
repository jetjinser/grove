;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove build)
  #:use-module (grove site)
  #:export (build-site
            render-site))

(define* (build-site sources read-page #:key
                     (title "Site")
                     (module-sources '())
                     (load-modules #f))
  (unless (null? module-sources)
    (unless (procedure? load-modules)
      (error "site module sources require a module loader" module-sources))
    (load-modules module-sources))
  (make-site (map read-page sources) #:title title))

(define (render-site site renderers)
  (apply append (map (lambda (renderer) (renderer site)) renderers)))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove build)
  #:use-module (srfi srfi-64)
  #:use-module (grove build)
  #:use-module (grove ir)
  #:use-module (grove site))

(test-begin "grove build")

(let ((events '()))
  (let ((site
          (build-site
            '(page-a page-b)
            (lambda (source)
              (set! events (append events (list source)))
              (make-site-page (symbol->string source)
                              (symbol->string source)
                              (make-content-document '())))
            #:module-sources '(shared-module)
            #:load-modules
            (lambda (sources)
              (set! events (append events (list (cons 'modules sources))))))))
    (test-equal "module sources load before page sources"
      '((modules shared-module) page-a page-b)
      events)
    (test-equal "site build preserves page source order"
      '("page-a" "page-b")
      (map site-page-id (site-pages site)))
    (test-equal "site source paths omit symbolic build identifiers"
      '()
      (site-sources site))))

(test-equal "site records page and module file paths"
  '("module.grv" "page.grv")
  (site-sources
    (build-site
      '("page.grv")
      (lambda (source)
        (make-site-page "page" "Page" (make-content-document '()) source))
      #:module-sources '("module.grv")
      #:load-modules (lambda (sources) #t))))

(test-error "module sources require a loader" #t
  (build-site '() (lambda (source) source)
    #:module-sources '(shared-module)))

(test-error "cached renderers require procedures" #t
  (make-cached-renderer #f #:cache-key "renderer-v1"))

(test-error "cached renderers require non-empty keys" #t
  (make-cached-renderer (lambda (site) '()) #:cache-key ""))

(test-error "incremental callbacks must be procedures" #t
  (make-cached-renderer (lambda (site) '())
    #:cache-key "renderer-v1"
    #:render-changed #t))

(test-equal "site builds without an optional module phase"
  '()
  (site-pages (build-site '() (lambda (source) source))))

(test-end "grove build")

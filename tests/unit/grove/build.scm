;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove build)
  #:use-module (srfi srfi-64)
  #:use-module (grove build))

(test-begin "grove build")

(test-error "cached renderers require procedures" #t
  (make-cached-renderer #f #:cache-key "renderer-v1"))

(test-error "cached renderers require non-empty keys" #t
  (make-cached-renderer (lambda (site) '()) #:cache-key ""))

(test-error "incremental callbacks must be procedures" #t
  (make-cached-renderer (lambda (site) '())
    #:cache-key "renderer-v1"
    #:render-changed #t))

(test-error "renderer input collectors must be procedures" #t
  (make-cached-renderer (lambda (site) '())
    #:cache-key "renderer-v1"
    #:inputs '()))

(test-end "grove build")

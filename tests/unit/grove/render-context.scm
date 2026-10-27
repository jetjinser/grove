;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove render-context)
  #:use-module (srfi srfi-64)
  #:use-module (grove render context))

(test-begin "grove render context")

(test-assert "context association storage stays private"
  (not (module-variable
         (module-public-interface (resolve-module '(grove render context)))
         'render-context-values)))

(let ((context (make-render-context #:page "p" #:custom-key "v")))
  (test-equal "construction keeps known keys"
    "p" (render-context-ref context 'page))
  (test-equal "construction keeps unknown keys"
    "v" (render-context-ref context 'custom-key)))

(let* ((text (string-copy "authored"))
       (data (list (cons 'label text)))
       (context (make-render-context #:custom data)))
  (string-set! text 0 #\x)
  (set-car! data '(changed . #t))
  (test-equal "contexts snapshot mutable strings and pair data on input"
    '((label . "authored"))
    (render-context-ref context 'custom))
  (let ((exposed (render-context-ref context 'custom)))
    (string-set! (cdar exposed) 0 #\x)
    (set-car! exposed '(changed-again . #t)))
  (test-equal "context accessors return independent mutable-data snapshots"
    '((label . "authored"))
    (render-context-ref context 'custom)))

(let* ((groups (list (cons "guide" (list 'english 'chinese))))
       (context (make-render-context #:translation-groups groups)))
  (set-car! (cdar groups) 'changed)
  (test-equal "translation groups are copied as ordinary context data"
    '(("guide" english chinese))
    (render-context-ref context 'translation-groups))
  (let ((exposed (render-context-ref context 'translation-groups)))
    (set-car! (cdar exposed) 'changed-again))
  (test-equal "translation group access cannot mutate a context"
    '(("guide" english chinese))
    (render-context-ref context 'translation-groups)))

(let* ((value (string-copy "overlay"))
       (base (make-render-context #:page "base"))
       (derived (render-context-with base #:custom value)))
  (string-set! value 0 #\x)
  (test-equal "context derivation snapshots newly supplied values"
    "overlay" (render-context-ref derived 'custom)))

(let ((context (make-render-context #:model "model"
                                    #:project/extra "value")))
  (test-equal "context keys are open to project models and extensions"
    "model" (render-context-ref context 'model))
  (test-equal "namespace keys are retained as supplied"
    "value" (render-context-ref context 'project/extra)))

(let ((context (make-render-context #:hreflang? #t #:language-switcher? #f)))
  (test-equal "explicit #f is preserved"
    #f (render-context-ref context 'language-switcher?))
  (test-assert "explicit #f is distinguishable from absence"
    (render-context-has? context 'language-switcher?))
  (test-assert "absent optional keys are not present"
    (not (render-context-has? context 'og-image))))

(let ((context (make-render-context)))
  (test-assert "generic contexts invent no project defaults"
    (and (not (render-context-has? context 'hreflang?))
         (not (render-context-has? context 'language-switcher?)))))

(let* ((base (make-render-context #:hreflang? #t
                                  #:language-switcher? #t
                                  #:page "base"))
       (derived (render-context-with base #:hreflang? #f #:page "derived")))
  (test-equal "with overlays a site-level boolean with #f"
    #f (render-context-ref derived 'hreflang?))
  (test-equal "with overrides a value"
    "derived" (render-context-ref derived 'page))
  (test-equal "with leaves the base value untouched"
    "base" (render-context-ref base 'page))
  (test-equal "with keeps unrelated keys"
    #t (render-context-ref derived 'language-switcher?)))

(let* ((base (make-render-context #:og-image "img"))
       (derived (render-context-with base #:og-image #f)))
  (test-equal "with can clear an optional value with #f"
    #f (render-context-ref derived 'og-image))
  (test-assert "an explicitly cleared value remains present"
    (render-context-has? derived 'og-image)))

(let* ((base (make-render-context #:page "base" #:og-image "img"))
       (derived (render-context-with base #:page #f #:og-image #f)))
  (test-equal "with overlays #f for an existing value"
    #f (render-context-ref derived 'page))
  (test-equal "with overlays #f for an optional value"
    #f (render-context-ref derived 'og-image)))

(let* ((base (make-render-context #:page "p" #:custom "v"))
       (derived (render-context-with base #:extra "e")))
  (test-equal "with adds unknown keys"
    "e" (render-context-ref derived 'extra))
  (test-equal "with keeps existing unknown keys"
    "v" (render-context-ref derived 'custom)))

(let* ((base (make-render-context #:page "p" #:custom "v"))
       (derived (render-context-without base 'page 'custom)))
  (test-assert "without removes keys"
    (not (render-context-has? derived 'page)))
  (test-equal "without makes removed keys read as false"
    #f (render-context-ref derived 'page))
  (test-assert "without keeps the base untouched"
    (render-context-has? base 'page)))

(test-equal "repeated keywords keep their last value"
  "second"
  (render-context-ref
    (make-render-context #:page "first" #:page "second")
    'page))

(test-error "non-keyword entries are rejected" #t
  (make-render-context 'page "p"))

(test-error "ref requires a render context" #t
  (render-context-ref 'not-a-context 'page))

(test-end "grove render context")

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove render-context)
  #:use-module (srfi srfi-64)
  #:use-module (grove render context))

(test-begin "grove render context")

(let ((context (make-render-context #:page "p" #:custom-key "v")))
  (test-equal "construction keeps known keys"
    "p" (render-context-ref context 'page))
  (test-equal "construction keeps unknown keys"
    "v" (render-context-ref context 'custom-key)))

(let ((context (make-render-context #:forest "forest"
                                    #:routes "routes"
                                    #:page "page"
                                    #:anchor-prefix "anchor"
                                    #:site-url "site"
                                    #:default-locale "en"
                                    #:hreflang? #f
                                    #:language-switcher? #t
                                    #:translation-groups 'groups
                                    #:og-image "image"
                                    #:og-image-alt "alt"
                                    #:locale "zh-Hans"
                                    #:locale-object "locale-object"
                                    #:messages "messages")))
  (test-equal "typed forest accessor" "forest"
    (render-context-forest context))
  (test-equal "typed routes accessor" "routes"
    (render-context-routes context))
  (test-equal "typed page accessor" "page"
    (render-context-page context))
  (test-equal "typed anchor-prefix accessor" "anchor"
    (render-context-anchor-prefix context))
  (test-equal "typed site-url accessor" "site"
    (render-context-site-url context))
  (test-equal "typed default-locale accessor" "en"
    (render-context-default-locale context))
  (test-equal "typed hreflang accessor" #f
    (render-context-hreflang? context))
  (test-equal "typed language-switcher accessor" #t
    (render-context-language-switcher? context))
  (test-equal "typed translation-groups accessor" 'groups
    (render-context-translation-groups context))
  (test-equal "typed og-image accessor" "image"
    (render-context-og-image context))
  (test-equal "typed og-image-alt accessor" "alt"
    (render-context-og-image-alt context))
  (test-equal "typed locale accessor" "zh-Hans"
    (render-context-locale context))
  (test-equal "typed locale-object accessor" "locale-object"
    (render-context-locale-object context))
  (test-equal "typed messages accessor" "messages"
    (render-context-messages context)))

(let ((context (make-render-context #:hreflang? #t #:language-switcher? #f)))
  (test-equal "explicit #f is preserved"
    #f (render-context-ref context 'language-switcher?))
  (test-assert "explicit #f is distinguishable from absence"
    (render-context-has? context 'language-switcher?))
  (test-assert "absent optional keys are not present"
    (not (render-context-has? context 'og-image))))

(let ((context (make-render-context)))
  (test-equal "hreflang? defaults to true"
    #t (render-context-ref context 'hreflang?))
  (test-equal "language-switcher? defaults to true"
    #t (render-context-ref context 'language-switcher?)))

(let* ((base (make-render-context #:hreflang? #t #:page "base"))
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

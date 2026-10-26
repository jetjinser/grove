;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 textual-ports)
             (forester input scribble)
             (forester render html)
             (forester project new)
             (forester render text)
             (forester site)
             (forester style html)
             (forester style i18n)
             (grove i18n)
             (grove input scribble)
             (grove output)
             (grove project)
             (grove project site)
             (grove render html presentation)
             (grove render html routes)
             (grove style html))

(define examples-directory (dirname (current-filename)))
(define output-directory "build")

(define source-directory (string-append examples-directory "/plants"))
(define module-directory (string-append examples-directory "/modules"))
(define style-css-path (string-append examples-directory "/style.css"))
(define read-document (make-scribble-document-reader))

(define style-css
  (call-with-input-file style-css-path get-string-all))

;; Chrome strings such as backmatter headings come from a message catalog. This
;; is separate from the bilingual content itself, which is authored per tree.
(define chrome-messages
  (make-message-catalog
    '(("en" . ((backmatter-context . "Context")
               (backmatter-references . "References")
               (backmatter-backlinks . "Backlinks")
               (backmatter-related . "Related")))
      ("zh-Hans" . ((backmatter-context . "语境")
                    (backmatter-references . "参考")
                    (backmatter-backlinks . "反向链接")
                    (backmatter-related . "相关"))))
    #:default "en"))

(define bilingual-style
  (compose-html-styles
    default-html-style
    default-bilingual-style
    (make-html-style
      #:stylesheets
      (list (make-artifact "style.css" style-css (list style-css-path))))))

(define default-locale (make-locale "en"))

(define bilingual-routes
  (make-html-routes))

(define bilingual-project
  (make-site-project
    #:create-source (make-forester-source-creator
                      #:source-directory source-directory
                      #:default-prefix "bi")
    #:locale "en"
    #:title "Bilingual Notes"
    #:description "One idea in two languages, shown side by side."
    #:home "hello"
    #:source-directory source-directory
    #:extra-sources (list style-css-path)
    #:prepare (make-scribble-module-setup module-directory)
    #:read-page (make-page-reader read-document)
    #:analyze make-forest
    #:renderers
    (list (make-html-renderer
            #:options
            (make-html-options
              #:presentation (make-html-presentation #:style bilingual-style)
              #:routes bilingual-routes
              #:html-attributes (locale-html-attributes default-locale)
              #:messages chrome-messages))
          (make-text-renderer
            #:options (make-text-options #:prefix "text")))
    #:output-directory output-directory))

bilingual-project

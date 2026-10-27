;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 textual-ports)
             (forester render html)
             (forester project)
             (forester render text)
             (forester style html)
             (forester style i18n)
             (grove i18n)
             (grove input scribble)
             (grove output)
             (grove render html presentation)
             (grove render html routes)
             (grove style html))

(define examples-directory (dirname (current-filename)))
(define output-directory "build")

(define source-directory (string-append examples-directory "/plants"))
(define module-directory (string-append examples-directory "/modules"))
(define style-css-path (string-append examples-directory "/style.css"))

(define style-css
  (call-with-input-file style-css-path get-string-all))

;; Built-in interface strings such as backmatter headings come from a message
;; catalog. This is separate from the bilingual content itself, which is
;; authored per tree.
(define interface-messages
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

(define bilingual-routes
  (make-html-routes))

(define bilingual-project
  (make-forester-project
    #:reader-locale "en"
    #:title "Bilingual Notes"
    #:description "One idea in two languages, shown side by side."
    #:home "hello"
    #:source-directory source-directory
    #:default-prefix "bi"
    #:prepare (make-scribble-module-setup module-directory)
    #:html-options
    (make-html-options
      #:presentation (make-html-presentation #:style bilingual-style)
      #:routes bilingual-routes
      #:locale "en"
      #:messages interface-messages)
    #:additional-renderers
    (list (make-text-renderer
            #:options (make-text-options #:prefix "text")))
    #:output-directory output-directory))

bilingual-project

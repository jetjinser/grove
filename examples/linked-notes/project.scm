;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw)
             (ice-9 textual-ports)
             (forester browse)
             (forester render graph)
             (forester render gemtext)
             (forester render html)
             (forester render site)
             (forester render rss)
             (forester render sitemap)
             (forester project)
             (grove render static)
             (forester render text)
             (forester render text routes)
             (grove input scribble)
             (grove output)
             (grove render html presentation)
             (grove render html routes)
             (grove style html))

(define examples-directory (dirname (current-filename)))
(define output-directory "build")

(define source-directory (string-append examples-directory "/plants"))
(define module-directory (string-append examples-directory "/modules"))
(define notes-css-path (string-append examples-directory "/style.css"))

(define notes-css
  (call-with-input-file notes-css-path get-string-all))

(define notes-style
  (make-html-style
    #:stylesheets
    (list
      (make-artifact "style.css" notes-css (list notes-css-path)))))

(define notes-routes
  (make-html-routes))

(define notes-text-routes
  (make-text-routes
    #:page-path
    (lambda (id)
      (string-append id ".txt"))))

(define notes-settings
  (make-site-settings #:site-url "https://notes.example"))

(define notes-project
  (make-forester-project
    #:default-prefix "note"
    #:title "Linked Notes"
    #:description "Connected notes on mathematics and its ideas."
    #:home "math-0001"
    #:source-directory source-directory
    #:prepare
    (make-scribble-module-setup module-directory)
    #:html-options
    (make-html-options
      #:presentation (make-html-presentation #:style notes-style)
      #:routes notes-routes
      #:settings notes-settings)
    #:additional-renderers
    (append
      (list (make-text-renderer
              #:options
              (make-text-options #:prefix "text" #:routes notes-text-routes))
            (make-gemtext-renderer
              #:options (make-gemtext-options #:prefix "gemini"))
            (make-static-assets-renderer
              #:source-directory (string-append examples-directory "/static")))
      (list (make-rss-renderer #:settings notes-settings #:routes notes-routes)
            (make-sitemap-renderer #:settings notes-settings #:routes notes-routes)
            (make-graph-renderer #:path "forest-graph.scm")))
    #:browse (make-forest-browser)
    #:output-directory output-directory))

notes-project

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester project)
  #:use-module (forester input scribble)
  #:use-module (forester project new)
  #:use-module (forester render html)
  #:use-module (forester site)
  #:use-module (grove input scribble)
  #:use-module (grove project site)
  #:export (make-forester-project))

(define* (make-forester-project
          #:key home
          (source-directory "plants")
          (output-directory "build")
          (title "Site")
          (description #f)
          (prepare (lambda () '()))
          (html-options (make-html-options))
          (additional-renderers '())
          (derived '())
          (browse #f)
          (locale #f)
          (pipelines '())
          (default-prefix #f)
          (tree-template #f))
  "Build a Forester project with standard HTML, derived artifacts, and pipelines.
HOME is the tree ID rendered at the site index. HTML-OPTIONS configures the
standard HTML renderer; ADDITIONAL-RENDERERS adds peer outputs alongside it."
  (unless (and (string? home) (> (string-length home) 0))
    (error "Forester project requires a non-empty home tree ID" home))
  (make-site-project
    #:title title
    #:description description
    #:source-directory source-directory
    #:home home
    #:prepare prepare
    #:read-page (make-page-reader (make-scribble-document-reader))
    #:analyze make-forest
    #:renderers (cons (make-html-renderer #:options html-options)
                      additional-renderers)
    #:derived derived
    #:pipelines pipelines
    #:browse browse
    #:locale locale
    #:create-source
    (make-forester-source-creator
      #:source-directory source-directory
      #:default-prefix default-prefix
      #:template (or tree-template #f))
    #:output-directory output-directory))

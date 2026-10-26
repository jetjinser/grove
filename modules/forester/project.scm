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
          (renderers #f)
          (browse #f)
          (locale #f)
          (pipelines '())
          (default-prefix #f)
          (tree-template #f))
  "Build a Forester project with standard HTML and optional asset pipelines."
  (make-site-project
    #:title title
    #:description description
    #:source-directory source-directory
    #:home home
    #:prepare prepare
    #:read-page (make-page-reader (make-scribble-document-reader))
    #:analyze make-forest
    #:renderers (or renderers (list (make-html-renderer)))
    #:pipelines pipelines
    #:browse browse
    #:locale locale
    #:create-source
    (make-forester-source-creator
      #:source-directory source-directory
      #:default-prefix default-prefix
      #:template (or tree-template #f))
    #:output-directory output-directory))

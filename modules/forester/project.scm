;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester project)
  #:use-module (forester input scribble)
  #:use-module (forester project new)
  #:use-module (forester render html)
  #:use-module (forester site)
  #:use-module (grove input scribble)
  #:use-module (grove project site)
  #:use-module (srfi srfi-13)
  #:export (make-forester-project))

(define (forester-source? filename)
  (and (not (string-prefix? "." filename))
       (string-suffix? ".plant" filename)))

(define* (make-forester-project
          #:key home
          (source-directory "plants")
          (output-directory "build")
          (collect-sources #f)
          (title "Site")
          (description #f)
          (prepare (lambda () '()))
          (extra-sources '())
          (html-options (make-html-options))
          (html-rules-id #f)
          (additional-renderers '())
          (transforms '())
          (browse #f)
          (reader-locale #f)
          (pipelines '())
          (cache? #t)
          (default-prefix #f)
          (tree-template #f))
  "Build a Forester project with standard HTML and optional peer renderers.
HOME is the tree ID rendered at the site index. HTML-OPTIONS configures the
standard HTML renderer; HTML-RULES-ID identifies opaque HTML rules for renderer
caching. ADDITIONAL-RENDERERS adds peer formats and projections; TRANSFORMS
compose content changes before Forester analysis. COLLECT-SOURCES can replace
directory traversal while retaining the standard reader and analysis stages.
PREPARE returns tracked paths such as loaded modules, not tree pages.
EXTRA-SOURCES declares fixed project dependencies; neither set is read as
Forester page content."
  (unless (and (string? home) (> (string-length home) 0))
    (error "Forester project requires a non-empty home tree ID" home))
  (make-site-project
    #:title title
    #:description description
    #:source-directory source-directory
    #:source-filter forester-source?
    #:collect-sources collect-sources
    #:home home
    #:prepare prepare
    #:extra-sources extra-sources
    #:read-page (make-page-reader (make-scribble-document-reader))
    #:analyze make-forest
    #:transforms transforms
    #:renderers (cons (make-html-renderer #:options html-options
                                           #:rules-id html-rules-id)
                      additional-renderers)
    #:pipelines pipelines
    #:cache? cache?
    #:browse browse
    #:reader-locale reader-locale
    #:create-source
    (make-forester-source-creator
      #:source-directory source-directory
      #:default-prefix default-prefix
      #:template tree-template)
    #:output-directory output-directory))

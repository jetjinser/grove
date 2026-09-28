;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester style html)
  #:use-module (grove output)
  #:use-module (grove style html)
  #:use-module (srfi srfi-1)
  #:export (make-forester-html-style))

(define forester-stylesheet
  (make-artifact
    "forester.css"
    ".forest-tree-block { margin: 1rem 0; border-radius: .3rem; transition: background-color .12s ease; }\n.forest-tree-block:hover, .forest-tree-block:focus-within { background-color: rgba(0, 100, 255, .04); }\n.forest-tree-block summary { cursor: pointer; list-style: none; }\n.forest-tree-block summary::marker { content: ''; }\n.forest-tree-block summary::-webkit-details-marker { display: none; }\n.forest-tree-title { font-weight: 600; }\n.forest-tree-id { margin-left: .15em; font-size: .75em; opacity: .65; }\n.forest-tree-id:hover { opacity: 1; }\n.forest-tree-content { padding: .25rem .85rem; }"
    (list "modules/forester/style/html.scm")))

(define (forester-base-html-style)
  (make-html-style #:stylesheets (list forester-stylesheet)))

(define (forester-stylesheet-present? style)
  (any (lambda (artifact)
         (string=? (artifact-path artifact) "forester.css"))
       (html-style-stylesheets style)))

(define* (make-forester-html-style #:key (style (make-html-style)))
  (unless (html-style? style)
    (error "Forester HTML style requires an HTML style" style))
  (if (forester-stylesheet-present? style)
      style
      (compose-html-styles (forester-base-html-style) style)))

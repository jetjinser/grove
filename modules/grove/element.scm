;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove element)
  #:use-module (grove ir)
  #:use-module (grove string-fun)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:export (make-element))

(define (element-command tag style void? flow-kind kind anchor)
  (lambda body
    (for-each
      (lambda (argument)
        (when (keyword? argument)
          (error "custom elements accept no command arguments" tag argument)))
      body)
    (when (and void? (pair? body))
      (error "void element takes no content" tag))
    (make-content-element kind body #:style style #:flow-kind flow-kind
                          #:properties
                          (if anchor `((anchor . ,anchor)) '()))))

(define* (make-element #:key tag class (attributes '()) (flow-kind 'inline)
                       (void? #f) (anchor #f) (kind 'span))
  "Define a custom element: return a command that renders BODY inside
TAG.  The HTML shape belongs to the vocabulary author - the renderer
adds no attributes of its own and TEXT output keeps the body.  CLASS
joins the tag's class; ATTRIBUTES are static attribute pairs, so a
namespace declaration rides along as (xmlns:my . \"...\").  VOID?
emits a self-closing tag and rejects content.  FLOW-KIND (inline or
block) tells the composition whether the element flows with text or
stands as its own block; block elements are not wrapped in <p>.
ANCHOR declares the occurrence as a linkable anchor: it renders as
the tag's id and registers the element with the anchor registry, so
fragment links and queries can resolve it (a term definition, say).
KIND overrides the content element kind (default SPAN) - renderer
features that match on kinds, like #:side-lane, only see elements
whose kind is declared here."
  (unless tag
    (error "custom element requires a tag" tag))
  (unless (symbol? kind)
    (error "custom element kind must be a symbol" kind))
  (when anchor
    (unless (non-empty-string? anchor)
      (error "custom element anchor must be a non-empty string" anchor)))
  ;;     Validate at definition time, not first use.
  (let ((tag (make-html-tag tag))
        (attributes
          (make-html-attributes
            (if anchor
                (append attributes (list (cons 'id anchor)))
                attributes)))
        (void-markers (if void? (list (make-html-void)) '()))
        (class-attributes
          (if class
              (make-html-attributes `((class . ,class)))
              '())))
    (let ((style
            (make-content-style
              #f (append (list tag)
                         void-markers
                         (filter (lambda (property) (not (null? property)))
                                 (list attributes class-attributes))))))
      (element-command (html-tag-name tag) style void? flow-kind kind anchor))))

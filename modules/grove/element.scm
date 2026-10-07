;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove element)
  #:use-module (grove ir)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:export (make-element))

(define (element-command tag style void?)
  (lambda body
    (for-each
      (lambda (argument)
        (when (keyword? argument)
          (error "custom elements accept no command arguments" tag argument)))
      body)
    (when (and void? (pair? body))
      (error "void element takes no content" tag))
    (make-content-element 'span body #:style style #:flow-kind 'inline)))

(define* (make-element #:key tag class (attributes '()) (flow-kind 'inline)
                       (void? #f))
  "Define a custom element: return a command that renders BODY inside
TAG.  The HTML shape belongs to the vocabulary author - the renderer
adds no attributes of its own and TEXT output keeps the body.  CLASS
joins the tag's class; ATTRIBUTES are static attribute pairs, so a
namespace declaration rides along as (xmlns:my . \"...\").  VOID?
emits a self-closing tag and rejects content."
  (unless tag
    (error "custom element requires a tag" tag))
  ;;     Validate at definition time, not first use.
  (let ((tag (make-html-tag tag))
        (attributes (make-html-attributes attributes))
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
      (element-command (html-tag-name tag) style void?))))

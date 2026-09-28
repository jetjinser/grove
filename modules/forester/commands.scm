;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester commands)
  #:use-module (grove ir)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (title link subtree summary published updated))

(define (title . body)
  (make-content-element 'title body))

(define (summary . body)
  (make-content-element 'summary body))

(define (published date)
  (make-content-element 'published (list date)))

(define (updated date)
  (make-content-element 'updated (list date)))

(define* (link target . label)
  (make-content-element 'link
                        (if (null? label) (list target) label)
                        #:properties `((target . ,target))))

(define (subtree id . body)
  (unless (and (string? id) (not (string-null? id)))
    (error "Forester subtree ID must be a non-empty string" id))
  (let ((titles (filter (lambda (element)
                          (and (content-element? element)
                               (eq? (content-element-kind element) 'title)))
                        body)))
    (unless (= (length titles) 1)
      (error "Forester subtree requires exactly one direct @title" id
             (length titles)))
    (let* ((title-element (car titles))
           (title-content (content-element-children title-element))
           (title-text (string-trim-both (content-verbatim-string title-content))))
      (when (string-null? title-text)
        (error "Forester subtree title cannot be empty" id))
      (make-content-element
        'subtree
        (remove (lambda (element) (eq? element title-element)) body)
        #:properties `((tree-id . ,id) (title . ,title-text))))))

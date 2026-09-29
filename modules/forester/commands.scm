;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester commands)
  #:use-module (grove ir)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (title link image tag taxon subtree transclude summary published updated))

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

(define* (image source . alternative-text)
  (unless (and (string? source) (not (string-null? source)))
    (error "Forester image source must be a non-empty string" source))
  (make-content-element 'image alternative-text
                        #:properties `((source . ,source))))

(define (valid-topic-id? id)
  (and (string? id)
       (> (string-length id) 0)
       (let ((first (string-ref id 0))
             (last (string-ref id (- (string-length id) 1))))
         (and (or (char-alphabetic? first) (char-numeric? first))
              (or (char-alphabetic? last) (char-numeric? last))))
       (every (lambda (character)
                (or (char-alphabetic? character)
                    (char-numeric? character)
                    (char=? character #\-)))
              (string->list id))))

(define* (topic-element kind id label)
  (unless (valid-topic-id? id)
    (error "Forester topic ID must contain only letters, digits, and hyphens, and begin/end with a letter or digit"
           id))
  (make-content-element kind label #:properties `((id . ,id))))

(define* (tag id . label)
  (topic-element 'tag id (if (null? label) (list id) label)))

(define* (taxon id . label)
  (topic-element 'taxon id (if (null? label) (list id) label)))

(define subtree-id-unspecified (list 'subtree-id-unspecified))

(define* (subtree #:key (id subtree-id-unspecified) . arguments)
  (let* ((named? (not (eq? id subtree-id-unspecified)))
         ;; Guile's rest argument contains keyword arguments as well.
         (body (if named? (drop arguments 2) arguments)))
    (when (and named?
               (not (and (string? id)
                         (not (string-null? id))
                         (not (string-index id #\#)))))
      (error "Forester subtree ID must be a non-empty string without #" id))
    (let ((titles (filter (lambda (element)
                            (and (content-element? element)
                                 (eq? (content-element-kind element) 'title)))
                          body)))
      (unless (= (length titles) 1)
        (error "Forester subtree requires exactly one direct @title"
               (length titles)))
      (let* ((title-element (car titles))
             (title-content (content-element-children title-element))
             (title-text (string-trim-both
                           (content-verbatim-string title-content))))
        (when (string-null? title-text)
          (error "Forester subtree title cannot be empty"))
        (make-content-element
          'subtree
          (remove (lambda (element) (eq? element title-element)) body)
          #:properties
          (append `((title . ,title-text))
                  (if named? `((tree-id . ,id)) '())))))))

(define (transclude id)
  (unless (and (string? id) (not (string-null? id)))
    (error "Forester transclusion target must be a non-empty tree ID" id))
  (make-content-element 'transclude '() #:properties `((target . ,id))))

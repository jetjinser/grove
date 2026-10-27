;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html attributes)
  #:use-module (grove private html-attributes)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (content-style-html-attributes
            merge-html-attributes))

(define* (content-style-html-attributes style #:optional (id-prefix ""))
  "Return STYLE's HTML attributes, scoping IDs and local fragment links by ID-PREFIX."
  (unless (or (not style) (content-style? style))
    (error "not a content style" style))
  (unless (string? id-prefix)
    (error "HTML style ID prefix must be a string" id-prefix))
  (if (not style) '()
      (append
        (let ((name (content-style-name style)))
          (if name
              `((class . ,(if (symbol? name) (symbol->string name) name)))
              '()))
        (append-map
          (lambda (property)
            (if (html-attributes? property)
                (map (lambda (attribute)
                       (let ((name (car attribute)) (value (cdr attribute)))
                         (cons name
                           (cond
                             ((eq? name 'id) (string-append id-prefix value))
                             ((and (eq? name 'href) (string-prefix? "#" value))
                              (string-append "#" id-prefix (substring value 1)))
                             (else value)))))
                     (html-attributes-assoc property))
                '()))
          (content-style-properties style)))))

(define (html-attribute-assoc? attributes)
  (and (list? attributes)
       (every (lambda (attribute)
                (and (pair? attribute)
                     (symbol? (car attribute))
                     (string? (cdr attribute))))
              attributes)))

(define (merge-html-attributes original additions)
  "Merge style ADDITIONS into renderer-owned ORIGINAL HTML attributes.
Classes and inline styles concatenate; other style values override, except
that a style cannot replace a renderer-owned ID."
  (unless (and (valid-html-attribute-list? original)
               (html-attribute-assoc? additions))
    (error "renderer attributes must be unique symbol/string pairs, and style additions must be symbol/string pairs"
           original additions))
  (fold
    (lambda (addition attributes)
      (let* ((name (car addition))
             (value (cdr addition))
             (existing (assq name attributes)))
        (cond
          ((not existing) (append attributes (list addition)))
          ((eq? name 'id)
           (error "HTML style cannot replace a renderer-owned ID" (cdr existing) value))
          ((eq? name 'class)
           (map (lambda (attribute)
                  (if (eq? (car attribute) name)
                      (cons name (string-append (cdr attribute) " " value))
                      attribute)) attributes))
          ((eq? name 'style)
           (map (lambda (attribute)
                  (if (eq? (car attribute) name)
                      (cons name (string-append (cdr attribute) "; " value))
                      attribute)) attributes))
          (else
           (map (lambda (attribute)
                  (if (eq? (car attribute) name) addition attribute))
                attributes)))))
    original additions))

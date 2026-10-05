;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private content-index)
  #:use-module (grove string-fun)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module (forester relations)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (content->plain-text
            element-plain-text?
            build-content-node-index))

(define (element->plain-text kind properties parts)
  (case kind
    ((math-token)
     (let ((text (string-concatenate parts)))
       (if (and (eq? (assq-ref properties 'token-kind) 'mo)
                (member text '("+" "=" "×" "·" "→"
                               "≈" "<" ">" "≤" "≥")))
           (format #f " ~a " text)
           text)))
    ((math-frac)
     (format #f "(~a)/(~a)" (or (first parts) "")
             (or (second parts) "")))
    ((math-sup) (format #f "~a^(~a)" (or (first parts) "")
                        (or (second parts) "")))
    ((math-sub) (format #f "~a_(~a)" (or (first parts) "")
                        (or (second parts) "")))
    ((math-parens) (string-append "(" (string-concatenate parts) ")"))
    ((math-vector) (format #f "vec(~a)" (string-concatenate parts)))
    ((math-column) (string-join parts ", "))
    ((math-align) (string-join parts " ; "))
    (else (string-concatenate parts))))

(define (content->plain-text content)
  (cond
    ((string? content) content)
    ((content-text? content) (content-text-value content))
    ((content-layout? content) (content-layout-raw content))
    ((content-element? content)
     (element->plain-text
       (content-element-kind content)
       (content-element-properties content)
       (map content->plain-text (content-element-children content))))
    ((content-document? content)
     (string-concatenate (map content->plain-text
                              (content-document-children content))))
    (else "")))

(define (element-plain-text? element)
  (and (content-element? element)
       (every (lambda (child) (not (content-element? child)))
              (content-element-children element))))

(define (validate-topic-facts! facts)
  (let ((topics
          (filter-map
            (lambda (row)
              (and (memq (list-ref row 3) '(tag taxon))
                   (list (list-ref row 1)
                         (assq-ref (list-ref row 5) 'id)
                         (list-ref row 6))))
            facts)))
    (for-each
      (lambda (topic)
        (let ((id (cadr topic))
              (label (caddr topic)))
          (unless (and (non-empty-string? id)
                       (non-empty-string? label))
            (error "invalid topic IR element" topic))
          (unless (and (string=? id (string-downcase id))
                       (string=? id label))
            (error "topic IR must contain one canonical lowercase value" topic))))
      topics)))

(define (build-content-node-index pages)
  "Build the authored-content relation and its node-ID lookup vector."
  (let ((index '())
        (next-id 0))
    ;; Each visit returns its preorder facts alongside its accumulated text.
    ;; Reusing child text avoids recursively flattening every subtree again.
    (letrec
      ((visit
         (lambda (page content parent-id position)
           (cond
             ((not (content-element? content))
              (cons '() (content->plain-text content)))
             ((eq? (content-element-kind content) 'forest-query)
              (cons '() ""))
             (else
              (let* ((id next-id)
                     (kind (content-element-kind content))
                     (properties (content-element-properties content))
                     (children (content-element-children content)))
                (set! next-id (1+ next-id))
                (set! index (cons (cons id content) index))
                (let* ((child-results
                         (if (eq? kind 'subtree)
                             '()
                             (map (lambda (child child-position)
                                    (visit page child id child-position))
                                  children
                                  (iota (length children)))))
                       (text (if (eq? kind 'subtree)
                                 (content->plain-text content)
                                 (element->plain-text
                                   kind properties
                                   (map cdr child-results)))))
                  (cons (cons
                          (list id (site-page-id page) parent-id kind position
                                properties text (string-length text)
                                (element-plain-text? content))
                          (append-map car child-results))
                        text))))))))
      (let ((facts
              (append-map
                (lambda (page)
                  (let ((children
                          (content-document-children
                            (site-page-document page))))
                    (append-map
                      car
                      (map (lambda (child position)
                             (visit page child #f position))
                           children
                           (iota (length children))))))
                pages)))
        (validate-topic-facts! facts)
        (let ((node-index (make-vector next-id #f)))
          (for-each (lambda (entry)
                      (vector-set! node-index (car entry) (cdr entry)))
                    index)
          (values (make-content-node-relation facts) node-index))))))

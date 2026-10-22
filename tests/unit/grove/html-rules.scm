;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove html-rules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (grove ir)
  #:use-module (grove render html)
  #:use-module (grove render html rules)
  #:use-module (forester commands))

(test-begin "grove html rules")

(define (image-element)
  (image "forest.jpg" "A forest"))

(define (img-rule element children)
  `(img (@ (src ,(content-element-property element 'source))
           (alt ,(content-verbatim-string children)))))

(define (picture-rule element children)
  (let ((source (content-element-property element 'source))
        (alt (content-verbatim-string children)))
    `(picture
       (source (@ (type "image/avif")
                  (srcset ,(string-append source ".avif"))))
       (img (@ (src ,source) (alt ,alt))))))

(define base
  (make-html-rules (list (cons 'image img-rule))))

(define overlay
  (make-html-rules (list (cons 'image picture-rule))))

(test-assert "empty is the identity on the left"
  (eq? (html-rules-ref (compose-html-rules empty-html-rules base) 'image)
       img-rule))

(test-assert "empty is the identity on the right"
  (eq? (html-rules-ref (compose-html-rules base empty-html-rules) 'image)
       img-rule))

(test-assert "overlay wins"
  (eq? (html-rules-ref (compose-html-rules base overlay) 'image)
       picture-rule))

(test-assert "composition is associative"
  (let* ((a (make-html-rules (list (cons 'image img-rule))))
         (b (make-html-rules
              (list (cons 'link (lambda (element children) '(a))))))
         (c (make-html-rules (list (cons 'image picture-rule)))))
    (eq? (html-rules-ref (compose-html-rules a (compose-html-rules b c)) 'image)
         (html-rules-ref (compose-html-rules (compose-html-rules a b) c) 'image))))

(test-assert "a rule set is usable as content handlers"
  (let ((sxml (content->sxml (image-element)
                             #:handlers (html-rules->handlers
                                          (compose-html-rules base overlay)))))
    (and (pair? (memq 'picture sxml))
         (any (lambda (node)
                (and (pair? node) (eq? (car node) 'source)))
              sxml))))

(test-assert "the default rule still renders when no overlay matches"
  (let ((sxml (content->sxml (image-element)
                             #:handlers (html-rules->handlers base))))
    (and (pair? (memq 'img sxml))
         (not (memq 'picture sxml)))))

(define-html-rules dsl-rules
  (image (element children)
    (picture-rule element children)))

(test-assert "define-html-rules expands to a rule value"
  (let ((sxml (content->sxml (image-element)
                             #:handlers (html-rules->handlers dsl-rules))))
    (and (html-rules? dsl-rules)
         (pair? (memq 'picture sxml)))))

(test-error "duplicate kinds are rejected" #t
  (make-html-rules (list (cons 'image img-rule)
                         (cons 'image picture-rule))))

(test-end "grove html rules")

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove html-rules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (grove ir)
  #:use-module (grove render html)
  #:use-module (grove render html rules)
  #:use-module (grove render html presentation)
  #:use-module (grove style html)
  #:use-module (forester commands))

(test-begin "grove html rules")

(define (image-element)
  (image "forest.jpg" "A forest"))

(define (img-rule context element children next)
  `(img (@ (src ,(content-element-property element 'source))
           (alt ,(content-verbatim-string children)))))

(define (picture-rule context element children next)
  (let ((source (content-element-property element 'source))
        (alt (content-verbatim-string children)))
    (if (string=? source "forest.jpg")
        `(picture
           (source (@ (type "image/avif")
                      (srcset ,(string-append source ".avif"))))
           (img (@ (src ,source) (alt ,alt))))
        (next))))

(define (render-with rules)
  (content->sxml (image-element) #:handlers (html-rules->handlers rules)))

(define base
  (make-html-rules (list (cons 'image img-rule))))

(define overlay
  (make-html-rules (list (cons 'image picture-rule))))

(test-equal "empty is the identity on the left"
  (render-with base)
  (render-with (compose-html-rules empty-html-rules base)))

(test-equal "empty is the identity on the right"
  (render-with base)
  (render-with (compose-html-rules base empty-html-rules)))

(test-assert "overlay wins"
  (pair? (memq 'picture (render-with (compose-html-rules base overlay)))))

(test-assert "composition is associative"
  (let* ((a (make-html-rules (list (cons 'image img-rule))))
         (b (make-html-rules
              (list (cons 'link (lambda (context element children next)
                                  '(a))))))
         (c (make-html-rules (list (cons 'image picture-rule)))))
    (equal? (render-with (compose-html-rules a (compose-html-rules b c)))
            (render-with (compose-html-rules (compose-html-rules a b) c)))))

(test-assert "composition is idempotent"
  (let* ((rules (compose-html-rules base overlay))
         (again (compose-html-rules rules overlay)))
    (equal? (render-with rules) (render-with again))))

(test-assert "an overlay can delegate to the base rule"
  (let ((sxml (content->sxml
                (image "missing.jpg" "A missing forest")
                #:handlers (html-rules->handlers
                             (compose-html-rules base overlay)))))
    (and (pair? (memq 'img sxml))
         (not (memq 'picture sxml)))))

(let ((contextual
        (make-html-rules
          (list (cons 'image
                      (lambda (context element children next)
                        `(p ,(assq-ref context 'label))))))))
  (test-assert "rules receive an explicit context"
    (let ((sxml (content->sxml
                  (image-element)
                  #:handlers (html-rules->handlers
                               contextual
                               #:context '((label . "ctx"))))))
      (and (pair? (memq 'p sxml))
           (string=? "ctx" (cadr (memq 'p sxml)))))))

(test-assert "a rule set is usable as content handlers"
  (let ((sxml (render-with (compose-html-rules base overlay))))
    (and (pair? (memq 'picture sxml))
         (any (lambda (node)
                (and (pair? node) (eq? (car node) 'source)))
              sxml))))

(test-assert "the default rule still renders when no overlay matches"
  (let ((sxml (render-with base)))
    (and (pair? (memq 'img sxml))
         (not (memq 'picture sxml)))))

(define-html-rules dsl-rules
  (image (context element children next)
    (picture-rule context element children next)))

(test-assert "define-html-rules expands to a rule value"
  (let ((sxml (render-with dsl-rules)))
    (and (html-rules? dsl-rules)
         (pair? (memq 'picture sxml)))))

(test-error "duplicate kinds are rejected" #t
  (make-html-rules (list (cons 'image img-rule)
                         (cons 'image picture-rule))))

(let* ((style-a (make-html-style))
       (style-b (make-html-style))
       (a (make-html-presentation #:style style-a #:rules base))
       (b (make-html-presentation #:style style-b #:rules overlay)))
  (test-assert "presentation composition is associative"
    (let* ((left (compose-html-presentations
                   a (compose-html-presentations b empty-html-presentation)))
           (right (compose-html-presentations
                    (compose-html-presentations a b) empty-html-presentation)))
      (equal? (render-with (html-presentation-rules left))
              (render-with (html-presentation-rules right)))))
  (test-assert "presentation composition keeps the overlay rules"
    (pair? (memq 'picture
                 (render-with
                   (html-presentation-rules
                     (compose-html-presentations a b)))))))

(test-end "grove html rules")

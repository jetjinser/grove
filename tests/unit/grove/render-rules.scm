;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove render-rules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (grove ir)
  #:use-module (grove render html)
  #:use-module (grove render rules)
  #:use-module (grove render html presentation)
  #:use-module (grove render context)
  #:use-module (grove style html)
  #:use-module (grove content transform)
  #:use-module (grove render html image)
  #:use-module (grove render rules)
  #:use-module (grove render text)
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
  (content->sxml (image-element) #:handlers (render-rules->handlers rules)))

(define base
  (make-render-rules (list (cons 'image img-rule))))

(define overlay
  (make-render-rules (list (cons 'image picture-rule))))

(test-equal "empty is the identity on the left"
  (render-with base)
  (render-with (compose-render-rules empty-render-rules base)))

(test-equal "empty is the identity on the right"
  (render-with base)
  (render-with (compose-render-rules base empty-render-rules)))

(test-assert "overlay wins"
  (pair? (memq 'picture (render-with (compose-render-rules base overlay)))))

(test-assert "composition is associative"
  (let* ((a (make-render-rules (list (cons 'image img-rule))))
         (b (make-render-rules
              (list (cons 'link (lambda (context element children next)
                                  '(a))))))
         (c (make-render-rules (list (cons 'image picture-rule)))))
    (equal? (render-with (compose-render-rules a (compose-render-rules b c)))
            (render-with (compose-render-rules (compose-render-rules a b) c)))))

(test-assert "composition is idempotent"
  (let* ((rules (compose-render-rules base overlay))
         (again (compose-render-rules rules overlay)))
    (equal? (render-with rules) (render-with again))))

(test-assert "an overlay can delegate to the base rule"
  (let ((sxml (content->sxml
                (image "missing.jpg" "A missing forest")
                #:handlers (render-rules->handlers
                             (compose-render-rules base overlay)))))
    (and (pair? (memq 'img sxml))
         (not (memq 'picture sxml)))))

(let ((contextual
        (make-render-rules
          (list (cons 'image
                      (lambda (context element children next)
                        `(p ,(render-context-ref context 'page))))))))
  (test-assert "rules receive an explicit context"
    (let ((sxml (content->sxml
                  (image-element)
                  #:handlers (render-rules->handlers
                               contextual
                               #:context (make-render-context #:page "ctx")))))
      (and (pair? (memq 'p sxml))
           (string=? "ctx" (cadr (memq 'p sxml)))))))

(test-assert "a rule set is usable as content handlers"
  (let ((sxml (render-with (compose-render-rules base overlay))))
    (and (pair? (memq 'picture sxml))
         (any (lambda (node)
                (and (pair? node) (eq? (car node) 'source)))
              sxml))))

(test-assert "the default rule still renders when no overlay matches"
  (let ((sxml (render-with base)))
    (and (pair? (memq 'img sxml))
         (not (memq 'picture sxml)))))

(define-render-rules dsl-rules
  (image (context element children next)
    (picture-rule context element children next)))

(test-assert "define-render-rules expands to a rule value"
  (let ((sxml (render-with dsl-rules)))
    (and (render-rules? dsl-rules)
         (pair? (memq 'picture sxml)))))

(test-error "duplicate kinds are rejected" #t
  (make-render-rules (list (cons 'image img-rule)
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


(let* ((add-text
         (make-content-transform
           (lambda (document)
             (make-content-document
               (append (content-document-children document)
                       (list (make-content-text "x")))))))
       (document (make-content-document '()))
       (composed (compose-content-transforms add-text add-text)))
  (test-equal "content transform identity"
    0 (length (content-document-children
                (transform-document identity-content-transform document))))
  (test-equal "content transforms compose in order"
    2 (length (content-document-children
                (transform-document composed document))))
  (test-equal "content transform composition is associative"
    (content-document-children
      (transform-document
        (compose-content-transforms add-text
                                    (compose-content-transforms add-text add-text))
        document))
    (content-document-children
      (transform-document
        (compose-content-transforms
          (compose-content-transforms add-text add-text)
          add-text)
        document))))

(define-image-sets image-sets
  (image-set "forest.jpg"
    (variant "image/avif" "/images/forest.avif")
    (variant "image/jxl" "/images/forest.jxl")
    (fallback "/images/forest.jpg")))

(let* ((rules (make-render-rules
                (list (cons 'image (image-set-rule image-sets)))))
       (sxml (render-with rules)))
  (test-assert "image-set-rule emits a picture"
    (pair? (memq 'picture sxml)))
  (test-assert "image-set-rule keeps every variant"
    (= 2 (length (filter (lambda (node)
                           (and (pair? node) (eq? (car node) 'source)))
                         sxml))))
  (test-assert "image-set-rule uses the fallback in img"
    (any (lambda (node)
           (and (pair? node) (eq? (car node) 'img)))
         sxml)))

(let ((rules (make-render-rules
               (list (cons 'image
                           (lambda (context element children next)
                             "PICTURE"))))))
  (test-equal "text renderer accepts renderer rules"
    "PICTURE"
    (content->text (image-element) #:rules rules)))

(let ((rules (make-render-rules
               (list (cons 'paragraph
                           (lambda (context element children next)
                             (next)))))))
  (test-equal "next reaches the built-in HTML interpretation"
    '(p "Hello")
    (content->sxml (make-content-element 'paragraph (list "Hello"))
                   #:rules rules)))

(let* ((base (make-render-rules
               (list (cons 'paragraph
                           (lambda (context element children next)
                             (next))))))
       (overlay (make-render-rules
                  (list (cons 'paragraph
                              (lambda (context element children next)
                                (next))))))
       (rules (compose-render-rules base overlay)))
  (test-equal "next reaches the base rule then the built-in interpretation"
    '(p "Hello")
    (content->sxml (make-content-element 'paragraph (list "Hello"))
                   #:rules rules)))

(let ((rules (make-render-rules
               (list (cons 'paragraph
                           (lambda (context element children next)
                             (next)))))))
  (test-equal "next reaches the built-in text interpretation"
    "Hello\n"
    (content->text (make-content-element 'paragraph (list "Hello"))
                   #:rules rules)))

(test-end "grove html rules")

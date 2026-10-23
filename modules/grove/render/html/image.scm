;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html image)
  #:use-module (grove ir)
  #:use-module (grove render rules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (image-variant?
            make-image-variant
            image-variant-type
            image-variant-url
            image-set?
            make-image-set
            image-set-source
            image-set-variants
            image-set-fallback
            define-image-sets
            image-set-rule))

;; An image set is data: a source name, alternative encodings, and a fallback
;; URL.  It carries no codec policy; an asset pipeline produces the files.
;; The rule interprets an image element with the matching set as <picture>.

(define-record-type <image-variant>
  (make-image-variant type url)
  image-variant?
  (type image-variant-type)
  (url image-variant-url))

(define-record-type <image-set>
  (%make-image-set source variants fallback)
  image-set?
  (source image-set-source)
  (variants image-set-variants)
  (fallback image-set-fallback))

(define* (make-image-set source #:key (variants '()) (fallback #f))
  (unless (and (string? source) (not (string-null? source)))
    (error "image set source must be a non-empty string" source))
  (unless (and (list? variants) (every image-variant? variants))
    (error "image set variants must be image variants" variants))
  (unless (and (string? fallback) (not (string-null? fallback)))
    (error "image set fallback must be a non-empty string" fallback))
  (%make-image-set source variants fallback))

(define-syntax define-image-sets
  (syntax-rules ()
    ((_ name (image-set source (variant type url) ... (fallback fallback-url)) ...)
     (define name
       (list
         (cons source
               (make-image-set source
                               #:variants
                               (list (make-image-variant type url) ...)
                               #:fallback fallback-url))
         ...)))))

(define (image-set-rule sets)
  (unless (and (list? sets) (every (lambda (entry)
                                     (and (pair? entry)
                                          (string? (car entry))
                                          (image-set? (cdr entry))))
                                   sets))
    (error "image-set-rule requires an association list of image sets" sets))
  (lambda (context element children next)
    (let* ((source (content-element-property element 'source))
           (alt (content-verbatim-string children))
           (set (assoc-ref sets source)))
      (if set
          `(picture
             ,@(map (lambda (variant)
                      `(source (@ (type ,(image-variant-type variant))
                                  (srcset ,(image-variant-url variant)))))
                    (image-set-variants set))
             (img (@ (src ,(image-set-fallback set))
                     (alt ,alt))))
          (next)))))

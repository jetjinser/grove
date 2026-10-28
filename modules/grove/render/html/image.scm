;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html image)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove private snapshot)
  #:use-module (grove private string-fun)
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
  (%make-image-variant type url)
  image-variant?
  (type image-variant-type/raw)
  (url image-variant-url/raw))

(define (image-variant-type variant)
  (snapshot-data (image-variant-type/raw variant)))

(define (image-variant-url variant)
  (snapshot-data (image-variant-url/raw variant)))

(define (make-image-variant type url)
  "Construct an image source variant from its MIME TYPE and URL."
  (unless (non-empty-string? type)
    (error "image variant type must be a non-empty MIME type" type))
  (unless (non-empty-string? url)
    (error "image variant URL must be a non-empty string" url))
  (%make-image-variant (snapshot-data type) (snapshot-data url)))

(define-record-type <image-set>
  (%make-image-set source variants fallback)
  image-set?
  (source image-set-source/raw)
  (variants image-set-variants/raw)
  (fallback image-set-fallback/raw))

(define (image-set-source image-set)
  (snapshot-data (image-set-source/raw image-set)))

(define (image-set-variants image-set)
  (snapshot-data (image-set-variants/raw image-set)))

(define (image-set-fallback image-set)
  (snapshot-data (image-set-fallback/raw image-set)))

(define* (make-image-set source #:key (variants '()) fallback)
  "Describe an image source, its alternate VARIANTS, and required FALLBACK URL.
SOURCE selects the set; FALLBACK is used by the final @code{<img>} element."
  (unless (non-empty-string? source)
    (error "image set source must be a non-empty string" source))
  (unless (and (list? variants) (every image-variant? variants))
    (error "image set variants must be image variants" variants))
  (unless (non-empty-string? fallback)
    (error "image set fallback must be a non-empty string" fallback))
  (%make-image-set (snapshot-data source)
                   (snapshot-data variants)
                   (snapshot-data fallback)))

(define-syntax define-image-sets
  (syntax-rules ()
    ((_ name (image-set source (variant type url) ... (fallback fallback-url)) ...)
     (define name
       (list
         (make-image-set source
                         #:variants
                         (list (make-image-variant type url) ...)
                         #:fallback fallback-url)
         ...)))))

(define (image-set-rule sets)
  (unless (and (list? sets) (every image-set? sets))
    (error "image-set-rule requires a list of image sets" sets))
  (let ((sources (map image-set-source sets)))
    (unless (= (length sources) (length (delete-duplicates sources string=?)))
      (error "image-set-rule requires unique image sources" sources)))
  (set! sets (snapshot-data sets))
  (lambda (context element children next)
    (let* ((source (ir:content-element-property/raw element 'source))
           (alt (content-verbatim-string children))
           (set (find (lambda (candidate)
                        (string=? source (image-set-source candidate)))
                      sets)))
      (if set
          `(picture
             ,@(map (lambda (variant)
                      `(source (@ (type ,(image-variant-type variant))
                                  (srcset ,(image-variant-url variant)))))
                    (image-set-variants set))
             (img (@ (src ,(image-set-fallback set))
                     (alt ,alt))))
          (next)))))

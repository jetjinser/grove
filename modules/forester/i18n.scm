;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester i18n)
  #:use-module (grove i18n)
  #:use-module (grove ir)
  #:use-module (forester style i18n)
  #:use-module (language scribble command)
  #:use-module (language scribble flow)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (bilingual
            lang
            term
            make-content-translation
            make-content-bilingual
            bilingual-lane?
            pair-translations
            bilingual-text-handlers))

;; Bilingual authoring lowers to ordinary, cacheable Content IR:
;;
;;   bilingual        a sequence of pairs
;;     bilingual-pair  one aligned group of language lanes
;;       bilingual-lane  one language, carrying lang/dir properties
;;
;; Text direction and HTML presentation are expressed through the existing
;; properties and style APIs, so no renderer change is required. The default
;; HTML renderer emits side-by-side lanes; text output stacks them, and
;; `bilingual-text-handlers' marks each lane when passed to `content->text'.

(define (check-direction dir)
  (unless (member dir '("ltr" "rtl" "auto"))
    (error "text direction must be ltr, rtl, or auto" dir))
  dir)

(define* (make-content-translation locale children #:key (dir #f) (role #f)
                                   (style #f))
  "Build one language lane for LOCALE from decoded content CHILDREN."
  (unless (locale? locale)
    (error "translation requires a locale" locale))
  (let* ((tag (locale-tag locale))
         (dir (if dir (check-direction dir) (locale-dir locale))))
    (make-content-element 'bilingual-lane children
      #:properties `((lang . ,tag) (dir . ,dir)
                     ,@(if role `((role . ,role)) '()))
      #:style (bilingual-lane-style tag dir style)
      #:flow-kind 'block)))

(define (bilingual-lane? element)
  (and (content-element? element)
       (eq? (content-element-kind element) 'bilingual-lane)))

(define (pair-translations lanes)
  "Group consecutive language lanes into aligned pairs.

A lane whose language already appears in the current group starts a new group,
so `en zh en zh' becomes two pairs while `en zh fr' stays one three-lane group."
  (unless (and (list? lanes) (every bilingual-lane? lanes))
    (error "pair-translations requires bilingual lanes" lanes))
  (let loop ((remaining lanes) (group '()) (groups '()))
    (cond
      ((null? remaining)
       (reverse (if (null? group) groups (cons (reverse group) groups))))
      (else
       (let* ((lane (car remaining))
              (tag (content-element-property lane 'lang))
              (repeated?
                (any (lambda (lane)
                       (equal? tag (content-element-property lane 'lang)))
                     group)))
         (if repeated?
             (loop (cdr remaining) (list lane)
                   (cons (reverse group) groups))
             (loop (cdr remaining) (cons lane group) groups)))))))

(define* (make-content-bilingual lanes #:key (style #f))
  "Build a bilingual block from a flat list of language lanes."
  (unless (and (list? lanes) (every bilingual-lane? lanes))
    (error "bilingual requires language lanes" lanes))
  (make-content-element 'bilingual
    (map (lambda (group)
           (make-content-element 'bilingual-pair group
             #:style bilingual-pair-style
             #:flow-kind 'block))
         (pair-translations lanes))
    #:style (bilingual-container-style style)
    #:flow-kind 'block))

(define (whitespace-content? item)
  (cond
    ((string? item) (string-every char-whitespace? item))
    ((content-text? item)
     (string-every char-whitespace? (content-text-value item)))
    ((content-layout? item)
     (string-every char-whitespace? (content-layout-raw item)))
    (else #f)))

(define (bilingual-lanes body)
  (filter-map (lambda (item)
                (cond
                  ((bilingual-lane? item) item)
                  ((whitespace-content? item) #f)
                  (else (error "bilingual accepts only @lang blocks" item))))
              body))

(define*-public (bilingual #:key (style #f) . body)
  "Present several @lang blocks as aligned, side-by-side language lanes."
  (set! body (command-body body))
  (let ((lanes (bilingual-lanes body)))
    (unless (pair? lanes)
      (error "bilingual requires at least one @lang block"))
    (make-content-bilingual lanes #:style style)))

(define*-public (lang code #:key (dir #f) (role #f) (style #f) . body)
  "Author one language lane from a BCP 47 language tag."
  (set! body (command-body body))
  (make-content-translation (make-locale code) (decode-flow body)
    #:dir dir #:role role #:style style))

(define*-public (term code #:key (dir #f) (style #f) . body)
  "Mark inline foreign-language text with its language and direction."
  (set! body (command-body body))
  (let* ((locale (make-locale code))
         (dir (if dir (check-direction dir) (locale-dir locale)))
         (tag (locale-tag locale)))
    (make-content-element 'foreign-term body
      #:properties `((lang . ,tag) (dir . ,dir))
      #:style (foreign-term-style tag dir style))))

;; Handlers that mark each language lane when rendering to text.
(define bilingual-text-handlers
  `((bilingual . ,(lambda (element body) body))
    (bilingual-pair . ,(lambda (element body) (string-append body "\n")))
    (bilingual-lane
     . ,(lambda (element body)
          (string-append "[" (content-element-property element 'lang) "] "
                         body)))
    (foreign-term . ,(lambda (element body) body))))

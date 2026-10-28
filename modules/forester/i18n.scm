;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester i18n)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove i18n)
  #:use-module (grove ir)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (grove render rules)
  #:use-module (forester private style i18n)
  #:use-module (language scribble command)
  #:use-module (language scribble flow)
  #:use-module (ice-9 format)
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
            bilingual-text-rules))

;; Bilingual authoring lowers to ordinary, cacheable Content IR:
;;
;;   bilingual        a sequence of pairs
;;     bilingual-pair  one aligned group of language lanes
;;       bilingual-lane  one language, carrying lang/dir properties
;;
;; Text direction and HTML presentation are expressed through the existing
;; properties and style APIs, so no renderer change is required. The default
;; HTML renderer emits side-by-side lanes; text output stacks them through an
;; ordinary render-rule value that composes with project rules.

(define (check-direction dir)
  (unless (member dir '("ltr" "rtl" "auto"))
    (error "text direction must be ltr, rtl, or auto" dir))
  dir)

(define* (make-content-translation locale children #:key (dir #f) (style #f))
  "Build one language lane for LOCALE from decoded content CHILDREN."
  (unless (locale? locale)
    (error "translation requires a locale" locale))
  (let* ((tag (locale-tag locale))
         (dir (if dir (check-direction dir) (locale-dir locale))))
    (make-content-element 'bilingual-lane children
      #:properties `((lang . ,tag) (dir . ,dir))
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
              (tag (ir:content-element-property/raw lane 'lang))
              (repeated?
                (any (lambda (lane)
                       (equal? tag (ir:content-element-property/raw lane 'lang)))
                     group)))
         (if repeated?
             (loop (cdr remaining) (list lane)
                   (cons (reverse group) groups))
             (loop (cdr remaining) (cons lane group) groups)))))))

(define* (make-content-bilingual lanes #:key (style #f))
  "Build a parallel bilingual block from a flat list of language lanes."
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

(define (fold-affordance folded source-label)
  "The trailing hint inside a folded summary: a label or the folded languages."
  (if (and source-label (not (string-null? source-label)))
      source-label
      (string-join
        (map (lambda (lane) (ir:content-element-property/raw lane 'lang)) folded)
        " ")))

(define (source-summary folded source-label)
  "Build the disclosure summary for the folded lanes."
  (make-content-element 'summary
    (list (make-content-element 'bilingual-more
            (list (make-content-text
                    (fold-affordance folded source-label)))
            #:style (make-content-style #f
                      (list (make-html-tag 'span)
                            (make-html-attributes
                              '((class . "bilingual-more")))))))
    #:flow-kind 'block))

(define* (make-content-folded-bilingual lanes #:key (reader #f) (style #f)
                                        (source-label #f))
  "Build a reader-first bilingual block.

Each group keeps the reader lane visible and folds the rest into a disclosure.
READER, a locale, selects the visible language; without it the last lane of
each group is shown."
  (unless (and (list? lanes) (every bilingual-lane? lanes))
    (error "bilingual requires language lanes" lanes))
  (when (and reader (not (locale? reader)))
    (error "folded bilingual reader must be a locale" reader))
  (let ((reader-tag (and reader (locale-tag reader))))
    (make-content-element 'bilingual
      (map (lambda (group)
             (let* ((matched
                      (and reader-tag
                           (filter (lambda (lane)
                                     (locale-matches?
                                       reader-tag
                                       (ir:content-element-property/raw lane 'lang)))
                                   group)))
                    (visible
                      (if reader-tag
                          (begin
                            (when (null? matched)
                              (error
                                "bilingual reader ~a matches no lane in group ~a"
                                reader-tag
                                (map (lambda (lane)
                                       (ir:content-element-property/raw lane 'lang))
                                     group)))
                            matched)
                          (list (last group))))
                    (folded (if reader-tag
                                (remove (lambda (lane) (memq lane visible)) group)
                                (drop-right group 1))))
               (if (or (null? visible) (null? folded))
                   (make-content-element 'bilingual-pair group
                     #:style bilingual-pair-style
                     #:flow-kind 'block)
                   (make-content-element 'bilingual-pair
                     (append visible
                             (list
                               (make-content-element 'details
                                 (cons (source-summary folded source-label)
                                       folded)
                                 #:style bilingual-source-style
                                 #:flow-kind 'block)))
                     #:style bilingual-pair-style
                     #:flow-kind 'block))))
           (pair-translations lanes))
      #:style (bilingual-fold-container-style style)
      #:flow-kind 'block)))

(define (whitespace-content? item)
  (cond
    ((string? item) (string-every char-whitespace? item))
    ((content-text? item)
     (string-every char-whitespace? (ir:content-text-value/raw item)))
    ((content-layout? item)
     (string-every char-whitespace? (ir:content-layout-raw/raw item)))
    (else #f)))

(define (bilingual-lanes body)
  (filter-map (lambda (item)
                (cond
                  ((bilingual-lane? item) item)
                  ((whitespace-content? item) #f)
                  (else (error "bilingual accepts only @lang blocks" item))))
              body))

(define*-public (bilingual #:key (style #f) (mode 'fold)
                           (reader (current-reader-locale))
                           (source-label #f) . body)
  "Present several @lang blocks as language lanes.

MODE is `fold' (default) or `parallel'. In fold mode the reader's lane is shown
and doubles as the disclosure control; the other lanes fold into it. READER, a
locale, chooses the visible language, or the last lane of each group is shown
when READER is omitted. SOURCE-LABEL, when given, replaces the folded-language
hint. Parallel mode shows every lane as before."
  (set! body (command-body body))
  (let ((lanes (bilingual-lanes body)))
    (unless (pair? lanes)
      (error "bilingual requires at least one @lang block"))
    (case mode
      ((fold)
       (make-content-folded-bilingual lanes
         #:reader (and reader (make-locale reader))
         #:style style #:source-label source-label))
      ((parallel)
       (make-content-bilingual lanes #:style style))
      (else
       (error "bilingual mode must be fold or parallel" mode)))))

(define*-public (lang code #:key (dir #f) (style #f) . body)
  "Author one language lane from a BCP 47 language tag."
  (set! body (command-body body))
  (make-content-translation (make-locale code) (decode-flow body)
    #:dir dir #:style style))

(define*-public (term code #:key (dir #f) (style #f) . body)
  "Mark inline foreign-language text with its language and direction."
  (set! body (command-body body))
  (let* ((locale (make-locale code))
         (dir (if dir (check-direction dir) (locale-dir locale)))
         (tag (locale-tag locale)))
    (make-content-element 'foreign-term body
      #:properties `((lang . ,tag) (dir . ,dir))
      #:style (foreign-term-style tag dir style))))

;; A folded pair keeps the reader lane and folded lanes; the trailing hint is
;; decoration and is dropped. Plain summaries have no language and pass
;; through unlabeled. `next' preserves the generic text interpreter for kinds
;; that need no bilingual-specific treatment.
(define bilingual-text-rules
  (make-render-rules
    (list
      (cons 'bilingual (lambda (_context _element body _next) body))
      (cons 'bilingual-pair
        (lambda (_context _element body _next) (string-append body "\n")))
      (cons 'bilingual-lane
        (lambda (_context element body _next)
          (string-append "[" (ir:content-element-property/raw element 'lang) "] " body)))
      (cons 'foreign-term (lambda (_context _element _body next) (next)))
      (cons 'details (lambda (_context _element _body next) (next)))
      (cons 'summary
        (lambda (_context element body _next)
          (if (ir:content-element-property/raw element 'lang)
              (string-append "[" (ir:content-element-property/raw element 'lang) "] " body)
              body)))
      (cons 'bilingual-more (lambda (_context _element _body _next) "")))))

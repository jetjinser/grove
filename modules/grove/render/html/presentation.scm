;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html presentation)
  #:use-module (grove style html)
  #:use-module (grove render rules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (html-nav-link?
            make-html-nav-link
            html-nav-link-page-id
            html-nav-link-label
            html-nav-link-attributes
            html-home-link?
            make-html-home-link
            html-home-link-content
            html-home-link-attributes
            html-chrome?
            make-html-chrome
            empty-html-chrome
            compose-html-chromes
            html-chrome-head
            html-chrome-home-link
            html-chrome-site-nav
            html-chrome-title-decoration
            html-chrome-title-decorations
            html-chrome-metadata-items
            html-chrome-metadata-item-map
            html-presentation?
            make-html-presentation
            empty-html-presentation
            compose-html-presentations
            html-presentation-style
            html-presentation-rules
            html-presentation-chrome))

(define-record-type <html-nav-link>
  (%make-html-nav-link page-id label attributes)
  html-nav-link?
  (page-id html-nav-link-page-id)
  (label html-nav-link-label)
  (attributes html-nav-link-attributes))

(define* (make-html-nav-link page-id label #:key (attributes '()))
  "Describe a route-aware site navigation link."
  (unless (and (string? page-id) (string? label))
    (error "HTML navigation links require a page ID and string label"
           page-id label))
  (unless (valid-html-attributes? attributes)
    (error "HTML navigation attributes must be symbol/string pairs"
           attributes))
  (when (any (lambda (entry) (memq (car entry) '(href aria-current)))
            attributes)
    (error "HTML navigation attributes cannot override route state" attributes))
  (%make-html-nav-link page-id label attributes))

(define (sxml-atom? value)
  (and (not (procedure? value))
       (or (symbol? value) (string? value) (number? value)
           (char? value) (boolean? value))))

(define (unique-attribute-names? attributes)
  (let loop ((rest attributes) (seen '()))
    (or (null? rest)
        (and (not (memq (caar rest) seen))
             (loop (cdr rest) (cons (caar rest) seen))))))

(define (valid-html-attributes? attributes)
  (and (list? attributes)
       (every (lambda (entry)
                (and (pair? entry)
                     (symbol? (car entry))
                     (string? (cdr entry))))
              attributes)
       (let loop ((rest attributes) (seen '()))
         (or (null? rest)
             (and (not (memq (caar rest) seen))
                  (loop (cdr rest) (cons (caar rest) seen)))))))

(define (valid-sxml-attributes? attributes)
  (and (list? attributes)
       (every (lambda (attribute)
                (and (list? attribute)
                     (= (length attribute) 2)
                     (symbol? (car attribute))
                     (sxml-atom? (cadr attribute))))
              attributes)
       (unique-attribute-names? attributes)))

(define (static-sxml-node? node)
  (cond
    ((sxml-atom? node) #t)
    ((not (pair? node)) #f)
    ((not (list? node)) #f)
    ((not (symbol? (car node)))
     (every static-sxml-node? node))
    ((eq? (car node) '@) #f)
    ((eq? (car node) '*void*)
     (and (pair? (cdr node))
          (symbol? (cadr node))
          (let ((rest (cddr node)))
            (or (null? rest)
                (and (pair? (car rest))
                     (eq? (caar rest) '@)
                     (valid-sxml-attributes? (cdar rest))
                     (null? (cdr rest)))))))
    (else
     (let* ((rest (cdr node))
            (has-attributes? (and (pair? rest)
                                  (pair? (car rest))
                                  (eq? (caar rest) '@)))
            (attributes (and has-attributes? (cdar rest)))
            (children (if has-attributes? (cdr rest) rest)))
       (and (or (not has-attributes?)
                (valid-sxml-attributes? attributes))
            (every static-sxml-node? children))))))

(define (static-sxml-list? nodes)
  (and (list? nodes) (every static-sxml-node? nodes)))

(define-record-type <html-home-link>
  (%make-html-home-link content attributes)
  html-home-link?
  (content html-home-link-content)
  (attributes html-home-link-attributes))

(define* (make-html-home-link content #:key (attributes '()))
  "Describe the content and attributes of the routed home link.

The renderer supplies its href. CONTENT is a static SXML child list; ATTRIBUTES
is a list of symbol/string pairs."
  (unless (static-sxml-list? content)
    (error "HTML home-link content must be a static SXML list" content))
  (unless (valid-html-attributes? attributes)
    (error "HTML home-link attributes must be symbol/string pairs"
           attributes))
  (when (assq 'href attributes)
    (error "HTML home-link href is supplied by its route" attributes))
  (%make-html-home-link content attributes))

(define-record-type <html-chrome>
  (%make-html-chrome head home-link site-nav title-decorations metadata-items)
  html-chrome?
  (head html-chrome-head)
  (home-link html-chrome-home-link)
  (site-nav html-chrome-site-nav)
  (title-decorations html-chrome-title-decorations)
  (metadata-items html-chrome-metadata-item-map))

(define (valid-page-sxml-map? entries)
  (and (list? entries)
       (every (lambda (entry)
                (and (pair? entry) (string? (car entry))
                     (static-sxml-list? (cdr entry))))
              entries)
       (= (length entries)
          (length (delete-duplicates (map car entries) string=?)))))

(define* (make-html-chrome #:key (head '()) (home-link #f) (site-nav '())
                           (title-decorations '()) (metadata-items '()))
  "Build declarative HTML chrome values.

HEAD and SITE-NAV are static SXML child lists. TITLE-DECORATIONS and
METADATA-ITEMS map page IDs to static SXML child lists."
  (unless (and (static-sxml-list? head)
               (list? site-nav)
               (every (lambda (entry)
                        (or (html-nav-link? entry)
                            (static-sxml-node? entry)))
                      site-nav))
    (error "HTML chrome head and navigation must be SXML lists" head site-nav))
  (unless (or (not home-link) (html-home-link? home-link))
    (error "HTML chrome home link must be an HTML home-link value or #f"
           home-link))
  (unless (valid-page-sxml-map? title-decorations)
    (error "HTML title decorations must map page IDs to SXML lists"
           title-decorations))
  (unless (valid-page-sxml-map? metadata-items)
    (error "HTML metadata items must map page IDs to SXML lists"
           metadata-items))
  (%make-html-chrome head home-link site-nav title-decorations metadata-items))

(define empty-html-chrome (make-html-chrome))

(define (merge-page-sxml base overlay)
  (fold (lambda (entry result)
          (let ((prior (assoc (car entry) result)))
            (if prior
                (map (lambda (item)
                       (if (string=? (car item) (car entry))
                           (cons (car entry) (append (cdr item) (cdr entry)))
                           item))
                     result)
                (append result (list entry)))))
        base overlay))

(define (compose-html-chromes base overlay)
  "Compose chrome data: lists append, page fragments append by ID, and the
rightmost declared home link replaces earlier ones."
  (unless (and (html-chrome? base) (html-chrome? overlay))
    (error "HTML chromes can only be composed from HTML chrome values"
           base overlay))
  (%make-html-chrome
    (append (html-chrome-head base) (html-chrome-head overlay))
    (or (html-chrome-home-link overlay) (html-chrome-home-link base))
    (append (html-chrome-site-nav base) (html-chrome-site-nav overlay))
    (merge-page-sxml (html-chrome-title-decorations base)
                     (html-chrome-title-decorations overlay))
    (merge-page-sxml (html-chrome-metadata-item-map base)
                     (html-chrome-metadata-item-map overlay))))

(define (html-chrome-title-decoration chrome page-id)
  (let ((entry (and page-id
                    (assoc page-id
                           (html-chrome-title-decorations chrome)))))
    (if entry (cdr entry) '())))

(define (html-chrome-metadata-items chrome page-id)
  (let ((entry (and page-id
                    (assoc page-id (html-chrome-metadata-item-map chrome)))))
    (if entry (cdr entry) '())))

;; A presentation is a product of independent components: stylesheets/assets
;; and renderer rules.  Composition is componentwise, so the product monoid
;; laws reduce to the laws of each component.

(define-record-type <html-presentation>
  (%make-html-presentation style rules chrome)
  html-presentation?
  (style html-presentation-style)
  (rules html-presentation-rules)
  (chrome html-presentation-chrome))

(define* (make-html-presentation #:key (style (make-html-style))
                                 (rules empty-render-rules)
                                 (chrome empty-html-chrome))
  (unless (html-style? style)
    (error "presentation style must be an HTML style" style))
  (unless (render-rules? rules)
    (error "presentation rules must be render rules" rules))
  (unless (html-chrome? chrome)
    (error "presentation chrome must be HTML chrome" chrome))
  (%make-html-presentation style rules chrome))

(define empty-html-presentation
  (make-html-presentation))

(define (compose-html-presentations base overlay)
  "Compose presentations by appending styles and layering rules.

Styles accumulate in order; later rules take precedence and may delegate."
  (unless (and (html-presentation? base) (html-presentation? overlay))
    (error "presentations can only be composed from presentations"
           base overlay))
  (%make-html-presentation
    (compose-html-styles (html-presentation-style base)
                         (html-presentation-style overlay))
    (compose-render-rules (html-presentation-rules base)
                        (html-presentation-rules overlay))
    (compose-html-chromes (html-presentation-chrome base)
                          (html-presentation-chrome overlay))))

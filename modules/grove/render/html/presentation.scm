;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html presentation)
  #:use-module (grove style html)
  #:use-module (grove render rules)
  #:use-module (grove private snapshot)
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
            html-presentation?
            make-html-presentation
            empty-html-presentation
            compose-html-presentations
            html-presentation-style
            html-presentation-rules
            html-presentation-head
            html-presentation-home-link
            html-presentation-site-nav
            html-presentation-title-decoration
            html-presentation-title-decorations
            html-presentation-metadata-items
            html-presentation-metadata-item-map))

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
  (%make-html-nav-link (snapshot-data page-id)
                       (snapshot-data label)
                       (snapshot-data attributes)))

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
  (%make-html-home-link (snapshot-data content)
                        (snapshot-data attributes)))

(define (valid-page-sxml-map? entries)
  (and (list? entries)
       (every (lambda (entry)
                (and (pair? entry) (string? (car entry))
                     (static-sxml-list? (cdr entry))))
              entries)
       (= (length entries)
          (length (delete-duplicates (map car entries) string=?)))))

(define-record-type <html-presentation>
  (%make-html-presentation style rules head home-link site-nav
                           title-decorations metadata-items)
  html-presentation?
  (style html-presentation-style)
  (rules html-presentation-rules)
  (head html-presentation-head)
  (home-link html-presentation-home-link)
  (site-nav html-presentation-site-nav)
  (title-decorations html-presentation-title-decorations)
  (metadata-items html-presentation-metadata-item-map))

(define* (make-html-presentation #:key (style (make-html-style))
                                 (rules empty-render-rules)
                                 (head '()) (home-link #f) (site-nav '())
                                 (title-decorations '()) (metadata-items '()))
  "Build a declarative HTML presentation.

HEAD and SITE-NAV are static SXML child lists. TITLE-DECORATIONS and
METADATA-ITEMS map page IDs to static SXML child lists."
  (unless (html-style? style)
    (error "presentation style must be an HTML style" style))
  (unless (render-rules? rules)
    (error "presentation rules must be render rules" rules))
  (unless (and (static-sxml-list? head)
               (list? site-nav)
               (every (lambda (entry)
                        (or (html-nav-link? entry)
                            (static-sxml-node? entry)))
                      site-nav))
    (error "HTML presentation head and navigation must be SXML lists" head site-nav))
  (unless (or (not home-link) (html-home-link? home-link))
    (error "HTML presentation home link must be an HTML home-link value or #f"
           home-link))
  (unless (valid-page-sxml-map? title-decorations)
    (error "HTML title decorations must map page IDs to SXML lists"
           title-decorations))
  (unless (valid-page-sxml-map? metadata-items)
    (error "HTML metadata items must map page IDs to SXML lists"
           metadata-items))
  (%make-html-presentation
    style rules
    (snapshot-data head)
    (and home-link
         (%make-html-home-link
           (snapshot-data (html-home-link-content home-link))
           (snapshot-data (html-home-link-attributes home-link))))
    (map (lambda (entry)
           (if (html-nav-link? entry)
               (%make-html-nav-link
                 (snapshot-data (html-nav-link-page-id entry))
                 (snapshot-data (html-nav-link-label entry))
                 (snapshot-data (html-nav-link-attributes entry)))
               (snapshot-data entry)))
         site-nav)
    (snapshot-data title-decorations)
    (snapshot-data metadata-items)))

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

(define (html-presentation-title-decoration presentation page-id)
  (let ((entry (and page-id
                    (assoc page-id
                           (html-presentation-title-decorations presentation)))))
    (if entry (cdr entry) '())))

(define (html-presentation-metadata-items presentation page-id)
  (let ((entry (and page-id
                    (assoc page-id
                           (html-presentation-metadata-item-map presentation)))))
    (if entry (cdr entry) '())))

(define empty-html-presentation (make-html-presentation))

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
    (append (html-presentation-head base) (html-presentation-head overlay))
    (or (html-presentation-home-link overlay)
        (html-presentation-home-link base))
    (append (html-presentation-site-nav base)
            (html-presentation-site-nav overlay))
    (merge-page-sxml (html-presentation-title-decorations base)
                     (html-presentation-title-decorations overlay))
    (merge-page-sxml (html-presentation-metadata-item-map base)
                     (html-presentation-metadata-item-map overlay))))

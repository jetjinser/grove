;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html presentation)
  #:use-module (grove private alist)
  #:use-module (grove private html-attributes)
  #:use-module (grove private html-tag)
  #:use-module (grove style html)
  #:use-module (grove style html element)
  #:use-module (grove render rules)
  #:use-module (grove private snapshot)
  #:use-module (grove private procedure)
  #:use-module (grove private fingerprint)
  #:use-module (grove private files)
  #:use-module (grove private string-fun)
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
            html-presentation-title-decoration-for
            html-presentation-title-decorations
            html-presentation-metadata-items
            html-presentation-metadata-items-for
            html-presentation-metadata-item-map
            html-presentation-cache-identity
            html-presentation-page-sources
            html-presentation-cacheable?))

(define-record-type <html-nav-link>
  (%make-html-nav-link page-id label attributes)
  html-nav-link?
  (page-id html-nav-link-page-id/raw)
  (label html-nav-link-label/raw)
  (attributes html-nav-link-attributes/raw))

(define (html-nav-link-page-id link)
  (snapshot-data (html-nav-link-page-id/raw link)))

(define (html-nav-link-label link)
  (snapshot-data (html-nav-link-label/raw link)))

(define (html-nav-link-attributes link)
  (html-attributes-assoc (html-nav-link-attributes/raw link)))

(define* (make-html-nav-link page-id label #:key (attributes '()))
  "Describe a route-aware site navigation link."
  (unless (and (non-empty-string? page-id) (string? label))
    (error "HTML navigation links require a non-empty page ID and string label"
           page-id label))
  (let* ((attributes (make-html-attributes attributes))
         (assoc (html-attributes-assoc attributes)))
    (when (any (lambda (entry) (memq (car entry) '(href aria-current)))
              assoc)
      (error "HTML navigation attributes cannot override route state" assoc))
    (%make-html-nav-link (snapshot-data page-id)
                         (snapshot-data label)
                         attributes)))

(define (sxml-atom? value)
  (and (not (procedure? value))
       (or (symbol? value) (string? value) (number? value)
           (char? value) (boolean? value))))

(define (valid-sxml-attributes? attributes)
  (and (list? attributes)
       (every (lambda (attribute)
                (and (list? attribute)
                     (= (length attribute) 2)
                     (valid-html-attribute-name? (car attribute))
                     (sxml-atom? (cadr attribute))))
              attributes)
       (unique-symbol-key-alist? attributes)))

(define (valid-html-tag? tag)
  (and (symbol? tag)
       (valid-html-tag-name? (symbol->string tag))))

(define (static-sxml-tag? tag)
  (or (memq tag '(*text* *entity* comment PI))
      (valid-html-tag? tag)))

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
          (valid-html-tag? (cadr node))
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
       (and (static-sxml-tag? (car node))
            (or (not has-attributes?)
                (valid-sxml-attributes? attributes))
            (every static-sxml-node? children))))))

(define (static-sxml-list? nodes)
  (and (list? nodes) (every static-sxml-node? nodes)))

(define-record-type <html-home-link>
  (%make-html-home-link content attributes)
  html-home-link?
  (content html-home-link-content/raw)
  (attributes html-home-link-attributes/raw))

(define (html-home-link-content link)
  (snapshot-data (html-home-link-content/raw link)))

(define (html-home-link-attributes link)
  (html-attributes-assoc (html-home-link-attributes/raw link)))

(define* (make-html-home-link content #:key (attributes '()))
  "Describe the content and attributes of the routed home link.

The renderer supplies its href. CONTENT is a static SXML child list; ATTRIBUTES
is a list of symbol/string pairs."
  (unless (static-sxml-list? content)
    (error "HTML home-link content must be a static SXML list" content))
  (let* ((attributes (make-html-attributes attributes))
         (assoc (html-attributes-assoc attributes)))
    (when (assq 'href assoc)
      (error "HTML home-link href is supplied by its route" assoc))
    (%make-html-home-link (snapshot-data content) attributes)))

(define (valid-page-sxml-map? entries)
  (and (list? entries)
       (every (lambda (entry)
                (and (pair? entry) (non-empty-string? (car entry))
                     (static-sxml-list? (cdr entry))))
              entries)
       (unique-string-key-alist? entries)))

(define-record-type <html-presentation>
  (%make-html-presentation style rules head home-link site-nav
                           title-decorations metadata-items page-sources
                           cache-id)
  html-presentation?
  (style html-presentation-style)
  (rules html-presentation-rules)
  (head html-presentation-head/raw)
  (home-link html-presentation-home-link)
  (site-nav html-presentation-site-nav/raw)
  (title-decorations html-presentation-title-decorations/raw)
  (metadata-items html-presentation-metadata-item-map/raw)
  (page-sources html-presentation-page-sources/raw)
  (cache-id html-presentation-cache-id/raw))

(define (html-presentation-head presentation)
  (snapshot-data (html-presentation-head/raw presentation)))

(define (html-presentation-site-nav presentation)
  (snapshot-data (html-presentation-site-nav/raw presentation)))

(define (html-presentation-title-decorations presentation)
  (let ((value (html-presentation-title-decorations/raw presentation)))
    (if (procedure? value) value (snapshot-data value))))

(define (html-presentation-metadata-item-map presentation)
  (let ((value (html-presentation-metadata-item-map/raw presentation)))
    (if (procedure? value) value (snapshot-data value))))

(define (page-sxml-source? value)
  (or (valid-page-sxml-map? value)
      (procedure-accepts-arguments? value 2)))

(define (valid-page-sources? value)
  (or (and (filesystem-path-list? value)
           (unique-filesystem-path-list? value))
      (procedure-accepts-arguments? value 2)))

(define* (make-html-presentation #:key (style (make-html-style))
                                 (rules empty-render-rules)
                                 (head '()) (home-link #f) (site-nav '())
                                 (title-decorations '()) (metadata-items '())
                                 (page-sources '())
                                 (cache-id #f))
  "Build a declarative HTML presentation.

HEAD and SITE-NAV are static SXML child lists. TITLE-DECORATIONS and
METADATA-ITEMS may map page IDs to static SXML child lists, or be procedures
of FOREST and PAGE-ID that return such a list. PAGE-SOURCES is a path list or
a procedure of FOREST and PAGE-ID returning additional source paths for the
page artifact. Dynamic page projections are cacheable only when CACHE-ID
identifies their stable behavior."
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
  (unless (page-sxml-source? title-decorations)
    (error "HTML title decorations must map page IDs to SXML lists or accept a forest and page ID"
           title-decorations))
  (unless (page-sxml-source? metadata-items)
    (error "HTML metadata items must map page IDs to SXML lists or accept a forest and page ID"
           metadata-items))
  (unless (valid-page-sources? page-sources)
    (error "HTML page sources must be unique paths or accept a forest and page ID"
           page-sources))
  (unless (or (not cache-id) (fingerprintable-datum? cache-id))
    (error "HTML presentation cache ID must be stable Scheme data or #f"
           cache-id))
  (%make-html-presentation
    style rules
    (snapshot-data head)
    (and home-link
         (%make-html-home-link
           (snapshot-data (html-home-link-content home-link))
           (make-html-attributes
             (html-home-link-attributes home-link))))
    (map (lambda (entry)
           (if (html-nav-link? entry)
               (%make-html-nav-link
                 (snapshot-data (html-nav-link-page-id entry))
                 (snapshot-data (html-nav-link-label entry))
                 (make-html-attributes
                   (html-nav-link-attributes entry)))
               (snapshot-data entry)))
         site-nav)
    (if (procedure? title-decorations) title-decorations
        (snapshot-data title-decorations))
    (if (procedure? metadata-items) metadata-items
        (snapshot-data metadata-items))
    (if (procedure? page-sources) page-sources
        (snapshot-data page-sources))
    (and cache-id (snapshot-data cache-id))))

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

(define (page-sxml-fragments entries forest page-id who)
  (let ((fragments
          (if (procedure? entries)
              (entries forest page-id)
              (let ((entry (and page-id (assoc page-id entries))))
                (if entry (snapshot-data (cdr entry)) '())))))
    (unless (static-sxml-list? fragments)
      (error "dynamic HTML presentation must return a static SXML list"
             who page-id fragments))
    (snapshot-data fragments)))

(define (page-sxml-map-identity entries cache-id)
  (if (procedure? entries)
      (and cache-id (list 'dynamic-page-sxml cache-id))
      (snapshot-data entries)))

(define (html-presentation-cacheable? presentation)
  (or (not (or (procedure? (html-presentation-title-decorations/raw presentation))
               (procedure? (html-presentation-metadata-item-map/raw presentation))
               (procedure? (html-presentation-page-sources/raw presentation))))
      (html-presentation-cache-id/raw presentation)))

(define (html-presentation-cache-identity presentation)
  (let ((cache-id (html-presentation-cache-id/raw presentation)))
    (list (page-sxml-map-identity
            (html-presentation-title-decorations/raw presentation) cache-id)
          (page-sxml-map-identity
            (html-presentation-metadata-item-map/raw presentation) cache-id)
          (if (procedure? (html-presentation-page-sources/raw presentation))
              (and cache-id (list 'dynamic-page-sources cache-id))
              (snapshot-data (html-presentation-page-sources/raw presentation))))))

(define (html-presentation-page-sources presentation forest page-id)
  (let* ((source-spec (html-presentation-page-sources/raw presentation))
         (sources (if (procedure? source-spec)
                      (source-spec forest page-id)
                      source-spec)))
    (unless (and (filesystem-path-list? sources)
                 (unique-filesystem-path-list? sources))
      (error "HTML page source projection must return unique paths"
             page-id sources))
    (snapshot-data sources)))

(define (page-sxml-fragments/static entries page-id)
  (let ((entry (and page-id (assoc page-id entries))))
    (if entry (snapshot-data (cdr entry)) '())))

(define (html-presentation-title-decoration presentation page-id)
  (if (procedure? (html-presentation-title-decorations/raw presentation))
      (error "dynamic title decoration requires a forest; use html-presentation-title-decoration-for")
      (page-sxml-fragments/static
        (html-presentation-title-decorations/raw presentation) page-id)))

(define (html-presentation-title-decoration-for presentation forest page-id)
  (page-sxml-fragments (html-presentation-title-decorations/raw presentation)
                       forest page-id 'title-decoration))

(define (html-presentation-metadata-items presentation page-id)
  (if (procedure? (html-presentation-metadata-item-map/raw presentation))
      (error "dynamic metadata items require a forest; use html-presentation-metadata-items-for")
      (page-sxml-fragments/static
        (html-presentation-metadata-item-map/raw presentation) page-id)))

(define (html-presentation-metadata-items-for presentation forest page-id)
  (page-sxml-fragments (html-presentation-metadata-item-map/raw presentation)
                       forest page-id 'metadata-items))

(define (merge-page-sxml-sources base overlay)
  (if (or (procedure? base) (procedure? overlay))
      (lambda (forest page-id)
        (append (page-sxml-fragments base forest page-id 'base-presentation)
                (page-sxml-fragments overlay forest page-id 'overlay-presentation)))
      (merge-page-sxml base overlay)))

(define (merge-page-path-sources base overlay)
  (if (or (procedure? base) (procedure? overlay))
      (lambda (forest page-id)
        (delete-duplicates
          (append (if (procedure? base) (base forest page-id) base)
                  (if (procedure? overlay) (overlay forest page-id) overlay))
          string=?))
      (delete-duplicates (append base overlay) string=?)))

(define (page-sxml-source-identity presentation)
  (html-presentation-cache-identity presentation))

(define empty-html-presentation (make-html-presentation))

(define (compose-html-presentation-pair base overlay)
  (unless (and (html-presentation? base) (html-presentation? overlay))
    (error "presentations can only be composed from presentations"
           base overlay))
  (let* ((base-dynamic? (not (html-presentation-cacheable? base)))
         (overlay-dynamic? (not (html-presentation-cacheable? overlay)))
         (dynamic? (or (procedure? (html-presentation-title-decorations/raw base))
                    (procedure? (html-presentation-metadata-item-map/raw base))
                    (procedure? (html-presentation-page-sources/raw base))
                    (procedure? (html-presentation-title-decorations/raw overlay))
                    (procedure? (html-presentation-metadata-item-map/raw overlay))
                    (procedure? (html-presentation-page-sources/raw overlay))))
         (cache-id
           (and dynamic?
                (not base-dynamic?)
                (not overlay-dynamic?)
                (list 'composed-page-presentation
                      (page-sxml-source-identity base)
                      (page-sxml-source-identity overlay)
                      (html-presentation-cache-id/raw base)
                      (html-presentation-cache-id/raw overlay)))))
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
    (merge-page-sxml-sources
      (html-presentation-title-decorations/raw base)
      (html-presentation-title-decorations/raw overlay))
    (merge-page-sxml-sources
      (html-presentation-metadata-item-map/raw base)
      (html-presentation-metadata-item-map/raw overlay))
    (merge-page-path-sources
      (html-presentation-page-sources/raw base)
      (html-presentation-page-sources/raw overlay))
    cache-id)))

(define (compose-html-presentations . presentations)
  "Compose PRESENTATIONS from left to right, applying each field's law.

Styles, rules, head/navigation nodes, and page fragments accumulate in order;
the rightmost home link wins. With no arguments, return the empty presentation."
  (unless (every html-presentation? presentations)
    (error "presentations can only be composed from presentations"
           presentations))
  (fold (lambda (presentation base)
          (compose-html-presentation-pair base presentation))
        empty-html-presentation
        presentations))

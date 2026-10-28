;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render html)
  #:use-module (grove private render html)
  #:use-module (grove render rules)
  #:use-module (grove render context)
  #:use-module (grove highlight)
  #:use-module (sxml simple)
  #:export (content->sxml
            sxml->html))

(define* (content->sxml content #:key (id-prefix "")
                        (highlighters (default-highlighters))
                        (rules empty-render-rules)
                        (context (make-render-context)))
  "Render CONTENT to SXML using public render rules and context."
  (unless (render-rules? rules)
    (error "not a render rule set" rules))
  (unless (render-context? context)
    (error "not a render context" context))
  (unless (string? id-prefix)
    (error "HTML style ID prefix must be a string" id-prefix))
  (content->sxml/internal content #:id-prefix id-prefix
    #:highlighters highlighters #:rules rules #:context context))

(define (sxml-void? node)
  (and (pair? node) (eq? (car node) '*void*)))

(define (write-html-attributes attributes port)
  (for-each
    (lambda (attribute)
      (display " " port)
      (display (car attribute) port)
      (display "=\"" port)
      (sxml->xml (cadr attribute) port)
      (display "\"" port))
    attributes))

(define html-raw-text-tags
  ;;     HTML raw-text elements: their children are data, not markup,
  ;;     so entity escaping would corrupt them (a script containing
  ;;     `&&` must not become `&amp;&amp;`).
  '(script style))

(define html-void-tags
  ;;     HTML void elements have no end tag, so a childless occurrence
  ;;     serializes open-only.  Every OTHER childless element must get
  ;;     an explicit end tag: HTML parsing ignores the slash on a
  ;;     self-closing non-void start tag, which then stays open and
  ;;     swallows the markup that follows it.
  '(area base br col embed hr img input link meta param source track
    wbr))

(define (write-html node port)
  ;;     The HTML serializer.  Element markup is hand-emitted so that
  ;;     childless non-void elements get explicit end tags: HTML
  ;;     parsing ignores the slash on a self-closing non-void start
  ;;     tag, which then stays open and swallows the markup that
  ;;     follows it.  Author-void markers and raw-text elements are
  ;;     special-cased; strings, attribute values, and sxml special
  ;;     nodes escape through sxml->xml exactly as before.
  (cond
    ((not (pair? node)) (sxml->xml node port))
    ((and (pair? node) (not (symbol? (car node))))
     ;;     A fragment - a list of sibling nodes (the page body is
     ;;     one): emit the first node as text and continue with the
     ;;     rest, matching sxml->xml's node-list handling.
     (write-html (car node) port)
     (write-html (cdr node) port))
    ((sxml-void? node)
     (let* ((tag (cadr node))
            (rest (cddr node))
            (attributes
              (if (and (pair? rest) (pair? (car rest))
                       (eq? (caar rest) '@))
                  (cdar rest)
                  '())))
       (display "<" port)
       (display tag port)
       (write-html-attributes attributes port)
       (display "/>" port)))
    ((memq (car node) html-raw-text-tags)
     (let* ((rest (cdr node))
            (has-attributes?
              (and (pair? rest) (pair? (car rest))
                   (eq? (caar rest) '@)))
            (attributes (if has-attributes? (cdar rest) '()))
            (children (if has-attributes? (cdr rest) rest)))
       (display "<" port)
       (display (car node) port)
       (write-html-attributes attributes port)
       (display ">" port)
       (for-each
         (lambda (child)
           (cond
             ((string? child) (display child port))
             ((pair? child) (write-html child port))
             (else (sxml->xml child port))))
         children)
       (display "</" port)
       (display (car node) port)
       (display ">" port)))
    ((memq (car node) '(*text* *entity* comment PI))
     (sxml->xml node port))
    (else
     (let* ((rest (cdr node))
            (has-attributes?
              (and (pair? rest) (pair? (car rest))
                   (eq? (caar rest) '@)))
            (attributes (if has-attributes? (cdar rest) '()))
            (children (if has-attributes? (cdr rest) rest)))
       (display "<" port)
       (display (car node) port)
       (write-html-attributes attributes port)
       (if (and (null? children) (memq (car node) html-void-tags))
           (display " />" port)
           (begin
             (display ">" port)
             (for-each (lambda (child) (write-html child port)) children)
             (display "</" port)
             (display (car node) port)
             (display ">" port)))))))

(define (sxml->html sxml)
  (call-with-output-string (lambda (port) (write-html sxml port))))

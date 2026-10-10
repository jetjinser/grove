;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester browse)
  #:use-module (grove ir)
  #:use-module (grove render text)
  #:use-module (grove site)
  #:use-module (forester site)
  #:use-module (forester backmatter)
  #:use-module (forester i18n)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (browse-forest
            make-forest-browser))

;; Content kinds the terminal projection understands. Anything outside this set
;; is still rendered as its children, but the page view names it so that no
;; authored structure is dropped without a diagnostic.
(define terminal-known-kinds
  '(paragraph summary disclosure subtree transclude query-transclude
    math-derivation blockquote item itemize code code-block verbatim bold
    math math-token math-frac math-sup math-sub math-vector math-parens
    math-column math-align math-row math-cell
    math-diagnostic math-diagnostics math-semantic-check math-style
    bilingual bilingual-pair bilingual-lane bilingual-more foreign-term
    details summary
    link query-link tag taxon image forest-query title))

(define (unknown-content-kinds content)
  "Return the distinct element kinds in CONTENT that the terminal cannot project."
  (let ((seen '()))
    (let walk ((node content))
      (cond
        ((content-element? node)
         (let ((kind (content-element-kind node)))
           (unless (or (memq kind terminal-known-kinds) (memq kind seen))
             (set! seen (cons kind seen)))
           (for-each walk (content-element-children node))))
        ((content-document? node)
         (for-each walk (content-document-children node)))))
    (reverse seen)))

(define (tree-target element)
  (content-element-property element
    (if (eq? (content-element-kind element) 'subtree) 'tree-id 'target)))

(define (display-title title output)
  (format output "~a~%~a~%" title (make-string (string-length title) #\=)))

(define (display-metadata page output)
  (for-each
    (lambda (entry)
      (when (and (pair? entry) (cdr entry))
        (format output "~a: ~a~%" (symbol->string (car entry)) (cdr entry))))
    (site-page-metadata page)))

(define (terminal-content-handlers add-entry! references)
  (append
   bilingual-text-handlers
   `((tag . ,(lambda (_element _body) ""))
    (taxon . ,(lambda (_element _body) ""))
    (bold . ,(lambda (_element body) (string-append "*" body "*")))
    (verbatim . ,(lambda (_element body) (string-append "`" body "`")))
    (itemize . ,(lambda (_element body) body))
    (math-style . ,(lambda (_element body) body))
    (math-semantic-check . ,(lambda (_element body) body))
    (math-diagnostic . ,(lambda (_element body)
                         (string-append "[math check: " body "]")))
    (math-diagnostics . ,(lambda (_element body) body))
    (image . ,(lambda (element body)
                (let ((alt (string-trim-both (or body "")))
                      (source (content-element-property element 'source)))
                  (if (string-null? alt)
                      (format #f "[image: ~a]" source)
                      (format #f "[image: ~a — ~a]" alt source)))))
    (link . ,(lambda (_element body)
               (if (null? references)
                   body
                   (let* ((reference (car references))
                          (target (reference-target-page-id reference)))
                     (set! references (cdr references))
                     (format #f "[~a] ~a" (add-entry! 'link body target) body)))))
    (query-link . ,(lambda (element body)
                     (format #f "[~a] ~a"
                             (add-entry! 'link body
                                         (content-element-property element 'target))
                             body))))))

(define (render-content forest page add-entry! expanded)
  "Render PAGE's body to a string, recording navigable entries."
  (define references (outgoing-references forest (site-page-id page)))
  (define (render-tree element _render)
    (let* ((kind (content-element-kind element))
           (target (tree-target element))
           (target-page (find-page forest target))
           (position (add-entry! kind (site-page-title target-page) target)))
      (if (and (memq kind '(transclude query-transclude))
               (member target expanded string=?))
          (string-append
            (format #f ">= [~a] ~a (transclusion)~%"
                    position (site-page-title target-page))
            (render-content forest target-page add-entry! expanded)
            (format #f "-- end ~a --~%" (site-page-title target-page)))
          (format #f "~a [~a] ~a  (~a)~%"
                  (case kind ((subtree) "@") ((transclude) ">") (else "?"))
                  position (site-page-title target-page)
                  (case kind ((subtree) "tree")
                             ((transclude) "transclusion")
                             (else "query"))))))
  (content->text (site-page-document page)
    #:handlers (terminal-content-handlers add-entry! references)
    #:lazy-handlers (list (cons 'subtree render-tree)
                          (cons 'transclude render-tree)
                          (cons 'query-transclude render-tree))))

(define (outline-lines forest id depth)
  ;;     Indented titles of the tree's own subtrees and transclusions -
  ;;     the terminal counterpart of the HTML backmatter outline.  Two
  ;;     levels keep chapter-sized trees from flooding the screen; the
  ;;     depth bound also ends transclusion cycles.
  (append-map
    (lambda (element)
      (let* ((target-id (tree-occurrence-target element))
             (head
               (format #f "~a~a ~a"
                       (make-string (* 2 depth) #\space)
                       (case (content-element-kind element)
                         ((subtree) "@") ((transclude) ">") (else "?"))
                       (site-page-title (find-page forest target-id)))))
        (if (< depth 2)
            (cons head (outline-lines forest target-id (+ depth 1)))
            (list head))))
    (tree-occurrences forest id)))

(define (display-backmatter forest page add-entry! output)
  (define (section label targets)
    (unless (null? targets)
      (format output "~%~a:~%" label)
      (for-each
        (lambda (target)
          (format output "  [~a] ~a~%"
                  (add-entry! 'related (site-page-title target)
                              (site-page-id target))
                  (site-page-title target))
          (for-each
            (lambda (line)
              (format output "      ~a~%" line))
            (outline-lines forest (site-page-id target) 0)))
        targets)))
  (section "Related" (related-pages forest (site-page-id page)))
  (section "References" (reference-pages forest (site-page-id page)))
  (section "Backlinks" (backlinks forest (site-page-id page)))
  (section "Context" (context-pages forest (site-page-id page))))

(define (render-page forest page expanded output)
  "Write PAGE and return its navigable entries as (kind label target) lists."
  (define entries '())
  (define (add-entry! kind label target)
    (let ((position (+ 1 (length entries))))
      (set! entries (cons (list kind label target) entries))
      position))
  (display-title (site-page-title page) output)
  (display-metadata page output)
  (let ((metadata (site-page-metadata page)))
    (when (any (lambda (entry) (and (pair? entry) (cdr entry))) metadata)
      (newline output)))
  (display (render-content forest page add-entry! expanded) output)
  (display-backmatter forest page add-entry! output)
  (let ((unknown (unknown-content-kinds (site-page-document page))))
    (unless (null? unknown)
      (format output "~%[unsupported elements not fully projected: ~a]~%"
              (string-join (map symbol->string unknown) ", "))))
  (reverse entries))

(define (display-prompt output)
  (format output "~%[n] follow  b back  h home  xN expand  q quit > ")
  (force-output output))

(define* (browse-forest forest #:key start
                        (input (current-input-port))
                        (output (current-output-port)))
  "Browse FOREST in the terminal, starting at START or the site home page."
  (unless (forest? forest)
    (error "terminal browser requires a forest" forest))
  (let* ((pages (pages forest))
         (start-id (or start (site-home (site forest))
                       (and (pair? pages) (site-page-id (car pages))))))
    (unless start-id
      (error "forest has no pages to browse"))
    (find-page forest start-id)
    (let loop ((current start-id) (history '()) (expanded '()))
      (let* ((entries (render-page forest (find-page forest current) expanded
                                   output))
             (_ (display-prompt output))
             (answer (read-line input)))
        (cond
          ((or (eof-object? answer) (string=? (string-trim-both answer) "q"))
           #t)
          ((string=? (string-trim-both answer) "b")
           (if (pair? history)
               (loop (car history) (cdr history) expanded)
               (begin (display "No previous page.\n" output)
                      (loop current history expanded))))
          ((string=? (string-trim-both answer) "h")
           (loop start-id (if (string=? current start-id) history
                              (cons current history))
                 expanded))
          ((string-prefix? "x" (string-trim-both answer))
           (let* ((text (string-drop (string-trim-both answer) 1))
                  (number (string->number text))
                  (entry (and (integer? number) (exact? number)
                              (<= 1 number) (<= number (length entries))
                              (list-ref entries (1- number)))))
             (cond
               ((and entry (eq? (car entry) 'subtree))
                (display "Only transclusions can expand inline.\n" output)
                (loop current history expanded))
               ((and entry (memq (car entry) '(transclude query-transclude)))
                (let ((target (caddr entry)))
                  (loop current history
                        (if (member target expanded string=?)
                            (remove (lambda (id) (string=? id target)) expanded)
                            (cons target expanded)))))
               (else
                (display "Choose an expandable transclusion number.\n" output)
                (loop current history expanded)))))
          ((let ((number (string->number (string-trim-both answer))))
             (and (integer? number) (exact? number)
                  (<= 1 number) (<= number (length entries))))
           (loop (caddr (list-ref entries
                                  (- (string->number (string-trim-both answer)) 1)))
                 (cons current history) expanded))
          (else
           (display "Choose a listed number, b, h, xN, or q.\n" output)
           (loop current history expanded)))))))

(define* (make-forest-browser #:key start)
  "Return a project browse callback over a Forest."
  (lambda (forest . arguments)
    (apply browse-forest forest (append (list #:start start) arguments))))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove highlight)
  #:use-module (ts)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (make-highlighter highlighter? highlight-code
            highlighter-configuration highlighter-inputs))

(define-record-type <highlighter>
  (%make-highlighter language query patterns configuration inputs)
  highlighter?
  (language highlighter-language)
  (query highlighter-query)
  (patterns highlighter-patterns)
  (configuration highlighter-configuration)
  (inputs highlighter-inputs))

(define (byte-text bytes start end)
  (let ((slice (make-bytevector (- end start))))
    (bytevector-copy! bytes start slice 0 (- end start))
    (utf8->string slice)))

;; Predicates are host-side operations: Tree-sitter only returns their syntax.
(define (compile-predicate predicate)
  (define (texts name captures bytes)
    (filter-map
      (lambda (capture)
        (and (string=? name (car capture))
             (byte-text bytes (ts-node-start-byte (cdr capture))
                              (ts-node-end-byte (cdr capture))))) captures))
  (match predicate
    ((('string . (? (lambda (name) (member name '("eq?" "not-eq?" "match?" "not-match?"))) name))
      ('capture . capture) (kind . operand))
     (cond
       ((member name '("eq?" "not-eq?"))
        (unless (memq kind '(string capture)) (error "invalid equality operand" predicate))
        (lambda (captures bytes)
          (let* ((left (texts capture captures bytes))
                 (right (if (eq? kind 'capture) (texts operand captures bytes)
                            (make-list (length left) operand))))
            (and (pair? left) (= (length left) (length right))
                 (every (lambda (a b)
                          (if (string=? name "eq?") (string=? a b)
                              (not (string=? a b)))) left right)))))
       ((member name '("match?" "not-match?"))
        (unless (eq? kind 'string) (error "invalid regex operand" predicate))
        (let ((regex (make-regexp operand regexp/extended)))
          (lambda (captures bytes)
            (let ((values (texts capture captures bytes)))
              (and (pair? values)
                   (every (lambda (value)
                            (if (string=? name "match?")
                                (regexp-exec regex value)
                                (not (regexp-exec regex value)))) values))))))
       (else (error "unsupported highlight predicate" predicate))))
    ((('string . name) ('capture . capture) operands ...)
     (unless (and (member name '("any-of?" "not-any-of?"))
                  (pair? operands) (every (lambda (x) (eq? (car x) 'string)) operands))
       (error "unsupported highlight predicate" predicate))
     (lambda (captures bytes)
       (let ((values (texts capture captures bytes)))
         (and (pair? values)
              (every (lambda (value)
                       (if (string=? name "any-of?") (member value (map cdr operands))
                           (not (member value (map cdr operands))))) values)))))
    (_ (error "unsupported highlight predicate" predicate))))

(define (compile-pattern query index)
  (let ((priority 100) (predicates '()))
    (for-each
      (lambda (predicate)
        (match predicate
          ((('string . "set!") ('string . "priority") ('string . value))
           (let ((number (string->number value)))
             (unless (exact-integer? number) (error "invalid highlight priority" value))
             (set! priority number)))
          (_ (set! predicates (cons (compile-predicate predicate) predicates)))))
      (ts-query-predicates-for-pattern query index))
    (cons priority predicates)))

(define (make-highlighter library entry query-source)
  "Load a grammar library and compile its highlight query once.
Supported predicates: eq?, not-eq?, match?, not-match?, any-of?, not-any-of?.
Regexes use Guile's POSIX extended syntax. Only set! priority is supported;
other predicates/directives are rejected rather than silently ignored."
  (let* ((language (get-ts-language-from-file library entry))
         (query (ts-query-new language query-source)))
    (%make-highlighter language query
      (list->vector (map (lambda (index) (compile-pattern query index))
                        (iota (ts-query-pattern-count query))))
      (list library entry query-source)
      (if (file-exists? library) (list (canonicalize-path library)) '()))))

;; Each span is #(start end category priority pattern). Larger priority wins;
;; ties prefer the later query pattern, then the narrower capture.
(define (span-before? a b)
  (or (> (vector-ref a 3) (vector-ref b 3))
      (and (= (vector-ref a 3) (vector-ref b 3))
           (or (> (vector-ref a 4) (vector-ref b 4))
               (and (= (vector-ref a 4) (vector-ref b 4))
                    (< (- (vector-ref a 1) (vector-ref a 0))
                       (- (vector-ref b 1) (vector-ref b 0))))))))

(define (capture-spans highlighter source bytes)
  (let* ((parser (ts-parser-new #:language (highlighter-language highlighter)))
         (tree (ts-parser-parse-string parser source))
         (query (highlighter-query highlighter))
         (cursor (ts-query-cursor-new)))
    (ts-query-cursor-exec cursor query (ts-tree-root-node tree))
    (let loop ((spans '()))
      (let ((result (ts-query-cursor-next-match cursor)))
        (if (not result) spans
            (let* ((index (ts-query-match-pattern-index result))
                   (pattern (vector-ref (highlighter-patterns highlighter) index))
                   (captures
                     (map (lambda (capture)
                            (cons (ts-query-capture-name-for-id query (cdr capture))
                                  (car capture))) (ts-query-match-captures result))))
              (loop
                (if (every (lambda (predicate) (predicate captures bytes)) (cdr pattern))
                    (append
                      (filter-map
                        (lambda (capture)
                          (let ((name (car capture)) (node (cdr capture)))
                            (and (not (string-prefix? "_" name))
                                 (< (ts-node-start-byte node) (ts-node-end-byte node))
                                 (vector (ts-node-start-byte node) (ts-node-end-byte node)
                                         name (car pattern) index)))) captures)
                      spans)
                    spans))))))))

(define (highlight-code highlighter source)
  "Return ordered (CATEGORY . TEXT) pieces; CATEGORY is a capture name or #f.
Concatenating their text recovers SOURCE exactly, including malformed code.
Underscore-prefixed captures are used only for predicates, not styling."
  (let* ((bytes (string->utf8 source))
         (spans (capture-spans highlighter source bytes))
         (events (sort (append-map
                         (lambda (span)
                           (list (cons (vector-ref span 0) span)
                                 (cons (vector-ref span 1) span))) spans)
                       (lambda (a b) (< (car a) (car b))))))
    (define (emit start end active pieces)
      (if (= start end) pieces
          (let* ((category (and (pair? active) (vector-ref (car active) 2)))
                 (text (byte-text bytes start end)))
            (if (and (pair? pieces) (equal? category (caar pieces)))
                (cons (cons category (string-append (cdar pieces) text)) (cdr pieces))
                (cons (cons category text) pieces)))))
    (let loop ((events events) (position 0) (active '()) (pieces '()))
      (if (null? events)
          (reverse (emit position (bytevector-length bytes) active pieces))
          (let* ((boundary (caar events))
                 (pieces (emit position boundary active pieces)))
            (let update ((rest events) (active active))
              (if (and (pair? rest) (= boundary (caar rest)))
                  (let ((span (cdar rest)))
                    (update (cdr rest)
                      (if (= boundary (vector-ref span 0)) (cons span active)
                          (delq span active))))
                  (loop rest boundary (sort active span-before?) pieces))))))))

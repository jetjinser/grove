;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math rewrite)
  #:use-module (grove relation)
  #:use-module (grove math rule)
  #:use-module (grove math term)
  #:use-module (grove math vector)
  #:use-module (grove math identities)
  #:use-module (grove math arithmetic)
  #:use-module (grove math differentiation)
  #:use-module (grove math components)
  #:use-module (grove math spatial)
  #:use-module (grove math integration)
  #:use-module (grove math partial)
  #:use-module (grove math normalization)
  #:use-module (srfi srfi-1)
  #:export (rewriteo rewrite-stepo check-stepo unrewrite-rooto unrewrite-stepo
            math-rewrite-rules math-simplification-rules math-calculation-rules
            math-collection-rules math-backward-rules math-rewrite-rule-ref
            math-rule-names math-default-rules math-rewrite-innermost
            math-rule-required-facts math-selected-rules math-rewrite-answers
            rewrite-curl-curl)
  #:re-export (curl-curlo math-rewrite-rule? math-rewrite-rule-name
               math-rewrite-rule-equation math-rewrite-rule-condition
               math-rewrite-rule-condition-checked? math-rewrite-rule-group
               math-rewrite-rule-modes math-rewrite-rule-spec
               make-math-rule-spec math-rule-spec? math-rule-spec-left
               math-rule-spec-right math-rule-spec-premises math-rule-spec-guards
               math-rule-spec-match math-rule-spec-instantiate
               math-rule-spec-guard-kinds math-rule-spec-opaque-guards))

(define math-rewrite-rules
  (append math-vector-rules math-identity-rules math-differentiation-rule-definitions
          math-arithmetic-rules math-normalization-rules math-partial-rule-definitions
          math-component-rules math-spatial-rules math-polynomial-rules))

(define (math-rewrite-rule-ref name)
  (find (lambda (rule) (eq? name (math-rewrite-rule-name rule))) math-rewrite-rules))

(define* (math-rule-names #:key (mode 'forward) (groups #f))
  (map math-rewrite-rule-name
    (filter (lambda (rule)
              (and (memq mode (math-rewrite-rule-modes rule))
                   (or (not groups) (memq (math-rewrite-rule-group rule) groups))))
            math-rewrite-rules)))

(define math-simplification-rules (math-rule-names #:groups '(identity calculate collect normalize cancel)))
(define math-default-rules
  (map math-rewrite-rule-name
    (filter (lambda (rule) (not (memq (math-rewrite-rule-group rule)
                                      '(expand components spatial polynomial-calculus polynomial-algebra))))
            math-rewrite-rules)))
(define math-calculation-rules (math-rule-names #:groups '(calculate)))
(define math-collection-rules (math-rule-names #:groups '(collect)))
(define math-backward-rules
  (map math-rewrite-rule-name (filter math-rewrite-rule-backward-default? math-rewrite-rules)))

(define (math-rule-required-facts name input)
  (let ((rule (math-rewrite-rule-ref name)))
    (and rule ((math-rewrite-rule-required-facts rule) input))))

(define* (step-in-modeo assumptions input output rule path mode #:optional (rules math-rewrite-rules))
  (conde
    ((math-rules-stepo rules assumptions input output rule mode)
     (== path '()))
    ((fresh (operator arguments rewritten child-path)
       ;; An inverse query walks the known right side, a forward/check query
       ;; walks the left side. Operand indexing is identical in both cases.
       (if (eq? mode 'backward)
           (== `(,operator . ,arguments) output)
           (== `(,operator . ,arguments) input))
       (children-in-modeo assumptions arguments 0 rewritten rule child-path mode rules)
       (if (eq? mode 'backward)
           (== `(,operator . ,rewritten) input)
           (== `(,operator . ,rewritten) output))
       (== path child-path)))))

(define (children-in-modeo assumptions inputs index outputs rule path mode rules)
  (fresh (child rest rewritten child-path)
    (== `(,child . ,rest) inputs)
    (conde
      ((if (eq? mode 'backward)
           (step-in-modeo assumptions rewritten child rule child-path mode rules)
           (step-in-modeo assumptions child rewritten rule child-path mode rules))
       (== `(,rewritten . ,rest) outputs) (== path `(,index . ,child-path)))
      ((fresh (rewritten-rest rest-path)
         (children-in-modeo assumptions rest (+ index 1) rewritten-rest rule rest-path mode rules)
         (== `(,child . ,rewritten-rest) outputs) (== path rest-path))))))

(define (rewrite-stepo assumptions input output rule path)
  (step-in-modeo assumptions input output rule path 'forward))
(define (check-stepo assumptions input output rule path)
  (step-in-modeo assumptions input output rule path 'check))
(define (unrewrite-stepo assumptions input output rule path)
  (step-in-modeo assumptions input output rule path 'backward))
(define (unrewrite-rooto assumptions input output rule)
  (math-rules-stepo math-rewrite-rules assumptions input output rule 'backward))
(define (rewriteo assumptions input output)
  (fresh (rule path) (rewrite-stepo assumptions input output rule path)))

(define* (math-selected-rules selection defaults #:key (mode 'forward))
  (unless (memq mode '(forward backward check)) (error "unknown math query mode" mode))
  (let ((names (if selection selection defaults)))
    (unless (and (list? names) (every math-rewrite-rule-ref names))
      (error "expected a list of registered math rule names" names))
    (for-each
      (lambda (name)
        (unless (math-rewrite-rule-relation (math-rewrite-rule-ref name) mode)
          (error "math rule does not support the requested mode" name mode))) names)
    (delete-duplicates names)))

(define* (math-rewrite-answers assumptions term rules backward?
                              #:key (mode (if backward? 'backward 'forward)) (max-results #f))
  (run max-results (next rule path)
    (case mode
      ((backward) (step-in-modeo assumptions next term rule path mode (map math-rewrite-rule-ref rules)))
      ((check forward) (step-in-modeo assumptions term next rule path mode (map math-rewrite-rule-ref rules)))
      (else (error "unknown math query mode" mode)))))

(define (math-rewrite-innermost assumptions term names)
  "Choose one innermost forward edge, using the ordinary checked relations."
  (define rules (map math-rewrite-rule-ref names))
  (define (paths term)
    (append
      (if (memq (car term) '(var num zero)) '()
          (append-map (lambda (child index)
                        (map (lambda (path) (cons index path)) (paths child)))
                      (cdr term) (iota (length (cdr term)))))
      '(())))
  (define (replace term path replacement)
    (if (null? path) replacement
        (cons (car term)
          (map (lambda (child index)
                 (if (= index (car path)) (replace child (cdr path) replacement) child))
               (cdr term) (iota (length (cdr term)))))))
  (let visit ((remaining (paths term)))
    (and (pair? remaining)
         (let* ((path (car remaining)) (local (math-term-at-path term path))
                (answer
                  (any
                    (lambda (rule)
                      (and ((math-rewrite-rule-required-facts rule) local)
                           (let ((results (run 1 (output)
                                            ((math-rewrite-rule-relation rule 'forward) assumptions local output))))
                             (and (pair? results)
                                  (list (replace term path (car results)) (math-rewrite-rule-name rule) path)))))
                    rules)))
           (or answer (visit (cdr remaining)))))))

(define* (rewrite-curl-curl term #:key (assumptions '()))
  (let ((answers (run 1 (result) (curl-curlo assumptions term result))))
    (and (pair? answers) (car answers))))

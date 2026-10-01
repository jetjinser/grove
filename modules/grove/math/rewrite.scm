;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math rewrite)
  #:use-module (grove relation)
  #:use-module (grove math rule)
  #:use-module (grove math vector)
  #:use-module (grove math identities)
  #:use-module (grove math arithmetic)
  #:use-module (grove math differentiation)
  #:use-module (srfi srfi-1)
  #:export (rewriteo rewrite-stepo check-stepo unrewrite-rooto unrewrite-stepo
            math-rewrite-rules math-simplification-rules math-calculation-rules
            math-collection-rules math-backward-rules math-rewrite-rule-ref
            math-rule-names
            math-rule-required-facts math-selected-rules math-rewrite-answers
            rewrite-curl-curl)
  #:re-export (curl-curlo math-rewrite-rule? math-rewrite-rule-name
               math-rewrite-rule-equation math-rewrite-rule-condition
               math-rewrite-rule-condition-checked? math-rewrite-rule-group
               math-rewrite-rule-modes))

(define math-rewrite-rules
  (append math-vector-rules math-identity-rules math-differentiation-rule-definitions
          math-arithmetic-rules))

(define (math-rewrite-rule-ref name)
  (find (lambda (rule) (eq? name (math-rewrite-rule-name rule))) math-rewrite-rules))

(define* (math-rule-names #:key (mode 'forward) (groups #f))
  (map math-rewrite-rule-name
    (filter (lambda (rule)
              (and (memq mode (math-rewrite-rule-modes rule))
                   (or (not groups) (memq (math-rewrite-rule-group rule) groups))))
            math-rewrite-rules)))

(define math-simplification-rules (math-rule-names #:groups '(identity calculate collect)))
(define math-calculation-rules (math-rule-names #:groups '(calculate)))
(define math-collection-rules (math-rule-names #:groups '(collect)))
(define math-backward-rules
  (map math-rewrite-rule-name (filter math-rewrite-rule-backward-default? math-rewrite-rules)))

(define (math-rule-required-facts name input)
  (let ((rule (math-rewrite-rule-ref name)))
    (and rule ((math-rewrite-rule-required-facts rule) input))))

(define (step-in-modeo assumptions input output rule path mode)
  (conde
    ((math-rules-stepo math-rewrite-rules assumptions input output rule mode)
     (== path '()))
    ((fresh (operator arguments rewritten child-path)
       ;; An inverse query walks the known right side, a forward/check query
       ;; walks the left side. Operand indexing is identical in both cases.
       (if (eq? mode 'backward)
           (== `(,operator . ,arguments) output)
           (== `(,operator . ,arguments) input))
       (children-in-modeo assumptions arguments 0 rewritten rule child-path mode)
       (if (eq? mode 'backward)
           (== `(,operator . ,rewritten) input)
           (== `(,operator . ,rewritten) output))
       (== path child-path)))))

(define (children-in-modeo assumptions inputs index outputs rule path mode)
  (fresh (child rest rewritten child-path)
    (== `(,child . ,rest) inputs)
    (conde
      ((if (eq? mode 'backward)
           (step-in-modeo assumptions rewritten child rule child-path mode)
           (step-in-modeo assumptions child rewritten rule child-path mode))
       (== `(,rewritten . ,rest) outputs) (== path `(,index . ,child-path)))
      ((fresh (rewritten-rest rest-path)
         (children-in-modeo assumptions rest (+ index 1) rewritten-rest rule rest-path mode)
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

(define (allowed-ruleo names rule)
  (if (null? names) fail
      (conde ((== rule (car names))) ((allowed-ruleo (cdr names) rule)))))

(define* (math-rewrite-answers assumptions term rules backward?
                              #:key (mode (if backward? 'backward 'forward)))
  (run* (next rule path)
    (allowed-ruleo rules rule)
    (case mode
      ((backward) (unrewrite-stepo assumptions next term rule path))
      ((check) (check-stepo assumptions term next rule path))
      ((forward) (rewrite-stepo assumptions term next rule path))
      (else (error "unknown math query mode" mode)))))

(define* (rewrite-curl-curl term #:key (assumptions '()))
  (let ((answers (run 1 (result) (curl-curlo assumptions term result))))
    (and (pair? answers) (car answers))))

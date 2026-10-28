;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((grove math formula) #:prefix φ:) (grove math rule) (grove math rewrite)
             (grove math pipeline) (grove math arithmetic)
             (grove math differentiation) (grove math partial)
             (grove math integration)
             (grove relation) (srfi srfi-1) (srfi srfi-64))

(test-begin "math rule contracts")
(test-error "math rule relations must accept assumptions and two terms" #t
  (make-math-rewrite-rule 'bad-relation "x = x" "always" 'custom
    (lambda (_) #t) #:matcher (lambda (_) #f)))
(test-error "math rule matchers must accept one term" #t
  (make-math-rewrite-rule 'bad-matcher "x = x" "always" 'custom
    (lambda (_assumptions _input _output) #t)
    #:matcher (lambda () #f)))
(test-error "backward math relations must accept assumptions and two terms" #t
  (make-math-rewrite-rule 'bad-backward "x = x" "always" 'custom
    (lambda (_assumptions _input _output) #t)
    #:matcher (lambda (_) #f)
    #:backward (lambda (_) #f)))
(test-assert "math rule-pack modules expose rules, not implementation definitions"
  (and (not (module-variable
              (module-public-interface (resolve-module '(grove math partial)))
              'math-partial-rule-definitions))
       (not (module-variable
              (module-public-interface (resolve-module '(grove math integration)))
              'polynomial-calculus-diagnostics))))
(test-assert "domain rule packs consistently expose rewrite-rule records"
  (every (lambda (rules) (every math-rewrite-rule? rules))
    (list math-arithmetic-rules math-differentiation-rules math-partial-rules
          math-polynomial-rules)))
(test-equal "a built-in domain pack is directly queryable as rule records"
  '(differentiate-variable)
  (run* (name)
    (math-rules-stepo math-differentiation-rules '((scalar (var x)))
      '(differentiate (var x) (var x)) '(num "1") name 'check)))
(for-each
  (lambda (fixture)
    (let ((input (car fixture)) (rule (cadr fixture)) (path (caddr fixture))
          (facts '((scalar (var x)) (scalar (var y)))))
      (test-equal "local certificate checks agree with relational traversal"
        (run* (output)
          ((@@ (grove math rewrite) step-in-modeo)
            facts input output rule path 'check))
        (run* (output) (check-stepo facts input output rule path)))))
  '(((+ (var x) (num "0")) add-zero ())
    ((+ (+ (var x) (num "0")) (var y)) add-zero (0))
    ((+ (var x) (+ (var y) (num "0"))) add-zero (1))
    ((+ (var x) (num "1")) add-zero ())
    ((+ (var x) (num "0")) add-zero (9))
    ((+ (var x) (num "0")) missing-rule ())))
(test-equal "unknown certificate locations retain relational answers"
  (run* (output path)
    ((@@ (grove math rewrite) step-in-modeo) '((scalar (var x)))
      '(+ (var x) (num "0")) output 'add-zero path 'check))
  (run* (output path)
    (check-stepo '((scalar (var x)))
      '(+ (var x) (num "0")) output 'add-zero path)))
(test-equal "registry names are unique" (length math-rewrite-rules)
  (length (delete-duplicates (map math-rewrite-rule-name math-rewrite-rules))))
(for-each
  (lambda (rule)
    (test-assert "each rule declares a group and readable equation"
      (and (symbol? (math-rewrite-rule-group rule))
           (string? (math-rewrite-rule-equation rule))))
    (test-assert "every registered rule can check an authored transition"
      (procedure? (math-rewrite-rule-relation rule 'check)))
    (when (math-rewrite-rule-backward-default? rule)
      (test-assert "default backward rules declare backward support"
        (memq 'backward (math-rewrite-rule-modes rule)))))
  math-rewrite-rules)
(test-equal "calculation rule selection follows group declarations"
  '(evaluate-constant) math-calculation-rules)
(test-assert "default search excludes registered check-only theorems"
  (and (not (memq 'cancel-common-factor math-default-rules))
       (every (lambda (name)
                (math-rewrite-rule-relation (math-rewrite-rule-ref name) 'forward))
              math-default-rules)))
(test-equal "differentiation is explicitly constructive, not inverse integration"
  '(forward check) (math-rewrite-rule-modes (math-rewrite-rule-ref 'differentiate-power)))
(test-error "inverse derivative selection reports its unsupported mode" #t
  (φ:search "differentiate(x,x)" "1" #:backward? #t #:assumptions '("scalar(x)")
    #:rules '(differentiate-variable)))
(test-error "inverse arithmetic computation is not silently ignored" #t
  (φ:search "2*3" "6" #:backward? #t #:rules '(evaluate-constant)))
(test-error "unknown modes are rejected even with no rules" #t
  (math-selected-rules '() '() #:mode 'unknown))

;; This is the public, lower-level extension path shown in docs/math.md:
;; custom rules compose through an explicit relational query, not the registry.
(let ((double-rule
        (math-rule 'double "double(X) = X + X" "all values" 'custom
          (facts x) (double ,x) (+ ,x ,x) () ())))
  (test-equal "a declared local rule runs through the relational rule list"
    '(((+ (var x) (var x)) double))
    (run* (output name)
      (math-rules-stepo (list double-rule) '()
        '(double (var x)) output name 'forward))))

;; A fresh rule declaration supplies one left side and one premise template.
;; Both relational execution and diagnostic matching derive from that source.
(let* ((rule (math-rule 'custom "custom(f) = f" "smooth(f)" 'example
               (facts field) (custom ,field) ,field ((smooth ,field)) ()
               #:forward? #f))
       (input '(custom (var x))))
  (test-equal "a check-only rule declares exactly its supported mode"
    '(check) (math-rewrite-rule-modes rule))
  (test-equal "a declaration generates instantiated diagnostic premises"
    '((smooth (var x))) ((math-rewrite-rule-required-facts rule) input))
  (test-equal "a declaration excludes unrelated left-side shapes"
    #f ((math-rewrite-rule-required-facts rule) '(other (var x))))
  (test-assert "a declaration normalizes to one inspectable RuleSpec"
    (let ((spec (math-rewrite-rule-spec rule)))
      (and (math-rule-spec? spec)
           (equal? (math-rule-spec-left spec) '(custom (unquote field)))
           (equal? (math-rule-spec-right spec) '(unquote field))
           (equal? (math-rule-spec-premises spec) '((smooth (unquote field))))
           (null? (math-rule-spec-opaque-guards spec)))))
  (test-equal "a check-only rule verifies its named premise"
    '((var x))
    (run* (output)
      ((math-rewrite-rule-relation rule 'check) '((smooth (var x))) input output)))
  (test-equal "the same rule refuses to discharge a missing premise"
    '()
    (run* (output) ((math-rewrite-rule-relation rule 'check) '() input output)))
  (test-equal "the same rule has no forward generation callback"
    #f (math-rewrite-rule-relation rule 'forward)))
(test-assert "unrecognized procedural guards remain explicitly opaque"
  (pair? (math-rule-spec-opaque-guards
           (math-rewrite-rule-spec (math-rewrite-rule-ref 'differentiate-power)))))
(let ((rule (math-rule 'cancel "pair(X,X) = X" "same X twice" 'example
              (facts term) (pair ,term ,term) ,term () ())))
  (test-equal "repeated pattern placeholders constrain diagnostics"
    #f ((math-rewrite-rule-required-facts rule) '(pair (var x) (var y))))
  (test-equal "repeated pattern placeholders match equal operands"
    '() ((math-rewrite-rule-required-facts rule) '(pair (var x) (var x)))))
(let ((spec (make-math-rule-spec '(pair (unquote x) (unquote x))
                '(unquote x) '((smooth (unquote x))) '())))
  (test-equal "RuleSpec owns repeated-variable matching"
    '((x . (var x))) (math-rule-spec-match spec '(pair (var x) (var x))))
  (test-equal "RuleSpec rejects inconsistent repeated variables"
    #f (math-rule-spec-match spec '(pair (var x) (var y))))
  (test-equal "RuleSpec instantiates its own premise templates"
    '((smooth (var x)))
    (math-rule-spec-instantiate spec (math-rule-spec-premises spec)
      (math-rule-spec-match spec '(pair (var x) (var x))))))

(let* ((zero (string-copy "0"))
       (left (list '+ (list 'unquote 'x) (list 'num zero)))
       (right (list 'unquote 'x))
       (premises (list (list 'smooth (list 'unquote 'x))))
       (guards (list (list '=/= (list 'unquote 'x) (list 'num zero))))
       (spec (make-math-rule-spec left right premises guards)))
  (set-car! left '-)
  (string-set! zero 0 #\9)
  (set-car! premises '(changed))
  (set-car! guards '(changed))
  (test-equal "RuleSpec snapshots caller-owned declaration data"
    '(+ (unquote x) (num "0"))
    (math-rule-spec-left spec))
  (test-equal "RuleSpec accessors return independent declaration data"
    '(+ (unquote x) (num "0"))
    (let ((exposed (math-rule-spec-left spec)))
      (set-car! exposed '-)
      (string-set! (cadr (caddr exposed)) 0 #\9)
      (math-rule-spec-left spec)))
  (test-equal "RuleSpec snapshot keeps matching behavior stable"
    '((x . (var x)))
    (math-rule-spec-match spec '(+ (var x) (num "0"))))
  (test-equal "RuleSpec snapshots premise and guard lists"
    '(((smooth (unquote x))) ((=/= (unquote x) (num "0"))))
    (list (math-rule-spec-premises spec) (math-rule-spec-guards spec)))
  (let ((premise-copy (math-rule-spec-premises spec))
        (guard-copy (math-rule-spec-guards spec)))
    (set-car! premise-copy '(changed))
    (set-car! guard-copy '(changed))
    (test-equal "RuleSpec collection accessors return independent data"
      '(((smooth (unquote x))) ((=/= (unquote x) (num "0"))))
      (list (math-rule-spec-premises spec) (math-rule-spec-guards spec)))))

(let* ((equation (string-copy "left = right"))
       (condition (string-copy "facts hold"))
       (label (string-copy "Named rule"))
       (documentation (string-copy "Rule documentation"))
       (relation (lambda (_facts _input _output) #f))
       (rule (make-math-rewrite-rule
               'snapshot equation condition 'example relation
               #:matcher (lambda (_) '())
               #:label label
               #:documentation documentation)))
  (string-set! equation 0 #\x)
  (string-set! condition 0 #\x)
  (string-set! label 0 #\x)
  (string-set! documentation 0 #\x)
  (test-equal "math rules snapshot caller-owned declaration strings"
    '("left = right" "facts hold" "Named rule" "Rule documentation")
    (list (math-rewrite-rule-equation rule)
          (math-rewrite-rule-condition rule)
          (math-rewrite-rule-label rule)
          (math-rewrite-rule-documentation rule)))
  (let ((modes (math-rewrite-rule-modes rule))
        (equation-copy (math-rewrite-rule-equation rule))
        (condition-copy (math-rewrite-rule-condition rule))
        (label-copy (math-rewrite-rule-label rule))
        (rule-doc (math-rewrite-rule-documentation rule)))
    (set-car! modes 'changed)
    (string-set! equation-copy 0 #\x)
    (string-set! condition-copy 0 #\x)
    (string-set! label-copy 0 #\x)
    (string-set! rule-doc 0 #\x)
    (test-equal "math rule metadata accessors return independent copies"
      '(forward check "left = right" "facts hold" "Named rule"
        "Rule documentation")
      (append (math-rewrite-rule-modes rule)
              (list (math-rewrite-rule-equation rule)
                    (math-rewrite-rule-condition rule)
                    (math-rewrite-rule-label rule)
                    (math-rewrite-rule-documentation rule)))))
  (test-assert "mutating returned modes cannot change rule behavior"
    (procedure? (math-rewrite-rule-relation rule 'forward))))

(test-error "malformed RuleSpecs are rejected at construction"
  #t (make-math-rule-spec '(+ x 0) 'x 'not-a-list '()))
(test-equal "vector rule's diagnostic premises come from its relation declaration"
  '((c2 (var E)))
  (math-rule-required-facts 'div-curl-zero '(div (curl (var E)))))
(test-equal "finite inverse zero identity enumerates declared fields only"
  '((div (curl (var E))))
  (run* (input)
    (unrewrite-rooto '((vector (var E)) (smooth (var E)))
                    input '(num "0") 'div-curl-zero)))
(let* ((rules
          (list (math-rule 'foo-to-bar "foo(X) = bar(X)" "always" 'example
                  (facts x) (foo ,x) (bar ,x) () () #:backward #t)
                (math-rule 'other-to-bar "other(X) = bar(X)" "always" 'example
                  (facts x) (other ,x) (bar ,x) () () #:backward #t))))
  (define (reference rules input output name mode)
    (if (null? rules) fail
        (conde
          ((== name (math-rewrite-rule-name (car rules)))
           ((math-rewrite-rule-relation (car rules) mode) '() input output))
          ((reference (cdr rules) input output name mode)))))
  (test-equal "root pruning preserves partially ground forward answers"
    (run* (x output name) (reference rules `(foo ,x) output name 'forward))
    (run* (x output name) (math-rules-stepo rules '() `(foo ,x) output name 'forward)))
  (test-equal "root pruning preserves inverse enumeration"
    (run* (input name) (reference rules input '(bar (num "1")) name 'backward))
    (run* (input name) (math-rules-stepo rules '() input '(bar (num "1")) name 'backward)))
  (test-equal "open relational roots retain all rules"
    (run* (input output name) (reference rules input output name 'forward))
    (run* (input output name) (math-rules-stepo rules '() input output name 'forward))))

(test-end "math rule contracts")

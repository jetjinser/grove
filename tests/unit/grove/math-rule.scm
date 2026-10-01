;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((grove math formula) #:prefix φ:) (grove math rule) (grove math rewrite) (grove math pipeline)
             (grove relation) (srfi srfi-1) (srfi srfi-64))

(test-begin "math rule contracts")
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
(test-equal "differentiation is explicitly constructive, not inverse integration"
  '(forward check) (math-rewrite-rule-modes (math-rewrite-rule-ref 'differentiate-power)))
(test-error "inverse derivative selection reports its unsupported mode" #t
  (φ:search-backward "1" "differentiate(x,x)" #:assumptions '("scalar(x)")
    #:rules '(differentiate-variable)))
(test-error "inverse arithmetic computation is not silently ignored" #t
  (φ:search-backward "6" "2*3" #:rules '(evaluate-constant)))
(test-error "unknown modes are rejected even with no rules" #t
  (math-selected-rules '() '() #:mode 'unknown))

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
  (test-equal "a check-only rule verifies its named premise"
    '((var x))
    (run* (output)
      ((math-rewrite-rule-relation rule 'check) '((smooth (var x))) input output)))
  (test-equal "the same rule refuses to discharge a missing premise"
    '()
    (run* (output) ((math-rewrite-rule-relation rule 'check) '() input output)))
  (test-equal "the same rule has no forward generation callback"
    #f (math-rewrite-rule-relation rule 'forward)))
(let ((rule (math-rule 'cancel "pair(X,X) = X" "same X twice" 'example
              (facts term) (pair ,term ,term) ,term () ())))
  (test-equal "repeated pattern placeholders constrain diagnostics"
    #f ((math-rewrite-rule-required-facts rule) '(pair (var x) (var y))))
  (test-equal "repeated pattern placeholders match equal operands"
    '() ((math-rewrite-rule-required-facts rule) '(pair (var x) (var x)))))
(test-equal "vector rule's diagnostic premises come from its relation declaration"
  '((c2 (var E)))
  (math-rule-required-facts 'div-curl-zero '(div (curl (var E)))))
(test-equal "finite inverse zero identity enumerates declared fields only"
  '((div (curl (var E))))
  (run* (input)
    (unrewrite-rooto '((vector (var E)) (smooth (var E)))
                    input '(num "0") 'div-curl-zero)))
(test-end "math rule contracts")

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math proof)
  #:use-module (grove math assumption)
  #:use-module (grove math derivation)
  #:use-module (grove math rewrite)
  #:use-module (grove math semantic-check)
  #:use-module (grove math term)
  #:use-module (grove math types)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (math-proof? math-proof-premise math-proof-rule-instance
            math-proof-reflexivity math-proof-symmetry math-proof-transitivity
            math-proof-congruence
            math-proof-result? math-proof-result-status math-proof-result-valid?
            math-proof-result-left math-proof-result-right math-proof-result-errors
            math-proof-check math-proof-from-derivation))

;; Proof nodes record just the inference being justified. They are distinct
;; from math-derivation, which records how a search or rewrite pass proceeded.
(define-record-type <math-proof>
  (make-math-proof kind fields)
  math-proof?
  (kind proof-kind)
  (fields proof-fields))

(define (math-proof-premise index)
  (make-math-proof 'premise (list index)))

(define (math-proof-rule-instance rule substitution premise-proofs)
  (make-math-proof 'rule-instance (list rule substitution premise-proofs)))

(define (math-proof-reflexivity term)
  (make-math-proof 'reflexivity (list term)))

(define (math-proof-symmetry proof)
  (make-math-proof 'symmetry (list proof)))

(define (math-proof-transitivity left right)
  (make-math-proof 'transitivity (list left right)))

(define (math-proof-congruence from to path proof)
  (make-math-proof 'congruence (list from to path proof)))

(define-record-type <math-proof-result>
  (make-math-proof-result status left right errors)
  math-proof-result?
  (status math-proof-result-status)
  (left math-proof-result-left)
  (right math-proof-result-right)
  (errors math-proof-result-errors))

(define (math-proof-result-valid? result)
  (and (math-proof-result? result)
       (eq? (math-proof-result-status result) 'checked)))

(define (proof-error status message)
  (throw 'math-proof-error status message))

(define (substitution-ref substitution name)
  (let ((binding (assq name substitution)))
    (and binding (cdr binding))))

(define (instantiate-guard form substitution)
  (cond
    ((symbol? form) (or (substitution-ref substitution form) form))
    ((pair? form) (map (lambda (part) (instantiate-guard part substitution)) form))
    (else form)))

(define (replace-at-path term path replacement)
  (if (null? path) replacement
      (and (pair? term)
           (let ((index (car path)) (operands (cdr term)))
             (and (integer? index) (<= 0 index) (< index (length operands))
                  (let loop ((remaining operands) (position 0) (result '()))
                    (if (null? remaining)
                        (cons (car term) (reverse result))
                        (loop (cdr remaining) (+ position 1)
                          (cons (if (= position index)
                                    (replace-at-path (car remaining) (cdr path) replacement)
                                    (car remaining))
                                result)))))))))

(define (guard-type-bindings guards substitution)
  (filter-map
    (lambda (guard)
      (match guard
        (('zeroo type zero)
         (let ((value (substitution-ref substitution zero)))
           (cond ((equal? value '(num "0")) (cons type 'scalar))
                 ((equal? value '(zero vector)) (cons type 'vector))
                 (else #f))))
        (_ #f)))
    guards))

(define (check-guard guard assumptions substitution type-bindings)
  (match guard
    (('math-typeo _ term ('quote type))
     (let ((value (substitution-ref substitution term)))
       (and value
            (let ((inferred (math-infer-type assumptions value)))
              (and inferred (eq? type (math-typed-term-type inferred)))))))
    (('math-typeo _ term type)
     (let ((value (substitution-ref substitution term))
           (type (or (assq-ref type-bindings type) type)))
       (and value
            (let ((inferred (math-infer-type assumptions value)))
              (and inferred (eq? type (math-typed-term-type inferred)))))))
    (('zeroo type zero)
     (let ((expected (assq-ref type-bindings type))
           (actual (substitution-ref substitution zero)))
       (and expected actual
            (equal? actual (if (eq? expected 'scalar) '(num "0") '(zero vector))))))
    (('=/= left right)
     (not (equal? (instantiate-guard left substitution)
                  (instantiate-guard right substitution))))
    (_ (proof-error 'unsupported (format #f "opaque rule guard: ~s" guard)))))

(define (check-rule-instance fields assumptions)
  (match fields
    ((name substitution premise-proofs)
     (unless (and (symbol? name) (list? substitution)
                  (every (lambda (binding)
                           (and (pair? binding) (symbol? (car binding))
                                (math-term? (cdr binding)))) substitution)
                  (= (length substitution)
                     (length (delete-duplicates (map car substitution))))
                  (list? premise-proofs) (every math-proof? premise-proofs))
       (proof-error 'rejected "malformed rule-instance payload"))
     (let* ((rule (math-rewrite-rule-ref name))
            (spec (and rule (math-rewrite-rule-spec rule))))
       (unless spec (proof-error 'unsupported "rule has no fixed RuleSpec"))
       (let ((theorem-check (math-polynomial-check-rule name)))
         (unless (math-semantic-check-valid? theorem-check)
           (proof-error
             (if (eq? (math-semantic-check-status theorem-check) 'rejected)
                 'rejected 'unsupported)
             "rule theorem is not established by the scalar-polynomial checker")))
       (let* ((expected-names (delete-duplicates
                                (template-variables (math-rule-spec-left spec))))
              (actual-names (delete-duplicates (map car substitution)))
              (premises (math-rule-spec-instantiate spec
                          (math-rule-spec-premises spec) substitution))
              (guards (math-rule-spec-guards spec))
              (expected-indices
                (map (lambda (premise)
                       (or (list-index (lambda (fact) (equal? fact premise)) assumptions)
                           (proof-error 'rejected
                             (format #f "missing rule premise: ~s" premise))))
                     premises))
              (type-bindings (guard-type-bindings guards substitution)))
         (unless (and (every (lambda (name) (memq name actual-names)) expected-names)
                      (every (lambda (name) (memq name expected-names)) actual-names))
           (proof-error 'rejected "substitution does not match the rule's metavariables"))
         (let ((actual-indices
                 (map (lambda (premise-proof)
                        (unless (eq? (proof-kind premise-proof) 'premise)
                          (proof-error 'rejected "rule premises must be explicit premise references"))
                        (check-node premise-proof assumptions)
                        (car (proof-fields premise-proof)))
                      premise-proofs)))
           (unless (equal? actual-indices expected-indices)
             (proof-error 'rejected "premise references do not match the rule's required facts")))
         (for-each (lambda (guard)
                     (unless (check-guard guard assumptions substitution type-bindings)
                       (proof-error 'rejected (format #f "rule guard failed: ~s" guard))))
                   guards)
         (values (math-rule-spec-instantiate spec (math-rule-spec-left spec) substitution)
                 (math-rule-spec-instantiate spec (math-rule-spec-right spec) substitution)))))
    (_ (proof-error 'rejected "malformed rule-instance node"))))

(define (check-node proof assumptions)
  (unless (math-proof? proof) (proof-error 'rejected "expected a proof node"))
  (match (cons (proof-kind proof) (proof-fields proof))
    (('premise index)
     (unless (and (exact-integer? index) (>= index 0) (< index (length assumptions)))
       (proof-error 'rejected "premise reference is out of range"))
     (unless (math-assumption? (list-ref assumptions index))
       (proof-error 'rejected "referenced fact is not an assumption"))
     (let ((fact (list-ref assumptions index)))
       (if (and (pair? fact) (eq? (car fact) '=) (= (length fact) 3))
           (values (cadr fact) (caddr fact))
           (values #f #f))))
    (('rule-instance . fields) (check-rule-instance fields assumptions))
    (('reflexivity term)
     (unless (math-term? term) (proof-error 'rejected "invalid reflexivity term"))
     (values term term))
    (('symmetry child)
     (call-with-values (lambda () (check-node child assumptions))
       (lambda (left right)
         (unless left (proof-error 'rejected "symmetry requires an equality proof"))
         (values right left))))
    (('transitivity first second)
     (call-with-values (lambda () (check-node first assumptions))
       (lambda (left middle)
         (call-with-values (lambda () (check-node second assumptions))
           (lambda (other-middle right)
             (unless (and middle other-middle (equal? middle other-middle))
               (proof-error 'rejected "transitivity endpoints do not agree"))
             (values left right))))))
    (('congruence from to path child)
     (unless (and (math-term? from) (math-term? to) (list? path))
       (proof-error 'rejected "malformed congruence context"))
     (call-with-values (lambda () (check-node child assumptions))
       (lambda (local-from local-to)
         (unless (and local-from local-to
                      (equal? (math-term-at-path from path) local-from)
                      (equal? (math-term-at-path to path) local-to)
                      (equal? (replace-at-path from path local-to) to))
           (proof-error 'rejected "congruence does not preserve the stated context"))
         (values from to))))
    (_ (proof-error 'rejected "unknown proof inference"))))

(define (math-proof-check proof assumptions)
  "Independently check a proof tree under the supplied ordered assumptions."
  (unless (list? assumptions) (error "proof assumptions must be a list" assumptions))
  (catch 'math-proof-error
    (lambda ()
      (call-with-values (lambda () (check-node proof assumptions))
        (lambda (left right)
          (make-math-proof-result 'checked left right '()))))
    (lambda (key status message)
      (make-math-proof-result status #f #f (list message)))))

(define (template-variables template)
  (match template
    (('unquote name) (list name))
    ((head . tail) (append (template-variables head) (template-variables tail)))
    (_ '())))

(define (build-step-proof step assumptions)
  (unless (math-derivation-step? step) (throw 'math-proof-build-failed))
  (let* ((from (math-derivation-step-from step))
         (to (math-derivation-step-to step))
         (path (math-derivation-step-path step))
         (name (math-derivation-step-rule step))
         (rule (and (symbol? name) (math-rewrite-rule-ref name)))
         (spec (and rule (math-rewrite-rule-spec rule)))
         (local (and (math-term? from) (math-term-at-path from path)))
         (substitution (and spec local (math-rule-spec-match spec local))))
    (unless (and spec substitution (math-term? to)) (throw 'math-proof-build-failed))
    (unless (math-semantic-check-valid? (math-polynomial-check-rule name))
      (throw 'math-proof-build-failed))
    (let* ((expected-names (delete-duplicates
                             (template-variables (math-rule-spec-left spec))))
           (actual-names (delete-duplicates (map car substitution)))
           (right (math-rule-spec-instantiate spec (math-rule-spec-right spec) substitution))
           (premises (math-rule-spec-instantiate spec (math-rule-spec-premises spec) substitution))
           (premise-proofs
             (map (lambda (premise)
                    (let ((index (list-index (lambda (fact) (equal? fact premise)) assumptions)))
                      (unless index (throw 'math-proof-build-failed))
                      (math-proof-premise index))) premises))
           (rule-proof (math-proof-rule-instance name substitution premise-proofs)))
      (unless (and (every (lambda (name) (memq name actual-names)) expected-names)
                   (every (lambda (name) (memq name expected-names)) actual-names)
                   (equal? (math-term-at-path to path) right)
                   (equal? (replace-at-path from path right) to))
        (throw 'math-proof-build-failed))
      (math-proof-congruence from to path rule-proof))))

(define (math-proof-from-derivation derivation)
  "Elaborate a fixed-RuleSpec rewrite trace as a candidate proof tree.
Unsupported algorithm steps return #f and require another proof frontend."
  (unless (math-derivation? derivation)
    (error "expected a math derivation" derivation))
  (catch 'math-proof-build-failed
    (lambda ()
      (let ((terms (math-derivation-terms derivation))
            (steps (math-derivation-steps derivation))
            (assumptions (math-derivation-assumptions derivation)))
        (unless (and (list? terms) (pair? terms) (every math-term? terms)
                     (list? steps) (list? assumptions)
                     (= (length terms) (+ (length steps) 1)))
          (throw 'math-proof-build-failed))
        (let ((proofs
                (map (lambda (step from to)
                       (unless (and (math-derivation-step? step)
                                    (equal? from (math-derivation-step-from step))
                                    (equal? to (math-derivation-step-to step)))
                         (throw 'math-proof-build-failed))
                       (build-step-proof step assumptions))
                     steps (drop-right terms 1) (cdr terms))))
          (if (null? proofs) (math-proof-reflexivity (car terms))
              (fold (lambda (next accumulated)
                      (math-proof-transitivity accumulated next))
                    (car proofs) (cdr proofs))))))
    (lambda _ #f)))

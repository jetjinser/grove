;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math proof)
  #:use-module (grove private snapshot)
  #:use-module (grove math private assumption)
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
            math-proof-check math-proof-from-derivation
            math-proof-elaborate-derivation
            math-proof-step-report? math-proof-step-report-index
            math-proof-step-report-rule math-proof-step-report-premises
            math-proof-step-report-theorem-status math-proof-step-report-status
            math-proof-step-report-errors math-proof-step-report-proof
            math-proof-derivation-report? math-proof-derivation-report-status
            math-proof-derivation-report-steps math-proof-derivation-report-proof
            math-proof-derivation-report-errors))

;; Proof nodes record just the inference being justified. They are distinct
;; from math-derivation, which records how a search or rewrite pass proceeded.
(define-record-type <math-proof>
  (%make-math-proof kind fields)
  math-proof?
  (kind proof-kind)
  (fields proof-fields))

(define (make-math-proof kind fields)
  (%make-math-proof kind (snapshot-data fields)))

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
  (%make-math-proof-result status left right errors)
  math-proof-result?
  (status math-proof-result-status)
  (left proof-result-left/raw)
  (right proof-result-right/raw)
  (errors proof-result-errors/raw))

(define (make-math-proof-result status left right errors)
  (%make-math-proof-result status (snapshot-data left) (snapshot-data right)
                           (snapshot-data errors)))

(define (math-proof-result-left result)
  (snapshot-data (proof-result-left/raw result)))

(define (math-proof-result-right result)
  (snapshot-data (proof-result-right/raw result)))

(define (math-proof-result-errors result)
  (snapshot-data (proof-result-errors/raw result)))

(define (math-proof-result-valid? result)
  (and (math-proof-result? result)
       (eq? (math-proof-result-status result) 'checked)))

(define-record-type <math-proof-step-report>
  (%make-math-proof-step-report index rule premises theorem-status status errors proof)
  math-proof-step-report?
  (index math-proof-step-report-index)
  (rule math-proof-step-report-rule)
  (premises proof-step-premises/raw)
  (theorem-status math-proof-step-report-theorem-status)
  (status math-proof-step-report-status)
  (errors proof-step-errors/raw)
  (proof math-proof-step-report-proof))

(define (math-proof-step-report-premises report)
  (snapshot-data (proof-step-premises/raw report)))

(define (math-proof-step-report-errors report)
  (snapshot-data (proof-step-errors/raw report)))

(define (make-math-proof-step-report index rule premises theorem-status status errors proof)
  (%make-math-proof-step-report index rule (snapshot-data premises) theorem-status
                                status (snapshot-data errors) proof))

(define-record-type <math-proof-derivation-report>
  (%make-math-proof-derivation-report status steps proof errors)
  math-proof-derivation-report?
  (status math-proof-derivation-report-status)
  (steps proof-derivation-steps/raw)
  (proof math-proof-derivation-report-proof)
  (errors proof-derivation-errors/raw))

(define (math-proof-derivation-report-steps report)
  (snapshot-data (proof-derivation-steps/raw report)))

(define (math-proof-derivation-report-errors report)
  (snapshot-data (proof-derivation-errors/raw report)))

(define (make-math-proof-derivation-report status steps proof errors)
  (%make-math-proof-derivation-report status (snapshot-data steps) proof
                                      (snapshot-data errors)))

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

(define (guard-type-bindings guards substitution assumptions)
  (filter-map
    (lambda (guard)
      (match guard
        (('zeroo type zero)
         (let ((value (substitution-ref substitution zero)))
           (cond ((equal? value '(num "0")) (cons type 'scalar))
                 ((equal? value '(zero vector)) (cons type 'vector))
                 (else #f))))
        (('math-typeo _ term type)
         (and (symbol? type)
              (let* ((value (substitution-ref substitution term))
                     (inferred (and value (math-infer-type assumptions value))))
                (and inferred (cons type (math-typed-term-type inferred))))))
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
    (('conde ((== type ('quote first))) ((== type ('quote second))))
     (memq (assq-ref type-bindings type) (list first second)))
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
              (type-bindings (guard-type-bindings guards substitution assumptions)))
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
         (let* ((left (math-rule-spec-instantiate spec
                        (math-rule-spec-left spec) substitution))
                (right (math-rule-spec-instantiate spec
                         (math-rule-spec-right spec) substitution))
                (left-type (math-infer-type assumptions left))
                (right-type (math-infer-type assumptions right)))
           (unless (and left-type right-type
                        (eq? 'scalar (math-typed-term-type left-type))
                        (eq? 'scalar (math-typed-term-type right-type)))
             (proof-error 'rejected "rule instance is not well-typed in the scalar theory"))
           (values left right)))))
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
                      (equal? (math-term-replace-at-path from path local-to) to))
           (proof-error 'rejected "congruence does not preserve the stated context"))
         (values from to))))
    (_ (proof-error 'rejected "unknown proof inference"))))

(define (math-proof-check proof assumptions)
  "Independently check a proof tree under the supplied ordered assumptions."
  (unless (list? assumptions) (error "proof assumptions must be a list" assumptions))
  (let ((assumption-errors (math-assumption-diagnostics assumptions)))
    (if (pair? assumption-errors)
        (make-math-proof-result 'rejected #f #f
          (list (format #f "invalid assumptions: ~s" assumption-errors)))
        (catch 'math-proof-error
          (lambda ()
            (call-with-values (lambda () (check-node proof assumptions))
              (lambda (left right)
                (make-math-proof-result 'checked left right '()))))
          (lambda (key status message)
            (make-math-proof-result status #f #f (list message)))))))

(define (template-variables template)
  (match template
    (('unquote name) (list name))
    ((head . tail) (append (template-variables head) (template-variables tail)))
    (_ '())))

(define (failed-step-report index step theorem-status status message)
  (make-math-proof-step-report index
    (and (math-derivation-step? step) (math-derivation-step-rule step))
    '() theorem-status status (list message) #f))

(define (elaborate-step index step from to assumptions)
  (if (not (and (math-derivation-step? step)
                (equal? from (math-derivation-step-from step))
                (equal? to (math-derivation-step-to step))))
      (failed-step-report index step 'rejected 'rejected
        "derivation step endpoints do not match the surrounding chain")
      (let* ((path (math-derivation-step-path step))
             (name (math-derivation-step-rule step))
             (rule (and (symbol? name) (math-rewrite-rule-ref name)))
             (spec (and rule (math-rewrite-rule-spec rule))))
        (cond
          ((not rule)
           (failed-step-report index step 'rejected 'rejected "unknown rewrite rule"))
          ((not spec)
           (failed-step-report index step 'unsupported 'unsupported
             "rule has no fixed RuleSpec to elaborate"))
          (else
           (let* ((theorem-check (math-polynomial-check-rule name))
                  (theorem-status (math-semantic-check-status theorem-check))
                  (local (and (math-term? from) (list? path)
                              (math-term-at-path from path)))
                  (substitution (and local (math-rule-spec-match spec local)))
                  (premises (and substitution
                                 (math-rule-spec-instantiate
                                   spec (math-rule-spec-premises spec) substitution)))
                  (expected-names
                    (delete-duplicates (template-variables (math-rule-spec-left spec))))
                  (actual-names (if substitution (delete-duplicates (map car substitution)) '())))
             (cond
               ((not (math-semantic-check-valid? theorem-check))
                (make-math-proof-step-report index name (or premises '()) theorem-status
                  (if (eq? theorem-status 'rejected) 'rejected 'unsupported)
                  (math-semantic-check-diagnostics theorem-check) #f))
               ((not substitution)
                (make-math-proof-step-report index name '() theorem-status 'rejected
                  (list "step input does not match the registered RuleSpec") #f))
               ((not (and (every (lambda (variable) (memq variable actual-names)) expected-names)
                          (every (lambda (variable) (memq variable expected-names)) actual-names)))
                (make-math-proof-step-report index name premises theorem-status 'rejected
                  (list "RuleSpec metavariables do not match the instantiated step") #f))
               (else
                (let* ((application-check
                         (math-polynomial-check-step assumptions from to name path))
                       (premise-proofs
                         (map (lambda (premise)
                                (let ((position
                                        (list-index (lambda (fact) (equal? fact premise))
                                          assumptions)))
                                  (and position (math-proof-premise position))))
                           premises)))
                  (cond
                    ((not (math-semantic-check-valid? application-check))
                     (make-math-proof-step-report index name premises theorem-status
                       (if (eq? (math-semantic-check-status application-check) 'rejected)
                           'rejected 'unsupported)
                       (math-semantic-check-diagnostics application-check) #f))
                    ((any not premise-proofs)
                     (make-math-proof-step-report index name premises theorem-status 'rejected
                       (list "a declared rule premise is absent from the derivation assumptions") #f))
                    (else
                     (let* ((rule-proof
                              (math-proof-rule-instance name substitution premise-proofs))
                            (proof (math-proof-congruence from to path rule-proof))
                            (checked (math-proof-check proof assumptions)))
                       (make-math-proof-step-report index name premises theorem-status
                         (math-proof-result-status checked)
                         (math-proof-result-errors checked)
                         (and (math-proof-result-valid? checked) proof))))))))))))))

(define (combine-report-status reports)
  (cond ((any (lambda (report) (eq? 'rejected (math-proof-step-report-status report))) reports)
         'rejected)
        ((any (lambda (report) (eq? 'unsupported (math-proof-step-report-status report))) reports)
         'unsupported)
        (else 'checked)))

(define (math-proof-elaborate-derivation derivation)
  "Elaborate and check each derivation step, retaining partial status and dependencies."
  (unless (math-derivation? derivation)
    (error "expected a math derivation" derivation))
  (let ((terms (math-derivation-terms derivation))
        (steps (math-derivation-steps derivation))
        (assumptions (math-derivation-assumptions derivation)))
    (if (not (and (list? terms) (pair? terms) (every math-term? terms)
                  (list? steps) (list? assumptions)
                  (= (length terms) (+ (length steps) 1))))
        (make-math-proof-derivation-report 'rejected '() #f
          (list "malformed derivation structure"))
        (let* ((assumption-errors (math-assumption-diagnostics assumptions))
               (reports
                 (map elaborate-step (iota (length steps)) steps
                   (drop-right terms 1) (cdr terms)
                   (make-list (length steps) assumptions)))
               (status (combine-report-status reports)))
          (cond
            ((pair? assumption-errors)
             (make-math-proof-derivation-report 'rejected reports #f
               (list (format #f "invalid derivation assumptions: ~s" assumption-errors))))
            ((not (eq? 'checked status))
             (make-math-proof-derivation-report status reports #f
               (append-map math-proof-step-report-errors reports)))
            (else
             (let* ((proofs (map math-proof-step-report-proof reports))
                    (proof (if (null? proofs)
                               (math-proof-reflexivity (car terms))
                               (fold (lambda (next accumulated)
                                       (math-proof-transitivity accumulated next))
                                 (car proofs) (cdr proofs)))))
               (let ((checked (math-proof-check proof assumptions)))
                 (if (math-proof-result-valid? checked)
                     (make-math-proof-derivation-report 'checked reports proof '())
                     (make-math-proof-derivation-report
                       (math-proof-result-status checked) reports #f
                        (math-proof-result-errors checked)))))))))))

(define (math-proof-from-derivation derivation)
  "Return the checked proof for a fully supported derivation, or #f."
  (let ((report (math-proof-elaborate-derivation derivation)))
    (and (eq? 'checked (math-proof-derivation-report-status report))
         (math-proof-derivation-report-proof report))))

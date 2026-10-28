;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove math proof)
             ((grove math formula) #:prefix φ:)
             (grove math derivation)
             (grove math pipeline)
             (grove math rewrite)
             (grove math term)
             ((grove math semantic) #:prefix ◆:)
             (grove math semantic-check)
             (grove math verification)
             (srfi srfi-1)
             (srfi srfi-64))

(test-begin "math proof IR and checker")

(define scalar-x '((scalar (var x))))
(define add-zero-proof
  (math-proof-rule-instance 'add-zero
    '((term . (var x)) (zero . (num "0"))) '()))
(define add-zero-check (math-proof-check add-zero-proof scalar-x))

(let* ((zero (string-copy "0"))
       (from (list '+ '(var x) (list 'num zero)))
       (to '(var x))
       (path (list 0))
       (step (make-math-derivation-step from to 'add-zero path))
       (terms (list from to))
       (steps (list step))
       (assumptions (list '(scalar (var x))))
       (source (string-copy "math.plant"))
       (sources (list source))
       (requirements (list '(vector (var E))))
       (derivation (make-math-derivation
                     terms steps assumptions
                     #:sources sources #:requirements requirements)))
  (set-car! from '-)
  (string-set! zero 0 #\9)
  (set-car! path 9)
  (set-car! terms '(var changed))
  (set-car! steps #f)
  (set-car! assumptions '(changed))
  (string-set! source 0 #\x)
  (set-car! requirements '(changed))
  (test-equal "derivations snapshot caller-owned terms and collections"
    '(((+ (var x) (num "0")) (var x))
      ((scalar (var x)))
      ("math.plant")
      ((vector (var E))))
    (list (math-derivation-terms derivation)
          (math-derivation-assumptions derivation)
          (math-derivation-sources derivation)
          (math-derivation-requirements derivation)))
  (test-equal "derivation steps snapshot their terms and paths"
    '((+ (var x) (num "0")) (var x) add-zero (0))
    (let ((stored-step (car (math-derivation-steps derivation))))
      (list (math-derivation-step-from stored-step)
            (math-derivation-step-to stored-step)
            (math-derivation-step-rule stored-step)
            (math-derivation-step-path stored-step))))
  (let* ((exposed-terms (math-derivation-terms derivation))
         (exposed-term (car exposed-terms))
         (exposed-step (car (math-derivation-steps derivation)))
         (exposed-from (math-derivation-step-from exposed-step))
         (exposed-path (math-derivation-step-path exposed-step)))
    (set-car! exposed-terms '(var changed))
    (set-car! exposed-term '-)
    (string-set! (cadr (caddr exposed-term)) 0 #\8)
    (set-car! exposed-from '-)
    (set-car! exposed-path 8)
    (test-equal "derivation accessors return independent snapshots"
      '(((+ (var x) (num "0")) (var x))
        (+ (var x) (num "0"))
        (0))
      (list (math-derivation-terms derivation)
            (math-derivation-step-from
              (car (math-derivation-steps derivation)))
            (math-derivation-step-path
              (car (math-derivation-steps derivation)))))))

(test-assert "proof checks a rule application independently of rewrite search"
  (and (math-semantic-check-valid? (math-polynomial-check-rule 'add-zero))
       (math-proof-result-valid? add-zero-check)
       (equal? (math-proof-result-left add-zero-check) '(+ (var x) (num "0")))
       (equal? (math-proof-result-right add-zero-check) '(var x))))
(test-equal "a rule instance requires the assumptions needed by its type guard"
  'rejected (math-proof-result-status (math-proof-check add-zero-proof '())))
(test-equal "proof nodes reject missing premise references"
  'rejected (math-proof-result-status (math-proof-check (math-proof-premise 0) '())))
(let ((term '(+ (* (var x) (num "2")) (var y))))
  (test-equal "term paths enumerate root and operands in order"
    '(() (0) (0 0) (0 1) (1))
    (math-term-paths term))
  (test-equal "term replacement rebuilds the selected operand path"
    '(+ (* (var x) (num "3")) (var y))
    (math-term-replace-at-path term '(0 1) '(num "3")))
  (test-equal "term replacement supports the root path"
    '(var z)
    (math-term-replace-at-path term '() '(var z)))
  (test-equal "term replacement rejects nonexistent paths"
    #f (math-term-replace-at-path term '(2) '(var z)))
  (test-equal "term replacement does not descend into leaf encodings"
    #f (math-term-replace-at-path term '(0 0 0) '(var z)))
  (test-equal "term replacement rejects invalid replacement values"
    #f (math-term-replace-at-path term '(0) #f)))
(test-error "term path enumeration rejects invalid terms" #t
  (math-term-paths #f))
(test-equal "proof checker rejects contradictory assumptions"
  'rejected
  (math-proof-result-status
    (math-proof-check (math-proof-reflexivity '(var x))
      '((scalar (var x)) (vector (var x))))))
(test-assert "equality assumptions can serve as equality proof leaves"
  (let ((result (math-proof-check (math-proof-premise 0)
                  '((= (var x) (var y))))))
    (and (math-proof-result-valid? result)
         (equal? (math-proof-result-left result) '(var x))
         (equal? (math-proof-result-right result) '(var y)))))
(test-equal "rule instances reject missing or forged metavariable substitutions"
  'rejected
  (math-proof-result-status
    (math-proof-check
      (math-proof-rule-instance 'add-zero '((term . (var x))) '()) scalar-x)))
(test-equal "the checker refuses rules with no verified universal theorem"
  'unsupported
  (math-proof-result-status
    (math-proof-check
      (math-proof-rule-instance 'differentiate-power '() '()) scalar-x)))

(let* ((whole-from '(+ (+ (var x) (num "0")) (var y)))
       (whole-to '(+ (var x) (var y)))
       (context-proof (math-proof-congruence whole-from whole-to '(0) add-zero-proof))
       (context-check (math-proof-check context-proof
                        '((scalar (var x)) (scalar (var y)))))
       (chain-proof
         (math-proof-transitivity add-zero-proof
           (math-proof-symmetry add-zero-proof)))
       (chain-check (math-proof-check chain-proof scalar-x)))
  (test-assert "congruence checks an exact operand context"
    (and (math-proof-result-valid? context-check)
         (equal? (math-proof-result-left context-check) whole-from)
         (equal? (math-proof-result-right context-check) whole-to)))
  (test-assert "symmetry and transitivity compose checked equalities"
    (and (math-proof-result-valid? chain-check)
         (equal? (math-proof-result-left chain-check) '(+ (var x) (num "0")))
         (equal? (math-proof-result-right chain-check) '(+ (var x) (num "0")))))
  (test-equal "forged congruence contexts are rejected"
    'rejected
    (math-proof-result-status
      (math-proof-check
        (math-proof-congruence whole-from '(+ (var y) (var y)) '(0) add-zero-proof)
        '((scalar (var x)) (scalar (var y)))))))

(test-equal "transitivity rejects a discontinuous proof chain"
  'rejected
  (math-proof-result-status
    (math-proof-check
      (math-proof-transitivity add-zero-proof
      (math-proof-reflexivity '(var y))) scalar-x)))

(test-assert "a checked rewrite trace elaborates into an independently checked proof"
  (let* ((verification (φ:check-chain '("x+0" "x")
                        #:assumptions '("scalar(x)") #:steps '(add-zero)))
         (derivation (math-verification-derivation verification))
         (proof (math-proof-from-derivation derivation))
         (elaboration (math-proof-elaborate-derivation derivation))
         (step-report (car (math-proof-derivation-report-steps elaboration)))
         (checked (and proof
                       (math-proof-check proof
                         (math-derivation-assumptions derivation)))))
    (and (math-verification-valid? verification)
         (eq? 'checked (math-proof-derivation-report-status elaboration))
         (eq? 'add-zero (math-proof-step-report-rule step-report))
         (eq? 'semantically-checked
              (math-proof-step-report-theorem-status step-report))
         (eq? 'checked (math-proof-step-report-status step-report))
         (math-proof-result-valid? checked)
         (equal? (math-proof-result-left checked) '(+ (var x) (num "0")))
         (equal? (math-proof-result-right checked) '(var x)))))

(test-assert "conditional cancellation retains and proves its nonzero premise"
  (let* ((verification
           (φ:check-chain '("(x*a)/a" "x")
             #:assumptions '("scalar(x); scalar(a); nonzero(a)")
             #:steps '(cancel-common-factor)))
         (derivation (math-verification-derivation verification))
         (proof (math-proof-from-derivation derivation))
         (elaboration (math-proof-elaborate-derivation derivation))
         (step-report (car (math-proof-derivation-report-steps elaboration)))
         (checked (and proof
                       (math-proof-check proof
                         (math-derivation-assumptions derivation)))))
    (and (math-verification-valid? verification)
         (eq? 'checked (math-proof-step-report-status step-report))
         (equal? (math-proof-step-report-premises step-report)
                 '((nonzero (var a))))
         (math-proof-result-valid? checked)
         (equal? (math-proof-result-left checked)
                 '(/ (* (var x) (var a)) (var a)))
         (equal? (math-proof-result-right checked) '(var x)))))
(test-equal "conditional cancellation fails without its premise reference"
  'rejected
  (math-proof-result-status
    (math-proof-check
      (math-proof-rule-instance 'cancel-common-factor
        '((value . (var x)) (factor . (var a))) '())
      '((scalar (var x)) (scalar (var a))))))

(test-equal "elaboration keeps theorem failures visible for algorithm steps"
  'unsupported
  (let* ((x (◆:coordinate 'x))
         (p (◆:add (◆:power x (◆:constant 3))
                   (◆:multiply (◆:constant 2) x)))
         (calculation (math-differentiate p x #:assumptions (list x)))
         (elaboration
           (math-proof-elaborate-derivation
             (math-report-derivation calculation)))
         (steps (math-proof-derivation-report-steps elaboration))
         (status-of
           (lambda (name)
             (let ((step (find (lambda (candidate)
                                 (eq? name (math-proof-step-report-rule candidate)))
                         steps)))
               (and step
                    (list (math-proof-step-report-theorem-status step)
                          (math-proof-step-report-status step)))))))
    (and (math-report-complete? calculation)
         (equal? (map status-of '(differentiate-power multiply-one normalize-scalar))
                 '((unsupported unsupported)
                   (semantically-checked checked)
                   (unsupported unsupported)))
         (eq? 'unsupported (math-proof-derivation-report-status elaboration))
         (not (math-proof-derivation-report-proof elaboration))
         (math-proof-derivation-report-status elaboration))))
(test-equal "a forged derivation edge is rejected with its step location"
  '(rejected 0)
  (let* ((source (φ:term "x+0"))
         (target (φ:term "y"))
         (derivation
           (make-math-derivation
             (list source target)
             (list (make-math-derivation-step source target 'add-zero '()))
             scalar-x))
         (report (math-proof-elaborate-derivation derivation))
         (step (car (math-proof-derivation-report-steps report))))
    (list (math-proof-derivation-report-status report)
          (math-proof-step-report-index step))))

(let* ((term (list 'var 'x))
       (proof (math-proof-reflexivity term)))
  (set-car! term 'invalid)
  (let* ((checked (math-proof-check proof '()))
         (left (math-proof-result-left checked)))
    (test-equal "proof constructors snapshot mutable term input" '(var x)
      (math-proof-result-left checked))
    (set-car! left 'changed)
    (test-equal "proof result accessors return independent terms" '(var x)
      (math-proof-result-left checked))))

(let* ((source (φ:term "x+0"))
       (target (φ:term "x"))
       (derivation (make-math-derivation
                     (list source target)
                     (list (make-math-derivation-step source target 'add-zero '(0)))
                     scalar-x))
       (report (math-proof-elaborate-derivation derivation))
       (steps (math-proof-derivation-report-steps report)))
  (set-car! steps #f)
  (test-eq "proof derivation reports protect their step collection"
    'add-zero
    (math-proof-step-report-rule
      (car (math-proof-derivation-report-steps report)))))

(test-end "math proof IR and checker")

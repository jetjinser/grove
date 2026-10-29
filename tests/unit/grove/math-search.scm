;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((grove math formula) #:prefix φ:) (grove math calculation) (grove math rewrite)
             (grove math pass) (grove math term)
             (srfi srfi-1) (srfi srfi-64))
(define facts '("scalar(x); scalar(y)"))
(define (fingerprint report)
  (list (math-report-reason report) (math-report-states report) (math-report-limits report)
    (map (lambda (certificate)
           (list (math-derivation-terms certificate)
                 (map (lambda (step) (list (math-derivation-step-rule step) (math-derivation-step-path step)))
                      (math-derivation-steps certificate))))
         (math-report-candidates report))))
(test-begin "bounded mathematical search")
;; A small independently enumerable graph: each add-zero edge deletes exactly
;; one of the two authored zeros. Both paths have two edges, with no other paths.
(let ((report (φ:search "(x+0)+(y+0)" "x+y" #:assumptions facts #:rules '(add-zero))))
  (test-equal "exhaustive search finds both independently enumerated paths"
    '(((0) (1)) ((1) (0)))
    (map (lambda (certificate) (map math-derivation-step-path (math-derivation-steps certificate)))
         (math-report-candidates report)))
  (test-equal "explored states include the two distinct goal paths" 5 (math-report-states report))
  (test-assert "every exhaustive candidate independently replays"
    (every (lambda (certificate) (math-verification-valid? (math-verify-derivation certificate)))
           (math-report-candidates report))))
(for-each
  (lambda (depth)
    (for-each
      (lambda (states)
        (let* ((run (lambda () (φ:search "(x+0)+(y+0)" "x+y" #:assumptions facts
                                #:rules '(add-zero) #:max-depth depth #:max-states states)))
               (report (run)))
          (test-equal "identical bounded searches are reproducible" (fingerprint report) (fingerprint (run)))
          (test-assert "state budget bounds all explored states" (<= (math-report-states report) states))
          (test-assert "depth budget bounds every retained candidate"
            (every (lambda (certificate) (<= (length (math-derivation-steps certificate)) depth))
                   (math-report-candidates report)))
          (test-assert "all cutoff candidates remain independently replayable"
            (every (lambda (certificate) (math-verification-valid? (math-verify-derivation certificate)))
                   (math-report-candidates report)))
          (when (pair? (math-report-limits report))
            (test-assert "a bounded cutoff never claims exhaustion" (not (math-report-complete? report))))))
      '(1 2 4 5)))
  '(0 1 2))
(let ((report (φ:search "(x+0)+(y+0)" "x+y" #:assumptions facts #:rules '(add-zero) #:max-results 1)))
  (test-equal "result limit retains the already found path" 1 (length (math-report-candidates report)))
  (test-equal "result limit does not masquerade as an exhaustive result" 'result-limit (math-report-reason report)))
(let ((report (φ:search "x" "x" #:assumptions facts #:max-depth 0 #:max-states 1)))
  (test-assert "an exactly exhausted budget with no remaining work is complete" (math-report-complete? report))
  (test-equal "zero-edge certificates need no invented transition" '()
    (math-derivation-steps (car (math-report-candidates report)))))
(for-each
  (lambda (size)
    (let ((report (φ:search "y" "x" #:backward? #t
                    #:assumptions facts #:rules '(add-zero)
                    #:max-depth 3 #:max-states 6 #:max-term-size size)))
      (test-assert "inverse growth is bounded by the explored-state budget" (<= (math-report-states report) 6))
      (test-assert "unbounded inverse growth is reported as a cutoff" (not (math-report-complete? report)))
      (test-equal "inverse growth retains a term-size stopping cause" #t
        (and (memq 'term-size-limit (math-report-limits report)) #t))))
  '(1 3 5))
(test-error "Formula search direction must be boolean" #t
  (φ:search "x" "y" #:backward? 'reverse))
;; Inject a small cyclic transition graph at the answer boundary. This tests
;; traversal policy independently of the production rule implementation.
(let ((saved math-rewrite-answers))
  (dynamic-wind
    (lambda ()
      (module-set! (resolve-module '(grove math rewrite)) 'math-rewrite-answers
        (lambda* (facts term rules backward? #:key (max-results #f))
          (cond ((equal? term '(var x)) '(((var y) test-flip ())))
                ((equal? term '(var y)) '(((var x) test-flip ())))
                (else '())))))
    (lambda ()
      (let ((report (run-math-pass '(var x) '((scalar (var x)) (scalar (var y)))
                      #:name 'cycle-test #:rules '())))
        (test-equal "normalization diagnoses a cycle before recording a repeated term"
          'rewrite-cycle (math-report-reason report))
        (test-equal "cycle reports keep their valid graph prefix" '((var x) (var y))
          (math-derivation-terms (math-report-derivation report))))
      (let ((report (φ:search "x" "0" #:assumptions facts #:rules '() #:max-depth 8)))
        (test-equal "search avoids a cycle within each candidate path" 2 (math-report-states report))
        (test-assert "a fully explored cyclic component can be exhausted" (math-report-complete? report))))
    (lambda () (module-set! (resolve-module '(grove math rewrite)) 'math-rewrite-answers saved))))
(test-end "bounded mathematical search")

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((grove math formula) #:prefix φ:) (grove math calculation)
             (grove math presentation) (grove math provenance)
             (grove math private source) (grove math term)
             (grove math expression) (grove math private provenance)
             ((grove math semantic) #:prefix semantic:)
             (grove render html) (grove ir) (srfi srfi-1) (srfi srfi-64))
(define (located text line column)
  (let ((text (string-copy text)))
    (set-source-properties! text `((filename . "origins.plant") (line . ,line) (column . ,column))) text))
(define (final-origin report) (math-value-origin (math-report-result report)))
(test-begin "generated mathematical provenance")
(let* ((report (φ:simplify (located "(x+0)*(y+0)" 0 0) #:assumptions '("scalar(x); scalar(y)")))
       (origin (final-origin report)))
  (test-equal "copied left operand retains its exact authored column" 2
    (math-location-column (math-origin-location origin '(0))))
  (test-equal "copied right operand retains its exact authored column" 8
    (math-location-column (math-origin-location origin '(1))))
  (test-assert "provenance does not change independent replay"
    (math-verification-valid? (math-verify-derivation (math-report-derivation report))))
  (test-equal "every generated term has a corresponding source entry"
    (length (math-derivation-terms (math-report-derivation report)))
    (length (math-derivation-sources (math-report-derivation report)))))
(let* ((report (φ:collect (located "x+x" 0 0) #:assumptions '("scalar(x)")))
       (origin (final-origin report)))
  (test-equal "identical copied occurrences retain both possible authored sources"
    '(1 3) (map math-location-column (math-origin-locations origin '(1))))
  (test-assert "new coefficient is explicitly marked as generated"
    (math-origin-generated? (math-origin-at-path origin '(0))))
  (test-assert "new coefficient records its producing rule"
    (symbol? (math-origin-rule (math-origin-at-path origin '(0)))))
  (test-equal "generated coefficient points to its rewritten input, not a fictitious literal"
    "x+x" (math-location-excerpt (math-origin-location origin '(0)))))
(let* ((report (φ:differentiate (located "x^3+2*x" 4 2) "x" #:assumptions '("scalar(x)")))
       (passes (math-report-passes report))
       (first (math-report-derivation (car passes)))
       (second (math-report-derivation (cadr passes))))
  (test-equal "the following pass receives the preceding pass's complete origin tree"
    (map math-location-label (math-origin-locations (last (math-derivation-sources first))))
    (map math-location-label (math-origin-locations (car (math-derivation-sources second)))))
  (test-assert "generated results can be reused without losing their origins"
    (math-value-origin (math-report-result
      (φ:simplify (math-report-result report) #:assumptions '("scalar(x)")))))
  (test-assert "expanded step annotations expose authored source locations"
    (string-contains (sxml->html (content->sxml
      (car (math-completed-report->content report #:view 'full)))) "data-source=\"origins.plant:")))
(let* ((expression (φ:add (located "x" 0 0) (string-copy "y+0")))
       (result (φ:simplify expression #:assumptions '("scalar(x); scalar(y)"))))
  (test-equal "generated unlocated siblings never borrow another operand's location" #f
    (math-origin-location (final-origin result) '(1))))
(let* ((report (φ:differentiate (located "x^1" 0 0) "x"
                  #:assumptions '("scalar(x)") #:max-steps 2))
       (diagnostic (car (math-report-diagnostics report))))
  (test-assert "a cutoff after the differentiation pass retains a generated origin"
    (math-origin? (math-diagnostic-origin diagnostic)))
  (test-equal "later-pass diagnostics point back to the authored expression"
    "x^1" (math-location-excerpt (math-diagnostic-source diagnostic))))
(let* ((coordinate (located "x+1" 8 3))
       (report (φ:differentiate (located "x" 0 0) coordinate)))
  (test-equal "coordinate errors use the coordinate's own location" 9
    (math-location-line (math-diagnostic-source (car (math-report-diagnostics report))))))
(let ((report (φ:search (located "x" 0 0) (located "opaque(x)" 6 2)
                #:assumptions '("scalar(x)"))))
  (test-equal "target diagnostics are not attributed to the search's starting expression" 7
    (math-location-line (math-diagnostic-source (car (math-report-diagnostics report))))))
(let ((report (φ:search (located "x+0" 0 0) (located "x" 4 0)
                #:backward? #t
                #:assumptions '("scalar(x)") #:rules '(add-zero) #:max-depth 1)))
  (test-equal "backward certificates propagate provenance in the forward certificate order" 1
    (math-location-line (math-origin-location
      (last (math-derivation-sources (car (math-report-candidates report))))))))
(test-equal "unlocated generated expressions do not acquire invented origins" #f
  (final-origin (φ:simplify (string-copy "1+1"))))
(let* ((input (located "x+1" 3 0))
       (origin (math-source-origin input))
       (location (math-origin-location origin '(0)))
       (file (math-location-file location))
       (excerpt (math-location-excerpt location)))
  (string-set! file 0 #\X)
  (string-set! excerpt 0 #\y)
  (test-equal "source locations protect filename strings from accessor mutation"
    "origins.plant" (math-location-file location))
  (test-equal "source locations protect excerpts from accessor mutation"
    "x+1" (math-location-excerpt location)))
(test-end "generated mathematical provenance")

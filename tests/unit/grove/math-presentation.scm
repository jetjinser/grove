;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules ((grove math formula) #:prefix φ:) (grove math pipeline) (grove math source)
             (grove ir)
             (grove render html) (grove render text)
             (language scribble spec) (system base language)
             (language scribble parser)
             (srfi srfi-13) (srfi srfi-64))

(define (html content) (sxml->html (content->sxml content)))
(define (document source)
  (scribble-evaluate-document (open-input-string source)
    (default-environment scribble) "notes/proof.plant"))
(define (error-text thunk)
  (catch #t (lambda () (thunk) #f)
    (lambda (key . args) (format #f "~s" args))))
(define (count text fragment)
  (let loop ((offset 0) (n 0))
    (let ((next (string-contains text fragment offset)))
      (if next (loop (+ next (string-length fragment)) (+ n 1)) n))))

(test-begin "math presentation and source diagnostics")
(let* ((port (open-input-string "before @bold{nested}\nafter"))
       (fragments (scribble-parse port)))
  (test-equal "nested command parsing preserves the preceding text position" 0
    (assq-ref (source-properties (car fragments)) 'column))
  (test-equal "following text retains its actual source line" 1
    (assq-ref (source-properties (car (reverse fragments))) 'line)))
(define report (φ:differentiate "x^3+2*x" "x" #:assumptions '("scalar(x)")))
(define certificate (math-report-derivation report))
(define (display-report view)
  (car (math-completed-report->content report #:view view)))
(let* ((presented (display-report 'compact))
       (steps (cadr (content-element-children presented))))
  (test-assert "compact derivation steps use a collapsed subtree"
    (and (eq? (content-element-kind steps) 'disclosure)
         (string-prefix? "Derivation ("
                         (content-element-property steps 'title))
         (content-element-property steps 'addressable?)
         (not (content-element-property steps 'open?)))))
(let ((compact (html (display-report 'compact)))
      (full (html (display-report 'full)))
      (result (html (display-report 'result))))
  (test-assert "compact output has a native, initially closed disclosure"
    (and (string-contains compact "<details><summary>Derivation (")
         (not (string-contains compact "open="))
         (not (string-contains compact "<script"))))
  (test-equal "compact keeps endpoints plus both pass tables" 3 (count compact "<mtable>"))
  (test-equal "full renders each pass once" 2 (count full "<mtable>"))
  (test-equal "result displays only two endpoints" 2 (count result "<mtr>"))
  (test-assert "rule identifiers are machine attributes, labels are readable"
    (and (string-contains full "data-rule=\"differentiate-power\"")
         (string-contains full "data-path=")
         (string-contains full "Power rule")
         (not (string-contains full "differentiate-power (at"))))
  (test-equal "plain text retains all steps without duplicate endpoint display"
    (string-append (format #f "Derivation (~a steps)\n" (length (math-derivation-steps certificate)))
                   (content->text (display-report 'full)))
    (content->text (display-report 'compact))))
(test-assert "all views leave the certificate independently replayable"
  (math-verification-valid? (math-verify-derivation certificate)))
(test-error "invalid view fails explicitly" #t (display-report 'other))
(test-error "invalid diagnostic mode fails explicitly" #t
  (math-completed-report->content report #:on-error 'other))

(let ((shown (html (document "@with-assumptions[\"scalar(x)\"]{\n@check-derive[#:on-error 'show]{\n  differentiate(x,x)\n\n  2\n}}"))))
  (test-assert "authored target diagnostics retain line and indentation through a scope"
    (string-contains shown "proof.plant:5:3"))
  (test-assert "source snippet is accessible in a native disclosure"
    (string-contains shown "<details><summary>proof.plant:5:3</summary><pre translate=\"no\"><code translate=\"no\">  2</code></pre></details>"))
  (test-assert "an invalid chain does not render a claimed equality"
    (and (string-contains shown "class=\"math-diagnostic\"")
         (not (string-contains shown "<math ")))))
(test-assert "invalid chains still raise by default with precise source positions"
  (string-contains
    (error-text (lambda () (document "@check-derive[#:assumptions \"scalar(x)\"]{differentiate(x,x)\n  2}")))
    "notes/proof.plant:2:3"))
(let ((shown (html (document "@differentiate[\"x\" #:assumptions \"scalar(x)\" #:max-steps 1 #:on-error 'show]{x^3}"))))
  (test-assert "cutoffs are diagnoses rather than completed derivative displays"
    (and (string-contains shown "step limit")
         (string-contains shown "proof.plant:1:")
         (not (string-contains shown "<math ")))))

(let ((source (string-copy "x + (2 / 0)")))
  (set-source-properties! source '((filename . "notes/arithmetic.plant") (line . 7) (column . 2)))
  (let* ((report (φ:calculate source #:assumptions '("scalar(x)")))
         (diagnostic (car (math-report-diagnostics report)))
         (location (math-diagnostic-source diagnostic)))
    (test-equal "nested invalid arithmetic points at its operand expression" 8 (math-location-line location))
    (test-equal "operand paths map to their grouped source expression" 7 (math-location-column location))
    (test-equal "structured diagnostics retain their source filename" "notes/arithmetic.plant" (math-location-file location))
    (test-equal "structured diagnostics retain the authored source line" "x + (2 / 0)" (math-location-excerpt location))))
(let* ((source (string-copy "differentiate(x,x)"))
       (target (string-copy "2")))
  (set-source-properties! source '((filename . "saved.plant") (line . 0) (column . 0)))
  (set-source-properties! target '((filename . "saved.plant") (line . 3) (column . 1)))
  (let* ((checked (φ:check-chain (list source target) #:assumptions '("scalar(x)")))
         (replayed (math-verify-derivation (math-verification-derivation checked))))
    (test-equal "independent replay retains authored source attribution" 4
      (math-location-line (math-diagnostic-source (car (math-verification-diagnostics replayed)))))))
(test-equal "ordinary Scheme inputs do not receive fabricated locations" #f
  (math-diagnostic-source (car (math-verification-diagnostics
    (φ:check-chain (map string-copy '("differentiate(x,x)" "2")) #:assumptions '("scalar(x)"))))))
(let* ((located (string-copy "x")) (computed (string-copy " + y")))
  (set-source-properties! located '((filename . "mixed.plant") (line . 0) (column . 0)))
  (let ((origin (math-source-origin (math-source-text (list located computed)))))
    (test-equal "computed fragments do not inherit another fragment's source location" #f
      (math-origin-location origin '(1)))))
(test-end "math presentation and source diagnostics")

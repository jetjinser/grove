;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math private provenance)
  #:use-module (grove math private source)
  #:use-module (grove math term)
  #:use-module (grove math derivation)
  #:use-module (grove math diagnostic)
  #:use-module (grove math report)
  #:use-module (srfi srfi-1)
  #:export (math-step-origin math-certificate-with-origin math-report-with-origin))

;; Preserve siblings and copied subterms. Newly constructed nodes point to the
;; rewritten operand and its rule. If several identical input occurrences match,
;; retain all their origins rather than pretending to know which one was copied.
(define (math-step-origin step origin)
  (let* ((from (math-derivation-step-from step))
         (to (math-derivation-step-to step))
         (path (math-derivation-step-path step))
         (rule (math-derivation-step-rule step))
         (redex (math-term-at-path from path))
         (redex-origin (math-origin-at-path origin path)))
    (define (children term)
      (if (memq (car term) '(var num zero)) '() (cdr term)))
    (define (replacement term)
      (let ((matches (filter (lambda (path) (equal? term (math-term-at-path redex path)))
                             (math-term-paths redex))))
        (if (pair? matches)
            (math-merge-origins (map (lambda (path) (math-origin-at-path redex-origin path)) matches))
            (math-generated-origin (list redex-origin) (map replacement (children term)) rule))))
    (define (walk old new origin path)
      (if (null? path) (replacement new)
          (math-generated-origin (list origin)
            (map (lambda (index)
                   (let ((child-origin (math-origin-at-path origin (list index))))
                     (if (= index (car path))
                         (walk (list-ref (cdr old) index) (list-ref (cdr new) index)
                               child-origin (cdr path))
                         child-origin)))
                 (iota (length (cdr new)))) #f)))
    (and origin redex (walk from to origin path))))

(define (math-certificate-with-origin certificate origin)
  (let ((sources
          (let walk ((steps (math-derivation-steps certificate)) (origin origin) (sources (list origin)))
            (if (null? steps) (reverse sources)
                (let ((next (math-step-origin (car steps) origin)))
                  (walk (cdr steps) next (cons next sources)))))))
    (make-math-derivation (math-derivation-terms certificate)
      (math-derivation-steps certificate) (math-derivation-assumptions certificate)
      #:sources sources #:requirements (math-derivation-requirements certificate))))

(define (math-report-with-origin report origin)
  (let* ((certificates (map (lambda (certificate) (math-certificate-with-origin certificate origin))
                            (math-report-candidates report)))
         (final-origin (if (null? certificates) origin
                           (last (math-derivation-sources (car certificates)))))
         (passes
           (let attach ((remaining (math-report-passes report)) (origin origin))
             (if (null? remaining) '()
                 (let* ((pass (math-report-with-origin (car remaining) origin))
                        (certificate (math-report-derivation pass)))
                   (cons pass (attach (cdr remaining)
                                (if certificate (last (math-derivation-sources certificate)) origin))))))))
    (make-math-report (math-report-name report) certificates
      (math-report-reason report) (math-report-limits report) (math-report-states report)
      #:diagnostics
      (map (lambda (diagnostic)
             (math-locate-diagnostic diagnostic
               (if (memq (math-report-name report) '(search search-backward)) origin final-origin)
               (math-diagnostic-path diagnostic)))
           (math-report-diagnostics report))
      #:passes passes #:audit (math-report-audit report))))

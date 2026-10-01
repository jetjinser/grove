;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math source)
  #:use-module (grove ir)
  #:use-module (grove math syntax)
  #:use-module (grove math derivation)
  #:use-module (grove math diagnostic)
  #:use-module (grove math report)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (math-location? math-location-file math-location-line math-location-column
            math-location-excerpt math-location-label math-source-text math-source-lines
            math-source-origin math-origin? math-origin-location math-locate-diagnostic
            math-diagnostic-description math-report-with-source))

(define-record-type <math-location>
  (make-math-location file line column excerpt)
  math-location?
  (file math-location-file)
  (line math-location-line)
  (column math-location-column)
  (excerpt math-location-excerpt))

(define-record-type <math-origin>
  (make-math-origin text syntax prefix)
  math-origin?
  (text origin-text)
  (syntax origin-syntax)
  (prefix origin-prefix))

(define (math-location-label location)
  (format #f "~a:~a:~a" (math-location-file location)
          (math-location-line location) (math-location-column location)))

;; Keep a map alongside the plain string, outside the mathematical term IR.
;; Runtime Scheme strings without reader metadata intentionally stay unlocated.
(define (math-source-text fragments)
  (let collect ((fragments fragments) (offset 0) (texts '()) (segments '()))
    (if (null? fragments)
        (let ((text (string-concatenate (reverse texts))))
          (when (any (lambda (segment) (assq 'line (cdr segment))) segments)
            (set-source-properties! text `((segments . ,(reverse segments)))))
          text)
        (let* ((fragment (car fragments))
               (text (content-verbatim-string (list fragment)))
               (properties (cond ((string? fragment) (source-properties fragment))
                                 ((content-text? fragment) (source-properties (content-text-value fragment)))
                                 ((content-layout? fragment) (source-properties (content-layout-raw fragment))))))
          (collect (cdr fragments) (+ offset (string-length text)) (cons text texts)
            (cons (cons offset properties) segments))))))

(define (location-at text offset)
  (let* ((properties (source-properties text))
         (segments (or (assq-ref properties 'segments) (list (cons 0 properties))))
         (segment (find (lambda (segment) (<= (car segment) offset)) (reverse segments))))
    (and segment (integer? (assq-ref (cdr segment) 'line))
         (let* ((prefix (substring text (car segment) offset))
                (newlines (string-count prefix #\newline))
                (last-newline (string-rindex prefix #\newline))
                (line (+ 1 (assq-ref (cdr segment) 'line) newlines))
                (column (+ 1 (if last-newline (- (string-length prefix) last-newline 1)
                                 (+ (or (assq-ref (cdr segment) 'column) 0) (string-length prefix)))))
                (before (string-rindex (substring text 0 offset) #\newline))
                (after (string-index text #\newline offset)))
           (make-math-location (or (assq-ref (cdr segment) 'filename) "<input>") line column
             (substring text (if before (+ before 1) 0) (or after (string-length text))))))))

(define* (math-source-origin text #:key (prefix '()))
  (and (location-at text 0) (make-math-origin text (parse-math text) prefix)))

(define (expression-at expression path)
  (cond ((null? path) expression)
        ((eq? (math-expression-kind expression) 'group)
         (expression-at (car (math-expression-children expression)) path))
        ((< (car path) (length (math-expression-children expression)))
         (expression-at (list-ref (math-expression-children expression) (car path)) (cdr path)))
        (else #f)))

(define* (math-origin-location origin #:optional (path '()))
  (and origin
       (let* ((path (if (and (list? path)
                            (every (lambda (index) (and (exact-integer? index) (>= index 0))) path))
                       path '()))
              (prefix (origin-prefix origin))
              (path (if (and (>= (length path) (length prefix))
                             (equal? (take path (length prefix)) prefix))
                        (drop path (length prefix)) '()))
              (expression (expression-at (origin-syntax origin) path)))
         (location-at (origin-text origin)
                      (if expression (math-expression-start expression) 0)))))

(define (math-source-lines text)
  (let lines ((remaining (string-split text #\newline)) (offset 0) (result '()))
    (if (null? remaining) (reverse result)
        (let* ((line (car remaining)) (trimmed (string-trim-both line))
               (leading (- (string-length line) (string-length (string-trim line))))
               (location (location-at text (+ offset leading))))
          (when location
            (set-source-properties! trimmed
              `((filename . ,(math-location-file location))
                (line . ,(- (math-location-line location) 1))
                (column . ,(- (math-location-column location) 1)))))
          (lines (cdr remaining) (+ offset (string-length line) 1)
                 (if (string-null? trimmed) result (cons trimmed result)))))))

(define (math-locate-diagnostic diagnostic origin path)
  (make-math-diagnostic (math-diagnostic-step diagnostic) (math-diagnostic-rule diagnostic)
    (math-diagnostic-path diagnostic) (math-diagnostic-code diagnostic)
    (math-diagnostic-premises diagnostic) (math-diagnostic-message diagnostic)
    #:source (or (math-diagnostic-source diagnostic) (math-origin-location origin path))))

(define (math-diagnostic-description diagnostic)
  (let ((location (math-diagnostic-source diagnostic)))
    (string-append (if location (string-append (math-location-label location) ": ") "")
                   (math-diagnostic-message diagnostic))))

(define* (math-report-with-source report text #:key (prefix '()))
  (let ((origin (math-source-origin text #:prefix prefix)))
    (if (not origin) report
        (let attach ((report report) (initial? #t))
          (make-math-report (math-report-name report)
            (map (lambda (certificate)
                   (make-math-derivation (math-derivation-terms certificate)
                     (math-derivation-steps certificate) (math-derivation-assumptions certificate)
                     #:sources (if initial?
                                   (cons origin (make-list (- (length (math-derivation-terms certificate)) 1) #f))
                                   '())))
                 (math-report-candidates report))
            (math-report-reason report) (math-report-limits report) (math-report-states report)
            #:diagnostics
            (map (lambda (diagnostic)
                   (math-locate-diagnostic diagnostic origin
                     (if (and initial? (= (math-report-states report) 0))
                         (math-diagnostic-path diagnostic) '())))
                 (math-report-diagnostics report))
            #:passes (let phases ((passes (math-report-passes report)) (first? initial?))
                       (if (null? passes) '()
                           (cons (attach (car passes) first?) (phases (cdr passes) #f)))))))))

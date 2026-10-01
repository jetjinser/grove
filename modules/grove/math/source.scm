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
            math-location-excerpt math-location-label math-source-text math-source-lines math-source-slice
            math-source-origin math-origin? math-origin-location math-locate-diagnostic
            math-branch-origin math-origin-with-prefix
            math-diagnostic-description math-report-with-source math-report-with-origin))

(define-record-type <math-location>
  (make-math-location file line column excerpt)
  math-location?
  (file math-location-file)
  (line math-location-line)
  (column math-location-column)
  (excerpt math-location-excerpt))

(define-record-type <math-origin>
  (make-math-origin text syntax prefix embedded branches)
  math-origin?
  (text origin-text)
  (syntax origin-syntax)
  (prefix origin-prefix)
  (embedded origin-embedded)
  (branches origin-branches))

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

(define* (math-source-origin text #:key (prefix '()) (syntax #f) (embedded '()))
  (and (or (location-at text 0) (any cdr embedded))
       (make-math-origin text (or syntax (parse-math text)) prefix embedded #f)))

(define (math-branch-origin origins)
  (and (any identity origins) (make-math-origin #f #f '() '() origins)))

(define (math-origin-with-prefix origin prefix)
  (and origin (make-math-origin (origin-text origin) (origin-syntax origin)
                 prefix (origin-embedded origin) (origin-branches origin))))

(define (syntax-location origin expression path)
  (cond
    ((eq? (math-expression-kind expression) 'embedded)
     (math-origin-location (assq-ref (origin-embedded origin) (math-expression-start expression)) path))
    ((null? path) (location-at (origin-text origin) (math-expression-start expression)))
    ((eq? (math-expression-kind expression) 'group)
     (syntax-location origin (car (math-expression-children expression)) path))
    ((< (car path) (length (math-expression-children expression)))
     (syntax-location origin (list-ref (math-expression-children expression) (car path)) (cdr path)))
    (else (location-at (origin-text origin) 0))))

(define* (math-origin-location origin #:optional (path '()))
  (and origin
       (let* ((path (if (and (list? path)
                            (every (lambda (index) (and (exact-integer? index) (>= index 0))) path))
                       path '()))
              (prefix (origin-prefix origin))
              (path (if (and (>= (length path) (length prefix))
                             (equal? (take path (length prefix)) prefix))
                        (drop path (length prefix)) '()))
              (branches (origin-branches origin)))
         (if branches
             (if (null? path) (math-origin-location (find identity branches))
                 (and (< (car path) (length branches))
                      (math-origin-location (list-ref branches (car path)) (cdr path))))
             (syntax-location origin (origin-syntax origin) path)))))

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

(define (math-source-slice text start end)
  (let ((slice (substring text start end)) (location (location-at text start)))
    (when location
      (set-source-properties! slice
        `((filename . ,(math-location-file location))
          (line . ,(- (math-location-line location) 1))
          (column . ,(- (math-location-column location) 1)))))
    slice))

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
  (math-report-with-origin report (math-source-origin text #:prefix prefix)))

(define (math-report-with-origin report origin)
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
                           (cons (attach (car passes) first?) (phases (cdr passes) #f))))))))

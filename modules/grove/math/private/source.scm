;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove math private source)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove private snapshot)
  #:use-module (grove ir)
  #:use-module (grove math private syntax)
  #:use-module (grove math diagnostic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:export (math-location? math-location-file math-location-line math-location-column
            math-location-excerpt math-location-label math-source-text math-source-slice
            math-source-origin math-origin? math-origin-location math-origin-locations math-locate-diagnostic
            math-branch-origin math-origin-at-path math-generated-origin math-merge-origins
            math-origin-generated? math-origin-rule math-diagnostic-description))

(define-record-type <math-location>
  (%make-math-location file line column excerpt)
  math-location?
  (file location-file/raw)
  (line math-location-line)
  (column math-location-column)
  (excerpt location-excerpt/raw))

(define (make-math-location file line column excerpt)
  (%make-math-location (snapshot-data file) line column
                       (snapshot-data excerpt)))

(define (math-location-file location)
  (snapshot-data (location-file/raw location)))

(define (math-location-excerpt location)
  (snapshot-data (location-excerpt/raw location)))

(define-record-type <math-origin>
  (make-math-origin text syntax embedded branches causes rule)
  math-origin?
  (text origin-text)
  (syntax origin-syntax)
  (embedded origin-embedded)
  (branches origin-branches)
  (causes origin-causes)
  (rule math-origin-rule))

(define (math-origin-generated? origin)
  (and origin (math-origin-rule origin) #t))

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
                                 ((content-text? fragment) (source-properties (ir:content-text-value/raw fragment)))
                                 ((content-layout? fragment) (source-properties (ir:content-layout-raw/raw fragment))))))
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

(define* (math-source-origin text #:key (syntax #f) (embedded '()))
  (and (or (location-at text 0) (any cdr embedded))
       (make-math-origin text (or syntax (parse-math text)) embedded #f '() #f)))

(define (math-branch-origin origins)
  (and (any identity origins) (make-math-origin #f #f '() origins '() #f)))

(define (math-generated-origin causes children rule)
  (and (or (any identity causes) (any identity children))
       (make-math-origin #f #f '() children (filter identity causes) rule)))

(define (math-merge-origins origins)
  (let ((origins (delete-duplicates (filter identity origins))))
    (cond ((null? origins) #f)
          ((null? (cdr origins)) (car origins))
          (else (make-math-origin #f #f '() #f origins #f)))))

(define (valid-path? path)
  (and (list? path) (every (lambda (index) (and (exact-integer? index) (>= index 0))) path)))

(define (math-origin-at-path origin path)
  (and origin (valid-path? path)
       (cond
         ((and (origin-syntax origin) (eq? (math-expression-kind (origin-syntax origin)) 'embedded))
          (math-origin-at-path (assq-ref (origin-embedded origin)
                                (math-expression-start (origin-syntax origin))) path))
         ((null? path) origin)
         ((origin-branches origin)
          (and (< (car path) (length (origin-branches origin)))
               (math-origin-at-path (list-ref (origin-branches origin) (car path)) (cdr path))))
         ((origin-syntax origin)
          (let* ((syntax (origin-syntax origin)) (children (math-expression-children syntax))
                 (group? (eq? (math-expression-kind syntax) 'group)))
            (and (or group? (< (car path) (length children)))
                 (math-origin-at-path
                   (make-math-origin (origin-text origin)
                     (list-ref children (if group? 0 (car path)))
                     (origin-embedded origin) #f '() #f)
                   (if group? path (cdr path))))))
         (else (math-merge-origins
                 (map (lambda (cause) (math-origin-at-path cause path)) (origin-causes origin)))))))

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

(define* (math-origin-locations origin #:optional (path '()))
  (let ((selected (math-origin-at-path origin path)))
    (if (not selected) '()
        (delete-duplicates
          (cond
            ((origin-syntax selected)
             (let ((location (syntax-location selected (origin-syntax selected) '())))
               (if location (list location) '())))
            ((pair? (origin-causes selected))
             (append-map math-origin-locations (origin-causes selected)))
            (else (append-map math-origin-locations (or (origin-branches selected) '()))))
          (lambda (left right) (equal? (math-location-label left) (math-location-label right)))))))

(define* (math-origin-location origin #:optional (path '()))
  (let ((locations (math-origin-locations origin path)))
    (and (pair? locations) (car locations))))

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
    #:source (or (math-diagnostic-source diagnostic) (math-origin-location origin path))
    #:origin (or (math-diagnostic-origin diagnostic) (math-origin-at-path origin path))))

(define (math-diagnostic-description diagnostic)
  (let ((location (math-diagnostic-source diagnostic)))
    (string-append (if location (string-append (math-location-label location) ": ") "")
                   (math-diagnostic-message diagnostic))))

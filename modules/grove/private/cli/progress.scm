;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private cli progress)
  #:use-module (grove project)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-13)
  #:export (call-with-cli-progress cli-seconds print-cli-status print-cli-summary))

(define (cli-seconds)
  (/ (get-internal-real-time) internal-time-units-per-second))

(define (terminal-output? port)
  (and (isatty? port) (not (equal? (getenv "TERM") "dumb"))))

(define (display-path path)
  (let ((prefix (string-append (getcwd) "/")))
    (if (string-prefix? prefix path)
        (substring path (string-length prefix)) path)))

(define* (status-line port label message #:optional (color "1;32"))
  (let ((color? (and (terminal-output? port) (not (getenv "NO_COLOR")))))
    (when color? (format port "\x1b[~am" color))
    (format port "~12@a" label)
    (when color? (display "\x1b[0m" port))
    (unless (string-null? message) (format port " ~a" message))))

(define* (print-cli-status label message #:optional (port (current-output-port)))
  (status-line port label message)
  (newline port)
  (force-output port))

(define* (print-cli-summary report elapsed #:key output (checked? #f))
  (print-cli-status "Finished"
    (if checked?
        (format #f "~d artifact~:p checked in ~,2fs"
                (length (build-report-artifacts report)) elapsed)
        (format #f "~d artifact~:p (~d affected) in ~,2fs"
                (length (build-report-artifacts report))
                (length (build-report-affected-artifacts report)) elapsed)))
  (when output (print-cli-status "Output" output))
  (unless (build-report-rendered? report)
    (print-cli-status "Cached" "Renderer cache hit.")))

(define (progress-bar completed total width)
  (let ((filled (if (zero? total) width (quotient (* completed width) total))))
    (string-append "[" (make-string filled #\=)
                   (if (< filled width)
                       (string-append ">" (make-string (- width filled 1) #\space)) "")
                   "]")))

(define stage-verbs
  '((analyze . "Analyzed")
    (render . "Rendered")
    (validate . "Validated")
    (write . "Written")))

(define (call-with-cli-progress action filename procedure)
  "Keep one status line per input and a separate transient reading progress bar."
  (let* ((port (current-error-port))
         (terminal? (terminal-output? port))
         (columns (let ((value (string->number (or (getenv "COLUMNS") "80"))))
                    (if (and (exact-integer? value) (> value 20)) value 80)))
         (bar-width (max 4 (min 28 (- columns 40))))
         (total 0)
         (completed 0)
         (active? #f)
         (timed-stage #f)
         (stage-start 0))
    (define (clear-progress)
      (when active?
        (display "\r\x1b[2K" port)
        (set! active? #f)
        (force-output port)))
    (define (finish-stage)
      ;;     Report how long the previous stage took, so slow phases are
      ;;     visible instead of folding into the total.
      (when timed-stage
        (status-line port
          (assq-ref stage-verbs timed-stage)
          (format #f "in ~,2fs" (- (cli-seconds) stage-start))
          "1;36")
        (newline port)
        (force-output port)
        (set! timed-stage #f)))
    (define (draw-progress path)
      (when terminal?
        (clear-progress)
        (let* ((message (format #f "~a ~d/~d: ~a"
                          (progress-bar completed total bar-width)
                          completed total (display-path path)))
               (available (- columns 14))
               (message (if (> (string-length message) available)
                            (string-append (substring message 0 (- available 1)) "…")
                            message)))
          (status-line port "Reading" message "1;36")
          (set! active? #t)
          (force-output port))))
    (define (report stage detail)
      (case stage
        ((sources) (set! total detail))
        ((read)
         (clear-progress)
         (print-cli-status "Reading" (display-path detail) port)
         (draw-progress detail))
        ((input-cache)
         (when (> (car detail) 0)
           (clear-progress)
           (print-cli-status "Reused"
             (format #f "~d/~d document results" (car detail) (cdr detail)) port)))
        ((read-done)
         (set! completed (+ completed 1))
         (draw-progress detail))
        ((analysis-phase)
         ;;     Analysis phases, reported by the project's own
         ;;     analyzer; each names itself.
         (clear-progress)
         (print-cli-status "Analyzing"
           (format #f "~18a ~,2fs" (car detail) (cdr detail))
           port))
        ((render-page)
         ;;     Page-level progress. A terminal gets a transient bar;
         ;;     a redirected run gets one line per page, which also
         ;;     exposes a single slow page.
         (let* ((done (car detail)) (total (cadr detail))
                (path (caddr detail))
                (bar-width (max 4 (min 28 (- columns 40))))
                (message (format #f "~a ~d/~d: ~a"
                           (progress-bar done total bar-width) done total path)))
           (clear-progress)
           (if terminal?
               (begin
                 (status-line port "Rendering" message "1;36")
                 (set! active? #t)
                 (force-output port))
               (print-cli-status "Rendering" message port))))
        ((render-renderer)
         ;;     One line per renderer: artifacts and wall time locate a
         ;;     slow renderer without per-page noise.
         (clear-progress)
         (print-cli-status "Renderer"
           (format #f "~d: ~d artifacts in ~,2fs"
                   (+ (car detail) 1) (caddr detail) (cadr detail))
           port))
        (else
         (let ((label (case stage
                        ((analyze) "Analyzing")
                        ((render) "Rendering")
                        ((validate) "Validating")
                        ((write) "Writing")
                        (else #f))))
           ;;     A stage may arrive several times (renderers arrive one
           ;;     call each); time each stage as a whole.  Analyzing and
           ;;     Rendering announce themselves through their detail
           ;;     lines, so they print no stage line of their own.
           (when (and (assq stage stage-verbs)
                      (not (eq? timed-stage stage)))
             (finish-stage)
             (set! timed-stage stage)
             (set! stage-start (cli-seconds))
             (unless (memq stage '(analyze render))
               (clear-progress)
               (print-cli-status label (if detail (display-path detail) "") port)))))))
    (dynamic-wind
      (lambda () (print-cli-status action (display-path filename) port))
      (lambda () (procedure report))
      (lambda ()
        (clear-progress)
        (finish-stage)))))

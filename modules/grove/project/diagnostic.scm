;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project diagnostic)
  #:use-module (grove private files)
  #:use-module (grove private procedure)
  #:use-module (grove private snapshot)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-9)
  #:use-module (system vm debug)
  #:use-module (system vm frame)
  #:export (print-project-error
            call-with-project-stage
            project-error? project-error-stage project-error-path
            project-error-key project-error-arguments project-error-stack))

(define-record-type <project-error>
  (%make-project-error stage path key arguments stack)
  project-error?
  (stage project-error-stage)
  (path project-error-path/raw)
  (key project-error-key/raw)
  (arguments project-error-arguments/raw)
  (stack project-error-stack))

(define (make-project-error stage path key arguments stack)
  (%make-project-error stage (snapshot-data path) (snapshot-data key)
                       (snapshot-data arguments) stack))

(define (project-error-path error)
  (snapshot-data (project-error-path/raw error)))

(define (project-error-key error)
  (snapshot-data (project-error-key/raw error)))

(define (project-error-arguments error)
  (snapshot-data (project-error-arguments/raw error)))

(define (diagnostic-color? port)
  (and (isatty? port)
       (not (equal? (getenv "TERM") "dumb"))
       (not (getenv "NO_COLOR"))))

(define (display-colored port color text)
  (if (diagnostic-color? port)
      (format port "\x1b[~am~a\x1b[0m" color text)
      (display text port)))

(define (frame-source frame)
  (false-if-exception
   (and=> (find-source-for-addr (frame-instruction-pointer frame))
          (lambda (source)
            (and (source? source)
                 (let ((file (source-file source))
                       (line (source-line-for-user source))
                       (column (source-column source)))
                   (and file line (list file line column))))))))

(define (source-path file)
  (or (and (file-exists? file) file)
      (%search-load-path file)))

(define (source-context path line)
  (and path
       (false-if-exception
        (call-with-input-file path
          (lambda (port)
            (let ((first (max 1 (- line 2)))
                  (last (+ line 2)))
              (let loop ((number 1) (lines '()))
                (cond
                 ((> number last) (reverse lines))
                 (else
                  (let ((text (read-line port)))
                    (if (eof-object? text)
                        (reverse lines)
                        (loop (1+ number)
                              (if (>= number first)
                                  (cons (cons number text) lines)
                                  lines)))))))))))))

(define (display-frame-source port frame index)
  (let* ((source (frame-source frame))
         (call (false-if-exception
                (format #f "~@y"
                        (frame-call-representation
                         frame #:top-frame? (zero? index))))))
    (newline port)
    (display-colored port "1;36"
      (format #f "~d: ~a" index (or call "<unknown call>")))
    (if source
        (let* ((file (car source))
               (line (cadr source))
               (column (caddr source))
               (path (source-path file)))
          (format port " at ~a:~d~@[:~d~]~%"
                  file line column)
          (when path
            (for-each
             (lambda (entry)
               (let ((number (car entry))
                     (text (cdr entry)))
                 (format port "    ~a ~4d | "
                         (if (= number line) ">" " ") number)
                 (display-colored port (if (= number line) "1;31" "2") text)
                 (newline port)))
             (source-context path line))))
        (newline port))))

(define (display-source-backtrace stack port)
  (define (walk index frame)
    (when frame
      ;; Print callers before their callees, as Guile's backtrace does.
      (walk (1+ index) (frame-previous frame))
      (display-frame-source port frame index)))
  (walk 0 (stack-ref stack 0))
  (newline port))

(define (call-with-project-stage stage path thunk)
  "Attach execution context without flattening the original exception.
STAGE is a symbol, PATH is a non-empty path string or #f, and THUNK is a
procedure. Invalid context is rejected before THUNK runs."
  (unless (symbol? stage)
    (error "project stage must be a symbol" stage))
  (unless (or (not path) (filesystem-path-string? path))
    (error "project stage path must be a non-empty string or #f" path))
  (unless (procedure-accepts-arguments? thunk 0)
    (error "project stage requires a zero-argument thunk" thunk))
  (let ((prompt (make-prompt-tag 'grove-project-stage)))
    (catch #t
      (lambda ()
        (let*
            ((result
              (call-with-prompt
               prompt
               (lambda ()
                 (with-exception-handler
                   (lambda (exception)
                     (abort-to-prompt prompt exception))
                   thunk
                   #:unwind? #f))
               (lambda (continuation exception)
                 (if (eq? (exception-kind exception) 'grove-project-error)
                     (cons 'project-stage-error
                           (exception-args exception))
                     (cons 'project-stage-failure
                           (make-project-error
                            stage path
                            (exception-kind exception)
                            (exception-args exception)
                            (make-stack continuation))))))))
          (cond
           ((and (pair? result)
                 (eq? (car result) 'project-stage-failure))
            (throw 'grove-project-error (cdr result)))
           ((and (pair? result)
                 (eq? (car result) 'project-stage-error))
            (apply throw 'grove-project-error (cdr result)))
           (else result))))
      (lambda (key . arguments)
        (if (eq? key 'grove-project-error)
            (apply throw key arguments)
            (throw 'grove-project-error
                   (make-project-error stage path key arguments #f)))))))

(define (print-project-error error debug?)
  (let* ((port (current-error-port))
         (message
           (call-with-output-string
             (lambda (output)
               (print-exception output #f
                                (project-error-key error)
                                (project-error-arguments error))))))
    (format port "grove: ~a failed~a~%"
            (project-error-stage error)
            (if (project-error-path error)
                (format #f " (~a)" (project-error-path error)) ""))
    (if debug?
        (begin
          (when (project-error-stack error)
            (display-source-backtrace (project-error-stack error) port))
          (display message port))
        (begin
          (display (string-take message (min 700 (string-length message))) port)
          (when (> (string-length message) 700)
            (display "\n… use --debug for the full diagnostic.\n" port))))))

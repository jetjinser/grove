;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove private cli progress) (ice-9 format) (ice-9 regex)
             (srfi srfi-13) (srfi srfi-64))

(define (capture procedure)
  (let ((port (open-output-string)))
    (parameterize ((current-error-port port)) (procedure))
    (get-output-string port)))

(define (regexp-escape text)
  (let ((port (open-output-string)))
    (string-for-each
      (lambda (char)
        (if (member char '(#\. #\$ #\^ #\* #\+ #\? #\( #\) #\[ #\]
                            #\{ #\} #\| #\\))
            (begin (display #\\ port) (display char port))
            (display char port)))
      text)
    (get-output-string port)))

;;     Stage durations are wall-clock, so the millisecond count in a
;;     status line is not stable; tests match "[0-9]+ms" there.
(define (duration-regex)
  "[0-9]+ms")

(test-begin "CLI progress events")

(test-assert "analysis phases print as their own status lines"
  (let ((out (capture
               (lambda ()
                 (call-with-cli-progress "Building" "project.scm"
                   (lambda (report)
                     (report 'analyze #f)
                     (report 'analysis-phase-start 'queries)
                     (report 'analysis-phase '(queries . 1.5))
                     (report 'analyze #f)))))))
    (and (string-match
           (string-append
             (regexp-escape (format #f "~12@a project.scm~%" "Building"))
             (regexp-escape (format #f "~12@a ~18a 1.50s~%" "Analyzing" "queries"))
             (regexp-escape (format #f "~12@a in " "Analyzed"))
             (duration-regex) "\n")
           out)
         #t)))
(test-assert "sub-second durations print in milliseconds"
  (let ((out (capture
               (lambda ()
                 (call-with-cli-progress "Building" "project.scm"
                   (lambda (report)
                     (report 'analysis-phase '(content . 0.16))
                     (report 'render-renderer '(1 0.06 9))))))))
    (and (string-contains out
           (format #f "~12@a ~18a 160ms~%" "Analyzing" "content"))
         (string-contains out
           (format #f "~12@a 2: 9 artifacts in 60ms~%" "Renderer")))))
(test-assert "redirected progress preserves ordered statuses and reports input reuse"
  (let ((out (capture
               (lambda ()
                 (call-with-cli-progress "Building" (string-append (getcwd) "/project.scm")
                   (lambda (report)
                     (report 'sources 2)
                     (report 'read (string-append (getcwd) "/plants/example.plant"))
                     (report 'read-done "plants/example.plant")
                     (report 'input-cache '(0 . 2))
                     (report 'input-cache '(1 . 2))
                     (report 'unknown "ignored")
                     (for-each (lambda (stage) (report stage #f))
                               '(analyze render validate))
                     (report 'write "build")))))))
    (and (string-match
           (string-append
             (regexp-escape (format #f "~12@a project.scm~%" "Building"))
             (regexp-escape (format #f "~12@a plants/example.plant~%" "Reading"))
             (regexp-escape (format #f "~12@a 1/2 document results~%" "Reused"))
             (regexp-escape (format #f "~12@a in " "Analyzed"))
             (duration-regex) "\n"
             (regexp-escape (format #f "~12@a in " "Rendered"))
             (duration-regex) "\n"
             (regexp-escape (format #f "~12@a~%" "Validating"))
             (regexp-escape (format #f "~12@a in " "Validated"))
             (duration-regex) "\n"
             (regexp-escape (format #f "~12@a build~%" "Writing"))
             (regexp-escape (format #f "~12@a in " "Written"))
             (duration-regex) "\n")
           out)
         #t)))
(test-equal "progress preserves the caller's result" 'completed
  (let ((port (open-output-string)))
    (parameterize ((current-error-port port))
      (call-with-cli-progress "Checking" "project.scm" (lambda (_) 'completed)))))
(test-assert "progress preserves failure and restores the diagnostic port"
  (let ((original (current-error-port)))
    (catch 'progress-failure
      (lambda ()
        (capture
          (lambda ()
            (call-with-cli-progress "Building" "project.scm"
              (lambda (_) (throw 'progress-failure 'expected)))))
        #f)
      (lambda (_ value)
        (and (eq? value 'expected) (eq? original (current-error-port)))))))
(test-end "CLI progress events")

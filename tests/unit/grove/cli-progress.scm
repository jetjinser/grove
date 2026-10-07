;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (grove private cli progress) (ice-9 format)
             (srfi srfi-13) (srfi srfi-64))

(define (capture procedure)
  (let ((port (open-output-string)))
    (parameterize ((current-error-port port)) (procedure))
    (get-output-string port)))

(test-begin "CLI progress events")

(test-equal "analysis phases print as their own status lines"
  (string-append
    (format #f "~12@a project.scm~%" "Building")
    (format #f "~12@a ~18a ~,2fs~%" "Analyzing" "queries" 1.5)
    (format #f "~12@a in 0.00s~%" "Analyzed"))
  (capture
    (lambda ()
      (call-with-cli-progress "Building" "project.scm"
        (lambda (report)
          (report 'analyze #f)
          (report 'analysis-phase '(queries . 1.5))
          (report 'analyze #f))))))
(test-equal "redirected progress preserves ordered statuses and reports input reuse"
  (string-append
    (format #f "~12@a project.scm~%" "Building")
    (format #f "~12@a plants/example.plant~%" "Reading")
    (format #f "~12@a 1/2 document results~%" "Reused")
    (format #f "~12@a in 0.00s~%" "Analyzed")
    (format #f "~12@a in 0.00s~%" "Rendered")
    (format #f "~12@a~%" "Validating")
    (format #f "~12@a in 0.00s~%" "Validated")
    (format #f "~12@a build~%" "Writing")
    (format #f "~12@a in 0.00s~%" "Written"))
  (capture
    (lambda ()
      (call-with-cli-progress "Building" (string-append (getcwd) "/project.scm")
        (lambda (report)
          (report 'sources 2)
          (report 'read (string-append (getcwd) "/plants/example.plant"))
          (report 'read-done "plants/example.plant")
          (report 'input-cache '(0 . 2))
          (report 'input-cache '(1 . 2))
          (report 'unknown "ignored")
          (for-each (lambda (stage) (report stage #f)) '(analyze render validate))
          (report 'write "build"))))))
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

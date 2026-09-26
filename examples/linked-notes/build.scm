;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw)
             (system base language)
             (language scribble spec)
             (language scribble site))

(define environment (default-environment scribble))
(define examples-directory (dirname (current-filename)))
(define output-directory
  (or (and (pair? (cdr (command-line))) (cadr (command-line)))
      "/tmp/grove-linked-notes"))

(unless (file-exists? output-directory)
  (mkdir output-directory))

(define (read-page id title filename)
  (make-site-page
    id title
    (call-with-input-file
      (string-append examples-directory "/" filename)
      (lambda (port)
        (eval ((language-reader scribble) port environment)
              environment)))))

(for-each
  (lambda (output)
    (call-with-output-file
      (string-append output-directory "/" (car output) ".html")
      (lambda (port) (display (cdr output) port))))
  (site->html
    (list (read-page "algebra" "Algebra" "algebra.scrbl")
          (read-page "limits" "Limits" "limits.scrbl"))))

(display "Wrote pages to ")
(display output-directory)
(newline)

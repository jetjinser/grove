;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw)
             (language scribble site)
             (language scribble site load))

(define examples-directory (dirname (current-filename)))
(define output-directory
  (or (and (pair? (cdr (command-line))) (cadr (command-line)))
      "/tmp/grove-linked-notes"))

(unless (file-exists? output-directory)
  (mkdir output-directory))

(for-each
  (lambda (output)
    (call-with-output-file
      (string-append output-directory "/" (car output) ".html")
      (lambda (port) (display (cdr output) port))))
  (site->html (load-site-pages examples-directory)))

(display "Wrote pages to ")
(display output-directory)
(newline)

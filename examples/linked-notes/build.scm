;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw)
             (grove input scribble)
             (grove render html)
             (grove site))

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
  (site->html
    (make-site (load-site-pages examples-directory)
               #:title "Linked Notes")))

(display "Wrote pages to ")
(display output-directory)
(newline)

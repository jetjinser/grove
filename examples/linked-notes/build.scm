;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw)
             (grove input scribble)
             (grove render html)
             (grove render text)
             (grove site))

(define examples-directory (dirname (current-filename)))
(define output-directory
  (or (and (pair? (cdr (command-line))) (cadr (command-line)))
      "/tmp/grove-linked-notes"))

(unless (file-exists? output-directory)
  (mkdir output-directory))

(define text-directory (string-append output-directory "/text"))
(unless (file-exists? text-directory)
  (mkdir text-directory))

(define notes-site
  (make-site (load-site-pages examples-directory)
             #:title "Linked Notes"))

(define (write-pages directory extension pages)
  (for-each
    (lambda (output)
      (call-with-output-file
        (string-append directory "/" (car output) "." extension)
        (lambda (port) (display (cdr output) port))))
    pages))

(write-pages output-directory "html" (site->html notes-site))
(write-pages text-directory "txt" (site->text notes-site))

(display "Wrote HTML to ")
(display output-directory)
(display " and text to ")
(display text-directory)
(newline)

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove output)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-64)
  #:use-module (grove output))

(define output-directory
  (format #f "/tmp/grove-output-test-~a" (getpid)))
(define output-path (string-append output-directory "/nested/page.txt"))

(test-begin "grove output")

(dynamic-wind
  (lambda () #t)
  (lambda ()
    (write-artifacts output-directory
      (list (make-artifact "nested/page.txt" "artifact content")))
    (test-assert "artifact writer creates nested output directories"
      (file-exists? output-path))
    (test-equal "artifact writer writes artifact contents"
      "artifact content"
      (call-with-input-file output-path get-string-all)))
  (lambda ()
    (when (file-exists? output-path)
      (delete-file output-path))
    (let ((nested (string-append output-directory "/nested")))
      (when (file-exists? nested)
        (rmdir nested)))
    (when (file-exists? output-directory)
      (rmdir output-directory))))

(test-end "grove output")

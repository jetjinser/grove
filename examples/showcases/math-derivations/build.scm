;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw))
(define example-directory (dirname (current-filename)))
(define output-directory
  (or (and (pair? (cdr (command-line))) (cadr (command-line)))
      "/tmp/grove-math-derivations"))
(primitive-load
  (string-append example-directory "/../../common/forester-showcase.scm"))
(build-forester-showcase example-directory
  #:title "Checked mathematical notes"
  #:home-page "vector-analysis"
  #:description "Vector identities and scalar calculus with replayable steps."
  #:site-url "https://math.example"
  #:output-directory output-directory
  #:cache-key "math-derivations")
(display output-directory)
(newline)

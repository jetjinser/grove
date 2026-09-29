;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw))

(define example-directory (dirname (current-filename)))
(define output-directory
  (or (and (pair? (cdr (command-line))) (cadr (command-line)))
      "/tmp/grove-forest-category"))

(primitive-load
  (string-append example-directory "/../../common/forester-showcase.scm"))

(build-forester-showcase
  example-directory
  #:title "A Small Category Theory Forest"
  #:description "A CC0 excerpt adapted from two trees in Jinser Kafka's forest."
  #:site-url "https://category.example"
  #:output-directory output-directory
  #:cache-key "forest-category")

(display "Wrote category forest outputs to ")
(display output-directory)
(newline)

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw))

(define example-directory (dirname (current-filename)))
(define output-directory
  (or (and (pair? (cdr (command-line))) (cadr (command-line)))
      "/tmp/grove-heat-conduction"))

(primitive-load
  (string-append example-directory "/../../common/forester-showcase.scm"))

(build-forester-showcase
  example-directory
  #:title "Heat: a derivation in notes"
  #:description "A linked, worked derivation of the heat equation."
  #:site-url "https://heat.example"
  #:output-directory output-directory
  #:cache-key "heat-conduction")

(display "Wrote heat-conduction site to ")
(display output-directory)
(newline)

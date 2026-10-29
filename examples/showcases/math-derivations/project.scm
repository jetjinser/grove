;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw))
(define example-directory (dirname (current-filename)))

(primitive-load
  (string-append example-directory "/../../common/forester-showcase.scm"))
(make-forester-showcase example-directory
  #:title "Checked mathematical notes"
  #:home-page "vector-analysis"
  #:description "Vector identities and scalar calculus with replayable steps."
  #:site-url "https://math.example")

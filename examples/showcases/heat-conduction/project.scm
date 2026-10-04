;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 ftw))

(define example-directory (dirname (current-filename)))
(define output-directory "build")

(primitive-load
  (string-append example-directory "/../../common/forester-showcase.scm"))

(make-forester-showcase
  example-directory
  #:title "Heat: a derivation in notes"
  #:home-page "heat-0001"
  #:description "A linked, worked derivation of the heat equation."
  #:site-url "https://heat.example"
  #:output-directory output-directory
  #:cache-key "heat-conduction")


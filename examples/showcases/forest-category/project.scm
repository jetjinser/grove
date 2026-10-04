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
  #:title "A Small Category Theory Forest"
  #:home-page "cat-0001"
  #:description "A CC0 excerpt adapted from two trees in Jinser Kafka's forest."
  #:site-url "https://category.example"
  #:output-directory output-directory)


;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove preview-watch)
  #:use-module (srfi srfi-64)
  #:use-module (grove private preview watch))

(test-begin "grove preview watch")

(define baseline
  '(("a.plant" . 100) ("b.plant" . 100)))

(test-equal "modified known paths are changes"
  '("a.plant")
  (snapshot-changes baseline '(("a.plant" . 200) ("b.plant" . 100))))

(test-equal "additions are changes by default"
  '("new.plant")
  (snapshot-changes baseline '(("a.plant" . 100) ("b.plant" . 100)
                               ("new.plant" . 100))))

(test-equal "removals are always changes"
  '("b.plant")
  (snapshot-changes baseline '(("a.plant" . 100))))

(test-equal "additions can be ignored after a build widens the watch set"
  '("a.plant")
  (snapshot-changes baseline
                    '(("a.plant" . 200) ("b.plant" . 100)
                      ("module.go" . 100) ("module.scm" . 100))
                    #:ignore-additions? #t))

(test-equal "ignoring additions still flags modified known paths"
  '("a.plant")
  (snapshot-changes baseline
                    '(("a.plant" . 200) ("b.plant" . 100)
                      ("module.go" . 100) ("module.scm" . 999))
                    #:ignore-additions? #t))

(test-end "grove preview watch")

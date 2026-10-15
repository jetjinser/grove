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

(test-assert "store paths cannot be project edits"
  (and (unwatched-path? "/gnu/store/abc-site-ccache/grove/project.go")
       (unwatched-path? "/nix/store/abc-site-ccache/grove/project.go")
       (not (unwatched-path? "sources/a.plant"))
       (not (unwatched-path? "/home/user/notes/a.plant"))))

(test-assert "compiled artifacts cannot be project edits"
  (and (unwatched-path? "modules/probe/helper.go")
       (unwatched-path? "/home/user/project/.cache/gcrypt/hash.go")
       (not (unwatched-path? "sources/one.txt"))))

(test-equal "compiled artifacts stay out of snapshots"
  '(("a.plant" . #f))
  (watch-snapshot '() '("a.plant" "modules/probe/helper.go") #f))

(test-equal "store paths stay out of snapshots"
  '(("a.plant" . #f))
  (watch-snapshot '()
                  (list "a.plant"
                        "/gnu/store/abc-site-ccache/grove/collect.go"
                        "/nix/store/abc-site-ccache/ice-9/match.go")
                  #f))

(test-equal "store paths stay out of walked roots"
  '(("a.plant" . #f))
  (watch-snapshot '("/gnu/store/abc-profile/lib/guile/3.0/site-ccache")
                  '("a.plant")
                  #f))

(test-end "grove preview watch")

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Default development environment; optional external tools live in
;;; integration-manifest.scm.
;;;
;;;   guix shell -m manifest.scm
;;;   blue check

(use-modules (gnu packages)
             (guix profiles)
             (ice-9 ftw))

(primitive-load
 (string-append (dirname (canonicalize-path (current-filename)))
                "/guix/package-definitions.scm"))

(packages->manifest
 (append
  (list blue faster-minikanren grove-highlights guile libunistring libidn2
        guile-srfi-275 grove-glibc-locales)
  (map specification->package
       (list ;; /bin/sh and env for subprocess-spawning code under a
             ;; containerized shell.
             "bash"
             "coreutils"
             "guile-ssh"
             "guile-gcrypt"
             "guile-quickcheck"
             "guile-ts"
             "reuse"))))

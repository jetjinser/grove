;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (blueprint install)
  #:use-module (blue build)
  #:use-module (blue states)
  #:use-module (blue types blueprint)
  #:use-module (blue types command)
  #:export (install-command))

(define-command (install-command _)
  ((invoke "install")
   (category 'install)
   (synopsis "Install the project")
   (help "Install all buildables."))
  (install! (blueprint-buildables (current-blueprint))))

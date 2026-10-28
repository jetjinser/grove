;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (invalid-command-entry)
  #:export (command-modules))

(define-public command-modules '((forester commands) "not-a-module-name"))

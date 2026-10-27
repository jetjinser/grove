;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove style highlight)
  #:use-module (grove output)
  #:use-module (grove private string-fun)
  #:use-module (grove style html)
  #:export (default-highlight-style))

(define default-highlight-style
  (make-html-style
    #:stylesheets
    (list (make-artifact "highlight.css"
            (include-text "grove/style/highlight.css")
            '("modules/grove/style/highlight.scm" "modules/grove/style/highlight.css")))))

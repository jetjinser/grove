;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester style html)
  #:use-module (grove output)
  #:use-module (grove string-fun)
  #:use-module (grove style html)
  #:use-module (grove style highlight)
  #:export (default-html-style))

(define forester-stylesheet
  (make-artifact
    "forester.css"
    (include-text "forester/style/default.css")
    (list "modules/forester/style/html.scm"
          "modules/forester/style/default.css")))

(define default-html-style
  (compose-html-styles
    (make-html-style #:stylesheets (list forester-stylesheet))
    default-highlight-style))

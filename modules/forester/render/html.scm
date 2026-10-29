;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render html)
  #:use-module (forester private render html-renderer)
  #:re-export (site->html
               render-html
               make-html-options
               html-options?
               make-html-renderer))

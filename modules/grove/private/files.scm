;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private files)
  #:export (file-signature))

(define (file-signature path)
  "Identify the current file, including same-size edits and atomic replacements."
  (let ((info (stat path #f)))
    (and info
         (list (stat:type info) (stat:dev info) (stat:ino info) (stat:size info)
               (stat:mtime info) (stat:mtimensec info)
               (stat:ctime info) (stat:ctimensec info)))))

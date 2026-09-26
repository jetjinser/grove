;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove collect)
  #:use-module (ice-9 ftw)
  #:export (collect-directory))

(define (collect-directory directory include?)
  (map (lambda (filename)
         (string-append directory "/" filename))
       (scandir directory include? string<?)))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render artifact)
  #:use-module (grove output)
  #:use-module (srfi srfi-1)
  #:export (find-artifact-by-path artifact-reusable?))

(define (find-artifact-by-path artifacts path)
  (find (lambda (artifact)
          (string=? (artifact-path artifact) path))
        artifacts))

(define (artifact-reusable? current previous changed)
  (let ((sources (artifact-sources current)))
    (and previous
         (equal? sources (artifact-sources previous))
         (not (and sources
                   (any (lambda (path) (member path sources string=?))
                        changed))))))

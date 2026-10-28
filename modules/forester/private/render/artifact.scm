;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private render artifact)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module (grove output)
  #:use-module (srfi srfi-1)
  #:export (reuse-artifact))

(define (find-artifact-by-path artifacts path)
  (find (lambda (artifact)
          (string=? (out:artifact-path/raw artifact) path))
        artifacts))

(define (artifact-reusable? sources previous changed)
  (and previous
       (equal? sources (out:artifact-sources/raw previous))
       (not (and sources
                 (any (lambda (path) (member path sources string=?))
                      changed)))))

(define (reuse-artifact path sources previous-artifacts changed render)
  (let ((previous (find-artifact-by-path previous-artifacts path)))
    (if (artifact-reusable? sources previous changed)
        previous
        (make-artifact path (render) sources))))

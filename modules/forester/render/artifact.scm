;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render artifact)
  #:use-module ((grove private output) #:prefix out:)
  #:use-module (grove output)
  #:use-module (srfi srfi-1)
  #:export (find-artifact-by-path artifact-reusable? reuse-artifact))

(define (find-artifact-by-path artifacts path)
  (find (lambda (artifact)
          (string=? (out:artifact-path/raw artifact) path))
        artifacts))

(define (artifact-reusable? current previous changed)
  (let ((sources (out:artifact-sources/raw current)))
    (and previous
         (equal? sources (out:artifact-sources/raw previous))
         (not (and sources
                   (any (lambda (path) (member path sources string=?))
                        changed))))))

(define (reuse-artifact path sources previous-artifacts changed render)
  (let* ((previous (find-artifact-by-path previous-artifacts path))
         (current (make-artifact path "" sources)))
    (if (artifact-reusable? current previous changed)
        previous
        (make-artifact path (render) sources))))

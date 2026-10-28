;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private project incremental)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module (grove private build-state)
  #:use-module (srfi srfi-1)
  #:export (renderer-identities-match?
            renderer-cache-available?
            all-renderers-cacheable?
            changed-renderer-artifact-paths
            reused-renderer-artifact-paths
            retired-pipeline-paths))

(define (renderer-identities-match? current previous)
  (and (list? current) (list? previous) (equal? current previous)))

(define (renderer-cache-available? current previous)
  (and (list? current)
       (or (null? current)
           (and (list? previous)
                (let loop ((current current) (previous previous))
                  (and (pair? current)
                       (or (and (car current)
                                (equal? (car current)
                                        (and (pair? previous) (car previous))))
                           (loop (cdr current)
                                 (if (pair? previous) (cdr previous) '())))))))))

(define (all-renderers-cacheable? identities)
  (and (list? identities) (every identity identities)))

(define (renderer-identity-changed? current previous index)
  (if (and (list? current) (list? previous))
      (not (equal? (and (< index (length current)) (list-ref current index))
                   (and (< index (length previous))
                        (list-ref previous index))))
      #t))

(define (changed-renderer-artifact-paths artifacts owners previous-artifacts
                                         current-identities previous-identities)
  (delete-duplicates
    (append
      (map out:artifact-path/raw
           (filter
             (lambda (artifact)
               (let ((owner (assoc (out:artifact-path/raw artifact) owners)))
                 (and owner
                      (integer? (cdr owner))
                      (renderer-identity-changed? current-identities
                                                  previous-identities
                                                  (cdr owner)))))
             artifacts))
      (map build-state-artifact-path
           (filter
             (lambda (entry)
               (let ((renderer (build-state-artifact-owner entry)))
                 (and (integer? renderer)
                      (renderer-identity-changed? current-identities
                                                  previous-identities
                                                  renderer))))
             previous-artifacts)))
    string=?))

(define (reused-renderer-artifact-paths artifacts owners rendered-indices)
  (map out:artifact-path/raw
       (filter
         (lambda (artifact)
           (let ((owner (assoc (out:artifact-path/raw artifact) owners)))
             (and owner
                  (integer? (cdr owner))
                  (not (member (cdr owner) rendered-indices)))))
         artifacts)))
;;     Pipeline outputs that vanished from a pipeline's current set
;;     must not resurrect from the previous build's state: previous
;;     entries a pipeline owns (string pipeline name) whose paths the
;;     current run no longer emits are retired.
(define (retired-pipeline-paths pipeline-artifacts previous-artifacts)
  (let ((current (map out:artifact-path/raw pipeline-artifacts)))
    (filter-map
      (lambda (entry)
        (let ((path (build-state-artifact-path entry))
              (owner (build-state-artifact-owner entry)))
          (and (string? owner)
               (not (member path current string=?))
               path)))
      previous-artifacts)))

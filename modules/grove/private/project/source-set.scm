;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private project source-set)
  #:use-module (grove private build-state)
  #:use-module (grove private files)
  #:use-module (grove private input dependencies)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-project-source-set
            project-source-set-entries
            project-source-set-changes
            project-source-set-stable?
            project-source-changes-added
            project-source-changes-modified
            project-source-changes-removed
            project-source-changes-all
            paths-intersect?))

;; One build's tracked inputs, separated by whether Grove or a dynamic read
;; discovered them. This is the boundary shared by full builds and retained-
;; model rebuilds; artifact source lists form the outgoing dependency edges.
(define-record-type <project-source-set>
  (%make-project-source-set entries changes baseline)
  project-source-set?
  (entries project-source-set-entries)
  (changes project-source-set-changes)
  (baseline project-source-set-baseline))

(define-record-type <project-source-changes>
  (%make-project-source-changes added modified removed)
  project-source-changes?
  (added project-source-changes-added)
  (modified project-source-changes-modified)
  (removed project-source-changes-removed))

(define (paths-intersect? paths candidates)
  (any (lambda (path) (member path candidates string=?)) paths))

(define (source-entry-by-path entries path)
  (find (lambda (entry)
          (string=? (build-state-source-path entry) path))
        entries))

(define (source-paths entries)
  (map build-state-source-path entries))

(define (changed-sources old-sources current-sources)
  ;; Unfingerprinted paths do not count as additions or removals. Runtime
  ;; paths may relocate between builds, so equal-content runtime additions and
  ;; removals cancel; declared project sources remain path-sensitive.
  (let* ((added
           (filter (lambda (entry)
                     (and (build-state-source-fingerprint entry)
                          (not (source-entry-by-path
                                 old-sources (build-state-source-path entry)))))
                   current-sources))
         (removed
           (filter (lambda (entry)
                     (and (build-state-source-fingerprint entry)
                          (not (source-entry-by-path
                                 current-sources (build-state-source-path entry)))))
                   old-sources))
         (modified
           (filter (lambda (entry)
                     (let ((previous
                             (source-entry-by-path
                               old-sources (build-state-source-path entry))))
                       (and previous
                            (not (equal? (build-state-source-fingerprint previous)
                                         (build-state-source-fingerprint entry))))))
                   current-sources))
         (churned
           (lset-intersection
             (lambda (left right)
               (equal? (build-state-source-fingerprint left)
                       (build-state-source-fingerprint right)))
             (filter (lambda (entry)
                       (eq? (build-state-source-kind entry) 'runtime))
                     added)
             (filter (lambda (entry)
                       (eq? (build-state-source-kind entry) 'runtime))
                     removed)))
         (churned-paths (source-paths churned)))
    (%make-project-source-changes
      (source-paths (filter (lambda (entry)
                              (not (member (build-state-source-path entry)
                                           churned-paths string=?)))
                            added))
      (source-paths modified)
      (source-paths (filter (lambda (entry)
                              (not (member (build-state-source-path entry)
                                           churned-paths string=?)))
                            removed)))))

(define* (make-project-source-set declared runtime previous
                                  #:key (fingerprint input-fingerprint)
                                        (baseline '())
                                        (omit-unavailable? #f))
  "Create this build's tracked input entries and their changes from PREVIOUS.
DECLARED and RUNTIME are unique path lists; declared paths remain path-sensitive
while equal-content runtime paths may move between build processes."
  (unless (unique-filesystem-path-list? declared)
    (error "declared project source paths must be unique" declared))
  (unless (unique-filesystem-path-list? runtime)
    (error "runtime project source paths must be unique" runtime))
  (let* ((declared (delete-duplicates declared string=?))
         (runtime (lset-difference string=?
                                   (delete-duplicates runtime string=?)
                                   declared))
         (entries
           (append-map
             (lambda (paths kind)
               (filter-map (lambda (path)
                             (let ((digest (fingerprint path)))
                               (and (or digest (not omit-unavailable?))
                                    (list path digest kind))))
                           paths))
             (list declared runtime) '(source runtime))))
    (%make-project-source-set
      entries
      (and previous (changed-sources previous entries))
      baseline)))

(define* (project-source-set-stable? source-set
                                     #:key (fingerprint input-fingerprint))
  "Return whether SOURCE-SET inputs stayed unchanged through the build."
  (let ((entries (project-source-set-entries source-set)))
    (not
      (or (any (lambda (entry)
                 (let ((source (source-entry-by-path entries (car entry))))
                   (not (equal? (cadr entry)
                                (if source
                                    (build-state-source-fingerprint source)
                                    (fingerprint (car entry)))))))
               (project-source-set-baseline source-set))
          (any (lambda (entry)
                 (not (equal? (build-state-source-fingerprint entry)
                              (fingerprint
                                (build-state-source-path entry)))))
               entries)))))

(define (project-source-changes-all changes)
  (append (project-source-changes-added changes)
          (project-source-changes-modified changes)
          (project-source-changes-removed changes)))

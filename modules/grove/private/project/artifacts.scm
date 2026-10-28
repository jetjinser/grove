;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private project artifacts)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module (grove output)
  #:use-module (grove private artifact-store)
  #:use-module (grove private build-state)
  #:use-module (grove private files)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (paths-intersect?
            source-entry-by-path
            artifact-entry-by-path
            changed-sources
            affected-artifacts
            load-cached-artifacts))

(define (source-paths entries)
  (map build-state-source-path entries))

(define (paths-intersect? paths candidates)
  (any (lambda (path) (member path candidates string=?)) paths))

(define (source-entry-by-path entries path)
  (find (lambda (entry)
          (string=? (build-state-source-path entry) path))
        entries))

(define (artifact-entry-by-path entries path)
  (find (lambda (entry)
          (string=? (build-state-artifact-path entry) path))
        entries))

(define (changed-sources old-sources current-sources)
  ;;     An absent file has no content and cannot shape the outputs.
  ;;     Project loads record load-path candidates that do not exist,
  ;;     and those candidate sets shift between build processes, so
  ;;     absent entries are dropped before the comparison instead of
  ;;     counting as additions and removals. Runtime inputs (module
  ;;     files resolved through the load path) can additionally land
  ;;     under different absolute paths in different build processes
  ;;     without changing what the build produces, so a
  ;;     removed-and-re-added pair with the same content cancels.
  ;;     Declared project sources never cancel: a renamed page is a
  ;;     real change.
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
                                         (build-state-source-fingerprint
                                           entry))))))
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
         (churned-paths (source-paths churned))
         (added (filter (lambda (entry)
                          (not (member (build-state-source-path entry)
                                       churned-paths string=?)))
                        added))
         (removed (filter (lambda (entry)
                            (not (member (build-state-source-path entry)
                                         churned-paths string=?)))
                          removed)))
    (values (source-paths added)
            (source-paths modified)
            (source-paths removed))))

(define (artifact-affected? artifact previous changed)
  (let* ((path (out:artifact-path/raw artifact))
         (old (artifact-entry-by-path previous path))
         (sources (out:artifact-sources/raw artifact))
         (old-sources (and old (build-state-artifact-sources old))))
    (or (not old)
        (not (equal? (build-state-artifact-fingerprint old)
                     (content-fingerprint (out:artifact-content/raw artifact))))
        (not (equal? sources old-sources))
        (not sources)
        (paths-intersect? (append (or sources '())
                                  (or old-sources '()))
                          changed))))

(define (affected-artifacts artifacts previous changed)
  (append
    (map out:artifact-path/raw
         (filter (lambda (artifact)
                   (artifact-affected? artifact previous changed))
                 artifacts))
    (map build-state-artifact-path
         (filter (lambda (entry)
                   (not (find (lambda (artifact)
                                (string=? (out:artifact-path/raw artifact)
                                          (build-state-artifact-path entry)))
                              artifacts)))
                 previous))))

(define (load-cached-artifacts directory entries)
  (let loop ((entries entries) (artifacts '()))
    (if (null? entries)
        (reverse artifacts)
        (let* ((entry (car entries))
               (path (string-append directory "/"
                                    (build-state-artifact-path entry)))
               ;;     The output file may be missing in a fresh
               ;;     checkout whose build directory was deleted: the
               ;;     shared store holds the bytes under the entry's
               ;;     fingerprint, so restore the file instead of
               ;;     paying for a re-render.
               (_ (when (and (not (regular-file? path #:follow-links? #f))
                             (build-state-artifact-fingerprint entry))
                    (restore-artifact-blob!
                     (build-state-artifact-fingerprint entry) path)))
               (content
                 (and (regular-file? path #:follow-links? #f)
                      (catch #t
                        (lambda ()
                          (if (eq? (build-state-artifact-kind entry) 'bytes)
                              (let ((port (open-file path "rb")))
                                (dynamic-wind
                                  (lambda () #t)
                                  (lambda () (get-bytevector-all port))
                                  (lambda () (close-port port))))
                              (call-with-input-file path
                                (lambda (port)
                                  (set-port-encoding! port "UTF-8")
                                  (get-string-all port)))))
                        (lambda args #f)))))
          (if (and content
                   (equal? (content-fingerprint content)
                           (build-state-artifact-fingerprint entry)))
              (loop (cdr entries)
                    (cons ((if (eq? (build-state-artifact-kind entry) 'bytes)
                               make-byte-artifact make-artifact)
                           (build-state-artifact-path entry) content
                           #:sources (build-state-artifact-sources entry))
                          artifacts))
              #f)))))

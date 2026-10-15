;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private preview watch)
  #:use-module (grove private files)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (absolute-path file-stamp watch-snapshot snapshot-changes
            unwatched-path?))

(define (absolute-path path)
  "Resolve even nonexistent paths, removing dot components without following links."
  (let ((path (if (absolute-file-name? path) path
                  (string-append (getcwd) "/" path))))
    (string-append "/"
      (string-join
        (reverse
          (fold (lambda (part result)
                  (cond
                    ((member part '("" ".")) result)
                    ((string=? part "..") (if (null? result) result (cdr result)))
                    (else (cons part result))))
                '() (string-split path #\/)))
        "/"))))

(define (path-within? path directory)
  (and directory
       (or (string=? path directory)
           (string-prefix? (string-append directory "/") path))))

(define (store-path? path)
  ;;     Store items are immutable. A stamp flip under a store prefix
  ;;     means the environment swapped underneath the session - never
  ;;     that a project file was edited - and load-path resolutions can
  ;;     even disagree about whether such a path exists, so stamps
  ;;     alternate between a signature and nothing.
  (or (string-prefix? "/gnu/store/" path)
      (string-prefix? "/nix/store/" path)))

(define grove-compiled-root
  (let ((cache #f))
    (lambda ()
      ;;     The compiled cache changes behind builds, and the worker's
      ;;     load-path candidates do not agree with the serve process's
      ;;     resolution of the same tree. Watching it re-queues rebuilds
      ;;     that no project edit caused. The source tree stays watched:
      ;;     a grove module edit restarts the worker on purpose.
      (unless cache
        (let ((compiled (search-path %load-compiled-path "grove/project.go")))
          (set! cache (and compiled
                       (dirname (dirname (absolute-path compiled)))))))
      cache)))

(define (unwatched-path? path)
  "Paths that cannot be project edits: immutable store items, compiled
artifacts, and grove's own compiled tree. A .go file changes behind
builds and load-path resolutions, never because a source was edited."
  (or (store-path? path)
      (string-suffix? ".go" path)
      (let ((root (grove-compiled-root)))
        (and root (path-within? path root)))))

(define (file-stamp path)
  (cons path (file-signature path)))

(define (watch-snapshot roots files output)
  "Record file identity and high-resolution timestamps, detecting additions too.
Directory symlinks are not traversed. Explicit files may live outside ROOTS.
Store paths and grove's compiled tree are excluded: they are not project
edits, and their stamps churn between load-path resolutions."
  (define (walk path)
    (cond
      ((unwatched-path? path) '())
      (else
       (let ((info (catch 'system-error (lambda () (lstat path)) (lambda _ #f))))
         (cond
           ((path-within? path output) '())
           ((not info) (list (cons path #f)))
           ((eq? (stat:type info) 'directory)
            (append-map
              (lambda (name) (walk (string-append path "/" name)))
              (or (scandir path (lambda (name) (not (string-prefix? "." name)))) '())))
           (else (list (file-stamp path))))))))
  (sort
    (delete-duplicates
      (append (append-map walk roots)
              (map file-stamp (filter (lambda (path)
                                        (and (not (path-within? path output))
                                             (not (unwatched-path? path))))
                                      files)))
      (lambda (left right) (string=? (car left) (car right))))
    (lambda (left right) (string<? (car left) (car right)))))

(define (snapshot-changes previous current)
  ;;     The periodic scan's diff: additions are edits (new sources),
  ;;     removals are edits, stamp changes on known paths are edits.
  ;;     Post-build comparisons do not belong here - the serve loop
  ;;     diffs watch-set structure across a build instead, because a
  ;;     build's own writes must not requeue it.
  (delete-duplicates
    (append
      (filter-map (lambda (entry)
                    (and (not (equal? entry (assoc (car entry) previous)))
                         (car entry)))
                  current)
      (filter-map (lambda (entry)
                    (and (not (assoc (car entry) current)) (car entry)))
                  previous))
    string=?))

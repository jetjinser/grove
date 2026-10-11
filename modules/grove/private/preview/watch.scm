;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private preview watch)
  #:use-module (grove private files)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (absolute-path file-stamp watch-snapshot snapshot-changes))

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
  (or (string=? path directory)
      (string-prefix? (string-append directory "/") path)))

(define (file-stamp path)
  (cons path (file-signature path)))

(define (watch-snapshot roots files output)
  "Record file identity and high-resolution timestamps, detecting additions too.
Directory symlinks are not traversed. Explicit files may live outside ROOTS."
  (define (walk path)
    (let ((info (catch 'system-error (lambda () (lstat path)) (lambda _ #f))))
      (cond
        ((path-within? path output) '())
        ((not info) (list (cons path #f)))
        ((eq? (stat:type info) 'directory)
         (append-map
           (lambda (name) (walk (string-append path "/" name)))
           (or (scandir path (lambda (name) (not (string-prefix? "." name)))) '())))
        (else (list (file-stamp path))))))
  (sort
    (delete-duplicates
      (append (append-map walk roots)
              (map file-stamp (filter (lambda (path) (not (path-within? path output))) files)))
      (lambda (left right) (string=? (car left) (car right))))
    (lambda (left right) (string<? (car left) (car right)))))

(define* (snapshot-changes previous current #:key (ignore-additions? #f))
  ;;     IGNORE-ADDITIONS? is for the post-build comparison: a build
  ;;     widens the watch set on its first run (loaded modules, store
  ;;     object paths join FILES), and counting those first sightings
  ;;     as edits made every serve start spend one full rebuild on
  ;;     nothing.  Mid-build edits are stamps that differ on paths the
  ;;     baseline already knew; removals still trigger.
  (delete-duplicates
    (append
      (filter-map (lambda (entry)
                    (and (not (equal? entry (assoc (car entry) previous)))
                         (or (not ignore-additions?)
                             (assoc (car entry) previous))
                         (car entry)))
                  current)
      (filter-map (lambda (entry)
                    (and (not (assoc (car entry) current)) (car entry)))
                  previous))
    string=?))

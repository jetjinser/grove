;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Size reporting and mtime-based pruning for the per-user shared
;;; store.  Every subtree (artifacts, keys, input-cache) is a pure
;;; cache: entries are content-derived and every consumer verifies
;;; what it reads, so deletion is always safe and never changes
;;; results - it can only cost recomputation.

(define-module (grove private store-gc)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-19)
  #:export (store-summary
            store-prune
            format-bytes))

(define (store-subtrees root)
  "Return an association list of (NAME PATH) for the store's content
subtrees that exist under ROOT."
  (filter-map
   (lambda (name)
     (let ((path (string-append root "/" name)))
       (and (file-exists? path) (cons name path))))
   '("artifacts" "keys" "input-cache")))

(define (store-entries path)
  "Return a list of (PATH SIZE MTIME) for every non-directory entry
under PATH, walking the tree recursively.  Broken symlinks count as
entries (they are garbage like everything else here)."
  (let walk ((path path) (entries '()))
    (let ((info (false-if-exception (lstat path))))
      (cond
       ((not info) entries)
       ((eq? (stat:type info) 'directory)
        (let inner ((names (or (scandir path) '())) (entries entries))
          (if (null? names)
              entries
              (if (member (car names) '("." ".."))
                  (inner (cdr names) entries)
                  (inner (cdr names)
                         (walk (string-append path "/" (car names))
                               entries))))))
       (else
        (cons (list path (stat:size info) (stat:mtime info))
              entries))))))

(define (store-summary root)
  "Return an association list of (NAME ENTRIES BYTES) per subtree
plus a (total ...) entry summarizing the whole store."
  (let loop ((subtrees (append (store-subtrees root)
                               (list (cons "total" root))))
             (summary '()))
    (if (null? subtrees)
        (reverse summary)
        (let* ((name (car (car subtrees)))
               (path (cdr (car subtrees)))
               (entries (store-entries path))
               (bytes (apply + (map cadr entries))))
          (loop (cdr subtrees)
                (cons (list name (length entries) bytes) summary))))))

(define (store-prune root cutoff-days dry-run)
  "Delete every entry under ROOT whose mtime is older than
CUTOFF-DAYS days, then remove directories left empty.  Return two
values: the number of entries removed and their total size in bytes.
In DRY-RUN nothing is deleted and the values describe what would be
removed."
  (let* ((now (time-second (current-time)))
         (cutoff (- now (* cutoff-days 86400)))
         (candidates
           (filter (lambda (entry) (< (caddr entry) cutoff))
                   (store-entries root)))
         (count (length candidates))
         (bytes (apply + (map cadr candidates))))
    (unless dry-run
      (for-each
       (lambda (entry)
         (false-if-exception (delete-file (car entry))))
       candidates)
      ;;     Bottom-up: rmdir only removes empty directories, so
      ;;     deeper levels go first and anything still holding files
      ;;     is left alone.
      (let walk ((path root))
        (let ((info (false-if-exception (lstat path))))
          (when (and info (eq? (stat:type info) 'directory))
            (for-each
             (lambda (name)
               (unless (member name '("." ".."))
                 (walk (string-append path "/" name))))
             (or (scandir path) '()))
            (false-if-exception (rmdir path))))))
    (values count bytes)))

(define (format-bytes bytes)
  "Render BYTES in binary units with one decimal place."
  (let loop ((value (exact->inexact bytes)) (units '("B" "KiB" "MiB" "GiB" "TiB")))
    (if (or (< value 1024) (null? (cdr units)))
        (format #f "~,1f ~a" value (car units))
        (loop (/ value 1024) (cdr units)))))

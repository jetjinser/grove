;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove store-gc)
  #:use-module (grove private store-gc)
  #:use-module (grove private cache-dir)
  #:use-module (grove private fingerprint)
  #:use-module (blue file-system directory)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-64))

(test-begin "grove store gc")

;;     The store directory honors GROVE_STORE_DIR, so the tests can
;;     point it at a throwaway tree unique to this process.
(define store
  (string-append "/tmp/kilo/store-gc-test-" (number->string (getpid))))

(setenv "GROVE_STORE_DIR" store)

(define (touch path mtime-seconds)
  (mkdir-p (dirname path))
  (call-with-output-file path (lambda (port) (display "x" port)))
  (utime path mtime-seconds mtime-seconds))

(define now (time-second (current-time)))
(define old (- now (* 90 86400)))

;;     A fresh store with an old blob, an old key, a fresh blob, and
;;     a fresh analysis, spread over the subtrees.
(touch (string-append store "/artifacts/deadbeef") old)
(touch (string-append store "/keys/deadbeef") old)
(touch (string-append store "/artifacts/cafebabe") now)
(touch (string-append store "/input-cache/proj/doc") now)
(touch (string-append store "/input-cache/proj/dead") old)


(define (basename path)
  (let ((index (string-index-right path #\/)))
    (if index (substring path (+ index 1)) path)))

(define (store-entries-probe path)
  "Local file listing for assertions: (BASENAME ...) per entry.
Broken symlinks and plain files count; directories and absent
paths do not."
  (if (file-exists? path)
      (filter (lambda (name)
                (not (file-is-directory? (string-append path "/" name))))
              (scandir path (lambda (name)
                              (not (member name '("." ".."))))))
      '()))

(define summary (store-summary store))
(define (entry name)
  (find (lambda (e) (string=? (car e) name)) summary))

;;     The total entry counts the store as it is NOW, not as the
;;     pre-prune summary above recorded it.
(define (fresh-total)
  (cadr (find (lambda (e) (string=? (car e) "total"))
              (store-summary store))))

;;     Three subtrees plus the total entry.
(test-equal "summary counts every subtree"
  4
  (length summary))

(test-equal "artifacts subtree counts two blobs"
  2
  (cadr (entry "artifacts")))

(test-equal "total counts every file"
  5
  (cadr (entry "total")))

(test-assert "summary carries byte sizes"
  (every (lambda (e) (>= (caddr e) 1)) summary))

(test-equal "store sizes use binary units with one decimal place"
  '("0.0 B" "1.0 KiB" "1.5 MiB" "1.0 TiB")
  (map format-bytes '(0 1024 1572864 1099511627776)))

;;; Dry run: nothing is removed.  The stale set = one artifact, one
;;; key, and one analysis.
(let-values (((count bytes) (store-prune store 30 #t)))
  (test-equal "dry run would remove three stale entries"
    3 count)
  (test-assert "dry run frees at least three bytes"
    (>= bytes 3))
  (test-equal "dry run removes nothing"
    5
    (fresh-total)))

;;; Real run: the stale entries go, fresh ones stay.
(let-values (((count bytes) (store-prune store 30 #f)))
  (test-equal "gc removes three stale entries"
    3 count)
  (test-assert "gc frees at least three bytes"
    (>= bytes 3))
  (test-equal "fresh artifacts remain"
    '("cafebabe")
    (map basename (store-entries-probe (string-append store "/artifacts"))))
  (test-equal "stale keys are gone"
    '()
    (store-entries-probe (string-append store "/keys")))
  (test-equal "fresh analyses remain"
    '("doc")
    (map basename (store-entries-probe (string-append store "/input-cache/proj")))))

;;; Empty directories are swept after pruning: the store keeps only
;;; the two fresh entries and the emptied keys subtree is gone.
(test-equal "empty directories are swept"
  2
  (fresh-total))
(test-assert "the emptied keys subtree is swept"
  (not (file-exists? (string-append store "/keys"))))

(test-end "grove store gc")

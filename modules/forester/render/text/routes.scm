;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render text routes)
  #:use-module (grove private snapshot)
  #:use-module (grove output)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-1)
  #:export (text-routes?
            make-text-routes
            text-page-path
            text-index-path
            text-relative-path))

(define-record-type <text-routes>
  (%make-text-routes page-path index-path)
  text-routes?
  (page-path text-page-path-procedure)
  (index-path text-index-path/raw))

(define (text-index-path routes)
  (snapshot-data (text-index-path/raw routes)))

(define* (make-text-routes #:key
                           (page-path (lambda (id)
                                        (string-append id ".txt")))
                           (index-path "index.txt"))
  (unless (procedure? page-path)
    (error "text page path requires a procedure" page-path))
  (unless (artifact-path? index-path)
    (error "text index requires a relative path" index-path))
  (%make-text-routes page-path (snapshot-data index-path)))

(define (text-page-path routes id)
  (unless (text-routes? routes)
    (error "text page path requires text routes" routes))
  (let ((path ((text-page-path-procedure routes) (snapshot-data id))))
    (unless (artifact-path? path)
      (error "text page requires a relative path" id path))
    (snapshot-data path)))

(define (text-relative-path from-path to-path)
  "Return the relative URL path from artifact FROM-PATH to TO-PATH.

Both arguments must be valid relative artifact paths."
  (unless (artifact-path? from-path)
    (error "text-relative-path requires a relative source artifact path"
           from-path))
  (unless (artifact-path? to-path)
    (error "text-relative-path requires a relative target artifact path"
           to-path))
  (let* ((from-directory (drop-right (string-split from-path #\/) 1))
         (to-parts (string-split to-path #\/))
         (common
           (let loop ((from from-directory) (to to-parts) (count 0))
             (if (and (pair? from) (pair? to)
                      (string=? (car from) (car to)))
                 (loop (cdr from) (cdr to) (+ count 1))
                 count)))
         (up (make-list (- (length from-directory) common) ".."))
         (down (drop to-parts common)))
    (string-join (append up down) "/")))

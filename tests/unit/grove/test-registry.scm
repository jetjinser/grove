;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove test-registry)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 textual-ports))

(define (read-resource-list source name)
  (let* ((marker (string-append "(define " name))
         (start (string-contains source marker)))
    (unless start
      (error "test resource list is missing" name))
    (let ((end (string-contains source "\n(define " (+ start 1))))
      (unless end
        (error "test resource list has no following definition" name))
      (let* ((form (call-with-input-string
                     (substring source start end)
                     read))
             (quoted-list (caddr form)))
        (unless (and (list? form)
                     (eq? (car form) 'define)
                     (pair? quoted-list)
                     (eq? (car quoted-list) 'quote)
                     (list? (cadr quoted-list)))
          (error "test resource list has an unexpected shape" name form))
        (cadr quoted-list)))))

(define (unit-scheme-files directory prefix)
  (append-map
    (lambda (entry)
      (cond
        ((member entry '("." ".."))
         '())
        (else
         (let ((path (string-append directory "/" entry))
               (resource (if (string-null? prefix)
                             entry
                             (string-append prefix "/" entry))))
           (cond
             ((file-is-directory? path)
              (unit-scheme-files path resource))
             ((string-suffix? ".scm" entry)
              (list resource))
             (else '()))))))
    (scandir directory)))

(define module-root (getenv "GROVE_SRCDIR"))
(unless module-root
  (error "GROVE_SRCDIR is required to check test registration"))
(define source-root (dirname module-root))

(define tests-root (string-append source-root "/tests"))
(define unit-root (string-append tests-root "/unit"))
(define registry-source
  (call-with-input-file (string-append source-root "/blueprint/tests.scm")
    get-string-all))
(define registered-tests (read-resource-list registry-source "+test-resources+"))
(define registered-fixtures (read-resource-list registry-source "+fixture-resources+"))
(define discovered-tests
  (map (lambda (path) (string-append "unit/" path))
       (unit-scheme-files unit-root "")))

(test-begin "test registry")

(test-equal "every unit Scheme file is registered as a test or fixture"
  (sort discovered-tests string<?)
  (sort (append registered-tests registered-fixtures) string<?))

(test-end "test registry")

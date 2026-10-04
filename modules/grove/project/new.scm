;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove project new)
  #:use-module (grove project)
  #:use-module (grove collect)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (create-tree))

(define (prefix? value)
  (and (string? value)
       (not (string-null? value))
       (string-every (lambda (c)
                       (or (char<=? #\a c #\z) (char<=? #\0 c #\9)
                           (char=? c #\-))) value)
       (not (char=? (string-ref value 0) #\-))
       (not (char=? (string-ref value (- (string-length value) 1)) #\-))))

(define (tree-number path prefix)
  (let ((name (basename path)) (stem (string-append prefix "-")))
    (and (string-prefix? stem name)
         (string-suffix? ".plant" name)
         (> (string-length name) (+ (string-length stem) 6))
         (let ((digits (substring name (string-length stem)
                                  (- (string-length name) 6))))
           (and (string-every (lambda (c)
                               (or (char<=? #\0 c #\9)
                                   (char<=? #\a (char-downcase c) #\z))) digits)
                (string->number digits 36))))))

(define (default-tree-template id date)
  (string-append "@use-modules[(forester commands)]\n\n@title{}\n@published{"
                 date "}\n"))

(define* (create-tree project #:key
                      (prefix (project-default-prefix project))
                      (destination (project-source-directory project)))
  "Create an empty titled .plant in DESTINATION and return its absolute path.
PREFIX defaults to the project prefix. IDs increment the largest base36 suffix
found among project sources and destination files, with at least four digits.
No source reader, preparation hook, analyzer, or renderer is invoked."
  (unless (prefix? prefix)
    (error "new requires a prefix of lowercase letters, digits, and internal hyphens; configure #:default-prefix or supply a prefix" prefix))
  (unless (and (string? destination) (file-is-directory? destination))
    (error "new requires an existing destination directory; supply --dest when no source directory is configured" destination))
  (let* ((directory (canonicalize-path destination))
         (paths (append (collect-project-sources project)
                        (collect-directory directory
                          (lambda (name) (string-suffix? ".plant" name)))))
         (largest (fold max 0 (filter-map (lambda (path) (tree-number path prefix)) paths))))
    (let loop ((number (+ largest 1)))
      (let* ((digits (number->string number 36))
             (id (string-append prefix "-"
                   (make-string (max 0 (- 4 (string-length digits))) #\0) digits))
             (path (string-append directory "/" id ".plant"))
             ;; Evaluate and validate before reserving the file: a faulty template
             ;; must not leave a new, empty source behind.
             (body ((or (project-tree-template project) default-tree-template)
                    id (strftime "%Y-%m-%d" (localtime (current-time)))))
             (port (begin
                     (unless (string? body)
                       (error "tree template must return source text" body))
                     (catch 'system-error
                     (lambda () (open path (logior O_WRONLY O_CREAT O_EXCL) #o644))
                     (lambda arguments
                       (if (= EEXIST (system-error-errno arguments)) #f
                           (apply throw arguments)))))))
        (if (not port)
            (loop (+ number 1))
            (begin
              (dynamic-wind
                (lambda () #t)
                (lambda ()
                  (set-port-encoding! port "UTF-8")
                  (display body port))
                (lambda () (close-port port)))
              path))))))

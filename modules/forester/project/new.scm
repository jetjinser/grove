;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester project new)
  #:use-module (grove collect)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (make-forester-source-creator))

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

(define* (make-forester-source-creator #:key default-prefix source-directory
                                       (template default-tree-template))
  "Return a project source creator for `.plant` Forester trees."
  (unless (or (not default-prefix) (prefix? default-prefix))
    (error "Forester default prefix must be lowercase letters, digits, and internal hyphens"
           default-prefix))
  (unless (or (not source-directory)
              (and (string? source-directory)
                   (not (string-null? source-directory))))
    (error "Forester source directory must be a non-empty path or #f"
           source-directory))
  (set! default-prefix (and default-prefix (string-copy default-prefix)))
  (set! source-directory
    (and source-directory (string-copy source-directory)))
  (set! template (or template default-tree-template))
  (unless (procedure? template)
    (error "Forester tree template must be a procedure" template))
  (lambda (requested-prefix destination)
    (let ((prefix (or requested-prefix default-prefix)))
      (unless (prefix? prefix)
        (if prefix
            (error "new requires a valid prefix; use lowercase letters, digits, and internal hyphens"
                   prefix)
            (error "new needs a prefix; pass one to grove new or configure #:default-prefix in project.scm")))
      (unless (and (string? destination) (file-is-directory? destination))
        (error "new requires an existing destination directory; supply --dest when no source directory is configured"
               destination))
      (let* ((directory (canonicalize-path destination))
             (paths (append (if (and source-directory
                                     (file-is-directory? source-directory))
                                (collect-directory source-directory
                                  (lambda (name)
                                    (string-suffix? ".plant" name)))
                                '())
                            (collect-directory directory
                              (lambda (name)
                                (string-suffix? ".plant" name)))))
             (largest (fold max 0
                            (filter-map
                              (lambda (path) (tree-number path prefix)) paths))))
        (let loop ((number (+ largest 1)))
          (let* ((digits (number->string number 36))
                 (id (string-append prefix "-"
                       (make-string (max 0 (- 4 (string-length digits))) #\0)
                       digits))
                 (path (string-append directory "/" id ".plant"))
                 (body (template id
                         (strftime "%Y-%m-%d"
                           (localtime (current-time)))))
                 (port (begin
                         (unless (string? body)
                           (error "Forester tree template must return source text"
                                  body))
                         (catch 'system-error
                           (lambda ()
                             (open path (logior O_WRONLY O_CREAT O_EXCL) #o644))
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
                  path))))))))

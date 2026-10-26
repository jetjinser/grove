;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private input dependencies)
  #:use-module (grove collect)
  #:use-module (grove private build-state)
  #:use-module (grove private files)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (runtime-inputs input-fingerprint current-input-fingerprints record-module-inputs!
            current-input-trace call-with-input-trace trace-inputs depend-on! disable-cache!))

;; Each document owns a trace. External effects must either register their
;; inputs before reading them or opt out; arbitrary Scheme I/O isn't intercepted.
(define current-input-trace (make-parameter #f))
(define current-input-fingerprints (make-parameter #f))

(define (trace-inputs trace)
  (hash-fold (lambda (path digest paths)
               (if (string? path) (cons path paths) paths)) '() trace))

(define (call-with-input-trace trace thunk)
  (let ((old-hook %load-hook))
    (parameterize ((current-input-trace trace))
      (dynamic-wind
        (lambda ()
          (set! %load-hook
            (lambda (file)
              (when old-hook (old-hook file))
              (when (and (string? file) (file-exists? file)
                         (not (file-is-directory? file)))
                (depend-on! file)))))
        thunk
        (lambda () (set! %load-hook old-hook))))))

(define (compiled-inputs filename source)
  (let ((relative
          (if (and filename (not (absolute-file-name? filename))) filename
              (and source
                (find identity (map
                  (lambda (root)
                    (and (file-exists? root)
                         (let ((prefix (string-append (canonicalize-path root) "/")))
                           (and (string-prefix? prefix source)
                                (substring source (string-length prefix))))))
                  %load-path))))))
    (if (and relative (string-suffix? ".scm" relative))
        (let ((suffix (string-append (string-drop-right relative 4) ".go")))
          (let loop ((roots %load-compiled-path) (paths '()))
            (if (null? roots) (reverse paths)
                (let* ((root (car roots))
                       (root (if (file-exists? root) (canonicalize-path root)
                                 (if (absolute-file-name? root) root
                                     (string-append (getcwd) "/" root))))
                       (path (string-append root "/" suffix)))
                  ;; Observe absent higher-priority candidates until the selected file.
                  (if (file-exists? path) (reverse (cons path paths))
                      (loop (cdr roots) (cons path paths)))))))
        '())))

(define (input-fingerprint path)
  ;; Only share digests inside one build. Re-stat on every observation, including
  ;; the post-evaluation check; edits invalidate the memo even if size/mtime match.
  (let* ((memo (current-input-fingerprints))
         (signature (file-signature path))
         (entry (and memo (hash-get-handle memo path))))
    (if (and entry (equal? signature (cadr entry)))
        (cddr entry)
        (let ((digest (and signature (file-fingerprint path))))
          (when (and memo (equal? signature (file-signature path)))
            (hash-set! memo path (cons signature digest)))
          digest))))

(define (depend-on! path)
  "Register a file input before reading it; return its absolute path."
  (let ((path (if (file-exists? path) (canonicalize-path path)
                  (if (absolute-file-name? path) path
                      (string-append (getcwd) "/" path))))
        (trace (current-input-trace)))
    (when trace
      (unless (hash-get-handle trace path)
        (hash-set! trace path (input-fingerprint path))))
    path))

(define (disable-cache!)
  "Run this document on every build, including its Scheme effects."
  (let ((trace (current-input-trace)))
    (when trace (hash-set! trace 'uncached #t))))

(define (runtime-inputs)
  ;; Include the package's complete source/data and compiled trees. This also
  ;; observes added/removed modules and compile-time include-text inputs, without
  ;; changing the fingerprint as documents load additional framework modules.
  (let ((source (%search-load-path "grove/project.scm"))
        (compiled (search-path %load-compiled-path "grove/project.go")))
    (delete-duplicates
      (append
        (if source (collect-directory (dirname (dirname source))) '())
        (if compiled
            (collect-directory (dirname (dirname compiled))
              (lambda (name) (string-suffix? ".go" name))) '()))
      string=?)))

(define (record-module-inputs! environment)
  (let ((seen (make-hash-table)))
    (define (visit module)
      (unless (hashq-ref seen module)
        (hashq-set! seen module #t)
        (let* ((name (module-name module))
               (actual (if (and name (eq? (module-kind module) 'interface))
                           (resolve-module name #f) module))
               (filename (module-filename actual))
               (source (and filename
                            (if (absolute-file-name? filename) filename
                                (%search-load-path filename))))
               (compiled (compiled-inputs filename source)))
          (for-each depend-on! (append (if (and source (file-exists? source)) (list source) '()) compiled))
          (for-each visit (module-uses actual)))))
    (visit environment)))

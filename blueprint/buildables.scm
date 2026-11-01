;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (blueprint buildables)
  #:use-module (blue build)
  #:use-module (blue stencils guile)
  #:use-module (blue stencils template-file)
  #:use-module (blue stencils texinfo)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (grove-modules forester-modules grove-programs))

;;     The content-addressed module sharing (a local extension, not
;;     part of the pinned package sources): when the extension module
;;     ships with the working tree, BLUE methods restore compiled
;;     outputs from, and publish them to, a store shared by every
;;     checkout of this project.  Pinned builds without it run
;;     unchanged.
(let* ((dir (dirname (current-filename)))
       (ext (string-append dir "/extensions/content-store.scm")))
  (when (file-exists? ext)
    (false-if-exception
     (begin
       (add-to-load-path dir)
       (resolve-module '(blueprint extensions content-store))))))

;;     The module manifest is scanned from the tree when the blueprint
;;     loads, so a new file cannot be missed at install; grove/ssh.scm
;;     stays conditional because compiling it requires guile-ssh.
(define module-root
  (string-append (dirname (dirname (current-filename))) "/modules"))

(define (scan-module-sources directory)
  (define (relative name prefix)
    (if (string-null? prefix) name (string-append prefix "/" name)))
  (sort
    (let loop ((path directory) (prefix ""))
      (append-map
        (lambda (name)
          (cond
            ((member name '("." "..")) '())
            ((file-is-directory? (string-append path "/" name))
             (loop (string-append path "/" name) (relative name prefix)))
            ((string-suffix? ".scm" name)
             (list (relative name prefix)))
            (else '())))
        (scandir path)))
    string<?))

(define guile-ssh?
  (and=> (search-path %load-path "ssh/session.scm") (lambda (_path) #t)))

(define all-module-sources (scan-module-sources module-root))

(define grove-source-files
  (filter (lambda (source) (string-prefix? "grove/" source))
          (if guile-ssh?
              all-module-sources
              (delete "grove/ssh.scm" all-module-sources))))

(define forester-source-files
  (filter (lambda (source)
            (any (lambda (prefix) (string-prefix? prefix source))
                 '("forester/" "language/")))
          all-module-sources))

(define grove-modules
  (append (map
    (λ (source)
      (guile-module
        (inputs source)
        (outputs (->.go source))
        (load-path #%~(list #%?srcdir))
        (optimizations
          '(#:seal-private-binddings? #t))))
    grove-source-files)
    (list (template-file
            (inputs "grove/style/highlight.css")
            (outputs "grove/style/highlight.css")
            (install-location #%~(string-append #%?guile-module-dir "/grove/style"))))))

(define forester-modules
  (append
    (map
      (λ (source)
        (guile-module
          (inputs source)
          (outputs (->.go source))
          (load-path #%~(list #%?srcdir))
          (optimizations
            '(#:seal-private-binddings? #t))))
      forester-source-files)
    (list
      (template-file
        (inputs "forester/style/default.css")
        (outputs "forester/style/default.css")
        (install-location #%~(string-append #%?guile-module-dir "/forester/style")))
      (template-file
        (inputs "forester/style/i18n.css")
        (outputs "forester/style/i18n.css")
        (install-location #%~(string-append #%?guile-module-dir "/forester/style"))))))

(define (runtime-library-paths paths extension required?)
  ;; Resolve devshell symlinks to the dependency packages themselves.
  (delete-duplicates
    (filter-map
      (lambda (module)
        (let* ((filename (string-append module extension))
               (path (search-path paths filename)))
          (cond
            (path
             (string-drop-right (canonicalize-path path)
                                (+ 1 (string-length filename))))
            (required? (error "missing Grove runtime dependency" module))
            (else #f))))
      (append '("gcrypt/hash" "faster-minikanren/mk-guile"
                "srfi/:275/iri" "ts"
                "grove/highlight/languages")
              (if guile-ssh? '("ssh/session") '())))
    string=?))

(define (runtime-extension-paths modules)
  ;; Locate the native extension directories of bundled Guile modules so the
  ;; installed launcher can find them without the development shell.
  (delete-duplicates
    (filter-map
      (lambda (module)
        (let ((path (search-path %load-path (string-append module ".scm"))))
          (and path
               (let* ((canonical (canonicalize-path path))
                      (marker "/share/guile/site/")
                      (index (string-contains canonical marker)))
                 (and index
                      (let* ((prefix (substring canonical 0 index))
                             (rest (substring canonical
                                              (+ index (string-length marker))))
                             (version (car (string-split rest #\/))))
                        (string-append prefix "/lib/guile/" version
                                       "/extensions")))))))
      modules)
    string=?))

(define grove-programs
  (list
    (template-file
      (inputs "../bin/grove.in")
      (outputs "bin/grove")
      (mode #o755)
      (install-permissions #o755)
      (local-variables
        #%~(list (cons 'GUILE_MODULE_DIR #%?guile-module-dir)
                 (cons 'GUILE_OBJECT_DIR #%?guile-object-dir)
                 (cons 'GUILE_RUNTIME_LOAD_PATH
                       (format #f "~s" (runtime-library-paths %load-path ".scm" #t)))
                 (cons 'GUILE_RUNTIME_COMPILED_PATH
                       (format #f "~s" (runtime-library-paths %load-compiled-path ".go" #f)))
                 (cons 'GUILE_RUNTIME_EXTENSION_PATH
                       (format #f "~s"
                         (runtime-extension-paths
                           (if guile-ssh? '("ssh/session") '()))))))
      (install-location #%~#%?bindir))))

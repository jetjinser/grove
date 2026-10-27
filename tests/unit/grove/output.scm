;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove output)
  #:use-module (grove private files)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-64)
  #:use-module (grove output))

(define test-root
  (format #f "/tmp/grove-output-test-~a" (getpid)))
(define output-directory (string-append test-root "/output"))
(setenv "GROVE_CACHE_DIR" (string-append test-root "/cache"))
(define output-path (string-append output-directory "/nested/page.txt"))
(define obsolete-path (string-append output-directory "/obsolete.html"))
(define unmanaged-path (string-append output-directory "/user.txt"))
(define linked-path (string-append output-directory "/linked.txt"))
(define binary-path (string-append output-directory "/images/pixel.bin"))
(define transition-path (string-append output-directory "/transition"))
(define transition-page-path
  (string-append output-directory "/transition/page.html"))

(test-begin "grove output")

(let* ((path (string-copy "page.txt"))
       (content (string-copy "artifact content"))
       (source (string-copy "source.plant"))
       (sources (list source))
       (artifact (make-artifact path content sources)))
  (string-set! path 0 #\x)
  (string-set! content 0 #\x)
  (string-set! source 0 #\x)
  (set-car! sources "changed.plant")
  (test-equal "text artifacts snapshot caller-owned strings and sources"
    '("page.txt" "artifact content" ("source.plant"))
    (list (artifact-path artifact)
          (artifact-content artifact)
          (artifact-sources artifact)))
  (let ((path (artifact-path artifact))
        (content (artifact-content artifact))
        (sources (artifact-sources artifact)))
    (string-set! path 0 #\x)
    (string-set! content 0 #\x)
    (set-car! sources "changed-again.plant"))
  (test-equal "text artifact accessors return independent snapshots"
    '("page.txt" "artifact content" ("source.plant"))
    (list (artifact-path artifact)
          (artifact-content artifact)
          (artifact-sources artifact))))

(let* ((bytes (u8-list->bytevector '(0 255 1 128)))
       (artifact (make-byte-artifact "pixel.bin" bytes)))
  (bytevector-u8-set! bytes 0 9)
  (test-assert "binary artifacts snapshot construction bytes"
    (bytevector=? (u8-list->bytevector '(0 255 1 128))
                  (artifact-content artifact)))
  (let ((exposed (artifact-content artifact)))
    (bytevector-u8-set! exposed 0 9))
  (test-assert "binary artifact accessors return independent bytes"
    (bytevector=? (u8-list->bytevector '(0 255 1 128))
                  (artifact-content artifact))))

(test-error "artifact sources reject empty paths" #t
  (make-artifact "page.txt" "content" '("")))
(test-error "artifact paths reject NUL characters before writing" #t
  (make-artifact (string-append "page" (string #\nul) ".txt") "content"))
(test-error "artifact prefix requires a relative path" #t
  (prefix-artifacts "../assets" '()))
(test-error "artifact prefix requires a list of artifacts" #t
  (prefix-artifacts "assets" '(not-an-artifact)))
(test-error "artifact prefix validates the complete artifact set" #t
  (prefix-artifacts "assets"
    (list (make-artifact "page.txt" "one")
          (make-artifact "page.txt" "two"))))
(let ((sources '("page.plant")))
  (test-equal "prefixing preserves text, byte content, and source paths"
    '("assets/page.txt" "text" ("page.plant")
      "assets/image.bin" (0 255) ("page.plant"))
    (let ((artifacts
            (prefix-artifacts "assets"
              (list (make-artifact "page.txt" "text" sources)
                    (make-byte-artifact "image.bin"
                      (u8-list->bytevector '(0 255)) sources)))))
      (list (artifact-path (car artifacts))
            (artifact-content (car artifacts))
            (artifact-sources (car artifacts))
            (artifact-path (cadr artifacts))
            (bytevector->u8-list (artifact-content (cadr artifacts)))
            (artifact-sources (cadr artifacts))))))

(dynamic-wind
  (lambda () (system* "rm" "-rf" test-root))
  (lambda ()
    (write-artifacts output-directory
      (list (make-artifact "nested/page.txt" "artifact content")
            (make-artifact "obsolete.html" "old page")))
    (test-assert "artifact writer creates nested output directories"
      (file-exists? output-path))
    (test-equal "artifact writer writes artifact contents"
      "artifact content"
      (call-with-input-file output-path get-string-all))
    (test-equal "undeclared artifact sources remain unknown"
      #f
      (artifact-sources
        (make-artifact "unknown.txt" "unknown source set")))
    (let ((inode (stat:ino (stat output-path))))
      (write-artifacts output-directory
        (list (make-artifact "nested/page.txt" "artifact content")
              (make-artifact "obsolete.html" "old page")))
      (test-equal "unchanged artifact contents are not rewritten"
        inode
        (stat:ino (stat output-path))))
    (call-with-output-file unmanaged-path
      (lambda (port) (display "user-owned" port)))
    (symlink unmanaged-path linked-path)
    (write-artifacts output-directory
      (list (make-artifact "linked.txt" "user-owned")))
    (test-assert "artifact writer replaces symlink outputs without changing their targets"
      (and (regular-file? linked-path #:follow-links? #f)
           (equal? "user-owned"
                   (call-with-input-file unmanaged-path get-string-all))))
    (write-artifacts output-directory
      (list (make-artifact "nested/page.txt" "updated content")))
    (test-assert "artifact writer removes outputs absent from the next build"
      (not (file-exists? obsolete-path)))
    (test-equal "artifact writer preserves files absent from its manifest"
      "user-owned"
      (call-with-input-file unmanaged-path get-string-all))
    (mkdir (string-append output-directory "/.grove-input-cache"))
    (call-with-output-file (string-append output-directory "/.grove-input-cache/user")
      (lambda (port) (display "user cache" port)))
    (call-with-output-file (string-append output-directory "/.grove-build-state")
      (lambda (port) (display "user state" port)))
    (write-artifacts output-directory
      (list (make-artifact "nested/page.txt" "updated content")))
    (test-equal "artifact writes preserve unmanaged Grove-named files"
      '("user cache" "user state")
      (list (call-with-input-file
              (string-append output-directory "/.grove-input-cache/user")
              get-string-all)
            (call-with-input-file
              (string-append output-directory "/.grove-build-state")
              get-string-all)))
    (test-equal "artifact writer refreshes current artifact contents"
      "updated content"
      (call-with-input-file output-path get-string-all))
    (let ((bytes (u8-list->bytevector '(0 255 1 128))))
      (write-artifacts output-directory
        (list (make-byte-artifact "images/pixel.bin" bytes)))
      (test-assert "artifact writer preserves binary artifact bytes"
        (let ((port (open-file binary-path "rb")))
          (dynamic-wind
            (lambda () #t)
            (lambda () (bytevector=? bytes (get-bytevector-all port)))
            (lambda () (close-port port)))))
      (write-artifacts output-directory '())
      (test-assert "artifact writer removes stale binary artifacts"
        (not (file-exists? binary-path))))
    (write-artifacts output-directory '())
    (test-assert "empty builds remove previously generated artifacts"
      (not (file-exists? output-path)))
    (write-artifacts output-directory
      (list (make-artifact "transition" "first as a file")))
    (write-artifacts output-directory
      (list (make-artifact "transition/page.html" "now nested")))
    (test-equal "a prior artifact file can become an artifact directory"
      "now nested"
      (call-with-input-file transition-page-path get-string-all))
    (write-artifacts output-directory
      (list (make-artifact "transition" "now a file again")))
    (test-equal "a prior artifact directory can become an artifact file"
      "now a file again"
      (call-with-input-file transition-path get-string-all))
    (write-artifacts output-directory
      (list (make-artifact "guard/owned.txt" "managed")))
    (call-with-output-file (string-append output-directory "/guard/user.txt")
      (lambda (port) (display "unmanaged" port)))
    (test-error "artifact writer does not replace a directory with unmanaged files"
      #t
      (write-artifacts output-directory
        (list (make-artifact "guard" "would replace the directory"))))
    (test-equal "unmanaged files survive a blocked file/directory transition"
      "unmanaged"
      (call-with-input-file (string-append output-directory "/guard/user.txt")
        get-string-all))
    (with-fluids ((%default-port-encoding "ISO-8859-1"))
      (let* ((text "平方 ◆:x")
             (artifacts (list (make-artifact "nested/page.txt" text))))
        (write-artifacts output-directory artifacts)
        (test-assert "text artifacts use UTF-8 independently of default port encoding"
          (let ((port (open-file output-path "rb")))
            (dynamic-wind
              (lambda () #t)
              (lambda () (bytevector=? (string->utf8 text) (get-bytevector-all port)))
              (lambda () (close-port port)))))
        (utime output-path 0 0)
        (write-artifacts output-directory artifacts)
        (test-equal "UTF-8 comparison preserves unchanged artifacts" 0
          (stat:mtime (stat output-path)))
        (write-artifacts output-directory '())))
    (test-equal "Grove-named output paths are ordinary artifact paths"
      '(".grove-artifacts" ".grove-build-state/data"
        ".grove-input-cache/user" ".grove-build-lock")
      (map artifact-path
        (validate-artifacts
          (list (make-artifact ".grove-artifacts" "user data")
                (make-artifact ".grove-build-state/data" "user data")
                (make-artifact ".grove-input-cache/user" "user data")
                (make-artifact ".grove-build-lock" "user data"))))))
  (lambda () (system* "rm" "-rf" test-root)))

(test-end "grove output")

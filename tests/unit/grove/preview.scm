;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove preview)
  #:use-module (grove output)
  #:use-module (grove private preview http)
  #:use-module (grove private preview watch)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-64)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri))

(define table (artifact-table
                (list (make-artifact "index.html" "首页")
                      (make-artifact "a+b.txt" "plus")
                      (make-byte-artifact "image.png" (u8-list->bytevector '(0 255 128))))))
(define handler (make-preview-handler (lambda () table)))
(define* (request path #:optional (method 'GET))
  (call-with-values
    (lambda () (handler (build-request (string->uri (string-append "http://localhost" path))
                                       #:method method #:port (open-input-string "")) #f))
    list))

(test-begin "preview artifacts and watching")
(test-equal "root serves the authored index artifact" "首页"
  (utf8->string (cadr (request "/"))))
(test-equal "HTML content length uses encoded bytes" 6
  (response-content-length (car (request "/"))))
(test-equal "binary artifacts retain their exact bytes" '(0 255 128)
  (bytevector->u8-list (cadr (request "/image.png"))))
(test-equal "HEAD has the GET content length" 6
  (response-content-length (car (request "/" 'HEAD))))
(test-equal "HEAD has no body" #f (cadr (request "/" 'HEAD)))
(test-equal "binary MIME type is inferred from artifact path" '(image/png)
  (response-content-type (car (request "/image.png"))))
(test-equal "plus remains literal in URL paths" "plus"
  (utf8->string (cadr (request "/a+b.txt"))))
(for-each
  (lambda (path)
    (test-equal (string-append "invalid URL path is rejected: " path) 400
      (response-code (car (request path)))))
  '("/../secret" "/%2e%2e/secret" "/%00" "/a//b"))
(test-equal "unmanaged files and cache metadata are not served" 404
  (response-code (car (request "/.grove-build-state"))))
(test-equal "missing artifacts do not generate directory listings" 404
  (response-code (car (request "/missing"))))
(test-equal "unsupported methods return 405" 405
  (response-code (car (request "/" 'POST))))
(set! table (artifact-table (list (make-artifact "index.html" "new"))))
(test-equal "handler observes a complete replacement snapshot" "new"
  (utf8->string (cadr (request "/"))))
(test-equal "artifacts removed from the new snapshot vanish" 404
  (response-code (car (request "/image.png"))))

(define root
  (mkdtemp (string-append (or (getenv "TMPDIR") "/tmp")
                          "/grove-watch-test-XXXXXX")))
(define output (string-append root "/build"))
(define source (string-append root "/article.plant"))
(define external (string-append root "-external.css"))
(define external-alias (string-append root "-external-alias.css"))
(define (write-file path text)
  (call-with-output-file path (lambda (port) (display text port))))
(define (snapshot) (watch-snapshot (list root) (list external) output))
(dynamic-wind
  (lambda ()
    (mkdir output)
    (mkdir (string-append root "/.git"))
    (write-file source "one")
    (write-file external "css"))
  (lambda ()
    (let ((before (snapshot)))
      (write-file (string-append output "/generated.html") "generated")
      (write-file (string-append root "/.git/index") "git")
      (test-equal "output and hidden directories cannot trigger rebuild loops" before (snapshot))
      (usleep 1000)
      (write-file source "two")
      (test-equal "same-sized edits are detected by precise file metadata" (list source)
        (snapshot-changes before (snapshot))))
    (let ((before (snapshot)) (new (string-append root "/new.plant")))
      (write-file new "new")
      (test-equal "new source paths are discovered" (list new)
        (snapshot-changes before (snapshot)))
      (delete-file source)
      (test-assert "source deletions are detected"
        (member source (snapshot-changes before (snapshot)))))
    (let ((before (snapshot)))
      (delete-file external)
      (test-equal "external dependency deletion is observed" (list external)
        (snapshot-changes before (snapshot)))
      (let ((missing (snapshot)))
        (write-file external "restored")
        (test-equal "external dependency restoration is observed" (list external)
          (snapshot-changes missing (snapshot)))))
    (symlink external external-alias)
    (let ((before (watch-snapshot '() (list external-alias) output)))
      (usleep 1000)
      (write-file external "changed through alias")
      (test-equal "watcher observes edits through file symlinks" (list external-alias)
        (snapshot-changes before
                          (watch-snapshot '() (list external-alias) output))))
    (symlink root (string-append root "/cycle"))
    (test-assert "directory symlinks do not cause recursive scans" (< (length (snapshot)) 10))
    (test-equal "path normalization also works for nonexistent outputs"
      (string-append root "/preview") (absolute-path (string-append root "/a/../preview"))))
  (lambda ()
    (system* "rm" "-rf" root)
    (when (file-exists? external-alias) (delete-file external-alias))
    (when (file-exists? external) (delete-file external))))
(let ((listener (socket AF_INET SOCK_STREAM 0)))
  (dynamic-wind
    (lambda () (bind listener AF_INET INADDR_LOOPBACK 0) (listen listener 4))
    (lambda ()
      (define (exchange headers)
        (let ((client (socket AF_INET SOCK_STREAM 0)))
          (dynamic-wind
            (lambda () (connect client (getsockname listener)))
            (lambda ()
              (send client (string->utf8 headers))
              (serve-ready-client listener handler)
              (let ((response (get-bytevector-all client)))
                (if (eof-object? response) "" (utf8->string response))))
            (lambda () (close-port client)))))
      (test-assert "the socket server sends an encoded HTTP response"
        (string-contains (exchange "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n")
          (utf8->string (hash-ref table "index.html"))))
      (test-equal "malformed requests close the connection without a response" ""
        (exchange "INVALID\r\n\r\n"))
      (test-equal "incomplete requests expire without blocking the server" ""
        (exchange "GET / HTTP/1.1\r\n")))
    (lambda () (close-port listener))))
(test-end "preview artifacts and watching")

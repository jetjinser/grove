;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private preview http)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module (grove output)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 iconv)
  #:use-module (srfi srfi-13)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (artifact-table make-preview-handler serve-ready-client))

(define (artifact-table artifacts)
  (validate-artifacts artifacts)
  (let ((table (make-hash-table)))
    (for-each (lambda (artifact)
                (let ((content (out:artifact-content/raw artifact)))
                  (hash-set! table (out:artifact-path/raw artifact)
                    (if (string? content) (string->utf8 content) content))))
              artifacts)
    table))

(define (content-type path)
  (let ((extension (string-downcase (let ((index (string-rindex path #\.)))
                                       (if index (substring path (+ index 1)) "")))))
    (or (assoc-ref
          '(("html" . (text/html (charset . "utf-8")))
            ("css" . (text/css (charset . "utf-8")))
            ("js" . (text/javascript (charset . "utf-8")))
            ("txt" . (text/plain (charset . "utf-8")))
            ("scm" . (text/plain (charset . "utf-8")))
            ("json" . (application/json)) ("xml" . (application/xml))
            ("svg" . (image/svg+xml)) ("png" . (image/png))
            ("jpg" . (image/jpeg)) ("jpeg" . (image/jpeg))
            ("gif" . (image/gif)) ("webp" . (image/webp))
            ("ico" . (image/vnd.microsoft.icon)) ("pdf" . (application/pdf))
            ("woff" . (font/woff)) ("woff2" . (font/woff2)))
          extension)
        '(application/octet-stream))))

(define (request-path request)
  (let ((path (catch #t
                (lambda () (uri-decode (uri-path (request-uri request))
                                      #:decode-plus-to-space? #f))
                (lambda _ #f))))
    (and path
         (not (string-index path #\nul))
         (cond
           ((string=? path "/") "index.html")
           ((string-prefix? "/" path)
            (let ((relative (substring path 1)))
              (and (artifact-path? relative) relative)))
           (else #f)))))

(define (make-preview-handler current-artifacts)
  "Serve an immutable artifact table, fetching its current version once per request."
  (lambda (request body)
    (let* ((method (request-method request))
           (path (request-path request))
           (bytes (and path (hash-ref (current-artifacts) path)))
           (code (cond ((not (memq method '(GET HEAD))) 405)
                       ((not path) 400)
                       (bytes 200)
                       (else 404)))
           (content (or (and (= code 200) bytes)
                        (string->utf8 (format #f "~a\n" code))))
           (headers
             `((content-type . ,(if (= code 200) (content-type path)
                                    '(text/plain (charset . "utf-8"))))
               (content-length . ,(bytevector-length content))
               (cache-control . (no-store))
               (connection . (close))
               ,@(if (= code 405) '((allow . (GET HEAD))) '()))))
      (values (build-response #:code code #:headers headers)
              (if (eq? method 'HEAD) #f content)))))

(define (byte-slice bytes start end)
  (let ((result (make-bytevector (- end start))))
    (bytevector-copy! bytes start result 0 (- end start))
    result))

(define (wait-ready client write? deadline)
  (and (< (get-internal-real-time) deadline)
       (let ((ready (select (if write? '() (list client))
                            (if write? (list client) '()) '() 0 50000)))
         (or (pair? (if write? (cadr ready) (car ready)))
             (wait-ready client write? deadline)))))

(define (receive-headers client deadline)
  (let ((buffer (make-bytevector 4096)))
    (let loop ((text ""))
      (cond
        ((string-contains text "\r\n\r\n") text)
        ((>= (string-length text) 16384) #f)
        ((not (wait-ready client #f deadline)) #f)
        (else
         (let ((count (recv! client buffer)))
           (and (positive? count)
                (loop (string-append text
                        (bytevector->string (byte-slice buffer 0 count) "ISO-8859-1"))))))))))

(define (send-bytes client bytes deadline)
  (let loop ((offset 0))
    (cond
      ((= offset (bytevector-length bytes)) #t)
      ((not (wait-ready client #t deadline)) #f)
      (else
       (let ((count (send client (byte-slice bytes offset (min (+ offset 65536) (bytevector-length bytes))))))
         (and (positive? count) (loop (+ offset count))))))))

(define (serve-ready-client listener handler)
  "Handle one bounded, nonpersistent request using Guile's HTTP codecs.
Raw socket I/O keeps partial requests and disconnected clients from blocking
preview lifecycle handling indefinitely."
  (catch 'system-error
    (lambda ()
      (let ((client (car (accept listener)))
            (deadline (+ (get-internal-real-time) (* 2 internal-time-units-per-second))))
        (dynamic-wind
          (lambda () (fcntl client F_SETFL (logior (fcntl client F_GETFL) O_NONBLOCK)))
          (lambda ()
            (let* ((headers (receive-headers client deadline))
                   (request
                     (and headers
                          (catch #t
                            (lambda () (call-with-input-string headers read-request))
                            (lambda (key . arguments)
                              (if (eq? key 'preview-stop) (apply throw key arguments) #f))))))
              (when request
                (call-with-values (lambda () (handler request #f))
                  (lambda (response body)
                    (let ((header-bytes
                            (string->utf8
                              (call-with-output-string
                                (lambda (port) (write-response response port))))))
                      (when (send-bytes client header-bytes deadline)
                        (when body (send-bytes client body deadline)))))))))
          (lambda () (close-port client)))))
    (lambda _ #f)))

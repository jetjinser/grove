;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove build state)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 binary-ports)
  #:use-module (grove output)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (build-state-name
            build-state-temp-name
            content-fingerprint
            file-fingerprint
            read-build-state
            write-build-state))

(define build-state-name ".grove-build-state")
(define build-state-temp-name ".grove-build-state.tmp")

(define (fingerprint-bytes bytes)
  (let ((digest (sha256 bytes))
        (digits "0123456789abcdef"))
    (list->string
     (append-map
      (lambda (index)
        (let ((byte (bytevector-u8-ref digest index)))
          (list (string-ref digits (quotient byte 16))
                (string-ref digits (modulo byte 16)))))
      (iota (bytevector-length digest))))))

(define (content-fingerprint content)
  (cond
   ((string? content) (fingerprint-bytes (string->utf8 content)))
   ((bytevector? content) (fingerprint-bytes content))
   (else (error "content fingerprint requires text or bytes" content))))

(define (file-fingerprint path)
  (let ((port (open-file path "rb")))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (fingerprint-bytes (get-bytevector-all port)))
      (lambda () (close-port port)))))

(define (distinct-paths? entries path-index)
  (let ((paths (map (lambda (entry) (list-ref entry path-index)) entries)))
    (= (length paths) (length (delete-duplicates paths string=?)))))

(define (valid-source-entry? entry)
  (and (list? entry)
       (= (length entry) 2)
       (string? (car entry))
       (valid-fingerprint? (cadr entry))))

(define (valid-fingerprint? value)
  (and (string? value)
       (= (string-length value) 64)
       (every (lambda (character)
                (or (and (char>=? character #\0)
                         (char<=? character #\9))
                    (and (char>=? character #\a)
                         (char<=? character #\f))))
              (string->list value))))

(define (valid-output-entry? entry)
  (and (list? entry)
       (= (length entry) 5)
       (artifact-path? (car entry))
       (valid-fingerprint? (cadr entry))
       (or (not (caddr entry))
           (and (list? (caddr entry)) (every string? (caddr entry))))
       (or (not (cadddr entry))
           (and (integer? (cadddr entry)) (>= (cadddr entry) 0)))
       (memq (list-ref entry 4) '(text bytes))))

(define (valid-build-state? state)
  (and (list? state)
       (= (length state) 4)
       (eq? (car state) 'grove-build-state)
       (list? (cadr state))
       (every valid-source-entry? (cadr state))
       (distinct-paths? (cadr state) 0)
       (list? (caddr state))
       (every valid-output-entry? (caddr state))
       (distinct-paths? (caddr state) 0)
       (or (not (cadddr state)) (string? (cadddr state)))))

(define (read-build-state path)
  (if (not (file-exists? path))
      #f
      (call-with-input-file path
        (lambda (port)
          (let ((state (read port)))
            (and (valid-build-state? state)
                 (eof-object? (read port))
                 state))))))

(define (write-build-state path temp-path sources artifacts owners cache-key)
  (let ((state
          (list 'grove-build-state
                sources
                (map (lambda (artifact)
                       (list (artifact-path artifact)
                             (content-fingerprint (artifact-content artifact))
                             (artifact-sources artifact)
                             (cdr (assoc (artifact-path artifact) owners))
                             (if (string? (artifact-content artifact))
                                 'text 'bytes)))
                     artifacts)
                cache-key)))
    (unless (valid-build-state? state)
      (error "refusing to write invalid Grove build state" state))
    (call-with-output-file temp-path
      (lambda (port)
        (write state port)
        (newline port)))
    (rename-file temp-path path)))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private input cache)
  #:use-module (grove ir)
  #:use-module (grove private input dependencies)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (grove build state)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:export (current-input-cache make-input-cache-context
            input-cache-dependencies input-cache-summary input-cache-disabled? prune-input-cache!
            register-input-dependencies!
            cached-document))

;; A build scopes this parameter; checks and standalone reads never reuse results.
(define current-input-cache (make-parameter #f))

(define (make-input-cache-context directory enabled? dependencies)
  (and enabled?
    (let ((globals (make-hash-table)))
      (for-each (lambda (path) (hash-set! globals path #t)) dependencies)
      (list (string-append directory "/.grove-input-cache")
            enabled? dependencies (make-hash-table) globals))))

(define (register-input-dependencies! context paths)
  (when context
    (for-each (lambda (path) (hash-set! (list-ref context 4) path #t)) paths)))

(define (input-cache-dependencies context)
  (if context
      (hash-map->list (lambda (path fingerprint) path) (list-ref context 4))
      '()))

(define (input-cache-summary context)
  (if context
      (let ((statuses (list-ref context 3)))
        (cons (hash-count (lambda (path status) (eq? status 'hit)) statuses)
              (hash-count (const #t) statuses)))
      '(0 . 0)))

(define (input-cache-disabled? context)
  (and context
       (> (hash-count (lambda (path status) (eq? status 'disabled))
                      (list-ref context 3)) 0)))

(define (prune-input-cache! context)
  (when context
    (match context
      ((directory enabled? dependencies statuses globals)
       (when (file-exists? directory)
         (let ((retained (filter identity (hash-map->list
                           (lambda (path status)
                             (and (memq status '(hit miss))
                                  (content-fingerprint (canonicalize-path path))))
                           statuses))))
           (for-each
             (lambda (name)
               (let ((path (string-append directory "/" name)))
                 (when (and (not (member name retained string=?))
                            (not (file-is-directory? path)))
                   (delete-file path))))
             (scandir directory
               (lambda (name) (not (member name '("." ".."))))))))))))

;; This is private storage, not another public content representation. Only
;; data-only values are accepted: queries with closures must be evaluated anew.
(define (pack value)
  (let ((active (make-hash-table))
        (data-active (make-hash-table))
        (data-known (make-hash-table)))
    (define (atomic-datum? value)
      (or (null? value) (boolean? value) (string? value) (symbol? value)
          (number? value) (char? value) (keyword? value)))
    (define (plain-data? value)
      (cond
        ((atomic-datum? value) #t)
        ((pair? value)
         (when (hashq-ref data-active value) (throw 'uncacheable-content))
         (let ((known (hashq-get-handle data-known value)))
           (if known (cdr known)
               (begin
                 (hashq-set! data-active value #t)
                 (let ((plain? (and (plain-data? (car value))
                                    (plain-data? (cdr value)))))
                   (hashq-remove! data-active value)
                   (hashq-set! data-known value plain?)
                   plain?)))))
        (else #f)))
    (define (visit value)
      (when (hashq-ref active value) (throw 'uncacheable-content))
      (hashq-set! active value #t)
      (let ((encoded (encode value)))
        (hashq-remove! active value)
        encoded))
    (define (encode value)
      (cond
        ((content-document? value)
         (list 'document (visit (content-document-children value))
               (visit (content-document-source value))))
        ((content-text? value)
         (list 'text (content-text-value value) (visit (content-text-source value))
               (visit (content-text-location value))))
        ((content-layout? value)
         (list 'layout (content-layout-kind value) (content-layout-raw value)
               (visit (content-layout-source value))))
        ((content-element? value)
         (list 'element (content-element-kind value)
               (visit (content-element-children value))
               (visit (content-element-properties value))
               (visit (content-element-style value))
               (visit (content-element-source value))
               (content-element-flow-kind value)))
        ((content-style? value)
         (list 'style (content-style-name value)
               (visit (content-style-properties value))))
        ((html-attributes? value)
         (list 'attributes (visit (html-attributes-assoc value))))
        ((html-tag? value) (list 'tag (html-tag-name value)))
        ;; Preserve ordinary data as one datum. Only pairs containing IR records
        ;; need structural encoding; properties and source locations do not.
        ((pair? value)
         (if (plain-data? value) (list 'datum value)
             (list 'pair (visit (car value)) (visit (cdr value)))))
        ((atomic-datum? value)
         (list 'datum value))
        (else (throw 'uncacheable-content))))

    (visit value)))

(define (unpack value)
  (match value
    (('document children source)
     (make-content-document (unpack children) (unpack source)))
    (('text text source location)
     (make-content-text text (unpack source) #:location (unpack location)))
    (('layout kind raw source) (make-content-layout kind raw (unpack source)))
    (('element kind children properties style source flow-kind)
     (make-content-element kind (unpack children) (unpack source)
       #:properties (unpack properties) #:style (unpack style)
       #:flow-kind flow-kind))
    (('style name properties) (make-content-style name (unpack properties)))
    (('attributes assoc) (make-html-attributes (unpack assoc)))
    (('tag name) (make-html-tag name))
    (('pair first rest) (cons (unpack first) (unpack rest)))
    (('datum datum) datum)
    (_ (error "invalid cached content"))))

(define (ensure-directory directory)
  (unless (file-exists? directory)
    (ensure-directory (dirname directory))
    (mkdir directory)))

(define (read-entry path)
  (catch #t
    (lambda ()
      (call-with-input-file path
        (lambda (port)
          (set-port-encoding! port "UTF-8")
          (let ((entry (read port)))
            (and (eof-object? (read port)) entry)))))
    (lambda _ #f)))

(define (save-entry path entry)
  (let ((temporary (string-append path "." (number->string (getpid)) ".tmp")))
    (dynamic-wind
      (lambda () (ensure-directory (dirname path)))
      (lambda ()
        (call-with-output-file temporary
          (lambda (port)
            (set-port-encoding! port "UTF-8")
            (write entry port)))
        (rename-file temporary path))
      (lambda ()
        (when (file-exists? temporary) (delete-file temporary))))))

(define* (cached-document path reader dependencies read-document status
                          #:optional (reuse? #t))
  (let ((context (current-input-cache))
        (canonical (canonicalize-path path)))
    (if (not context)
        (read-document)
        (match context
          ((directory enabled? shared statuses globals)
           (define (report result)
             (hash-set! statuses path result)
             (status path result))
           (define (register files)
             (for-each (lambda (file)
                         (unless (string=? file canonical) (hash-set! globals file #t))) files))
           (register (append shared dependencies))
           (let* ((paths (delete-duplicates (cons path (append shared dependencies))
                                           string=?))
                  (snapshot (map (lambda (file) (list file (input-fingerprint file))) paths))
                  (identity (list reader (version) (setlocale LC_CTYPE)
                                  %load-path %load-compiled-path snapshot))
                  (entry-path (string-append directory "/"
                                (content-fingerprint (canonicalize-path path))))
                  (entry (and reuse? (read-entry entry-path)))
                  (hit (catch #t
                         (lambda ()
                           (match entry
                             ((stored extra reusable? document)
                              (and (equal? stored identity)
                                   reusable?
                                   (every (lambda (input)
                                            (match input
                                              ((file digest)
                                               (equal? digest (input-fingerprint file)))
                                              (_ #f))) extra)
                                   (begin (register (map car extra))
                                          (unpack document))))
                             (_ #f)))
                         (lambda _ #f))))
             (if (content-document? hit)
                 (begin (report 'hit) hit)
                 (let* ((trace (make-hash-table))
                        (document (call-with-input-trace trace read-document))
                        (extra (sort
                                 (hash-fold (lambda (file digest inputs)
                                              (if (and (string? file) (not (string=? file canonical)))
                                                  (cons (list file digest) inputs) inputs))
                                            '() trace)
                                 (lambda (a b) (string<? (car a) (car b))))))
                   (register (map car extra))
                   (report 'miss)
                   (catch 'uncacheable-content
                     (lambda ()
                       (let* ((reusable? (and reuse? (not (hash-ref trace 'uncached))))
                              (encoded (and reusable? (pack document))))
                         (unless reusable? (report 'disabled))
                         ;; Compare real files again, not the memoized snapshot.
                         (when (and reusable? (every (lambda (input)
                                        (equal? (cadr input) (input-fingerprint (car input))))
                                      (append snapshot extra)))
                           (save-entry entry-path (list identity extra reusable? encoded)))))
                     (lambda _ (report 'uncacheable)))
                   document))))))))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private input cache)
  #:use-module (grove ir)
  #:use-module (grove private cache-dir)
  #:use-module (grove private input dependencies)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (grove build state)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:export (current-input-cache current-project-key make-input-cache-context
            current-input-cache-timings
            input-cache-dependencies input-cache-summary input-cache-disabled? prune-input-cache!
            register-input-dependencies!
            cached-document))

;; A build scopes this parameter; checks and standalone reads never reuse results.
(define current-input-cache (make-parameter #f))

;; Set by the project loader: the content digest of the project file.
;; Document cache keys derive from it, so two checkouts of one project
;; - and only that project - meet on the same entries.
(define current-project-key (make-parameter #f))

;; Profile-mode accumulator: a hash the CLI installs and reads back
;; after the build.  cached-document adds reused/fresh seconds and
;; counts here - the reuse path covers the whole hit side (entry read,
;; identity check, dependency verification, unpack), the fresh path
;; the whole miss side (read-document).
(define current-input-cache-timings (make-parameter #f))

;;     Cross-checkout normalization: paths inside the loading process's
;;     working directory key project-relative (identical across
;;     checkouts of one project); foreign paths (module trees, stores)
;;     key as given - they are machine-stable parts of the toolchain
;;     identity, not of the project.
(define (normalize-key-path path)
  (let ((root (getcwd)))
    (cond
      ((string=? path root) ".")
      ((string-prefix? (string-append root "/") path)
       (substring path (+ (string-length root) 1)))
      (else path))))

(define (entry-key path)
  (content-fingerprint
    (string-append (or (current-project-key) "")
                   "\0"
                   (normalize-key-path path))))

;;     Profile-mode miss diagnosis: when an entry exists but its
;;     identity check fails, name the first element that differs, so
;;     sharing problems (paths that should normalize but do not,
;;     toolchain drift) are visible instead of silent misses.
(define (first-identity-difference stored current)
  (let loop ((a stored) (b current) (path '(identity)))
    (cond
      ((equal? a b) #f)
      ((and (pair? a) (pair? b))
       (or (loop (car a) (car b) (cons "car" path))
           (loop (cdr a) (cdr b) (cons "cdr" path))))
      (else
       (let ((show (lambda (v)
                     (let ((s (format #f "~s" v)))
                       (if (> (string-length s) 160)
                           (string-append (substring s 0 160) "…")
                           s)))))
         (format #f "~s: stored ~a, current ~a"
                 (reverse path) (show a) (show b)))))))

(define (make-input-cache-context directory enabled? dependencies)
  (and enabled?
    (let ((globals (make-hash-table)))
      (for-each (lambda (path) (hash-set! globals path #t)) dependencies)
      ;;     The document cache lives in the machine-global store, not
      ;;     a per-output directory: its keys are content-derived, so
      ;;     concurrent checkouts and worktrees share one set of
      ;;     entries instead of recomputing identical analyses.
      (list (string-append (grove-store-directory) "/input-cache")
            enabled? dependencies (make-hash-table) globals
            (or (current-project-key) "")))))

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
      ((directory enabled? dependencies statuses globals project-key)
       (when (file-exists? directory)
         (let ((retained (filter identity (hash-map->list
                           (lambda (path status)
                             ;;     A vanished path is a stale entry:
                             ;;     drop it instead of failing the
                             ;;     build - the cache is hygiene, not
                             ;;     an invariant.
                             (and (memq status '(hit miss))
                                  (false-if-exception
                                   (entry-key path))))
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
  ;;     Profile mode times each call as a whole and books it to the
  ;;   reuse side (entry read, identity check, dependency verification,
  ;;   unpack) or the fresh side (read-document) - the split that says
  ;;   whether warm builds are paying for verification or materialization.
  (let ((context (current-input-cache))
        (canonical (canonicalize-path path))
        (started (and (current-input-cache-timings)
                      (get-internal-real-time)))
        (timings (current-input-cache-timings)))
    (define (timed kind value)
      (when timings
        (hash-set! timings kind
                   (+ (/ (- (get-internal-real-time) started)
                         internal-time-units-per-second)
                      (hash-ref timings kind 0)))
        (hash-set! timings (list 'count kind)
                   (+ 1 (hash-ref timings (list 'count kind) 0))))
      value)
    (if (not context)
        (timed 'fresh (read-document))
        (match context
          ((directory enabled? shared statuses globals project-key)
           (define (report result)
             (hash-set! statuses path result)
             (status path result))
           (define (register files)
             (for-each (lambda (file)
                         (unless (string=? file canonical) (hash-set! globals file #t))) files))
           ;;     Every read resolves the same shared dependency list, so
           ;;     the deduplication must be linear: the list mixes the
           ;;     framework's runtime inputs with the project's assets and
           ;;     srfi-1 delete-duplicates would rescan it per read.
           (define (unique-paths paths)
             (let ((seen (make-hash-table)))
               (filter (lambda (file)
                         (and (not (hash-ref seen file))
                              (begin (hash-set! seen file #t) #t)))
                       paths)))
           (register (append shared dependencies))
           (let* ((paths (unique-paths (cons path (append shared dependencies))))
                  (snapshot (map (lambda (file)
                                   (let ((key (normalize-key-path file)))
                                     (list key (input-fingerprint key))))
                                 paths))
                  (identity (list reader (version) (setlocale LC_CTYPE)
                                  %load-path %load-compiled-path snapshot))
                  (entry-path (string-append directory "/"
                                (entry-key path)))
                  (entry (and reuse? (read-entry entry-path)))
                  (hit (catch #t
                         (lambda ()
                           (match entry
                             ((stored extra reusable? document)
                              (let ((checked
                                      (and (equal? stored identity)
                                           reusable?
                                           (every (lambda (input)
                                                    (match input
                                                      ((file digest)
                                                       (equal? digest (input-fingerprint file)))
                                                      (_ #f))) extra)
                                           (begin (register (map car extra))
                                                  (unpack document)))))
                                (when (and timings (not checked) (pair? entry))
                                  (format (current-error-port)
                                    "Profile cache miss diagnosis: ~a\n"
                                    (or (first-identity-difference stored identity)
                                        "dependency fingerprints changed")))
                                checked))
                             (_ #f)))
                         (lambda _ #f))))
             (if (content-document? hit)
                 (begin (report 'hit) (timed 'reused hit))
                 (let* ((trace (make-hash-table))
                        (document (call-with-input-trace trace read-document))
                        (extra (sort
                                 (hash-fold (lambda (file digest inputs)
                                              (if (and (string? file) (not (string=? file canonical)))
                                                  (cons (list (normalize-key-path file) digest)
                                                        inputs)
                                                  inputs))
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
                   (timed 'fresh document)))))))))


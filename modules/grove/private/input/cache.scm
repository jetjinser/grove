;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private input cache)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove ir)
  #:use-module (grove private cache-dir)
  #:use-module (grove private files)
  #:use-module (grove private fingerprint)
  #:use-module (grove private input dependencies)
  #:use-module (grove private html-style)
  #:use-module (grove style)
  #:use-module (grove style html element)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (current-input-cache current-project-key make-input-cache-context
            current-input-cache-timings
            input-cache-dependencies input-cache-summary input-cache-disabled? prune-input-cache!
            register-input-dependencies!
            cached-document))

;; A build scopes this parameter; checks and standalone reads never reuse results.
(define current-input-cache (make-parameter #f))

(define-record-type <input-cache-context>
  (%make-input-cache-context enabled? directory shared statuses globals)
  input-cache-context?
  (enabled? input-cache-context-enabled?)
  (directory input-cache-context-directory)
  (shared input-cache-context-shared)
  (statuses input-cache-context-statuses)
  (globals input-cache-context-globals))

;; Set by the project loader: a content digest of the project file and
;; its load-time inputs. Document and asset-pipeline cache keys derive
;; from it, so code imported by project.scm invalidates both safely.
(define current-project-key (make-parameter #f))

;; Profile-mode accumulator: a hash the CLI installs and reads back
;; after the build.  cached-document adds reused/fresh seconds and
;; counts here - the reuse path covers the whole hit side (entry read,
;; identity check, dependency verification, unpack), the fresh path
;; the whole miss side (read-document).
(define current-input-cache-timings (make-parameter #f))

(define (entry-key path)
  (content-fingerprint
    (string-append (or (current-project-key) "")
                   "\0"
                   (normalize-input-path path))))

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

(define (make-input-cache-context enabled? dependencies)
  (unless (boolean? enabled?)
    (error "input cache option must be a boolean" enabled?))
  (unless (and (list? dependencies)
               (every filesystem-path-string? dependencies))
    (error "input cache dependencies must be a list of paths" dependencies))
  (let ((globals (make-hash-table)))
    (for-each (lambda (path) (hash-set! globals path #t)) dependencies)
    ;;     Dependency collection remains active when reuse is disabled.
    ;;     The document cache lives in the per-user shared store, not
    ;;     a per-output directory: its keys are content-derived, so
    ;;     concurrent checkouts and worktrees share one set of entries.
    (%make-input-cache-context
      enabled? (and enabled?
                    (string-append (grove-store-directory) "/input-cache"))
      dependencies (make-hash-table) globals)))

(define (register-input-dependencies! context paths)
  (when context
    (for-each (lambda (path)
                (hash-set! (input-cache-context-globals context) path #t))
              paths)))

(define (input-cache-dependencies context)
  (if context
      (hash-map->list (lambda (path fingerprint) path)
                      (input-cache-context-globals context))
      '()))

(define (input-cache-summary context)
  (if (and context (input-cache-context-enabled? context))
      (let ((statuses (input-cache-context-statuses context)))
        (cons (hash-count (lambda (path status) (eq? status 'hit)) statuses)
              (hash-count (const #t) statuses)))
      '(0 . 0)))

(define (input-cache-disabled? context)
  (and context
       (or (not (input-cache-context-enabled? context))
           (> (hash-count (lambda (path status) (eq? status 'disabled))
                          (input-cache-context-statuses context)) 0))))

(define (prune-input-cache! context)
  (when (and context (input-cache-context-enabled? context))
    (let ((directory (input-cache-context-directory context))
          (statuses (input-cache-context-statuses context)))
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
              (lambda (name) (not (member name '("." "..")))))))))))

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
         (list 'document (visit (ir:content-document-children/raw value))
               (visit (ir:content-document-source/raw value))))
        ((content-text? value)
         (list 'text (ir:content-text-value/raw value) (visit (ir:content-text-source/raw value))
               (visit (ir:content-text-location/raw value))))
        ((content-layout? value)
         (list 'layout (content-layout-kind value) (ir:content-layout-raw/raw value)
               (visit (ir:content-layout-source/raw value))))
        ((content-element? value)
         (list 'element (content-element-kind value)
               (visit (ir:content-element-children/raw value))
               (visit (ir:content-element-properties/raw value))
               (visit (content-element-style value))
               (visit (ir:content-element-source/raw value))
               (visit (ir:content-element-location/raw value))
               (content-element-flow-kind value)))
        ((content-style? value)
         (list 'style (content-style-name value)
               (visit (content-style-properties value))))
        ((html-attributes? value)
         (list 'attributes (visit (html-attributes-assoc value))))
        ((html-tag? value) (list 'tag (html-tag-name value)))
        ((html-void? value) '(html-void))
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
     (make-content-document (unpack children) #:source (unpack source)))
    (('text text source location)
     (make-content-text text #:source (unpack source)
       #:location (unpack location)))
    (('layout kind raw source)
     (make-content-layout kind raw #:source (unpack source)))
    (('element kind children properties style source location flow-kind)
     (make-content-element kind (unpack children) #:source (unpack source)
       #:properties (unpack properties) #:style (unpack style)
       #:location (unpack location)
       #:flow-kind flow-kind))
    (('style name properties) (make-content-style name (unpack properties)))
    (('attributes assoc) (make-html-attributes (unpack assoc)))
    (('tag name) (make-html-tag name))
    (('html-void) (make-html-void))
    (('pair first rest) (cons (unpack first) (unpack rest)))
    (('datum datum) datum)
    (_ (error "invalid cached content"))))

(define (read-entry path)
  (and (regular-file? path #:follow-links? #f)
       (catch #t
         (lambda ()
           (call-with-input-file path
             (lambda (port)
               (set-port-encoding! port "UTF-8")
               (let ((entry (read port)))
                 (and (eof-object? (read port)) entry)))))
         (lambda _ #f))))

(define (save-entry path entry)
  (ensure-directory (dirname path))
  (call-with-staging-file! path (lambda (port) (write entry port))))

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
        (let* ((directory (input-cache-context-directory context))
               (shared (input-cache-context-shared context))
               (statuses (input-cache-context-statuses context))
               (globals (input-cache-context-globals context)))
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
                                   (let ((key (normalize-input-path file)))
                                     (list (normalize-input-path key)
                                           (input-fingerprint file))))
                                 paths))
                  (identity (list reader (version) (setlocale LC_CTYPE)
                                  %load-path %load-compiled-path snapshot))
                  (reuse? (and reuse? (input-cache-context-enabled? context)))
                  (entry-path (and (input-cache-context-enabled? context)
                                   (string-append directory "/"
                                                  (entry-key path))))
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
                                                  (cons (list (normalize-input-path file) digest)
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
                   (timed 'fresh document))))))))


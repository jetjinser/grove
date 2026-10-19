;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private preview worker)
  #:use-module (grove project)
  #:use-module (grove project load)
  #:use-module (grove private cache-dir)
  #:use-module (grove private build-lock)
  #:use-module (grove private cli progress)
  #:use-module (grove private input cache)
  #:use-module (grove project diagnostic)
  #:use-module (grove build state)
  #:use-module (grove output)
  #:use-module (grove private preview watch)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:export (worker-main))

(define (existing-files paths)
  (filter (lambda (path)
            (let ((info (stat path #f)))
              (and info (eq? (stat:type info) 'regular))))
          (delete-duplicates paths string=?)))

(define (watch-roots filename project sources modules)
  (let* ((extra-files (map absolute-path (project-extra-sources project)))
         (roots
           (delete-duplicates
             (map (lambda (path)
                    (let ((absolute (absolute-path path)))
                      (if (file-exists? absolute) (canonicalize-path absolute) absolute)))
               (append (list (dirname filename))
                       (if (project-source-directory project)
                           (list (project-source-directory project)) '())
                       (map dirname
                         (filter (lambda (path)
                                   (not (or (member path modules string=?)
                                            (member path extra-files string=?))))
                                 sources))))
             string=?)))
    ;; Do not rescan nested roots, or recursively scan the parent directory
    ;; of a single declared external file.
    (filter (lambda (root)
              (not (any (lambda (parent)
                          (and (not (string=? root parent))
                               (string-prefix? (string-append parent "/") root)))
                        roots)))
            roots)))

;;     The resident worker's own record of what the world looked like
;;     after its last successful build is what turns a rebuild request
;;     into forest reuse or a fresh analysis.  Stamps are file
;;     signatures, so reverted edits and same-size writes are caught.
(define (diff-known-stamps known paths)
  (let loop ((paths (delete-duplicates paths string=?))
             (changed '()) (removed '()))
    (if (null? paths)
        (list (reverse changed) (reverse removed))
        (let* ((path (car paths))
               (previous (assoc path known))
               (current (file-stamp path)))
          (cond
            ((equal? previous current)
             (loop (cdr paths) changed removed))
            (current
             (loop (cdr paths) (cons path changed) removed))
            (previous
             (loop (cdr paths) changed (cons path removed)))
            (else
             (loop (cdr paths) changed removed)))))))

(define (forest-reusable? project forest retained-state changed removed restart-paths)
  ;;     The forest is the analysis output, so it survives exactly the
  ;;     changes analysis cannot see: documents (the collector's set,
  ;;     including newly added ones) and anything else analysis read -
  ;;     prepared outputs, analyzer-time includes, module resolutions.
  ;;     Load-time extra sources ride the forest by design; removals and
  ;;     restart paths need the full path.  Everything else only
  ;;     invalidates render output, which rebuild-from-forest refreshes.
  (and forest retained-state
       (null? removed)
       ;;     The watch walk and the collector can spell one file two
       ;;     ways through a symlinked source directory, and a deleted
       ;;     file cannot be canonicalized at all: it surfaces as a
       ;;     change with no file behind it.  Compare canonical
       ;;     spellings and refuse reuse for vanished paths.
       (let* ((canonical
                (lambda (path)
                  (or (false-if-exception (canonicalize-path path)) path)))
              (documents (false-if-exception
                          (collect-project-sources project)))
              (analysis (last-analysis-inputs))
              (document-spellings (and documents (map canonical documents)))
              (analysis-spellings (and analysis (map canonical analysis))))
         (and documents
              (not (any (lambda (path)
                          (let ((spelling (canonical path)))
                            (or (member path restart-paths string=?)
                                (not (file-exists? path))
                                (member spelling document-spellings string=?)
                                (and analysis-spellings
                                     (member spelling analysis-spellings string=?)))))
                        changed))))))

(define (worker-state-path output)
  (string-append (grove-cache-directory output) "/" build-state-name))

(define (write-result! result-path payload)
  ;;     Serve polls this file while the worker is alive, so the
  ;;     write lands through a temp file and an atomic rename: a
  ;;     partially written result would be read as garbage.
  (let ((temporary (string-append result-path ".tmp")))
    (call-with-output-file temporary
      (lambda (port)
        (write payload port)
        (newline port)))
    (rename-file temporary result-path)))

(define (build-once request forest retained-state known result-path)
  ;;     Run one build request against the current generation.  The
  ;;     project is loaded fresh - renderers and load-time assets are
  ;;     per-request - but analysis comes from the retained forest
  ;;     whenever nothing analysis-visible changed.  Returns three
  ;;     values for the next generation: forest, build state, stamps.
  (match request
    ((request-serial filename output previous-modules previous-roots
                     previous-files debug? profile?)
     (let ((started (cli-seconds))
           (loaded '()) (loaded-stamps '()) (old-hook %load-hook))
       (dynamic-wind
         (lambda ()
           (set! %load-hook
             (lambda (path)
               (when old-hook (old-hook path))
               (when (and (string? path) (string-suffix? ".scm" path))
                 (let ((path (absolute-path path)))
                   (set! loaded (cons path loaded))
                   (set! loaded-stamps (cons (file-stamp path) loaded-stamps)))))))
         (lambda ()
           (call-with-loaded-project filename
             (lambda (project)
               (let* ((modules (existing-files (append previous-modules loaded)))
                      (configured project)
                      (output (absolute-path (project-output-directory configured)))
                      (files (map absolute-path
                                  (append previous-files
                                          (project-extra-sources configured))))
                      (roots (delete-duplicates
                               (append previous-roots
                                       (watch-roots filename project files modules))
                               string=?))
                      (before (watch-snapshot roots files output))
                       ;;     The diff set must see additions and
                       ;;     removals alike: the walked snapshot finds
                       ;;     newly created sources the last analysis
                       ;;     never named, while the serve-provided
                       ;;     file list keeps deleted sources visible
                       ;;     after they drop out of the tree.  Grove's
                       ;;     own tree and store paths stay out - worker
                       ;;     loads and environment resolutions are not
                       ;;     project edits.
                       (watched
                        (filter
                          (lambda (path) (not (unwatched-path? path)))
                          (delete-duplicates
                            (append files (list filename) (map car before))
                            string=?)))
                      (changes (diff-known-stamps known watched))
                      (changed (car changes))
                      (removed (cadr changes))
                      ;;     Restart paths never reach a request in
                      ;;     normal operation - serve restarts the
                      ;;     worker for them first - but a forest is
                      ;;     never trusted across one anyway.
                      (reuse? (forest-reusable? configured forest
                                                 retained-state changed removed
                                                 (cons filename modules)))
                      (profile-timings (and profile? (make-hash-table)))
                      (report
                        (parameterize ((current-input-cache-timings
                                         profile-timings))
                          (call-with-cli-progress "Building" filename
                            (lambda (progress)
                              (if reuse?
                                  (rebuild-from-forest configured forest
                                                       retained-state
                                                       changed removed
                                                       #:progress progress)
                                  (build-project/report configured
                                                        #:progress progress))))))
                      (modules (existing-files (append modules loaded)))
                      (files (delete-duplicates
                               (append (map absolute-path (build-report-sources report)) modules)
                               string=?))
                      (roots (watch-roots filename project files modules)))
                  ;; This is a private process protocol, not a second artifact API.
                  (write-result! result-path
                    (list 'preview request-serial output roots files modules
                          ;;     Loaded stamps may name grove's own and
                          ;;     dependency modules under store prefixes;
                          ;;     the serve side diffs this snapshot
                          ;;     against its own, so foreign paths must
                          ;;     stay out or every resolution difference
                          ;;     turns into a rebuild.
                          (delete-duplicates
                            (append before
                                    (filter (lambda (entry)
                                              (not (unwatched-path? (car entry))))
                                            loaded-stamps))
                            (lambda (left right) (string=? (car left) (car right))))
                         (map (lambda (artifact)
                                (list (artifact-path artifact) (artifact-content artifact)))
                              (build-report-artifacts report))))
                 (print-cli-summary report (- (cli-seconds) started) #:output output)
                 ;;     Profile mode: the cost split lands on stderr
                 ;;     after the summary; a reuse build shows the
                 ;;     analysis it skipped by having nothing to split.
                 (when profile-timings
                   (report-profile (current-error-port) profile-timings))
                 (values (if reuse? forest (last-build-model))
                         (read-build-state (worker-state-path output))
                         ;;     Keep the pre-build snapshot as the
                         ;;     next diff's baseline: edits that land
                         ;;     while a build runs were not in its
                         ;;     analysis, and a post-build snapshot
                         ;;     would silently absorb them.
                         before)))
             #:output-directory output))
         (lambda () (set! %load-hook old-hook)))))
    (unexpected (error "invalid preview worker request" unexpected))))

(define (serve-gone? live-path)
  ;;     The flock on .live names the live serve: the kernel frees it
  ;;     when the holder dies, so taking it means the serve is gone
  ;;     without having cleaned up.  A missing or unreadable file says
  ;;     the same - cleanup removes the whole workspace.
  (catch #t
    (lambda ()
      (or (not (file-exists? live-path))
          (let ((port (open-file live-path "r")))
            (dynamic-wind
              (lambda () #t)
              (lambda ()
                (let ((gone? (try-exclusive-lock port)))
                  (release-lock port)
                  gone?))
              (lambda () (close-port port))))))
    (lambda _ #t)))

(define (serve-watchdog worker-pid live-path)
  ;;     A renderer blocked in a child wait never returns to Scheme, so
  ;;     the worker cannot check its serve during a build; a forked
  ;;     watchdog can.  When the serve disappears it signals the worker's
  ;;     group, project subprocesses included.  The group check keeps a
  ;;     worker that never became a leader from signalling its launcher.
  (let loop ()
    (if (not (= (getppid) worker-pid))
        (primitive-exit 0)
        (if (serve-gone? live-path)
            (begin
              (when (= (getpgrp) worker-pid)
                (catch #t
                  (lambda () (kill (- (getpgrp)) SIGKILL))
                  (lambda _ #t)))
              (primitive-exit 0))
            (begin (usleep 200000) (loop))))))

(define (call-with-serve-watch live-path thunk)
  (let ((watchdog (primitive-fork)))
    (if (zero? watchdog)
        (begin
          (serve-watchdog (getppid) live-path)
          (primitive-exit 0))
        (dynamic-wind
          (lambda () #t)
          thunk
          (lambda ()
            (catch #t (lambda () (kill watchdog SIGKILL)) (lambda _ #t))
            (catch #t (lambda () (waitpid watchdog 0)) (lambda _ #t)))))))

(define (await-request request-path last-serial live-path serve-pid)
  ;;     A serve that exits cleanly removes the workspace; one that is
  ;;     killed leaves it behind but reparents this process.  Either way
  ;;     there is no one left to read a result, so stop.
  (let poll ()
    (if (or (not (file-exists? live-path))
            (not (= (getppid) serve-pid)))
        (exit 0)
        (let ((request (false-if-exception
                         (call-with-input-file request-path read))))
          (match request
            (((? (lambda (candidate)
                   (and (integer? candidate) (> candidate last-serial))))
              rest ...)
             request)
            (_ (usleep 50000) (poll)))))))

(define (worker-main arguments)
  (match arguments
    ((request-path result-path)
     (let ((live-path (string-append (dirname request-path) "/.live"))
           (serve-pid (getppid)))
       (call-with-serve-watch live-path
         (lambda ()
           (let loop ((serial 0) (forest #f) (retained-state #f) (known '()))
             (let ((request (await-request request-path serial live-path serve-pid)))
               (match request
                 ((request-serial filename output previous-modules previous-roots
                                  previous-files debug? profile?)
                  (catch 'grove-project-error
                    (lambda ()
                      (call-with-project-stage 'build #f
                        (lambda ()
                          (let-values (((next-forest next-state next-known)
                                        (build-once request forest retained-state
                                                    known result-path)))
                            (loop request-serial next-forest next-state
                                  next-known)))))
                    (lambda (unexpected error)
                      (print-project-error error profile?)
                      1)))
                 (unexpected
                  (error "invalid preview worker request" unexpected)))))))))
    (other (error "invalid preview worker arguments" other))))

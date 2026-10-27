;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove store-sharing)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64))

;;     Two checkouts of identical content, one shared content store:
;;     the second checkout must reuse the first's document analyses,
;;     and both must produce byte-identical artifacts.  This is the
;;     contract that makes parallel agents and git worktrees share
;;     build work instead of recomputing it.

(define root (format #f "/tmp/grove-store-sharing-~a" (getpid)))
(define store (string-append root "/store"))
(define sources (getenv "GROVE_SRCDIR"))

(define (write-file path text)
  (call-with-output-file path (lambda (port) (display text port))))

(define (setup-worktree worktree)
  (mkdir worktree)
  (mkdir (string-append worktree "/sources"))
  (write-file (string-append worktree "/sources/page.plant")
              "@title{Shared}\n\nShared corpus content.\n")
  (write-file (string-append worktree "/project.scm")
    (string-append
      "(use-modules (grove project) (grove build) (grove output)\n"
      "             (grove project site) (grove site) (grove input scribble)\n"
      "             (forester input scribble) (forester site) (forester commands)\n"
      "             (grove ir) (system base language) (language scribble spec)\n"
      "             (grove render plain-text)\n"
      "             (ice-9 textual-ports))\n"
      "(define environment-factory\n"
      "  (lambda ()\n"
      "    (let ((e (default-environment scribble)))\n"
      "      (eval '(use-modules (forester commands)) e)\n"
      "      e)))\n"
      "(make-site-project\n"
      "  #:title \"Sharing\"\n"
      "  #:home \"page\"\n"
      "  #:source-directory \"sources\"\n"
      "  #:read-page (make-page-reader\n"
      "                (make-scribble-document-reader\n"
      "                  #:environment-factory environment-factory))\n"
      "  #:renderers\n"
      "  (list (make-cached-renderer\n"
      "          (lambda (site)\n"
      "            (map (lambda (page)\n"
      "                   (make-artifact (string-append (site-page-id page) \".html\")\n"
      "                                  (content->plain-text (site-page-document page))))\n"
      "                 (site-pages site)))))\n"
      "  #:output-directory \"build\")\n"))
  (write-file (string-append worktree "/entry.scm")
    "(use-modules (grove private cli))\n(exit (main (cdr (command-line))))\n"))

(define run-counter 0)

(define (run worktree . arguments)
  (set! run-counter (+ run-counter 1))
  (let ((out (open-output-file (format #f "~a/.out.~a" root run-counter)))
        (err (open-output-file (format #f "~a/.err.~a" root run-counter)))
        (input (open-input-file "/dev/null")))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (let ((pid
               (spawn "guile"
                 (append (list "guile" "--no-auto-compile"
                               "-L" (dirname sources) "-L" sources
                               "-C" (string-append (dirname sources) "/build")
                               (string-append worktree "/entry.scm"))
                         arguments)
                 #:input input #:output out #:error err
                 #:environment
                 (cons (string-append "GROVE_STORE_DIR=" store)
                   (cons (string-append "GROVE_CACHE_DIR=" root "/cache")
                       (filter (lambda (entry)
                                 (not (or (string-prefix? "GROVE_STORE_DIR=" entry)
                                          (string-prefix? "GROVE_CACHE_DIR=" entry))))
                               (environ)))))))
          (let* ((status (cdr (waitpid pid)))
                 (result
                  (list (status:exit-val status)
                        (call-with-input-file
                          (format #f "~a/.out.~a" root run-counter)
                          get-string-all)
                        (call-with-input-file
                          (format #f "~a/.err.~a" root run-counter)
                          get-string-all)))
                 (exit-value (car result)))
            (unless (zero? exit-value)
              (format (current-error-port)
                      "store-sharing child exited ~a\nstdout:\n~a\nstderr:\n~a\n"
                      exit-value (cadr result) (caddr result)))
            result)))
      (lambda () (close-port input) (close-port out) (close-port err)))))

(test-begin "shared content store")

(dynamic-wind
  (lambda () #t)
  (lambda ()
    (mkdir root)
    (let ((wt-a (string-append root "/wt-a"))
          (wt-b (string-append root "/wt-b")))
      (setup-worktree wt-a)
      (setup-worktree wt-b)
      (let ((result (run wt-a "build" "--debug" "--profile" "--project" (string-append wt-a "/project.scm"))))
        (test-equal "the first checkout builds from a cold store" 0 (car result))
        (test-assert "the first checkout reads fresh"
          (string-contains (caddr result) "1 fresh")))
      (let ((result (run wt-b "build" "--debug" "--profile" "--project" (string-append wt-b "/project.scm"))))
        (test-equal "the second checkout builds successfully" 0 (car result))
        (test-assert "the second checkout reuses the shared analyses"
          (string-contains (caddr result) "1 reused")))
      (test-equal "artifacts are byte-identical across checkouts"
        (call-with-input-file (string-append wt-a "/build/page.html") get-string-all)
        (call-with-input-file (string-append wt-b "/build/page.html") get-string-all))
      (let ((result (run wt-a "build" "--debug" "--profile" "--project" (string-append wt-a "/project.scm"))))
        (test-equal "the first checkout still reuses after the share" 0 (car result))
        (test-assert "the first checkout's reuse is intact"
          (string-contains (caddr result) "1 reused")))))
  (lambda () (system* "rm" "-rf" root)))

(test-end "shared content store")

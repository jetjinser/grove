;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Standalone watcher profile. Run from the repository root:
;;;   guile --no-auto-compile -L modules -C build tests/perf/serve-watch.scm
;;; Optional positional arguments set seconds, source files and build files.

(use-modules (grove private preview watch)
             (ice-9 ftw)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-13))

(define (remove-tree path)
  (when (file-exists? path)
    (let ((info (lstat path)))
      (if (eq? 'directory (stat:type info))
          (begin
            (for-each (lambda (name)
                        (remove-tree (string-append path "/" name)))
                      (scandir path (lambda (name)
                                      (not (member name '("." ".."))))))
            (rmdir path))
          (delete-file path)))))

(define (write-sample path)
  (call-with-output-file path
    (lambda (port) (display "watch profile fixture\n" port))))

(define (write-numbered-files directory prefix count suffix)
  (do ((index 0 (+ index 1)))
      ((= index count))
    (write-sample
      (string-append directory "/" prefix (number->string index) suffix))))

(define (make-fixture root source-count build-count)
  (let* ((project (string-append root "/project"))
         (plants (string-append project "/plants"))
         (modules (string-append project "/modules"))
         (build (string-append project "/build"))
         (programs (string-append build "/programs"))
         (fonts (string-append build "/fonts"))
         (served (string-append root "/served-output")))
    (for-each mkdir (list project plants modules build programs fonts))
    (write-sample (string-append project "/project.scm"))
    (let ((plant-count (quotient source-count 2)))
      (write-numbered-files plants "plant-" plant-count ".plant")
      (write-numbered-files modules "module-"
                            (- source-count plant-count) ".scm"))
    (let ((program-count (quotient build-count 2)))
      (write-numbered-files programs "program-" program-count ".html")
      (write-numbered-files fonts "font-"
                            (- build-count program-count) ".woff2"))
    (values project build served)))

(define (profile-watch label roots files excluded-path seconds)
  (let* ((baseline (watch-snapshot roots files excluded-path))
         (start-real (get-internal-real-time))
         (start-run (get-internal-run-time))
         (scans 0)
         (scan-run 0)
         (diff-run 0)
         (longest-scan 0)
         (deadline (+ start-real (* seconds internal-time-units-per-second))))
    (let loop ((baseline baseline) (next-scan start-real))
      (let* ((now (get-internal-real-time))
             (scan? (>= now next-scan))
             (scan-start (get-internal-run-time))
             (current (if scan?
                          (watch-snapshot roots files excluded-path)
                          baseline))
             (scan-duration (- (get-internal-run-time) scan-start))
             (diff-start (get-internal-run-time)))
        (snapshot-changes baseline current)
        (set! diff-run (+ diff-run
                          (- (get-internal-run-time) diff-start)))
        (when scan?
          (set! scans (+ scans 1))
          (set! scan-run (+ scan-run scan-duration))
          (set! longest-scan (max longest-scan scan-duration)))
        (usleep 50000)
        (if (< (get-internal-real-time) deadline)
            (loop current
                  (if scan?
                      (+ now (/ internal-time-units-per-second 2))
                      next-scan))
            (let* ((real (/ (- (get-internal-real-time) start-real)
                            internal-time-units-per-second))
                   (run (/ (- (get-internal-run-time) start-run)
                           internal-time-units-per-second))
                   (scan-seconds (/ scan-run internal-time-units-per-second))
                   (diff-seconds (/ diff-run internal-time-units-per-second))
                   (to-ms (lambda (ticks)
                            (* 1000 (/ ticks internal-time-units-per-second)))))
              (format #t
                "~a: ~a entries, ~a scans / ~,2fs; CPU ~,3fs (~,2f% of one core); scan CPU ~,3fs, diff CPU ~,3fs; mean/max scan ~,2f/~,2fms~%"
                label (length baseline) scans real run (* 100 (/ run real))
                scan-seconds diff-seconds
                (if (zero? scans) 0 (to-ms (/ scan-run scans)))
                (to-ms longest-scan))))))))

(define (positive-argument text fallback)
  (let ((value (and text (string->number text))))
    (if (and (integer? value) (> value 0)) value fallback)))

(define (main arguments)
  (match arguments
    (() (main '("10" "37" "283")))
    ((seconds sources build-files)
     (let* ((seconds (positive-argument seconds 10))
            (sources (positive-argument sources 37))
            (build-files (positive-argument build-files 283))
            (workspace (mkdtemp
                         (string-append (or (getenv "TMPDIR") "/tmp")
                                        "/grove-watch-profile-XXXXXX"))))
       (dynamic-wind
         (lambda () #t)
         (lambda ()
           (call-with-values
             (lambda () (make-fixture workspace sources build-files))
             (lambda (project build served)
               (format #t "Fixture: ~a source files, ~a existing build files; duration ~as~%"
                       (+ sources 1) build-files seconds)
               ;; This is the current CLI shape: --output points outside the
               ;; project, so its existing build/ directory is scanned too.
               (profile-watch "current" (list project)
                              (list (string-append project "/project.scm"))
                              served seconds)
               ;; Isolate the cost of traversing a generated project/build tree.
               (profile-watch "without project/build" (list project)
                              (list (string-append project "/project.scm"))
                              build seconds))))
         (lambda () (remove-tree workspace)))))
    (_
     (format (current-error-port)
       "Usage: guile ... tests/perf/serve-watch.scm [SECONDS SOURCE-FILES BUILD-FILES]~%")
     2)))

(exit (main (cdr (command-line))))

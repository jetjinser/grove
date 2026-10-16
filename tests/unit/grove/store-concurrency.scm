;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove store-concurrency)
  #:use-module (grove private artifact-store)
  #:use-module (grove private fingerprint)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

;;     Concurrent writers meeting on the machine-global store: threads
;;     stand in for parallel builds and checkouts.  The discipline
;;     under test is that every publish installs a complete blob and
;;     every verified restore materializes the exact bytes - races may
;;     cost work, never bytes.

(define root (format #f "/tmp/grove-store-concurrency-~a" (getpid)))
(setenv "GROVE_STORE_DIR" (string-append root "/store"))

(test-begin "grove store concurrency")

(dynamic-wind
  (lambda () #t)
  (lambda ()
    (define thread-count 12)

    ;;     Multilingual body: string publishes must round-trip as the
    ;;     UTF-8 bytes that content-fingerprint hashed.
    (define text
      (let loop ((i 0) (lines '()))
        (if (= i 600)
            (string-concatenate (reverse lines))
            (loop (+ i 1)
                  (cons (format #f "page ~a: übersetzt über die Straße — 渲染输出 ✓\n" i)
                        lines)))))
    (define fingerprint (content-fingerprint text))

    (define (run-thunks thunks)
      (map join-thread (map call-with-new-thread thunks)))

    (define (publish! content)
      (publish-artifact-blob! (content-fingerprint content)
        (lambda (port) (display content port))))

    (define (blob-bytes blob-fingerprint)
      (call-with-input-file (artifact-blob-path blob-fingerprint) get-string-all))

    (test-assert "concurrent publishes of one fingerprint all succeed"
      (every (lambda (result) (eq? result #t))
             (run-thunks
              (make-list thread-count
                         (lambda () (publish! text))))))

    (test-equal "the raced blob holds the exact bytes"
      text (blob-bytes fingerprint))

    ;;     Republish storms: every rename replaces the blob wholesale,
    ;;     so the store entry keeps matching its own fingerprint.
    (test-assert "repeated publish storms keep the blob byte-exact"
      (let loop ((round 0))
        (or (= round 4)
            (and (every (lambda (result) (eq? result #t))
                        (run-thunks
                         (make-list thread-count
                                    (lambda () (publish! text)))))
                 (string=? (blob-bytes fingerprint) text)
                 (loop (+ round 1))))))

    ;;     Distinct fingerprints racing at once: no staging file may
    ;;     bleed into a neighbor's blob.
    (let* ((texts (map (lambda (i)
                         (format #f "artifact ~a\n~a" i text))
                       (iota thread-count)))
           (fingerprints (map content-fingerprint texts)))
      (run-thunks (map (lambda (content) (lambda () (publish! content)))
                       texts))
      (test-assert "concurrent distinct publishes land every blob"
        (every artifact-blob-exists? fingerprints))
      (test-assert "each raced blob holds only its own bytes"
        (every (lambda (pair)
                 (string=? (blob-bytes (car pair)) (cdr pair)))
               (map cons fingerprints texts))))

    ;;     Restorers racing on one output path: every writer stages
    ;;     under its own name, so no rename may strand partial bytes.
    (let ((output (string-append root "/outputs/shared/page.html")))
      (test-assert "concurrent restores to one output all succeed"
        (every (lambda (result) (eq? result #t))
               (run-thunks
                (make-list thread-count
                           (lambda ()
                             (restore-artifact-blob! fingerprint output))))))
      (test-equal "the raced restore holds the exact bytes"
        text
        (call-with-input-file output get-string-all)))

    ;;     Readers restoring while a publisher keeps renaming: a
    ;;     verified restore must yield exactly the fingerprinted bytes,
    ;;     and the store must end the storm with the intact blob.
    (let* ((outputs (map (lambda (i)
                           (format #f "~a/outputs/storm/~a" root i))
                         (iota 4)))
           (thunks
             (cons (lambda ()
                     (let loop ((i 0))
                       (when (< i 40)
                         (publish! text)
                         (usleep 300)
                         (loop (+ i 1))))
                     'published)
                   (map (lambda (output)
                          (lambda ()
                            (cons 'reads
                                  (let loop ((i 0) (reads '()))
                                    (if (= i 40)
                                        reads
                                        (loop (+ i 1)
                                              (cons (if (restore-artifact-blob!
                                                         fingerprint output)
                                                        (call-with-input-file
                                                         output get-string-all)
                                                        'miss)
                                                    reads)))))))
                        outputs))))
      (let* ((results (run-thunks thunks))
             (reads (append-map cdr (filter (lambda (result)
                                              (and (pair? result)
                                                   (eq? (car result) 'reads)))
                                            results))))
        (test-assert "restores racing a publisher only produce exact bytes"
          (every (lambda (read)
                   (or (eq? read 'miss)
                       (string=? read text)))
                 reads))
        (test-equal "the store survives the publish storm byte-exact"
          text (blob-bytes fingerprint)))))

  (lambda () (system* "rm" "-rf" root)))

(test-end "grove store concurrency")

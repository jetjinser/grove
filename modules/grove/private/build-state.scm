;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private build-state)
  #:use-module ((grove private output) #:prefix out:)
  #:use-module (grove output)
  #:use-module (grove private files)
  #:use-module (grove private fingerprint)
  #:re-export (content-fingerprint file-fingerprint)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (build-state-name
            build-state-temp-name
            build-state-sources
            build-state-artifacts
            build-state-identity
            build-state-source-path
            build-state-source-fingerprint
            build-state-source-kind
            build-state-artifact-path
            build-state-artifact-fingerprint
            build-state-artifact-sources
            build-state-artifact-renderer
            build-state-artifact-kind
            read-build-state
            write-build-state))

(define build-state-name ".grove-build-state")
(define build-state-temp-name ".grove-build-state.tmp")

;; Keep the on-disk S-expression layout behind named accessors.
(define-inlinable (build-state-sources state) (list-ref state 1))
(define-inlinable (build-state-artifacts state) (list-ref state 2))
(define-inlinable (build-state-identity state) (list-ref state 3))
(define-inlinable (build-state-source-path entry) (list-ref entry 0))
(define-inlinable (build-state-source-fingerprint entry) (list-ref entry 1))
(define-inlinable (build-state-source-kind entry) (list-ref entry 2))
(define-inlinable (build-state-artifact-path entry) (list-ref entry 0))
(define-inlinable (build-state-artifact-fingerprint entry) (list-ref entry 1))
(define-inlinable (build-state-artifact-sources entry) (list-ref entry 2))
(define-inlinable (build-state-artifact-renderer entry) (list-ref entry 3))
(define-inlinable (build-state-artifact-kind entry) (list-ref entry 4))

(define (distinct-paths? entries path-of)
  (let ((paths (map path-of entries)))
    (= (length paths) (length (delete-duplicates paths string=?)))))

(define (valid-source-entry? entry)
  (and (list? entry)
       (= (length entry) 3)
       (string? (build-state-source-path entry))
       (or (not (build-state-source-fingerprint entry))
           (valid-fingerprint? (build-state-source-fingerprint entry)))
       (memq (build-state-source-kind entry) '(source runtime))))

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
       (artifact-path? (build-state-artifact-path entry))
       (valid-fingerprint? (build-state-artifact-fingerprint entry))
       (or (not (build-state-artifact-sources entry))
           (and (list? (build-state-artifact-sources entry))
                (every string? (build-state-artifact-sources entry))))
       (or (not (build-state-artifact-renderer entry))
           (and (integer? (build-state-artifact-renderer entry))
                (>= (build-state-artifact-renderer entry) 0))
           ;;     Asset pipelines mark their entries with the
           ;;     pipeline's name instead of a renderer index.
           (and (string? (build-state-artifact-renderer entry))
                (not (string-null? (build-state-artifact-renderer entry)))))
       (memq (build-state-artifact-kind entry) '(text bytes))))

(define (valid-build-state? state)
  (and (list? state)
       (= (length state) 4)
       (eq? (car state) 'grove-build-state)
       (list? (build-state-sources state))
       (every valid-source-entry? (build-state-sources state))
       (distinct-paths? (build-state-sources state)
                        build-state-source-path)
       (list? (build-state-artifacts state))
       (every valid-output-entry? (build-state-artifacts state))
       (distinct-paths? (build-state-artifacts state)
                        build-state-artifact-path)
       (or (not (build-state-identity state))
           (string? (build-state-identity state)))))

(define (read-build-state path)
  (if (not (file-exists? path))
      #f
      (call-with-input-file path
        (lambda (port)
          (set-port-encoding! port "UTF-8")
          (let ((state (read port)))
            (and (valid-build-state? state)
                 (eof-object? (read port))
                 state))))))

(define (write-build-state path temp-path sources artifacts owners identity)
  (let ((state
          (list 'grove-build-state
                sources
                (map (lambda (artifact)
                       (list (out:artifact-path/raw artifact)
                             (content-fingerprint (out:artifact-content/raw artifact))
                             (out:artifact-sources/raw artifact)
                             (cdr (assoc (out:artifact-path/raw artifact) owners))
                             (if (string? (out:artifact-content/raw artifact))
                                 'text 'bytes)))
                     artifacts)
                identity)))
    (unless (valid-build-state? state)
      (error "refusing to write invalid Grove build state" state))
    (ensure-directory (dirname temp-path))
    (call-with-output-file temp-path
      (lambda (port)
        (set-port-encoding! port "UTF-8")
        (write state port)
        (newline port)))
    (rename-file temp-path path)))

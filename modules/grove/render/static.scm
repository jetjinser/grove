;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render static)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module (grove collect)
  #:use-module (grove build)
  #:use-module (grove output)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 ftw)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (make-static-assets-renderer))

(define* (make-static-assets-renderer #:key source-directory (prefix #f))
  (unless (and (string? source-directory) (file-is-directory? source-directory))
    (error "static asset source directory must exist" source-directory))
  (unless (or (not prefix) (artifact-path? prefix))
    (error "static asset prefix must be a relative artifact path or #f" prefix))
  (set! source-directory (string-copy source-directory))
  (set! prefix (and prefix (string-copy prefix)))
  (let ((render-all-assets
          (lambda ()
            (map
      (lambda (source)
        (let* ((relative (substring source (+ 1 (string-length source-directory))))
               (path (if prefix (string-append prefix "/" relative) relative))
               (port (open-file source "rb"))
               (bytes
                 (dynamic-wind
                   (lambda () #t)
                   (lambda () (get-bytevector-all port))
                   (lambda () (close-port port)))))
          (make-byte-artifact path bytes (list source))))
      (collect-directory source-directory)))))
   (make-cached-renderer
    (lambda (_model) (render-all-assets))
    #:configuration (list 'static source-directory prefix)
    #:inputs (lambda () (collect-directory source-directory))
    #:render-changed
    (lambda (_model changed _dirty-paths old-artifacts)
      (map
        (lambda (source)
          (let* ((relative
                   (substring source (+ 1 (string-length source-directory))))
                 (path (if prefix (string-append prefix "/" relative) relative))
                 (old (find (lambda (artifact)
                              (and (equal? (out:artifact-sources/raw artifact)
                                           (list source))
                                   (not (member source changed string=?))))
                            old-artifacts)))
            (or old
                (let ((port (open-file source "rb")))
                  (let ((bytes
                          (dynamic-wind
                            (lambda () #t)
                            (lambda () (get-bytevector-all port))
                            (lambda () (close-port port)))))
                    (make-byte-artifact path bytes (list source)))))))
        (collect-directory source-directory))))))

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

(define (normalize-source-directory directory)
  (let loop ((end (string-length directory)))
    (if (and (> end 1) (char=? (string-ref directory (- end 1)) #\/))
        (loop (- end 1))
        (substring directory 0 end))))

(define (static-asset-path source-directory prefix source)
  (let ((relative
          (substring source
                     (if (string-suffix? "/" source-directory)
                         (string-length source-directory)
                         (+ 1 (string-length source-directory))))))
    (if prefix (string-append prefix "/" relative) relative)))

(define (read-static-asset source-directory prefix source)
  (let ((port (open-file source "rb")))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (make-byte-artifact (static-asset-path source-directory prefix source)
                            (get-bytevector-all port)
                            (list source)))
      (lambda () (close-port port)))))

(define* (make-static-assets-renderer #:key source-directory (prefix #f))
  (unless (and (string? source-directory) (file-is-directory? source-directory))
    (error "static asset source directory must exist" source-directory))
  (unless (or (not prefix) (artifact-path? prefix))
    (error "static asset prefix must be a relative artifact path or #f" prefix))
  (set! source-directory
        (normalize-source-directory (string-copy source-directory)))
  (set! prefix (and prefix (string-copy prefix)))
  (make-cached-renderer
    (lambda (_model)
      (map (lambda (source)
             (read-static-asset source-directory prefix source))
           (collect-directory source-directory)))
    #:configuration (list 'static source-directory prefix)
    #:inputs (lambda () (collect-directory source-directory))
    #:render-changed
    (lambda (_model changed _dirty-paths old-artifacts)
      (map
        (lambda (source)
          (or (find (lambda (artifact)
                      (and (equal? (out:artifact-sources/raw artifact)
                                   (list source))
                           (not (member source changed string=?))))
                    old-artifacts)
              (read-static-asset source-directory prefix source)))
        (collect-directory source-directory)))))

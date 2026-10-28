;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private renderer)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (%make-cached-renderer
            renderer-cache-configuration?
            cached-renderer?
            cached-renderer-configuration
            cached-renderer-procedure
            cached-renderer-render-changed
            cached-renderer-inputs
            renderer-configurations))

(define-record-type <cached-renderer>
  (%make-cached-renderer configuration procedure render-changed inputs)
  cached-renderer?
  (configuration cached-renderer-configuration)
  (procedure cached-renderer-procedure)
  (render-changed cached-renderer-render-changed)
  (inputs cached-renderer-inputs))

(define (renderer-cache-configuration? value)
  "Return true when VALUE is acyclic, serializable cache-identity data."
  (let ((active (make-hash-table)))
    (define (walk value)
      (cond
       ((or (boolean? value) (char? value) (number? value)
            (symbol? value) (keyword? value)
            (string? value) (bytevector? value) (null? value))
        #t)
       ((or (pair? value) (vector? value))
        (and (not (hashq-ref active value))
             (begin
               (hashq-set! active value #t)
               (let ((valid?
                       (if (pair? value)
                           (and (walk (car value)) (walk (cdr value)))
                           (let loop ((index 0))
                             (or (= index (vector-length value))
                                 (and (walk (vector-ref value index))
                                      (loop (+ index 1))))))))
                 (hashq-remove! active value)
                 valid?))))
       (else #f)))
    (walk value)))

(define (renderer-configurations model renderers)
  ;; Keep peer configurations in order. A plain renderer contributes #f only
  ;; for its own slot and does not hide cache identities around it.
  (map (lambda (renderer)
         (and (cached-renderer? renderer)
              (let* ((configuration (cached-renderer-configuration renderer))
                     (value (if (procedure? configuration)
                                (configuration model)
                                configuration)))
                (unless (renderer-cache-configuration? value)
                  (error "renderer configuration must produce acyclic Scheme data"
                         value))
                value)))
       renderers))

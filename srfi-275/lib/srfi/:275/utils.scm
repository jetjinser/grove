;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;; SPDX-License-Identifier: MIT

(define-module (srfi :275 utils)
  #:use-module (rnrs base)
  #:use-module ((srfi :275 internal resource-identifier)
                #:prefix resource:)
  #:use-module (srfi :275 internal utils)
  #:use-module (srfi :275 internal path)
  #:re-export (encode-string decode-string)
  #:export (user-settable? host-settable? port-settable? path-settable?))

(define (make-settable? name resource-predicate reference-predicate
                        resource-operation reference-operation)
  (lambda (ident value)
    (cond
     ((resource-predicate ident)
      (resource-operation ident value))
     ((reference-predicate ident)
      (reference-operation ident value))
     (else
      (assertion-violation name "not a URI or IRI" ident)))))

(define user-settable?
  (make-settable? 'user-settable?
                  resource:resource-identifier?
                  resource:relative-reference?
                  (lambda (ident value)
                    (resource:authority-settable?
                     ident value resource:user-sets-authority?))
                  (lambda (ident value)
                    (resource:reference-authority-settable?
                     ident value resource:reference-user-sets-authority?))))

(define host-settable?
  (make-settable? 'host-settable?
                  resource:resource-identifier?
                  resource:relative-reference?
                  (lambda (ident value)
                    (resource:authority-settable?
                     ident value resource:host-sets-authority?))
                  (lambda (ident value)
                    (resource:reference-authority-settable?
                     ident value resource:reference-host-sets-authority?))))

(define port-settable?
  (make-settable? 'port-settable?
                  resource:resource-identifier?
                  resource:relative-reference?
                  (lambda (ident value)
                    (resource:authority-settable?
                     ident value resource:port-sets-authority?))
                  (lambda (ident value)
                    (resource:reference-authority-settable?
                     ident value resource:reference-port-sets-authority?))))

(define (path-settable? ident path)
  (unless (path? path)
    (assertion-violation 'path-settable? "not a path object" path))
  (cond
   ((resource:resource-identifier? ident)
    (resource:path-settable? ident path))
   ((resource:relative-reference? ident)
    (resource:reference-path-settable? ident path))
   (else (assertion-violation 'path-settable? "not a URI or IRI" ident))))

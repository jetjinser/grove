;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private query-output)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove ir)
  #:export (prepare-query-output))

(define* (prepare-query-output result source require-page
                               #:optional (known-anchor? #f))
  "Validate and normalize a query result before it joins the display tree.

KNOWN-ANCHOR?, when given, takes a target page id and a link fragment and
reports whether the page declares that anchor; unknown fragments warn."
  (define (validate-query-link target)
    (catch #t
      (lambda () (require-page target))
      (lambda (key . args)
        (error (format #f "Invalid query link at ~a to ~s: ~s"
                       source target (cons key args))))))
  (define (validate-query-fragment element)
    (let ((fragment (ir:content-element-property/raw element 'fragment))
          (target (ir:content-element-property/raw element 'target)))
      (when (and known-anchor? fragment (not (known-anchor? target fragment)))
        (format (current-warning-port)
          "warning: query at ~a: fragment ~s does not resolve in page ~a\n"
          source fragment target))))
  (define (prepare content)
    (cond
      ((content-element? content)
       (let ((kind (content-element-kind content)))
         (when (memq kind '(forest-query subtree))
           (error (format #f
             "Forester query at ~a returned unsupported ~a content"
             source kind)))
         (let* ((kind (case kind
                        ((link) 'query-link)
                        ((transclude) 'query-transclude)
                        (else kind)))
                (element
                  (make-content-element
                    kind (map prepare (ir:content-element-children/raw content))
                    (ir:content-element-source/raw content)
                    #:properties (ir:content-element-properties/raw content)
                    #:style (content-element-style content)
                    #:location (ir:content-element-location/raw content)
                    #:flow-kind (content-element-flow-kind content))))
           (when (memq kind '(query-link query-transclude))
             (validate-query-link
               (ir:content-element-property/raw element 'target)))
           (when (eq? kind 'query-link)
             (validate-query-fragment element))
           element)))
      (else content)))
  (unless (content-document? result)
    (error (format #f
      "Forester query at ~a must return a content document, got ~s"
      source result)))
  (make-content-document
    (map prepare (ir:content-document-children/raw result))
    (ir:content-document-source/raw result)))

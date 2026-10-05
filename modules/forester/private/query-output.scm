;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private query-output)
  #:use-module (grove ir)
  #:export (prepare-query-output))

(define (prepare-query-output result source find-page)
  "Validate and normalize a query result before it joins the display tree."
  (define (validate-query-link target)
    (catch #t
      (lambda () (find-page target))
      (lambda (key . args)
        (error (format #f "Invalid query link at ~a to ~s: ~s"
                       source target (cons key args))))))
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
                    kind (map prepare (content-element-children content))
                    (content-element-source content)
                    #:properties (content-element-properties content)
                    #:style (content-element-style content)
                    #:flow-kind (content-element-flow-kind content))))
           (when (memq kind '(query-link query-transclude))
             (validate-query-link
               (content-element-property element 'target)))
           element)))
      (else content)))
  (unless (content-document? result)
    (error (format #f
      "Forester query at ~a must return a content document, got ~s"
      source result)))
  (make-content-document
    (map prepare (content-document-children result))
    (content-document-source result)))

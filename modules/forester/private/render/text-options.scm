;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private render text-options)
  #:use-module (forester private backmatter)
  #:use-module (forester private render labels)
  #:use-module (forester private render-routes)
  #:use-module (forester render text routes)
  #:use-module (forester site)
  #:use-module (grove output)
  #:use-module (grove render rules)
  #:use-module (grove private snapshot)
  #:use-module (grove site)
  #:use-module (ice-9 format)
  #:export (normalize-text-renderer-options
            text-renderer-cache-identity))

(define (normalize-text-renderer-options renderer-name prefix routes backmatter
                                         reference-taxon-id locale messages rules)
  (let* ((prefix (snapshot-data prefix))
         (backmatter (snapshot-data backmatter))
         (reference-taxon-id (snapshot-data reference-taxon-id))
         (locale (snapshot-data locale)))
    (unless (or (not prefix) (artifact-path? prefix))
      (error (format #f
                     "Forester ~a prefix must be a relative artifact path or #f"
                     renderer-name)
             prefix))
    (unless (text-routes? routes)
      (error (format #f "Forester ~a options require text routes" renderer-name)
             routes))
    (validate-backmatter-sections backmatter)
    (check-reference-taxon-id reference-taxon-id)
    (set! locale (validate-interface-options locale messages))
    (unless (render-rules? rules)
      (error (format #f "Forester ~a options require render rules" renderer-name)
             rules))
    (values prefix routes backmatter reference-taxon-id locale messages rules)))

(define (text-renderer-cache-identity format forest prefix routes backmatter
                                      reference-taxon-id rules-id locale messages)
  (let* ((route-label
           (case format
             ((text) "text")
             ((gemtext) "Gemtext")
             (else (error "unknown text renderer format" format))))
         (route-table (resolve-text-route-table route-label forest routes))
         (site (site forest)))
    (list format prefix route-table
          (site-title site) (site-description site)
          (text-index-path routes) backmatter reference-taxon-id rules-id
          (interface-cache-identity locale messages))))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private project renderers)
  #:use-module ((grove private artifact) #:prefix out:)
  #:use-module (grove build)
  #:use-module (grove output)
  #:use-module (grove private build-state)
  #:use-module (grove private project artifacts)
  #:use-module (grove private renderer)
  #:use-module (grove project diagnostic)
  #:use-module (ice-9 i18n)
  #:use-module (srfi srfi-1)
  #:export (renderer-identity
            render-all
            render-with-cache))

(define (renderer-identity cache? model renderers)
  (and cache?
       (map
         (lambda (renderer configuration)
           (and (cached-renderer? renderer)
                (content-fingerprint
                  (call-with-output-string
                    (lambda (port)
                      (write (list (version) (setlocale LC_CTYPE) configuration)
                             port))))))
         renderers
         (renderer-configurations model renderers))))

(define (render-all model renderers progress)
  (let loop ((renderers renderers)
             (index 0)
             (artifacts '())
             (owners '())
             (rendered-indices '()))
    (if (null? renderers)
        (list artifacts owners (reverse rendered-indices))
        (let* ((start (get-internal-real-time))
               (generated (call-with-project-stage 'render #f
                            (lambda () (render-model model (list (car renderers))))))
               (seconds (/ (- (get-internal-real-time) start)
                           internal-time-units-per-second)))
          (progress 'render-renderer (list index seconds (length generated)))
          (loop (cdr renderers)
                (+ index 1)
                (append artifacts generated)
                (append owners
                        (map (lambda (artifact)
                               (cons (out:artifact-path/raw artifact) index))
                             generated))
                (cons index rendered-indices))))))

(define* (render-with-cache model renderers previous-entries cached-artifacts
                            previous-sources changed progress
                            #:key (model-retained? #f)
                            (renderer-identities 'unspecified)
                            (previous-renderer-identities 'unspecified))
  ;; MODEL-RETAINED? promises that MODEL is the same analysis the
  ;; previous-entries were rendered from (the retained model) and
  ;; that renderer's configuration is unchanged. render-model is then a pure function of
  ;; unchanged inputs for every artifact whose recorded sources did
  ;; not change, so a renderer without an incremental callback can
  ;; keep its whole previous output instead of re-rendering it -
  ;; but only when every one of its artifacts recorded sources:
  ;; undeclared render-time file reads are not visible here, and
  ;; such renderers conservatively re-run in full.
  (let loop ((renderers renderers)
             (index 0)
             (artifacts '())
             (owners '())
             (rendered-indices '()))
    (if (null? renderers)
        (list artifacts owners (reverse rendered-indices))
        (let* ((start (get-internal-real-time))
               (renderer (car renderers))
               (current-identity
                 (if (eq? renderer-identities 'unspecified)
                     'unspecified
                     (and (list? renderer-identities)
                          (< index (length renderer-identities))
                          (list-ref renderer-identities index))))
               (previous-identity
                 (if (eq? previous-renderer-identities 'unspecified)
                     'unspecified
                     (and (list? previous-renderer-identities)
                          (< index (length previous-renderer-identities))
                          (list-ref previous-renderer-identities index))))
               (identity-valid?
                 (or (eq? current-identity 'unspecified)
                     (and current-identity
                          (equal? current-identity previous-identity))))
               (old-entries
                 (filter (lambda (entry)
                           (equal? (build-state-artifact-renderer entry)
                                   index))
                         previous-entries))
               (old-artifacts
                 (if (list? cached-artifacts)
                     (filter (lambda (artifact)
                               (and (artifact-entry-by-path
                                      old-entries
                                      (out:artifact-path/raw artifact))
                                    #t))
                             cached-artifacts)
                     '()))
               (changed-proc
                 (and (cached-renderer? renderer)
                      (cached-renderer-render-changed renderer)))
               (partial? (and identity-valid?
                              (list? cached-artifacts)
                              changed-proc
                              (every (lambda (entry)
                                       (integer?
                                         (build-state-artifact-renderer entry)))
                                     old-entries)))
               (reusable?
                 (and identity-valid?
                      (list? cached-artifacts)
                      (pair? old-entries)
                      (not partial?)
                      (or (and (not model-retained?)
                               (not (eq? renderer-identities 'unspecified))
                               (null? changed))
                          ;; Every artifact must have recorded sources. A
                          ;; renderer that reads files without declaring them
                          ;; cannot reuse output from a retained model after
                          ;; tracked inputs change.
                          (and model-retained?
                               (every build-state-artifact-sources old-entries)
                               (not (any (lambda (entry)
                                           (paths-intersect?
                                             (build-state-artifact-sources entry)
                                             changed))
                                         old-entries))))))
               (dirty-entries
                 (filter (lambda (entry)
                           (or (any (lambda (path)
                                       (not (source-entry-by-path
                                             previous-sources path)))
                                    changed)
                               (not (build-state-artifact-sources entry))
                               (paths-intersect?
                                 (build-state-artifact-sources entry) changed)))
                         old-entries))
               (dirty-paths (map car dirty-entries))
               (generated
                 (and partial?
                      (call-with-project-stage 'render #f
                        (lambda ()
                          (changed-proc model changed dirty-paths old-artifacts)))))
               (rendered-fully? (or (not partial?) (not generated)))
               (new-artifacts
                 (if reusable?
                     old-artifacts
                     (call-with-project-stage 'render #f
                       (lambda ()
                         (if rendered-fully?
                             (render-model model (list renderer))
                             (begin
                               (unless (and (list? generated) (every artifact? generated))
                                 (error "incremental renderer must return artifacts or #f"
                                        generated))
                               generated))))))
               ;; An incremental callback returns the complete current output
               ;; set for its renderer. It can preserve unaffected artifacts
               ;; and omit outputs that have vanished.
               (current new-artifacts)
               (current-owners
                 (map (lambda (artifact)
                        (cons (out:artifact-path/raw artifact) index))
                      current))
               (seconds (/ (- (get-internal-real-time) start)
                           internal-time-units-per-second)))
          (progress 'render-renderer
                    (list index seconds (length current)))
          (loop (cdr renderers)
                (+ index 1)
                (append artifacts current)
                (append owners current-owners)
                (if reusable? rendered-indices
                    (cons index rendered-indices)))))))

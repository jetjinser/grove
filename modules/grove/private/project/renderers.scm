;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove private project renderers)
  #:use-module ((grove private output) #:prefix out:)
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
  (let ((configurations (renderer-configurations model renderers)))
    (and cache? configurations
         (content-fingerprint
           (call-with-output-string
             (lambda (port)
               (write (list (version) (setlocale LC_CTYPE) configurations)
                      port)))))))

(define (render-all model renderers progress)
  (let loop ((renderers renderers)
             (index 0)
             (artifacts '())
             (owners '()))
    (if (null? renderers)
        (list artifacts owners)
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
                             generated)))))))

(define* (render-with-cache model renderers previous-entries cached-artifacts
                            previous-sources changed progress
                            #:key (model-retained? #f))
  ;; MODEL-RETAINED? promises that MODEL is the same analysis the
  ;; previous-entries were rendered from (the retained forest) and
  ;; that renderer configurations are unchanged (identity checked
  ;; by the caller).  render-model is then a pure function of
  ;; unchanged inputs for every artifact whose recorded sources did
  ;; not change, so a renderer without an incremental callback can
  ;; keep its whole previous output instead of re-rendering it -
  ;; but only when every one of its artifacts recorded sources:
  ;; undeclared render-time file reads are not visible here, and
  ;; such renderers conservatively re-run in full.
  (let loop ((renderers renderers)
             (index 0)
             (artifacts '())
             (owners '()))
    (if (null? renderers)
        (list artifacts owners)
        (let* ((start (get-internal-real-time))
               (renderer (car renderers))
               (old-entries
                 (filter (lambda (entry)
                           (equal? (build-state-artifact-renderer entry)
                                   index))
                         previous-entries))
               (old-artifacts
                 (filter (lambda (artifact)
                           (and (artifact-entry-by-path
                                  old-entries (out:artifact-path/raw artifact))
                                #t))
                         cached-artifacts))
               (changed-proc
                 (and (cached-renderer? renderer)
                      (cached-renderer-render-changed renderer)))
               (partial? (and changed-proc
                              (every (lambda (entry)
                                       (integer?
                                         (build-state-artifact-renderer entry)))
                                     old-entries)))
               (reusable?
                 (and model-retained?
                      (pair? old-entries)
                      (not partial?)
                      ;; Every artifact must have recorded sources. A
                      ;; renderer that reads files without declaring them
                      ;; cannot safely reuse output from the retained model.
                      (every build-state-artifact-sources old-entries)
                      (not (any (lambda (entry)
                                  (paths-intersect?
                                    (build-state-artifact-sources entry)
                                    changed))
                                old-entries))))
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
                (append owners current-owners))))))

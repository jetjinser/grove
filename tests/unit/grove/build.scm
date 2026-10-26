;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove build)
  #:use-module (srfi srfi-64)
  #:use-module (grove build)
  #:use-module (grove private build-state)
  #:use-module (grove output)
  #:use-module (grove project))

(define render-with-cache
  (@@ (grove project) render-with-cache))
(define renderer-configurations
  (@@ (grove build) renderer-configurations))

(test-begin "grove build")

(test-assert "cached renderer representation stays private"
  (not (module-variable
         (module-public-interface (resolve-module '(grove build)))
         'cached-renderer?)))

(test-error "cached renderers require procedures" #t
  (make-cached-renderer #f))

(test-equal "default renderer configuration is empty data"
  '(())
  (renderer-configurations
    #f (list (make-cached-renderer (lambda (site) '())))))

(test-error "incremental callbacks must be procedures" #t
  (make-cached-renderer (lambda (site) '())
    #:render-changed #t))

(test-error "renderer input collectors must be procedures" #t
  (make-cached-renderer (lambda (site) '())
    #:inputs '()))

(let* ((configuration (list (cons 'theme (string-copy "light"))))
       (renderer
        (make-cached-renderer (lambda (_) '())
          #:configuration configuration)))
  (set-cdr! (car configuration) "dark")
  (test-equal "cached renderers snapshot declarative cache configuration"
    '(((theme . "light")))
    (renderer-configurations #f (list renderer))))

(define render-calls 0)
(define (plain-renderer model)
  ;;     A renderer without an incremental callback: render-with-cache
  ;;     can only keep its whole previous output or re-render it all.
  (set! render-calls (+ render-calls 1))
  (list (make-artifact "a.html" (string-append "page:" model) '("one.plant"))
        (make-artifact "shared.txt" "fresh-shared" '("theme.css"))))

(define previous-entries
  (list (list "a.html" (content-fingerprint "page:old") '("one.plant") 0 'text)
        (list "shared.txt" (content-fingerprint "fresh-shared")
              '("theme.css") 0 'text)))

(define cached-artifacts
  (list (make-artifact "a.html" "page:old" '("one.plant"))
        (make-artifact "shared.txt" "fresh-shared" '("theme.css"))))

(define previous-sources
  (list (list "one.plant" (content-fingerprint "one"))
        (list "theme.css" (content-fingerprint "css"))))

(define (run-render-cache changed . keywords)
  (set! render-calls 0)
  (apply render-with-cache "old" (list plain-renderer)
         previous-entries cached-artifacts
         previous-sources changed
         (lambda (stage detail) #t)
         keywords))

(test-equal "a retained-model renderer skips when nothing it recorded changed"
  '("a.html" "shared.txt" "page:old" 0)
  (let ((result (run-render-cache '() #:model-retained? #t)))
    (let ((artifacts (car result))
          (owners (cadr result)))
      (list (artifact-path (car artifacts))
            (artifact-path (cadr artifacts))
            (artifact-content (car artifacts))
            render-calls))))

(test-assert "skipped renderer artifacts keep their renderer ownership"
  (let ((owners (cadr (run-render-cache '() #:model-retained? #t))))
    (and (= (cdr (assoc "a.html" owners)) 0)
         (= (cdr (assoc "shared.txt" owners)) 0))))

(test-equal "a retained-model renderer re-renders when a recorded source changed"
  '(1 "page:old" "fresh-shared")
  (let ((result (run-render-cache '("theme.css") #:model-retained? #t)))
    (let ((artifacts (car result)))
      (list render-calls
            (artifact-content (car artifacts))
            (artifact-content (cadr artifacts))))))

(define opaque-entries
  (list (list "a.html" (content-fingerprint "page:old") #f 0 'text)
        (list "shared.txt" (content-fingerprint "fresh-shared") #f 0 'text)))

(define (run-opaque-render-cache changed)
  (set! render-calls 0)
  (render-with-cache "old" (list plain-renderer)
                     opaque-entries cached-artifacts
                     previous-sources changed
                     (lambda (stage detail) #t)
                     #:model-retained? #t))

(test-equal "a retained-model renderer without recorded sources re-runs"
  1
  (let ((result (run-opaque-render-cache '())))
    render-calls))

(test-equal "a fresh model re-renders even when nothing changed"
  '(1 "page:old")
  (let ((result (run-render-cache '())))
    (list render-calls (artifact-content (car (car result))))))

(test-end "grove build")

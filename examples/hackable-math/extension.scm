;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (examples hackable-math extension)
  #:use-module (forester site)
  #:use-module (grove build)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render context)
  #:use-module (grove render html routes)
  #:use-module (grove render rules)
  #:use-module (grove site)
  #:use-module (srfi srfi-1)
  #:export (make-compiled-math
            make-math-asset-renderer
            math-html-rules))

(define* (make-compiled-math expression asset-path #:key (alt #f))
  "Make a formula node whose SVG is produced by a project renderer.

EXPRESSION is compiler input. ASSET-PATH is the relative path emitted by
MAKE-MATH-ASSET-RENDERER. ALT defaults to EXPRESSION."
  (unless (and (string? expression) (not (string-null? expression)))
    (error "compiled math requires a non-empty expression" expression))
  (unless (artifact-path? asset-path)
    (error "compiled math requires a relative asset path" asset-path))
  (unless (or (not alt) (string? alt))
    (error "compiled math alternative text must be a string or #f" alt))
  (make-content-element 'compiled-math (list expression)
    #:properties `((asset-path . ,asset-path) (alt . ,(or alt expression)))))

(define (math-elements content)
  (cond
    ((content-document? content)
     (append-map math-elements (content-document-children content)))
    ((content-element? content)
     (if (eq? (content-element-kind content) 'compiled-math)
         (list content)
         (append-map math-elements (content-element-children content))))
    (else '())))

(define (site-math forest)
  (append-map
    (lambda (page)
      (map (lambda (element) (cons page element))
           (math-elements (site-page-document page))))
    (pages forest)))

(define (math-configuration compiler-id forest)
  (list 'compiled-math compiler-id
        (map (lambda (entry)
               (let ((page (car entry)) (element (cdr entry)))
                 (list (site-page-id page)
                       (content-element-property element 'asset-path)
                       (content->plain-text element))))
             (site-math forest))))

(define* (make-math-asset-renderer #:key compiler compiler-id
                                      (dependencies '()))
  "Return a cached renderer that turns compiled-math nodes into SVG artifacts.

COMPILER maps one expression string to SVG text. COMPILER-ID should include
the tool version and options that affect the output. DEPENDENCIES lists files
read by the compiler, such as macro packages or font configuration."
  (unless (procedure? compiler)
    (error "math asset renderer requires a compiler procedure" compiler))
  (unless (and (string? compiler-id) (not (string-null? compiler-id)))
    (error "math asset renderer requires a stable compiler ID" compiler-id))
  (unless (and (list? dependencies)
               (every (lambda (path)
                        (and (string? path) (not (string-null? path))))
                      dependencies))
    (error "math asset renderer dependencies must be a list of paths"
           dependencies))
  (make-cached-renderer
    (lambda (forest)
      (map
        (lambda (entry)
          (let* ((page (car entry))
                 (element (cdr entry))
                 (path (content-element-property element 'asset-path)))
            (unless (artifact-path? path)
              (error "compiled math node has an invalid asset path" path))
            (let* ((svg (compiler (content->plain-text element)))
                   (sources (delete-duplicates
                              (if (site-page-source page)
                                  (cons (site-page-source page) dependencies)
                                  dependencies)
                              string=?)))
              (unless (string? svg)
                (error "math compiler must return SVG text" svg))
              (make-artifact path svg (and (pair? sources) sources)))))
        (site-math forest)))
    #:configuration (lambda (forest) (math-configuration compiler-id forest))
    #:inputs (lambda () dependencies)))

(define math-html-rules
  (make-render-rules
    (list
      (cons 'compiled-math
        (lambda (context element _children next)
          (let ((routes (render-context-ref context 'routes))
                (path (content-element-property element 'asset-path))
                (alt (content-element-property element 'alt)))
            (if (and routes (artifact-path? path))
                `(img (@ (src ,(html-asset-url routes path))
                         (alt ,(or alt "Math formula"))))
                (next))))))))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester project)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (forester input scribble)
  #:use-module (forester render html)
  #:use-module (forester render text)
  #:use-module (forester site)
  #:use-module (grove input scribble)
  #:use-module (grove output)
  #:use-module (grove project)
  #:use-module (grove render html routes))

(define project-root
  (format #f "/tmp/forester-project-test-~a" (getpid)))
(define source-directory (string-append project-root "/source"))
(define module-directory (string-append source-directory "/modules"))
(define output-directory (string-append project-root "/output"))

(define (write-test-file path content)
  (call-with-output-file path
    (lambda (port) (display content port))))

(define (read-test-file path)
  (call-with-input-file path get-string-all))

(test-begin "forester project")

(dynamic-wind
  (lambda ()
    (mkdir project-root)
    (mkdir source-directory)
    (mkdir module-directory)
    (write-test-file
      (string-append module-directory "/commands.grv")
      "@(define-module (tests unit forester linked commands)\n  #:use-module (forester commands)\n  #:re-export (link title))")
    (write-test-file
      (string-append source-directory "/algebra.grv")
      "@use-modules[(tests unit forester linked commands)]\n@title{Algebra}\n@paragraph{See @link[\"limits\"]{limits}. @math{x^2+y^2=z^2}}\n@display-math{a^2+b^2=c^2}")
    (write-test-file
      (string-append source-directory "/limits.grv")
      "@use-modules[(tests unit forester linked commands)]\n@title{Limits}\n@paragraph{See @link[\"algebra\"]{algebra}.}"))
  (lambda ()
    (let* ((project
             (make-project
               #:title "Linked Notes"
               #:source-directory source-directory
               #:module-directory module-directory
               #:load-modules load-scribble-modules!
               #:read-page
               (make-forester-page-reader
                 (make-scribble-document-reader))
               #:analyze make-forest
               #:renderers
               (list (make-forester-html-renderer
                       #:routes
                       (make-html-routes
                         #:page-path
                         (lambda (id)
                           (string-append "notes/" id ".html"))))
                     (lambda (forest)
                       (prefix-artifacts "text"
                         (render-forest-text forest))))
               #:output-directory output-directory))
           (artifacts (build-project project))
           (paths (map artifact-path artifacts))
           (algebra-html
             (read-test-file
               (string-append output-directory "/notes/algebra.html")))
           (limits-html
             (read-test-file
               (string-append output-directory "/notes/limits.html")))
           (algebra-text
             (read-test-file
               (string-append output-directory "/text/algebra.txt"))))
      (test-assert "project composes HTML and text renderer outputs"
        (and (member "index.html" paths)
             (member "notes/algebra.html" paths)
             (member "notes/limits.html" paths)
             (member "text/index.txt" paths)
             (member "text/algebra.txt" paths)))
      (test-assert "HTML project output resolves page links"
        (string-contains algebra-html
          "href=\"/notes/limits.html\">limits</a>"))
      (test-assert "HTML project output contains reverse links"
        (string-contains limits-html
          "href=\"/notes/algebra.html\">Algebra</a>"))
      (test-assert "HTML project output preserves TeX for MathJax"
        (and (string-contains algebra-html "\\(x^2+y^2=z^2\\)")
             (string-contains algebra-html
               "mathjax@4/tex-chtml.js")))
      (test-assert "text target writes linked page content"
        (and (string-contains algebra-text "limits [limits.txt]")
             (file-exists? (string-append output-directory "/text/index.txt"))))
      (write-test-file
        (string-append source-directory "/broken.grv")
        (string-append
          "@use-modules[(tests unit forester linked commands)]\n"
          "@title{Broken}\n@link[\"missing\"]{missing}"))
      (test-error "project analysis rejects unresolved links before output"
        #t
        (build-project project))))
  (lambda ()
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (delete-file path)))
      (list (string-append output-directory "/index.html")
            (string-append output-directory "/notes/algebra.html")
            (string-append output-directory "/notes/limits.html")
            (string-append output-directory "/text/index.txt")
            (string-append output-directory "/text/algebra.txt")
            (string-append output-directory "/text/limits.txt")
            (string-append module-directory "/commands.grv")
            (string-append source-directory "/algebra.grv")
            (string-append source-directory "/limits.grv")
            (string-append source-directory "/broken.grv")))
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (rmdir path)))
      (list (string-append output-directory "/notes")
            (string-append output-directory "/text")
            output-directory
            module-directory
            source-directory
            project-root))))

(test-end "forester project")

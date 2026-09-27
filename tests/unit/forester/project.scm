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
  #:use-module (forester render rss)
  #:use-module (forester render sitemap)
  #:use-module (forester render text)
  #:use-module (forester site)
  #:use-module (grove input scribble)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove project)
  #:use-module (grove project site)
  #:use-module (grove render html routes)
  #:use-module (grove site))

(define project-root
  (format #f "/tmp/forester-project-test-~a" (getpid)))
(define source-directory (string-append project-root "/source"))
(define module-directory (string-append source-directory "/modules"))
(define output-directory (string-append project-root "/output"))
(define cache-output-directory (string-append project-root "/cache-output"))

(define (write-test-file path content)
  (call-with-output-file path
    (lambda (port) (display content port))))

(define (read-test-file path)
  (call-with-input-file path get-string-all))

(define (exception-contains? thunk text)
  (catch #t
    (lambda () (thunk) #f)
    (lambda (key . args)
      (string-contains (format #f "~s" (cons key args)) text))))

(test-begin "forester project")

(dynamic-wind
  (lambda ()
    (mkdir project-root)
    (mkdir source-directory)
    (mkdir module-directory)
    (write-test-file
      (string-append module-directory "/commands.grv")
      "@(define-module (tests unit forester linked commands)\n  #:use-module (forester commands)\n  #:re-export (link title summary published updated))")
    (write-test-file
      (string-append source-directory "/algebra.grv")
      (string-append
        "@use-modules[(tests unit forester linked commands)]\n"
        "@title{Algebra}\n"
        "@summary{A short algebra note.}\n"
        "@published{2026-09-27}\n"
        "@updated{2026-09-28}\n"
        "@paragraph{See @link[\"limits\"]{limits}. "
        "@math{x^2+y^2=z^2}}\n@display-math{a^2+b^2=c^2}"))
    (write-test-file
      (string-append source-directory "/limits.grv")
      "@use-modules[(tests unit forester linked commands)]\n@title{Limits}\n@paragraph{See @link[\"algebra\"]{algebra}.}")
    (write-test-file
      (string-append source-directory "/unlinked.grv")
      "@use-modules[(tests unit forester linked commands)]\n@title{Unlinked}\n@paragraph{Standalone.}")
    (write-test-file
      (string-append source-directory "/orphan.grv")
      "@use-modules[(tests unit forester linked commands)]\n@title{Orphan}\n@paragraph{Unreferenced.}"))
  (lambda ()
    (let* ((project
             (make-site-project
               #:title "Linked Notes"
               #:source-directory source-directory
               #:prepare (make-scribble-module-setup module-directory)
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
                     (make-forester-rss-renderer
                       #:site-url "https://notes.example"
                       #:routes
                       (make-html-routes
                         #:page-path
                         (lambda (id)
                           (string-append "notes/" id ".html"))))
                     (make-forester-sitemap-renderer
                       #:site-url "https://notes.example"
                       #:routes
                       (make-html-routes
                         #:page-path
                         (lambda (id)
                           (string-append "notes/" id ".html"))))
                     (make-forester-text-renderer #:prefix "text"))
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
               (string-append output-directory "/text/algebra.txt")))
           (rss-feed
             (read-test-file (string-append output-directory "/feed.xml")))
           (sitemap
             (read-test-file
               (string-append output-directory "/sitemap.xml"))))
      (test-assert "missing page titles identify the source path"
        (exception-contains?
          (lambda ()
            ((make-forester-page-reader
               (lambda (path) (make-content-document '())))
             (string-append source-directory "/no-title.grv")))
          (string-append source-directory "/no-title.grv")))
      (test-assert "page reader rejects impossible calendar dates"
        (exception-contains?
          (lambda ()
            ((make-forester-page-reader
               (lambda (_)
                 (make-content-document
                   (list
                     (make-content-element 'title '("Invalid date"))
                     (make-content-element
                       'published '("2026-02-31"))))))
             (string-append source-directory "/invalid-date.grv")))
          "must use YYYY-MM-DD"))
      (let* ((expected-sources
               (list (string-append module-directory "/commands.grv")
                     (string-append source-directory "/algebra.grv")
                     (string-append source-directory "/limits.grv")
                     (string-append source-directory "/unlinked.grv")
                     (string-append source-directory "/orphan.grv")))
             (expected-algebra-html-sources
               (list (string-append module-directory "/commands.grv")
                     (string-append source-directory "/algebra.grv")
                     (string-append source-directory "/limits.grv")))
             (algebra-html-artifact
               (find (lambda (artifact)
                       (string=? (artifact-path artifact)
                                 "notes/algebra.html"))
                     artifacts))
             (algebra-text-artifact
               (find (lambda (artifact)
                       (string=? (artifact-path artifact)
                                 "text/algebra.txt"))
                     artifacts)))
        (test-equal "page reader retains source paths"
          (string-append source-directory "/algebra.grv")
          (site-page-source
            ((make-forester-page-reader
               (make-scribble-document-reader))
             (string-append source-directory "/algebra.grv"))))
        (test-equal "HTML page artifacts depend on their backlinks"
          expected-algebra-html-sources
          (artifact-sources algebra-html-artifact))
        (test-equal "prefixed text artifacts retain source dependencies"
          expected-algebra-html-sources
          (artifact-sources algebra-text-artifact))
        (let ((algebra-page
                ((make-forester-page-reader
                   (make-scribble-document-reader))
                 (string-append source-directory "/algebra.grv"))))
          (test-equal "page reader extracts Forester metadata"
            '((summary . "A short algebra note.")
              (published . "2026-09-27")
              (updated . "2026-09-28"))
            (site-page-metadata algebra-page))
          (test-assert "page metadata stays out of the page body"
            (not (find (lambda (node)
                         (and (content-element? node)
                              (memq (content-element-kind node)
                                    '(summary published updated))))
                       (content-document-children
                         (site-page-document algebra-page))))))
        (test-assert "RSS uses summaries and updated dates"
          (and (string-contains rss-feed
                   "<description>A short algebra note.</description>")
                 (string-contains rss-feed
                   "<pubDate>Mon, 28 Sep 2026 00:00:00 GMT</pubDate>")))
        (test-assert "RSS channel description falls back to site title"
          (string-contains rss-feed
            "<description>Linked Notes</description>")))
      (test-assert "project composes HTML, RSS, and text renderer outputs"
        (and (member "index.html" paths)
             (member "feed.xml" paths)
             (member "sitemap.xml" paths)
             (member "notes/algebra.html" paths)
             (member "notes/limits.html" paths)
             (member "text/index.txt" paths)
             (member "text/algebra.txt" paths)))
      (test-assert "sitemap output uses the same routed page URLs"
        (and (string-contains
               sitemap "https://notes.example/notes/algebra.html")
             (string-contains sitemap "<lastmod>2026-09-28</lastmod>")))
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
      (let* ((incremental-project
               (make-site-project
                 #:source-directory source-directory
                 #:prepare (make-scribble-module-setup module-directory)
                 #:read-page
                 (make-forester-page-reader (make-scribble-document-reader))
                 #:analyze make-forest
                 #:renderers
                 (list (make-forester-html-renderer
                        #:routes
                         (make-html-routes
                           #:page-path
                           (lambda (id)
                             (string-append "notes/" id ".html")))
                         #:cache-key "forester-html-test-v1")
                       (make-forester-text-renderer
                         #:prefix "text"
                         #:cache-key "forester-text-test-v1"))
                 #:output-directory cache-output-directory
                 #:cache-key "forester-project-test-v1"))
             (first (build-project/report incremental-project)))
        (test-assert "cached Forester HTML renderer performs initial render"
          (build-report-rendered? first))
        (write-test-file
          (string-append source-directory "/unlinked.grv")
          "@use-modules[(tests unit forester linked commands)]\n@title{Unlinked}\n@paragraph{Changed standalone page.}")
        (let ((updated (build-project/report incremental-project)))
          (test-equal "unrelated pages are omitted from affected HTML outputs"
            '("index.html" "notes/unlinked.html" "text/index.txt"
              "text/unlinked.txt")
            (build-report-affected-artifacts updated))
          (test-assert "partial Forester render keeps backlink pages current"
            (string-contains
              (read-test-file
                (string-append cache-output-directory "/notes/limits.html"))
              "Algebra")))
        (write-test-file
          (string-append source-directory "/algebra.grv")
          (string-append
            "@use-modules[(tests unit forester linked commands)]\n"
            "@title{Algebra}\n"
            "@paragraph{See @link[\"limits\"]{limits}. "
            "@math{x^2+y^2=z^2}}\n"
            "@display-math{a^2+b^2=c^2}\n"
            "@paragraph{Also @link[\"unlinked\"]{unlinked}.}"))
        (let ((changed-links (build-project/report incremental-project)))
          (test-equal "new links update the newly linked page's backlinks"
            '("index.html" "notes/algebra.html" "notes/limits.html"
              "notes/unlinked.html" "text/index.txt" "text/algebra.txt"
              "text/limits.txt" "text/unlinked.txt")
            (build-report-affected-artifacts changed-links))
          (test-assert "new backlinks appear in the target page"
            (string-contains
              (read-test-file
                (string-append cache-output-directory
                               "/notes/unlinked.html"))
              "Algebra"))
          (test-assert "text target updates new backlink content"
            (string-contains
              (read-test-file
                (string-append cache-output-directory "/text/unlinked.txt"))
              "Algebra")))
        (write-test-file
          (string-append module-directory "/commands.grv")
          (string-append
            "(define-module (tests unit forester linked commands)\n"
            "  #:use-module (forester commands)\n"
            "  #:re-export (link title))\n"
            ";; changed module source\n"))
        (let ((changed-module (build-project/report incremental-project)))
          (test-equal "shared module changes are tracked as build inputs"
            (list (string-append module-directory "/commands.grv"))
            (build-report-modified-sources changed-module))
          (test-equal "shared module changes affect all rendered pages"
            '("index.html" "notes/algebra.html" "notes/limits.html"
              "notes/orphan.html" "notes/unlinked.html" "text/index.txt"
              "text/algebra.txt" "text/limits.txt" "text/orphan.txt"
              "text/unlinked.txt")
            (build-report-affected-artifacts changed-module)))
        (delete-file (string-append source-directory "/orphan.grv"))
        (let ((removed-page (build-project/report incremental-project)))
          (test-equal "removing a page affects indexes and its own outputs"
            '("index.html" "text/index.txt" "notes/orphan.html"
              "text/orphan.txt")
            (build-report-affected-artifacts removed-page))
          (test-assert "removed page output is cleaned from HTML and text targets"
            (and (not (file-exists?
                       (string-append cache-output-directory
                                      "/notes/orphan.html")))
                 (not (file-exists?
                       (string-append cache-output-directory
                                      "/text/orphan.txt"))))))
      (write-test-file
        (string-append source-directory "/broken.grv")
        (string-append
          "@use-modules[(tests unit forester linked commands)]\n"
          "@title{Broken}\n@link[\"missing\"]{missing}"))
      (test-error "project analysis rejects unresolved links before output"
        #t
        (build-project project)))))
  (lambda ()
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (delete-file path)))
      (list (string-append output-directory "/index.html")
            (string-append output-directory "/feed.xml")
            (string-append output-directory "/sitemap.xml")
            (string-append output-directory "/notes/algebra.html")
            (string-append output-directory "/notes/limits.html")
            (string-append output-directory "/notes/unlinked.html")
            (string-append output-directory "/notes/orphan.html")
            (string-append output-directory "/text/index.txt")
            (string-append output-directory "/text/algebra.txt")
            (string-append output-directory "/text/limits.txt")
            (string-append output-directory "/text/unlinked.txt")
            (string-append output-directory "/text/orphan.txt")
            (string-append output-directory "/.grove-artifacts")
            (string-append output-directory "/.grove-artifacts.tmp")
            (string-append output-directory "/.grove-build-state")
            (string-append output-directory "/.grove-build-state.tmp")
            (string-append cache-output-directory "/index.html")
            (string-append cache-output-directory "/notes/algebra.html")
            (string-append cache-output-directory "/notes/limits.html")
            (string-append cache-output-directory "/notes/unlinked.html")
            (string-append cache-output-directory "/notes/orphan.html")
            (string-append cache-output-directory "/text/index.txt")
            (string-append cache-output-directory "/text/algebra.txt")
            (string-append cache-output-directory "/text/limits.txt")
            (string-append cache-output-directory "/text/unlinked.txt")
            (string-append cache-output-directory "/text/orphan.txt")
            (string-append cache-output-directory "/.grove-artifacts")
            (string-append cache-output-directory "/.grove-artifacts.tmp")
            (string-append cache-output-directory "/.grove-build-state")
            (string-append cache-output-directory "/.grove-build-state.tmp")
            (string-append module-directory "/commands.grv")
            (string-append source-directory "/algebra.grv")
            (string-append source-directory "/limits.grv")
            (string-append source-directory "/unlinked.grv")
            (string-append source-directory "/orphan.grv")
            (string-append source-directory "/broken.grv")))
    (for-each
      (lambda (path)
        (when (file-exists? path)
          (rmdir path)))
      (list (string-append output-directory "/notes")
            (string-append output-directory "/text")
            (string-append cache-output-directory "/notes")
            (string-append cache-output-directory "/text")
            cache-output-directory
            output-directory
            module-directory
            source-directory
            project-root))))

(test-end "forester project")

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester project)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (forester project)
  #:use-module (forester input scribble)
  #:use-module (forester relations)
  #:use-module (forester render gemtext)
  #:use-module (forester render graph)
  #:use-module (forester render html)
  #:use-module (forester render rss)
  #:use-module (forester render sitemap)
  #:use-module (forester render site)
  #:use-module (forester render text)
  #:use-module (forester site)
  #:use-module (grove input scribble)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove pipeline)
  #:use-module (grove project)
  #:use-module (grove project site)
  #:use-module (grove relation dsl)
  #:use-module (grove render html routes)
  #:use-module (grove render html presentation)
  #:use-module (grove site))

(define project-root
  (mkdtemp "/tmp/forester-project-test-XXXXXX"))
(define source-directory (string-append project-root "/source"))
(define module-directory (string-append project-root "/modules"))
(define mathematics-directory (string-append source-directory "/mathematics"))
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

;; Project code can name a useful composition of public Forester predicates.
(defrel (short-math-paragrapho nodes paragraph-id maximum)
  (math-containing-paragrapho nodes paragraph-id)
  (shorter-than-o nodes paragraph-id maximum))

(test-begin "forester project")

(test-assert "Forester projects require a home tree ID"
  (exception-contains?
    (lambda () (make-forester-project))
    "requires a non-empty home tree ID"))

(test-assert "Forester project home tree ID cannot be empty"
  (exception-contains?
    (lambda () (make-forester-project #:home ""))
    "requires a non-empty home tree ID"))

(test-assert "Forester project caching defaults to enabled"
  (project-cache? (make-forester-project #:home "root")))

(test-assert "Forester project can disable lifecycle caching"
  (not (project-cache? (make-forester-project #:home "root" #:cache? #f))))

(test-equal "Forester projects honor a custom source order"
  '("second.plant" "first.plant")
  (collect-project-sources
    (make-forester-project
      #:home "root"
      #:collect-sources (lambda () '("second.plant" "first.plant")))))

(test-equal "Forester projects declare additional tracked inputs"
  '("glossary.scm")
  (project-extra-sources
    (make-forester-project #:home "root"
                           #:extra-sources '("glossary.scm"))))

(dynamic-wind
  (lambda ()
    (setenv "XDG_CACHE_HOME" (string-append project-root "/.cache"))
    (mkdir source-directory)
    (mkdir module-directory)
    (mkdir mathematics-directory)
    (write-test-file
      (string-append module-directory "/commands.plant")
      "@(define-module (tests unit forester linked commands)\n  #:use-module (forester commands)\n  #:re-export (link tag taxon title summary published updated subtree))")
    (write-test-file
      (string-append source-directory "/algebra.plant")
      (string-append
        "@use-modules[(tests unit forester linked commands)]\n"
        "@title{Algebra}\n"
        "@summary{A short algebra note.}\n"
        "@published{2026-09-27}\n"
        "@updated{2026-09-28}\n"
        "@taxon[\"MaThEmAtIcS\"]\n"
        "@tag[\"AlGeBrA\"]\n"
        "@subtree[#:id \"quadratic-equations\"]{@title{Quadratic equations}}\n"
        "@paragraph{See @link[\"limits\"]{limits}. "
        "@tex-math{x^2+y^2=z^2}}\n"
        "@paragraph{范畴论 is useful.}\n@tex-display-math{a^2+b^2=c^2}"))
    (write-test-file
      (string-append mathematics-directory "/limits.plant")
      (string-append
        "@use-modules[(tests unit forester linked commands)]\n"
        "@title{Limits}\n"
        "@taxon[\"mathematics\"]\n"
        "@tag[\"analysis\"]\n"
        "@paragraph{See @link[\"algebra\"]{algebra}. "
        "Read @link[\"quadratic-equations\"]{about quadratics}.}"))
    (write-test-file
      (string-append source-directory "/unlinked.plant")
      "@use-modules[(tests unit forester linked commands)]\n@title{Unlinked}\n@paragraph{Standalone.}")
    (write-test-file
      (string-append source-directory "/orphan.plant")
      "@use-modules[(tests unit forester linked commands)]\n@title{Orphan}\n@paragraph{Unreferenced.}")
    (write-test-file
      (string-append source-directory "/unicode.plant")
      "@use-modules[(tests unit forester linked commands)]\n@title{Unicode topics}\n@taxon[\"Straße\"]\n@paragraph{Unicode topics.}")
    (write-test-file
      (string-append source-directory "/unicode-accent.plant")
      "@use-modules[(tests unit forester linked commands)]\n@title{Unicode accent}\n@taxon[\"café\"]\n@paragraph{è̤}"))
  (lambda ()
    (let* ((project
             (make-site-project
                 #:title "Linked Notes"
                 #:source-directory source-directory
               #:prepare (make-scribble-module-setup module-directory)
               #:read-page
               (make-page-reader
                 (make-scribble-document-reader))
               #:analyze make-forest
               #:renderers
               (append
                (list (make-html-renderer
                       #:options
                       (make-html-options
                         #:routes
                         (make-html-routes
                           #:home-page "algebra"
                           #:page-path
                           (lambda (id)
                             (string-append "notes/" id ".html")))))
                     (make-text-renderer
                       #:options (make-text-options #:prefix "text"))
                     (make-gemtext-renderer
                       #:options (make-gemtext-options #:prefix "gemini")))
                (list
                 (make-rss-renderer
                   #:site-url "https://notes.example"
                   #:routes
                   (make-html-routes
                     #:home-page "algebra"
                     #:page-path
                     (lambda (id)
                       (string-append "notes/" id ".html"))))
                 (make-sitemap-renderer
                   #:site-url "https://notes.example"
                   #:routes
                   (make-html-routes
                     #:home-page "algebra"
                     #:page-path
                     (lambda (id)
                       (string-append "notes/" id ".html"))))))
                 #:output-directory output-directory))
           (artifacts (build-project project))
           (paths (map artifact-path artifacts))
           (algebra-html
             (read-test-file
               (string-append output-directory "/index.html")))
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
            ((make-page-reader
               (lambda (path) (make-content-document '())))
             (string-append source-directory "/no-title.plant")))
          (string-append source-directory "/no-title.plant")))
      (test-assert "page reader rejects impossible calendar dates"
        (exception-contains?
          (lambda ()
            ((make-page-reader
               (lambda (_)
                 (make-content-document
                   (list
                     (make-content-element 'title '("Invalid date"))
                     (make-content-element
                       'published '("2026-02-31"))))))
             (string-append source-directory "/invalid-date.plant")))
          "must use YYYY-MM-DD"))
      (let* ((expected-sources
               (list (string-append module-directory "/commands.plant")
                     (string-append source-directory "/algebra.plant")
                     (string-append mathematics-directory "/limits.plant")
                     (string-append source-directory "/unlinked.plant")
                     (string-append source-directory "/orphan.plant")))
             (expected-algebra-html-sources
               (list (string-append module-directory "/commands.plant")
                     (string-append source-directory "/algebra.plant")
                     (string-append mathematics-directory "/limits.plant")))
             (algebra-html-artifact
               (find (lambda (artifact)
                       (string=? (artifact-path artifact)
                                 "index.html"))
                     artifacts))
             (algebra-text-artifact
               (find (lambda (artifact)
                       (string=? (artifact-path artifact)
                                 "text/algebra.txt"))
                     artifacts)))
        (test-equal "page reader retains source paths"
          (string-append source-directory "/algebra.plant")
          (site-page-source
            ((make-page-reader
             (make-scribble-document-reader))
             (string-append source-directory "/algebra.plant"))))
        (test-equal "nested source directories do not become page IDs"
          "limits"
          (site-page-id
            ((make-page-reader
             (make-scribble-document-reader))
             (string-append mathematics-directory "/limits.plant"))))
        (test-equal "nested source paths remain available for diagnostics"
          (string-append mathematics-directory "/limits.plant")
          (site-page-source
            ((make-page-reader
             (make-scribble-document-reader))
             (string-append mathematics-directory "/limits.plant"))))
        (test-equal "HTML page artifacts depend on their backlinks"
          expected-algebra-html-sources
          (artifact-sources algebra-html-artifact))
        (test-equal "prefixed text artifacts retain source dependencies"
          expected-algebra-html-sources
          (artifact-sources algebra-text-artifact))
        (let ((algebra-page
                ((make-page-reader
                   (make-scribble-document-reader))
                 (string-append source-directory "/algebra.plant"))))
      (test-equal "page reader keeps only non-topic metadata in metadata"
            '((summary . "A short algebra note.")
              (published . "2026-09-27")
              (updated . "2026-09-28"))
            (site-page-metadata algebra-page))
          (test-equal "topic declarations remain as IR nodes"
            '(taxon tag)
            (map content-element-kind
                 (filter (lambda (node)
                           (and (content-element? node)
                                (memq (content-element-kind node)
                                      '(title tag taxon))))
                         (content-document-children
                           (site-page-document algebra-page))))))
        (let* ((read-page
                 (make-page-reader
                   (make-scribble-document-reader)))
               (forest
                 (make-forest
                   (make-site
                     (list
                       (read-page (string-append source-directory "/algebra.plant"))
                       (read-page
                         (string-append mathematics-directory "/limits.plant"))
                       (read-page
                         (string-append source-directory "/unicode.plant"))
                       (read-page
                         (string-append source-directory "/unicode-accent.plant")))))))
          (test-equal "miniKanren queries topic membership and labels"
            '(("algebra" "mathematics") ("limits" "mathematics"))
            (run* (page-id label)
              (topic-membero
                (content-node-relation forest)
                "mathematics"
                page-id
                label)))
          (test-equal "topic relations normalize selectors bound by earlier goals"
            '("algebra" "limits")
            (run* (page-id)
              (fresh (topic label)
                (== topic "MATHEMATICS")
                (topic-membero (content-node-relation forest) topic page-id label))))
          (test-equal "topic helper accepts mixed-case selectors"
            '("algebra")
            (topic-page-ids (content-node-relation forest) "ALGEBRA" 'tag))
          (test-equal "topic page helper preserves site ordering"
            '("algebra" "limits")
            (map site-page-id (topic-pages forest "mathematics")))
          (test-equal "topic queries can select tags separately from taxa"
            '("algebra")
            (topic-page-ids (content-node-relation forest) "algebra" 'tag))
          (test-equal "topic selectors fold case across the full Unicode range"
            '("unicode")
            (topic-page-ids (content-node-relation forest) "STRASSE" 'taxon))
          (test-equal "topic selectors match canonically equivalent spellings"
            '("unicode-accent")
            (topic-page-ids
              (content-node-relation forest)
              (string-append "cafe" (string (integer->char #x301)))
              'taxon))
          (test-equal "enumerated topics keep their canonical spellings"
            '("straße")
            (run* (topic)
              (fresh (label)
                (topic-membero (content-node-relation forest) topic "unicode" label))))
          (test-equal "enumerated accented topics keep composed spellings"
            '("café")
            (run* (topic)
              (fresh (label)
                (topic-membero
                  (content-node-relation forest) topic "unicode-accent" label))))
          (let* ((nodes (content-node-relation forest))
                 (page-data (page-relation forest))
                 (paragraph-ids
                   (run* (node-id)
                     (nodeo nodes
                       (node-id node-id)
                       (page-id "algebra")
                       (kind 'paragraph)))))
            (test-equal "page metadata queries compose with IR queries"
              '(("Algebra" "A short algebra note." "范畴论 is useful."))
              (run* (title summary text)
                (fresh (page-id label node-id)
                  (pageo page-data
                    (page-id page-id)
                    (title title))
                  (page-metadatao page-data page-id 'summary summary)
                  (topic-membero nodes "algebra" page-id label)
                  (nodeo nodes
                    (node-id node-id)
                    (page-id page-id)
                    (kind 'paragraph)
                    (text text))
                  (contains-texto nodes node-id "范畴论"))))
            (test-equal "content queries combine topic and paragraph constraints"
              '("范畴论 is useful.")
              (run* (text)
                (fresh (page-id label node-id)
                  (topic-membero nodes "algebra" page-id label)
                  (nodeo nodes
                    (node-id node-id)
                    (page-id page-id)
                    (kind 'paragraph)
                    (text text))
                  (shorter-than-o nodes node-id 120)
                  (contains-texto nodes node-id "范畴论"))))
            (test-equal "project-defined goals compose Forester predicates"
              '("See limits. x^2+y^2=z^2")
              (run* (text)
                (fresh (node-id)
                  (short-math-paragrapho nodes node-id 120)
                  (nodeo nodes
                    (node-id node-id)
                    (page-id "algebra")
                    (kind 'paragraph)
                    (text text)))))
            (test-equal "plain text paragraph goal excludes inline elements"
              '("范畴论 is useful.")
              (run* (text)
                (fresh (node-id)
                  (nodeo nodes
                    (node-id node-id)
                    (page-id "algebra")
                    (text text))
                  (plain-text-paragrapho nodes node-id)
                  (contains-texto nodes node-id "范畴论"))))
            (test-equal "node text and character counts include nested content"
              '(("See limits. x^2+y^2=z^2" 23))
              (run* (text character-count)
                (fresh (node-id)
                  (nodeo nodes
                    (node-id node-id)
                    (page-id "algebra")
                    (kind 'paragraph)
                    (text text)
                    (character-count character-count))
                  (contains-texto nodes node-id "See"))))
            (test-equal "node property queries select display math"
              '(#t)
              (run* (display?)
                (fresh (node-id)
                  (nodeo nodes
                    (node-id node-id)
                    (page-id "algebra")
                    (kind 'math))
                  (node-propertyo nodes node-id 'display? display?)
                  (== display? #t))))
            (test-equal "character counts use grapheme clusters"
              '(14)
              (run* (character-count)
                (fresh (node-id text)
                  (nodeo nodes
                    (node-id node-id)
                    (page-id "algebra")
                    (kind 'paragraph)
                    (text text)
                    (character-count character-count))
                  (contains-texto nodes node-id "范畴论"))))
            (test-equal "a multi-codepoint Foochow letter is one character"
              '((1 3))
              (run* (character-count codepoint-count)
                (fresh (node-id text)
                  (nodeo nodes
                    (node-id node-id)
                    (page-id "unicode-accent")
                    (kind 'paragraph)
                    (text text)
                    (character-count character-count)
                    (codepoint-count codepoint-count))
                  (contains-texto nodes node-id "è̤"))))
            (test-assert "content node IDs are dense in traversal order"
              (let ((ids
                      (run* (node-id)
                        (nodeo nodes
                          (node-id node-id)
                          (page-id "algebra")))))
                (equal? ids (iota (length ids)))))
            (test-assert "content node IDs resolve back to IR elements"
              (and (pair? paragraph-ids)
                   (eq? 'paragraph
                     (content-element-kind
                       (content-node forest (car paragraph-ids))))))
            (test-assert "unknown content node IDs are rejected"
              (exception-contains?
                (lambda () (content-node forest -1))
                "unknown content node"))
            (test-assert "paragraph and math goals compose through descendants"
              (pair? (run* (node-id)
                       (fresh (math-id)
                         (nodeo nodes
                           (node-id node-id)
                           (page-id "algebra")
                           (kind 'paragraph))
                         (descendanto nodes node-id math-id)
                         (math-nodeo nodes math-id)))))
            (test-assert "math-containing paragraph helper finds the same nodes"
              (pair? (run* (node-id)
                       (math-containing-paragrapho nodes node-id)
                       (nodeo nodes
                         (node-id node-id)
                         (page-id "algebra")))))))
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
             (member "forester.css" paths)
             (not (member "notes/algebra.html" paths))
             (member "notes/limits.html" paths)
             (member "text/index.txt" paths)
             (member "text/algebra.txt" paths)
             (member "gemini/index.gmi" paths)
             (member "gemini/algebra.gmi" paths)))
      (test-assert "project writes Gemtext artifacts with routed page links"
        (let ((algebra-gemtext
                (read-test-file
                  (string-append output-directory "/gemini/algebra.gmi"))))
          (and (file-exists? (string-append output-directory "/gemini/index.gmi"))
               (string-contains algebra-gemtext "=> index.gmi Home")
               (string-contains algebra-gemtext "=> limits.gmi Limits"))))
      (test-assert "sitemap output uses the same routed page URLs"
        (and (string-contains sitemap "https://notes.example/index.html")
             (not (string-contains sitemap
                                   "https://notes.example/notes/algebra.html"))
             (string-contains sitemap "<lastmod>2026-09-28</lastmod>")))
        (test-assert "HTML project output resolves page links"
        (string-contains algebra-html
          "href=\"/notes/limits.html\">limits</a>"))
      (test-assert "HTML does not impose topic navigation"
        (let* ((cut (or (string-contains algebra-html "<aside")
                        (string-length algebra-html)))
               (body (substring algebra-html 0 cut)))
          ;;     Taxa surface only inside backmatter entries; the page
          ;;     body and titles stay free of topic navigation.
          (and (not (string-contains algebra-html "page-topics"))
               (not (string-contains body "class=\"tag\""))
               (not (string-contains body "class=\"taxon\""))
               (not (string-contains algebra-html "Mathematics")))))
      (test-assert "HTML project emits named tree titles"
        (string-contains algebra-html
          "<h2 class=\"forest-tree-title\"><span>Quadratic equations</span>"))
      (test-assert "real .plant tree links resolve through configured routes"
        (string-contains limits-html
          "href=\"/notes/quadratic-equations.html\">about quadratics</a>"))
      (test-assert "HTML project output contains reverse links"
        (and (string-contains limits-html "data-tree-id=\"algebra\"")
             (string-contains limits-html "<span>Algebra</span>")))
      (test-assert "page titles and backmatter entries carry dates"
        (and (string-contains algebra-html
               "<div class=\"metadata\"><ul><li class=\"meta-item\"><time datetime=\"2026-09-27\">2026-09-27</time></li><li class=\"meta-item\"><time datetime=\"2026-09-28\">updated 2026-09-28</time></li></ul></div>")
             (string-contains limits-html
               "</h3><div class=\"metadata\"><ul><li class=\"meta-item\"><time datetime=\"2026-09-27\">2026-09-27</time></li>")
             (string-contains limits-html ">[algebra]</a>")))
      (test-assert "HTML project output preserves TeX for MathJax"
        (and (string-contains algebra-html "\\(x^2+y^2=z^2\\)")
             (string-contains algebra-html
               "mathjax@4/tex-chtml.js")))
      (test-assert "text target writes linked page content"
        (and (string-contains algebra-text "limits [limits.txt]")
             (not (string-contains algebra-text "Mathematics"))
             (string-contains
               (read-test-file (string-append output-directory
                                               "/text/limits.txt"))
               "algebra.txt#ref-1")
             (file-exists? (string-append output-directory "/text/index.txt"))))
      (let* ((incremental-project
               (make-site-project
                   #:source-directory source-directory
                   #:prepare (make-scribble-module-setup module-directory)
                 #:read-page
                 (make-page-reader (make-scribble-document-reader))
                 #:analyze make-forest
                 #:renderers
                 (list (make-html-renderer
                        #:options
                        (make-html-options
                          #:routes
                          (make-html-routes
                            #:home-page "algebra"
                            #:page-path
                            (lambda (id)
                              (string-append "notes/" id ".html")))))
                       (make-text-renderer
                         #:options (make-text-options #:prefix "text")))
                 #:output-directory cache-output-directory))
             (first (build-project/report incremental-project)))
        (test-assert "cached Forester HTML renderer performs initial render"
          (build-report-rendered? first))
        (write-test-file
          (string-append source-directory "/unlinked.plant")
          "@use-modules[(tests unit forester linked commands)]\n@title{Unlinked}\n@paragraph{Changed standalone page.}")
        (let ((updated (build-project/report incremental-project)))
          (test-equal "unrelated pages are omitted from affected HTML outputs"
            '("notes/unlinked.html" "text/index.txt"
              "text/unlinked.txt")
            (build-report-affected-artifacts updated))
          (test-assert "partial Forester render keeps backlink pages current"
            (string-contains
              (read-test-file
                (string-append cache-output-directory "/notes/limits.html"))
              "Algebra")))
        (write-test-file
          (string-append source-directory "/algebra.plant")
          (string-append
            "@use-modules[(tests unit forester linked commands)]\n"
            "@title{Algebra}\n"
            "@subtree[#:id \"quadratic-equations\"]{@title{Polynomial equations}}\n"
            "@paragraph{See @link[\"limits\"]{limits}. "
            "@tex-math{x^2+y^2=z^2}}\n"
            "@tex-display-math{a^2+b^2=c^2}\n"
            "@paragraph{Also @link[\"unlinked\"]{unlinked}.}"))
        (write-test-file
          (string-append mathematics-directory "/limits.plant")
          (string-append
            "@use-modules[(tests unit forester linked commands)]\n"
            "@title{Limits}\n"
            "@paragraph{See @link[\"algebra\"]{algebra}. "
            "Read @link[\"quadratic-equations\"]{about polynomials}.}"))
        (let ((changed-links (build-project/report incremental-project)))
          (test-equal "new links update the newly linked page's backlinks"
            '("index.html" "notes/limits.html"
              "notes/unlinked.html" "notes/quadratic-equations.html" "text/index.txt" "text/algebra.txt"
              "text/limits.txt" "text/unlinked.txt" "text/quadratic-equations.txt")
            (build-report-affected-artifacts changed-links))
          (test-assert "new backlinks appear in the target page"
            (and (string-contains
                   (read-test-file
                     (string-append cache-output-directory
                                    "/notes/unlinked.html"))
                   "data-tree-id=\"algebra\"")
                 (string-contains
                   (read-test-file
                     (string-append cache-output-directory
                                    "/notes/unlinked.html"))
                   "href=\"/index.html#ref-2\"")
                 (string-contains
                   (read-test-file
                     (string-append cache-output-directory
                                    "/notes/unlinked.html"))
                   "<span>Algebra</span>")))
          (test-assert "text target updates new backlink content"
            (string-contains
              (read-test-file
                (string-append cache-output-directory "/text/unlinked.txt"))
              "Algebra"))
          (test-assert "changed tree title updates its display"
            (string-contains
              (read-test-file
                (string-append cache-output-directory "/index.html"))
              "<h2 class=\"forest-tree-title\"><span>Polynomial equations</span>"))
          (test-assert "tree references resolve in cached builds"
            (string-contains
              (read-test-file
                (string-append cache-output-directory "/notes/limits.html"))
              "href=\"/notes/quadratic-equations.html\">about polynomials</a>")))
        (write-test-file
          (string-append module-directory "/commands.plant")
          (string-append
            "(define-module (tests unit forester linked commands)\n"
            "  #:use-module (forester commands)\n"
            "  #:re-export (link title))\n"
            ";; changed module source\n"))
        (let ((changed-module (build-project/report incremental-project)))
          (test-equal "shared module changes are tracked as build inputs"
            (list (string-append module-directory "/commands.plant"))
            (build-report-modified-sources changed-module))
          (test-equal "shared module changes affect all rendered pages"
            '("index.html" "notes/limits.html"
              "notes/orphan.html" "notes/unicode-accent.html"
              "notes/unicode.html" "notes/unlinked.html"
              "notes/quadratic-equations.html" "text/index.txt"
              "text/algebra.txt" "text/limits.txt" "text/orphan.txt"
              "text/unicode-accent.txt" "text/unicode.txt"
              "text/unlinked.txt" "text/quadratic-equations.txt")
            (build-report-affected-artifacts changed-module)))
        (delete-file (string-append source-directory "/orphan.plant"))
        (let ((removed-page (build-project/report incremental-project)))
          (test-equal "removing a page affects indexes and its own outputs"
            '("text/index.txt" "notes/orphan.html"
              "text/orphan.txt")
            (build-report-affected-artifacts removed-page))
          (test-assert "removed page output is cleaned from HTML and text targets"
            (and (not (file-exists?
                       (string-append cache-output-directory
                                      "/notes/orphan.html")))
                 (not (file-exists?
                       (string-append cache-output-directory
                                      "/text/orphan.txt"))))))
      (let* ((presentation-output
               (string-append project-root "/presentation-cache-output"))
             (routes
               (make-html-routes
                 #:home-page "algebra"
                 #:page-path
                 (lambda (id) (string-append "notes/" id ".html"))))
             (make-presentation-project
               (lambda (decoration)
                 (make-site-project
                     #:title "Linked Notes"
                     #:source-directory source-directory
                     #:prepare (make-scribble-module-setup module-directory)
                     #:read-page
                     (make-page-reader (make-scribble-document-reader))
                     #:analyze make-forest
                     #:renderers
                     (list
                       (make-html-renderer
                         #:options
                         (make-html-options
                           #:routes routes
                           #:mathjax-src #f
                           #:presentation
                           (make-html-presentation
                               #:title-decorations
                               `(("algebra" . ((span ,decoration))))))))
                     #:output-directory presentation-output))))
        (let ((first (build-project/report
                       (make-presentation-project "Title decoration first"))))
          (test-assert "initial title decoration is rendered"
            (and (build-report-rendered? first)
                 (string-contains
                   (read-test-file
                     (string-append presentation-output "/index.html"))
                   "Title decoration first"))))
        (let ((same (build-project/report
                      (make-presentation-project "Title decoration first"))))
          (test-assert "identical title decoration reuses cached output"
            (not (build-report-rendered? same))))
        (let ((changed (build-project/report
                         (make-presentation-project "Title decoration changed"))))
          (test-assert "changed title decoration invalidates cached output"
            (and (build-report-rendered? changed)
                 (string-contains
                   (read-test-file
                     (string-append presentation-output "/index.html"))
                   "Title decoration changed")))))
      (write-test-file
        (string-append source-directory "/broken.plant")
        (string-append
          "@use-modules[(tests unit forester linked commands)]\n"
          "@title{Broken}\n@link[\"missing\"]{missing}"))
      (test-error "project analysis rejects unresolved links before output"
        #t
        (build-project project)))))
  (lambda () (system* "rm" "-rf" project-root)))

(let ((root (mkdtemp "/tmp/forester-derived-project-test-XXXXXX")))
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (let* ((routes (make-html-routes #:home-page "root"))
             (page (make-site-page
                     "root" "Root"
                     (make-content-document
                       (list (make-content-element
                               'paragraph (list "Hello."))))))
             (make-derived-project
               (lambda (settings)
                 (make-site-project
                     #:title "Derived"
                     #:collect-sources (lambda () '("virtual.plant"))
                     #:read-page (lambda (_source) page)
                     #:analyze make-forest
                     #:renderers
                     (list (make-sitemap-renderer #:settings settings #:routes routes)
                           (make-rss-renderer #:settings settings #:routes routes)
                           (make-graph-renderer))
                     #:output-directory (string-append root "/output"))))
             (settings
               (make-site-settings
                 #:site-url "https://derived.example"
                 #:default-locale "en"))
             (project (make-derived-project settings))
             (initial-report (build-project/report project))
             (artifacts (build-report-artifacts initial-report))
             (sitemap (find (lambda (artifact)
                              (string=? (artifact-path artifact)
                                        "sitemap.xml"))
                            artifacts)))
        (test-equal "project runs output renderers"
          '("sitemap.xml" "feed.xml" "forest-graph.scm")
          (map artifact-path artifacts))
        (test-assert "projection renderers use shared settings"
          (string-contains (artifact-content sitemap)
                           "https://derived.example"))
        (let* ((updated-settings
                (make-site-settings
                  #:site-url "https://updated-derived.example"
                  #:default-locale "zh-Hans"))
               (updated-project (make-derived-project updated-settings))
               (updated-report (build-project/report updated-project))
               (affected (build-report-affected-artifacts updated-report))
               (updated-sitemap
                (find (lambda (artifact)
                        (string=? (artifact-path artifact) "sitemap.xml"))
                      (build-report-artifacts updated-report)))
               (updated-feed
                (read-test-file (string-append root "/output/feed.xml"))))
          (test-assert "changed settings invalidate projection renderers"
            (and (member "sitemap.xml" affected string=?)
                 (member "feed.xml" affected string=?)))
          (test-assert "updated sitemap uses its changed settings"
            (string-contains (artifact-content updated-sitemap)
                             "https://updated-derived.example"))
          (test-assert "updated feed uses its changed settings"
            (and (string-contains updated-feed
                                  "https://updated-derived.example")
                 (string-contains updated-feed
                                  "<language>zh-Hans</language>"))))))
    (lambda () (system* "rm" "-rf" root))))

(let ((root (mkdtemp "/tmp/forester-pipeline-project-test-XXXXXX")))
  (dynamic-wind
    (lambda ()
      (mkdir (string-append root "/plants"))
      (write-test-file
        (string-append root "/plants/hello.plant")
        "@use-modules[(forester commands)]\n@title{Hello}\n")
      (write-test-file
        (string-append root "/plants/topic.plant")
        "@use-modules[(forester commands)]\n@title{Topic}\n")
      (write-test-file (string-append root "/input.txt") "external result"))
    (lambda ()
      (let* ((input (string-append root "/input.txt"))
             (output (string-append root "/output"))
             (text-output (string-append root "/text-output"))
             (transform-output (string-append root "/transform-output"))
             (settings (make-site-settings #:site-url "https://notes.example"))
             (routes
               (make-html-routes
                 #:home-page "hello"
                 #:page-path
                 (lambda (id)
                   (if (string=? id "hello")
                       "index.html"
                       (string-append "notes/" id ".html")))))
             (html-options (make-html-options #:routes routes #:settings settings))
             (pipeline
               (make-asset-pipeline
                 #:name "forester-pipeline"
                 #:inputs (list input)
                 #:procedure
                 (lambda (in emit)
                   (emit "assets/result.txt"
                         (call-with-input-file (in input) get-string-all)))))
             (project
               (make-forester-project
                 #:home "hello"
                 #:source-directory (string-append root "/plants")
                 #:output-directory output
                 #:pipelines (list pipeline)
                 #:additional-renderers (list (make-graph-renderer))))
             (text-project
               (make-forester-project
                 #:home "hello"
                 #:source-directory (string-append root "/plants")
                 #:output-directory text-output
                 #:html-options html-options
                 #:additional-renderers
                 (list (make-text-renderer)
                       (make-sitemap-renderer #:settings settings #:routes routes))))
             (transform-project
               (make-forester-project
                 #:home "hello"
                 #:source-directory (string-append root "/plants")
                 #:output-directory transform-output
                 #:transforms
                 (list
                   (lambda (document)
                     (make-content-document
                       (append
                         (content-document-children document)
                         (list (make-content-element
                                 'paragraph
                                 '("Added before Forester analysis."))))
                       (content-document-source document))))))
             (text-artifacts (build-project text-project))
             (artifacts (build-project project))
             (_ (build-project transform-project))
             (transformed-html
               (read-test-file (string-append transform-output "/index.html")))
             (text-html
               (read-test-file (string-append text-output "/index.html")))
             (topic-html
               (read-test-file (string-append text-output "/notes/topic.html")))
             (sitemap
               (read-test-file (string-append text-output "/sitemap.xml"))))
        (test-assert "Forester projects can compose external asset pipelines"
          (and (member "assets/result.txt"
                       (map artifact-path artifacts)
                       string=?)
               (string=? "external result"
                 (read-test-file (string-append output "/assets/result.txt")))))
        (test-assert "Forester projects add peer renderers and keep default HTML"
          (and (member "index.html" (map artifact-path artifacts) string=?)
               (member "forest-graph.scm"
                       (map artifact-path artifacts)
                       string=?)))
        (test-assert "Forester projects compose HTML and text renderers"
          (and (member "index.html" (map artifact-path text-artifacts) string=?)
               (member "hello.txt"
                       (map artifact-path text-artifacts)
                       string=?)
               (member "notes/topic.html"
                       (map artifact-path text-artifacts)
                       string=?)))
        (test-assert "Forester projects transform content and keep standard HTML"
          (string-contains transformed-html
                           "Added before Forester analysis."))
        (test-assert "Forester HTML options configure the default renderer"
          (and (string-contains text-html "Hello")
               (string-contains topic-html "Topic")
               (string-contains sitemap
                                "https://notes.example/notes/topic.html")))))
    (lambda () (system* "rm" "-rf" root))))

(test-end "forester project")

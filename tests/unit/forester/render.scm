;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester render)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (forester render html)
  #:use-module (forester render rss)
  #:use-module (forester render sitemap)
  #:use-module (forester render text)
  #:use-module (forester render text routes)
  #:use-module (forester site)
  #:use-module (grove build)
  #:use-module (grove ir)
  #:use-module (grove render html)
  #:use-module (grove render html routes)
  #:use-module (grove output)
  #:use-module (grove site)
  #:use-module (grove style html)
  #:use-module (tests unit forester fixtures))

(test-begin "forester renderers")
(let* ((paragraph-node
         (car (content-document-children (site-page-document algebra))))
       (link-node (cadr (content-element-children paragraph-node)))
       (math-node (list-ref (content-element-children paragraph-node) 3)))
  (test-assert "links use the generic element container"
    (and (content-element? link-node)
         (eq? (content-element-kind link-node) 'link)
         (equal? (content-element-property link-node 'target) "limits")))
  (test-assert "math uses the generic element container"
    (and (content-element? math-node)
         (eq? (content-element-kind math-node) 'math))))

(let* ((rendered (render-test-html (make-forest (make-site (list algebra limits)))))
       (algebra-html (assoc-ref rendered "algebra"))
       (limits-html (assoc-ref rendered "limits")))
  (test-assert "at-exp link resolves to a page"
    (string-contains algebra-html "href=\"/limits.html\">limits</a>"))
  (test-assert "links produce reverse links"
    (and (string-contains algebra-html "class=\"backmatter backlinks\"")
         (string-contains algebra-html
                          "href=\"/limits.html#ref-1\">Limits</a>")))
  (test-assert "reverse links point back to their source"
    (string-contains limits-html
                     "href=\"/algebra.html#ref-1\">Algebra &amp; limits</a>"))
  (test-assert "backlinks have a semantic heading"
    (string-contains limits-html "<h2>Backlinks</h2>"))
  (test-assert "backmatter labels can be customized in HTML and text"
    (and (string-contains
           (assoc-ref (render-test-html
                        (make-forest (make-site (list algebra limits)))
                        #:backmatter-labels '((backlinks . "Link sources")))
                      "limits")
           "<h2>Link sources</h2>")
         (string-contains
           (assoc-ref (site->text
                        (make-forest (make-site (list algebra limits)))
                        #:backmatter-labels '((backlinks . "Link sources")))
                      "limits")
           "Link sources:")))
  (test-assert "inline math keeps TeX delimiters"
    (string-contains algebra-html "\\(x^2+y^2=z^2\\)"))
  (test-assert "display math keeps TeX delimiters"
    (string-contains algebra-html "\\[a^2+b^2=c^2\\]"))
  (test-assert "images use route-aware URLs and alternative text"
    (string-contains algebra-html
                     "<img src=\"/images/algebra.svg\" alt=\"An algebra diagram.\" />"))
  (test-assert "plain text retains image alternative text"
    (string-contains
      (assoc-ref (site->text
                   (make-forest (make-site (list algebra limits))))
                 "algebra")
      "An algebra diagram."))
  (test-assert "HTML loads MathJax"
    (string-contains algebra-html "mathjax@4/tex-chtml.js"))
  (test-assert "HTML defers MathJax until the document is ready"
    (string-contains algebra-html "defer=\"defer\""))
  (test-assert "MathJax script has an explicit closing tag"
    (string-contains algebra-html "</script>"))
  (test-assert "HTML text is escaped"
    (string-contains algebra-html "Algebra &amp; limits")))

(test-assert "HTML pages declare standards mode"
  (string-prefix? "<!DOCTYPE html>" (assoc-ref
                                       (render-test-html
                                         (make-forest
                                           (make-site (list algebra limits))))
                                       "algebra")))

(let* ((page
         (make-site-page
           "maxwell"
           "Maxwell equations"
           (evaluate-document
             "@notation:display-math{@notation:align{∇ · @notation:vec{D} &= ρ\n∇ × @notation:vec{E} &= −@notation:frac{∂@notation:vec{B}}{∂t}}}")))
       (html (assoc-ref (render-test-html
                          (make-forest (make-site (list page))))
                        "maxwell")))
  (test-assert "MathML Core pages render without loading MathJax"
    (and (string-contains html "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"")
         (string-contains html "<mtable>")
         (not (string-contains html "mathjax")))))

(let* ((target-document
         (make-content-document
           (list
             (make-content-element 'heading '("Overview"))
             (make-content-element 'heading '("Overview"))
             (make-content-element 'heading '("About: Trees?"))
             (make-content-element 'heading '("!!!"))
             (make-content-element 'heading '("Overview-2")))))
       (source-document
         (make-content-document
           (list
             (make-content-element
               'paragraph
               (list (make-content-element
                       'link '("second section")
                       #:properties '((target . "target#overview-2")))))
             (make-content-element
               'paragraph
               (list (make-content-element
                       'link '("the trees")
                       #:properties '((target . "target#about-trees"))))))))
       (target (make-site-page "target" "Target" target-document))
       (source (make-site-page "source" "Source" source-document))
       (forest (make-forest (make-site (list target source))))
       (html (render-test-html forest))
       (text (site->text forest))
       (target-html (assoc-ref html "target"))
       (target-text (assoc-ref text "target"))
       (source-html (assoc-ref html "source"))
       (source-text (assoc-ref text "source"))
       (outgoing (outgoing-references forest "source"))
       (first-reference (car outgoing))
       (section-references
         (incoming-references forest "target" "overview-2")))
  (test-equal "heading anchors slugify text and disambiguate duplicate IDs"
    '("overview" "overview-2" "about-trees" "section" "overview-2-2")
    (heading-ids forest "target"))
  (test-assert "HTML headings emit their indexed anchor IDs"
    (and (string-contains target-html "<h2 id=\"overview\">Overview</h2>")
         (string-contains target-html
                          "<h2 id=\"overview-2\">Overview</h2>")
         (string-contains target-html
                          "<h2 id=\"overview-2-2\">Overview-2</h2>")))
  (test-assert "HTML pages include a table of contents linked to heading IDs"
    (and (string-contains target-html
                          "class=\"table-of-contents\" aria-label=\"Contents\"")
         (string-contains target-html "href=\"#overview-2\">Overview</a>")))
  (test-assert "text pages include a heading directory and matching markers"
    (and (string-contains target-text "Contents")
         (string-contains target-text "- Overview [#overview-2]")
         (string-contains target-text "[#overview-2]")))
  (test-assert "HTML page links preserve fragment targets"
    (and (string-contains source-html
                          "href=\"/target.html#overview-2\">second section</a>")
         (string-contains source-html
                          "href=\"/target.html#about-trees\">the trees</a>")))
  (test-assert "plain-text page links preserve fragment targets"
    (and (string-contains source-text "target.txt#overview-2")
         (string-contains source-text "target.txt#about-trees")))
  (test-assert "fragment links produce section-aware backlinks"
    (and (string-contains target-html "href=\"/source.html#ref-1\">Source</a>")
         (string-contains target-html "href=\"#overview-2\">Overview</a>")
         (not (string-contains target-html "This page"))
         (not (string-contains target-text "This page"))))
  (test-assert "Forest references preserve source, target, label and fragment"
    (and (reference? first-reference)
         (string=? (reference-source-page-id first-reference) "source")
         (string=? (reference-source-id first-reference) "ref-1")
         (string=? (reference-target-page-id first-reference) "target")
         (string=? (reference-target-fragment first-reference)
                   "overview-2")
         (string=? (reference-label first-reference)
                   "second section")))
  (test-equal "incoming-reference queries can select one heading"
    '("second section")
    (map reference-label section-references))
  (test-equal "legacy link queries retain page and fragment targets"
    '("target#overview-2" "target#about-trees")
    (links forest "source")))

(test-error "links to missing heading fragments fail forest analysis" #t
  (make-forest
    (make-site
      (list
        (make-site-page "target" "Target"
          (make-content-document
            (list (make-content-element 'heading '("Existing")))))
        (make-site-page "source" "Source"
          (make-content-document
            (list (make-content-element
                    'link '("missing")
                    #:properties '((target . "target#not-here"))))))))))

(test-error "empty page fragments are rejected" #t
  (make-forest
    (make-site
      (list
        (make-site-page "target" "Target" (make-content-document '()))
        (make-site-page "source" "Source"
          (make-content-document
            (list (make-content-element
                    'link '("target")
                    #:properties '((target . "target#"))))))))))

(let* ((forest (make-forest (make-site (list algebra limits)
                                       #:title "Linked Notes"
                                       #:description "Connected notes on math.")))
       (artifacts
         (render-model
           forest
           (list (make-html-renderer
                   #:routes (make-html-routes #:home-page "algebra"))
                 (make-rss-renderer
                   #:site-url "https://notes.example/"
                   #:routes (make-html-routes
                              #:home-page "algebra"
                              #:page-path
                              (lambda (id)
                                (string-append "notes/" id ".html"))))
                 (lambda (value)
                   (prefix-artifacts "text" (render-text value))))))
       (paths (map artifact-path artifacts))
       (limits-html (artifact-content
                      (find (lambda (artifact)
                              (string=? (artifact-path artifact) "limits.html"))
                            artifacts)))
       (feed (artifact-content
               (find (lambda (artifact)
                       (string=? (artifact-path artifact) "feed.xml"))
                     artifacts)))
       (algebra-html (artifact-content
                       (find (lambda (artifact)
                               (string=? (artifact-path artifact)
                                         "algebra.html"))
                             artifacts)))
       (index-html (artifact-content
                     (find (lambda (artifact)
                             (string=? (artifact-path artifact) "index.html"))
                           artifacts)))
       (algebra-text (artifact-content
                       (find (lambda (artifact)
                               (string=? (artifact-path artifact)
                                         "text/algebra.txt"))
                             artifacts))))
  (test-assert "HTML, text, and RSS renderers compose into one artifact set"
    (and (member "index.html" paths)
         (member "algebra.html" paths)
         (member "forester.css" paths)
         (member "feed.xml" paths)
         (member "text/index.txt" paths)
         (member "text/algebra.txt" paths)))
  (test-assert "HTML pages link the external Forester stylesheet"
    (and (string-contains algebra-html "href=\"/forester.css\"")
         (string-contains
           (artifact-content
             (find (lambda (artifact)
                     (string=? (artifact-path artifact) "forester.css"))
                   artifacts))
           ".forest-tree-block:hover")
         (string-contains
           (artifact-content
             (find (lambda (artifact)
                     (string=? (artifact-path artifact) "forester.css"))
                   artifacts))
           ".backmatter { margin-top:")))
  (test-assert "user stylesheets follow the Forester defaults"
    (let* ((custom
             (make-artifact "custom.css" "body { color: black; }"))
           (styled-artifacts
             (render-html
               forest
               #:routes (make-html-routes #:home-page "algebra")
               #:style (make-html-style #:stylesheets (list custom))))
           (stylesheet-paths (map artifact-path (take styled-artifacts 2)))
           (styled-index
             (artifact-content
               (find (lambda (artifact)
                       (string=? (artifact-path artifact) "index.html"))
                     styled-artifacts)))
           (styled-algebra
             (artifact-content
               (find (lambda (artifact)
                       (string=? (artifact-path artifact) "algebra.html"))
                     styled-artifacts)))
           (base-position (string-contains styled-algebra "forester.css"))
           (custom-position (string-contains styled-algebra "custom.css")))
      (and (equal? stylesheet-paths '("forester.css" "custom.css"))
           base-position
           custom-position
           (< base-position custom-position))))
  (test-assert "RSS emits escaped metadata and absolute routed links"
    (and (string-contains feed "<title>Algebra &amp; limits</title>")
         (string-contains feed
           "https://notes.example/notes/algebra.html")
         (string-contains feed "&amp;")
         (string-contains feed
           "<description>Connected notes on math.</description>")))
  (test-assert "HTML index redirects to the configured home page"
    (string-contains index-html "content=\"0;url=/algebra.html\""))
  (test-assert "HTML pages prefer their own summary"
    (string-contains algebra-html
      "<meta name=\"description\" content=\"Algebra notes and mathematical ideas.\""))
  (test-assert "HTML pages fall back to the site description"
    (string-contains limits-html
      "<meta name=\"description\" content=\"Connected notes on math.\""))
  (test-assert "HTML page titles expose their stable tree ID"
    (and (string-contains algebra-html "[algebra]</a>")
         (string-contains algebra-html "href=\"/algebra.html\"")))
  (test-assert "HTML renderer produces standalone HTML artifacts"
    (string-contains algebra-html "<html>"))
  (test-assert "text renderer produces independent text artifacts"
    (string-contains algebra-text "are useful")))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (routes (make-html-routes #:home-page "algebra"))
       (artifacts (render-html forest #:routes routes))
       (index (artifact-content
                (find (lambda (artifact)
                        (string=? (artifact-path artifact) "index.html"))
                      artifacts))))
  (test-assert "configured HTML home page replaces the generated page list"
    (and (string-contains index "http-equiv=\"refresh\"")
         (string-contains index "content=\"0;url=/algebra.html\"")
         (not (string-contains index "href=\"/limits.html\""))))
  (test-error "configured HTML home page must exist" #t
    (render-html forest #:routes (make-html-routes #:home-page "missing"))))

(let* ((html (render-test-html (make-forest (make-site (list algebra limits)))))
       (limits-html (assoc-ref html "limits")))
  (test-assert "pages without mathematics do not load MathJax"
    (not (string-contains limits-html "mathjax"))))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (html (site->html forest
               #:routes (make-html-routes
                          #:home-page "algebra"
                          #:page-path
                          (lambda (id)
                            (string-append "notes/" id ".html")))
               #:mathjax-src "assets/mathjax.js"))
       (algebra-html (assoc-ref html "algebra")))
  (test-assert "projects can provide a local MathJax script"
    (string-contains algebra-html "src=\"/assets/mathjax.js\"")))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (html (render-test-html forest #:mathjax-src #f)))
  (test-assert "projects can disable browser-side math rendering"
    (not (string-contains (assoc-ref html "algebra") "mathjax"))))

(let* ((page
         (make-site-page
           "preview" "Preview"
           (make-content-document
             (list
               (make-content-element 'heading '("Details"))
               (make-content-element
                 'paragraph
                 (list "Run " (make-content-element 'code '("x + 1")) "."))
               (make-content-element 'code-block '("(display \"hi\")"))))))
       (feed
         (artifact-content
           (car (render-rss
                  (make-forest (make-site (list page)))
                  #:site-url "https://notes.example")))))
  (test-assert "RSS fallback descriptions omit text-target markup"
    (and (string-contains feed "Details")
         (string-contains feed "Run x + 1.")
         (string-contains feed "(display")
         (not (string-contains feed "```"))
         (not (string-contains feed "=======\n"))
         (not (string-contains feed "`x + 1`")))))

(let* ((forest (make-forest (make-site (list algebra limits)
                                       #:title "Linked Notes")))
       (routes (make-html-routes
                 #:home-page "algebra"
                 #:page-path
                 (lambda (id) (string-append "notes/" id ".html"))
                 #:url-prefix "/published/"))
       (feed (artifact-content
               (car (render-rss
                      forest #:site-url "https://notes.example" #:routes routes))))
       (sitemap (artifact-content
                  (car (render-sitemap
                         forest #:site-url "https://notes.example"
                         #:routes routes)))))
  (test-assert "RSS absolute links include the HTML URL prefix"
    (and (string-contains
           feed "https://notes.example/published/notes/algebra.html")
         (string-contains
           feed "https://notes.example/published/index.html")))
  (test-assert "sitemap absolute links include the HTML URL prefix"
    (and (string-contains
           sitemap "https://notes.example/published/notes/algebra.html")
         (string-contains
           sitemap "https://notes.example/published/index.html"))))

(let* ((pages
         (list
           (make-site-page "older" "Older" (make-content-document '())
                           #:metadata '((published . "2024-01-01")))
           (make-site-page "undated" "Undated" (make-content-document '()))
           (make-site-page "newer" "Newer" (make-content-document '())
                           #:metadata '((published . "2025-01-01")))))
       (feed
         (artifact-content
           (car (render-rss
                  (make-forest (make-site pages))
                  #:site-url "https://notes.example"))))
       (newer-position (string-contains feed "<title>Newer</title>"))
       (older-position (string-contains feed "<title>Older</title>"))
       (undated-position (string-contains feed "<title>Undated</title>")))
  (test-assert "RSS lists dated pages newest first and undated pages last"
    (and newer-position older-position undated-position
         (< newer-position older-position)
         (< older-position undated-position))))

(let* ((forest
         (make-forest
           (make-site
             (list (make-site-page
                     "a&b" "A and B" (make-content-document '())
                     "notes/a&b.plant"
                     #:metadata '((updated . "2026-09-27")))
                   (make-site-page
                     "plain" "Plain" (make-content-document '())
                     "notes/plain.plant"))
             #:sources '("notes/a&b.plant" "notes/plain.plant"))))
       (artifact
         (car (render-sitemap
                forest
                #:site-url "https://notes.example/"
                #:sitemap-path "meta/sitemap.xml"
                #:routes
                (make-html-routes
                  #:home-page "a&b"
                  #:page-path
                  (lambda (id) (string-append "notes/" id ".html"))
                  #:index-path "home/index.html"))))
       (content (artifact-content artifact)))
  (test-equal "sitemap renderer supports a configured artifact path"
    "meta/sitemap.xml"
    (artifact-path artifact))
  (test-equal "sitemap depends on every site source"
    '("notes/a&b.plant" "notes/plain.plant")
    (artifact-sources artifact))
  (test-assert "sitemap includes the index and routed page URLs"
    (and (string-contains
           content
           "<loc>https://notes.example/home/index.html</loc>")
         (string-contains
           content
           (string-append
             "<loc>https://notes.example/notes/a&amp;b.html</loc>\n"
             "      <lastmod>2026-09-27</lastmod>"))
         (string-contains
           content
           "<loc>https://notes.example/notes/plain.html</loc>\n    </url>")))
  (test-error "sitemap rejects invalid updated dates" #t
    (render-sitemap
      (make-forest
        (make-site
          (list (make-site-page
                  "invalid" "Invalid" (make-content-document '())
                  #:metadata '((updated . "2026-02-31"))))))
      #:site-url "https://notes.example")))

(let* ((forest
         (make-forest
           (make-site
             (list
               (make-site-page
                 "alpha" "Alpha"
                 (make-content-document
                   (list
                     (make-content-element
                       'paragraph
                       (list "See "
                             (make-content-element
                               'link '("Beta")
                               #:properties '((target . "beta")))
                             "."))))
                 "alpha.plant")
               (make-site-page "beta" "Beta" (make-content-document '())
                               "beta.plant")))))
       (routes
         (make-text-routes
           #:index-path "index.txt"
           #:page-path
           (lambda (id) (string-append "notes/" id ".md"))))
       (artifacts (render-text forest #:routes routes))
       (index (artifact-content (car artifacts)))
       (alpha (artifact-content (cadr artifacts))))
  (test-equal "text routes configure the index and page output paths"
    '("index.txt" "notes/alpha.md" "notes/beta.md")
    (map artifact-path artifacts))
  (test-assert "text links resolve relative to the source page route"
    (string-contains alpha "Beta [beta.md]"))
  (test-assert "nested text pages link back to the configured index"
    (string-contains alpha "Home: ../index.txt"))
  (test-assert "text index references configured page routes"
    (string-contains index "Alpha (notes/alpha.md)")))


(test-end "forester renderers")

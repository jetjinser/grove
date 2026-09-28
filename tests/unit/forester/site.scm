;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester site)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (system base language)
  #:use-module (language scribble spec)
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
  #:use-module (grove site))

(define environment (default-environment scribble))
(eval '(use-modules (forester commands)) environment)

(define (evaluate-document source)
  (eval ((language-reader scribble)
          (open-input-string source)
          environment)
        environment))

(define algebra
  (make-site-page
    "algebra"
    "Algebra & limits"
    (evaluate-document
      "@paragraph{The @link[\"limits\"]{limits} are useful. @math{x^2+y^2=z^2}}\n@display-math{a^2+b^2=c^2}")
    #:metadata '((summary . "Algebra notes and mathematical ideas."))))

(define limits
  (make-site-page
    "limits"
    "Limits"
    (evaluate-document
      "@paragraph{See @link[\"algebra\"]{the algebra note}.}")))

(define (page-with-links id targets)
  (make-site-page
    id id
    (make-content-document
      (map (lambda (target)
             (make-content-element
               'link (list target)
               #:properties `((target . ,target))))
           targets))))

(define (backlinks-by-incoming-references forest id)
  (let ((source-ids
          (delete-duplicates
            (map forest-reference-source-page-id
                 (forest-incoming-references forest id))
            equal?)))
    (filter (lambda (page)
              (member (site-page-id page) source-ids equal?))
            (site-pages (forest-site forest)))))

(define (common-backlinks-by-incoming-references forest left-id right-id)
  (let ((left (map site-page-id
                   (backlinks-by-incoming-references forest left-id)))
        (right (map site-page-id
                    (backlinks-by-incoming-references forest right-id))))
    (filter (lambda (page)
              (and (member (site-page-id page) left equal?)
                   (member (site-page-id page) right equal?)))
            (site-pages (forest-site forest)))))

(test-begin "forester site")

(let* ((destination (page-with-links "destination" '()))
       (source (page-with-links "source" '("destination" "destination")))
       (other-source (page-with-links "other-source" '("destination")))
       (unlinked (page-with-links "unlinked" '()))
       (forest
         (make-forest
           (make-site (list destination source other-source unlinked))))
       (backlinks (forest-backlinks forest "destination")))
  (test-equal "relation backlinks preserve the old page ordering and uniqueness"
    '("source" "other-source")
    (map site-page-id backlinks))
  (test-equal "relation backlinks match the previous incoming-reference filter"
    (map site-page-id (backlinks-by-incoming-references forest "destination"))
    (map site-page-id backlinks))
  (test-equal "incoming-reference queries still preserve each link occurrence"
    3
    (length (forest-incoming-references forest "destination"))))

(let* ((left (page-with-links "left" '()))
       (right (page-with-links "right" '()))
       (shared-late (page-with-links "shared-late" '("left" "right")))
       (left-only (page-with-links "left-only" '("left")))
       (shared-early (page-with-links "shared-early"
                                      '("left" "left" "right")))
       (right-only (page-with-links "right-only" '("right")))
       (forest
         (make-forest
           (make-site
             (list left shared-late left-only right shared-early right-only))))
       (common (forest-common-backlinks forest "left" "right")))
  (test-equal "relational joins find pages linking to both targets in site order"
    '("shared-late" "shared-early")
    (map site-page-id common))
  (test-equal "common backlinks match the incoming-reference intersection"
    (map site-page-id
         (common-backlinks-by-incoming-references forest "left" "right"))
    (map site-page-id common))
  (test-equal "joining a target to itself gives its unique backlink pages"
    (map site-page-id (forest-backlinks forest "left"))
    (map site-page-id (forest-common-backlinks forest "left" "left"))))

(define (bfs-reachable-page-ids forest source-id)
  (let visit ((pending (list source-id)) (visited '()))
    (if (null? pending)
        (map site-page-id
             (filter (lambda (page)
                       (and (not (equal? (site-page-id page) source-id))
                            (member (site-page-id page) visited equal?)))
                     (site-pages (forest-site forest))))
        (let ((current (car pending)))
          (if (member current visited equal?)
              (visit (cdr pending) visited)
              (visit
                (append
                  (cdr pending)
                  (map forest-reference-target-page-id
                       (forest-outgoing-references forest current)))
                (cons current visited)))))))

(let* ((isolated (page-with-links "isolated" '()))
       (hub (page-with-links "hub" '("left" "right")))
       (left (page-with-links "left" '("merge")))
       (right (page-with-links "right" '("merge")))
       (merge (page-with-links "merge" '("hub")))
       (forest (make-forest (make-site (list isolated hub left right merge)))))
  (test-equal "reachability includes the transitive closure once in site order"
    '("left" "right" "merge")
    (map site-page-id (forest-reachable-pages forest "hub")))
  (test-equal "reachability excludes the source even when a cycle returns to it"
    '("hub" "left" "right")
    (map site-page-id (forest-reachable-pages forest "merge")))
  (test-equal "isolated pages have no reachable pages"
    '()
    (forest-reachable-pages forest "isolated"))
  (test-equal "relation reachability agrees with breadth-first traversal"
    (map (lambda (page)
           (cons (site-page-id page)
                 (bfs-reachable-page-ids forest (site-page-id page))))
         (site-pages (forest-site forest)))
    (map (lambda (page)
           (cons (site-page-id page)
                 (map site-page-id
                      (forest-reachable-pages forest (site-page-id page)))))
         (site-pages (forest-site forest)))))

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

(let* ((rendered (forest->html (make-forest (make-site (list algebra limits)))))
       (algebra-html (assoc-ref rendered "algebra"))
       (limits-html (assoc-ref rendered "limits")))
  (test-assert "at-exp link resolves to a page"
    (string-contains algebra-html "href=\"/limits.html\">limits</a>"))
  (test-assert "links produce reverse links"
    (and (string-contains algebra-html "class=\"backlinks\"")
         (string-contains algebra-html
                          "href=\"/limits.html#ref-1\">Limits</a>")))
  (test-assert "reverse links point back to their source"
    (string-contains limits-html
                     "href=\"/algebra.html#ref-1\">Algebra &amp; limits</a>"))
  (test-assert "inline math keeps TeX delimiters"
    (string-contains algebra-html "\\(x^2+y^2=z^2\\)"))
  (test-assert "display math keeps TeX delimiters"
    (string-contains algebra-html "\\[a^2+b^2=c^2\\]"))
  (test-assert "HTML loads MathJax"
    (string-contains algebra-html "mathjax@4/tex-chtml.js"))
  (test-assert "HTML defers MathJax until the document is ready"
    (string-contains algebra-html "defer=\"defer\""))
  (test-assert "MathJax script has an explicit closing tag"
    (string-contains algebra-html "</script>"))
  (test-assert "HTML text is escaped"
    (string-contains algebra-html "Algebra &amp; limits")))

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
       (html (forest->html forest))
       (text (forest->text forest))
       (target-html (assoc-ref html "target"))
       (target-text (assoc-ref text "target"))
       (source-html (assoc-ref html "source"))
       (source-text (assoc-ref text "source"))
       (outgoing (forest-outgoing-references forest "source"))
       (first-reference (car outgoing))
       (section-references
         (forest-incoming-references forest "target" "overview-2")))
  (test-equal "heading anchors slugify text and disambiguate duplicate IDs"
    '("overview" "overview-2" "about-trees" "section" "overview-2-2")
    (forest-heading-ids forest "target"))
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
         (string-contains target-html "href=\"#overview-2\">Overview</a>")))
  (test-assert "Forest references preserve source, target, label and fragment"
    (and (forest-reference? first-reference)
         (string=? (forest-reference-source-page-id first-reference) "source")
         (string=? (forest-reference-source-id first-reference) "ref-1")
         (string=? (forest-reference-target-page-id first-reference) "target")
         (string=? (forest-reference-target-fragment first-reference)
                   "overview-2")
         (string=? (forest-reference-label first-reference)
                   "second section")))
  (test-equal "incoming-reference queries can select one heading"
    '("second section")
    (map forest-reference-label section-references))
  (test-equal "legacy link queries retain page and fragment targets"
    '("target#overview-2" "target#about-trees")
    (forest-links forest "source")))

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
           (list (make-forester-html-renderer)
                 (make-forester-rss-renderer
                   #:site-url "https://notes.example/"
                   #:routes (make-html-routes
                              #:page-path
                              (lambda (id)
                                (string-append "notes/" id ".html"))))
                 (lambda (value)
                   (prefix-artifacts "text" (render-forest-text value))))))
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
         (member "feed.xml" paths)
         (member "text/index.txt" paths)
         (member "text/algebra.txt" paths)))
  (test-assert "RSS emits escaped metadata and absolute routed links"
    (and (string-contains feed "<title>Algebra &amp; limits</title>")
         (string-contains feed
           "https://notes.example/notes/algebra.html")
         (string-contains feed "&amp;")
         (string-contains feed
           "<description>Connected notes on math.</description>")))
  (test-assert "HTML index includes the site description"
    (string-contains index-html
      "<meta name=\"description\" content=\"Connected notes on math.\""))
  (test-assert "HTML pages prefer their own summary"
    (string-contains algebra-html
      "<meta name=\"description\" content=\"Algebra notes and mathematical ideas.\""))
  (test-assert "HTML pages fall back to the site description"
    (string-contains limits-html
      "<meta name=\"description\" content=\"Connected notes on math.\""))
  (test-assert "HTML renderer produces standalone HTML artifacts"
    (string-contains algebra-html "<html>"))
  (test-assert "text renderer produces independent text artifacts"
    (string-contains algebra-text "are useful")))

(let* ((html (forest->html (make-forest (make-site (list algebra limits)))))
       (limits-html (assoc-ref html "limits")))
  (test-assert "pages without mathematics do not load MathJax"
    (not (string-contains limits-html "mathjax"))))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (html (forest->html forest
               #:routes (make-html-routes
                          #:page-path
                          (lambda (id)
                            (string-append "notes/" id ".html")))
               #:mathjax-src "assets/mathjax.js"))
       (algebra-html (assoc-ref html "algebra")))
  (test-assert "projects can provide a local MathJax script"
    (string-contains algebra-html "src=\"/assets/mathjax.js\"")))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (html (forest->html forest #:mathjax-src #f)))
  (test-assert "projects can disable browser-side math rendering"
    (not (string-contains (assoc-ref html "algebra") "mathjax"))))

(let* ((root
         (make-site-page
           "root" "Root"
           (evaluate-document
             "@subtree[\"inline-1\"]{@title{Inline section}@paragraph{Owned text. @link[\"root\"]{Home}.}}")))
       (forest (make-forest (make-site (list root))))
       (html (forest->html forest #:mathjax-src #f))
       (text (forest->text forest))
       (inline (forest-page forest "inline-1")))
  (test-equal "inline subtrees become independently addressable trees"
    '("root" "inline-1")
    (map site-page-id (forest-pages forest)))
  (test-equal "subtree title is metadata, not body content"
    "Inline section"
    (site-page-title inline))
  (test-equal "subtree body remains available in its standalone document"
    '(paragraph)
    (map content-element-kind
         (content-document-children (site-page-document inline))))
  (test-assert "subtree content is rendered at its definition site"
    (string-contains (assoc-ref html "root")
                     "class=\"inline-subtree\""))
  (test-assert "subtrees receive standalone HTML routes"
    (string-contains (assoc-ref html "inline-1") "Owned text."))
  (test-assert "subtrees receive standalone text routes"
    (string-contains (assoc-ref text "inline-1") "Owned text."))
  (test-equal "references inside a subtree belong to that subtree"
    '("inline-1")
    (map forest-reference-source-page-id
         (forest-incoming-references forest "root"))))

(test-error "subtree IDs cannot collide with file page IDs" #t
  (make-forest
    (make-site
      (list (make-site-page "collision" "File page"
                            (make-content-document '()))
            (make-site-page
              "root" "Root"
              (evaluate-document
                "@subtree[\"collision\"]{@title{Inline}}"))))))

(test-error "unresolved links fail site analysis" #t
  (make-forest
    (make-site
      (list (make-site-page "broken" "Broken"
              (evaluate-document "@paragraph{@link[\"missing\"]{nowhere}}"))))))

(let ((message
        (catch #t
          (lambda ()
            (make-forest
              (make-site
                (list (make-site-page
                        "broken" "Broken"
                        (evaluate-document
                          "@paragraph{@link[\"missing\"]{nowhere}}")
                        "notes/broken.grv"))))
            #f)
          (lambda (key . args) (format #f "~s" (cons key args))))))
  (test-assert "unresolved-link errors identify source page and target"
    (and (string-contains message "notes/broken.grv")
         (string-contains message "broken")
         (string-contains message "missing"))))

(test-error "duplicate page ids fail site analysis" #t
  (make-site
    (list (make-site-page "same" "First" (evaluate-document "first"))
          (make-site-page "same" "Second" (evaluate-document "second")))))

(let ((message
        (catch #t
          (lambda ()
            (make-site
              (list (make-site-page
                      "same" "First" (evaluate-document "first")
                      "trees/math/cat-0001.grv")
                    (make-site-page
                      "same" "Second" (evaluate-document "second")
                      "trees/logic/cat-0001.grv")))
            #f)
          (lambda (key . args) (format #f "~s" (cons key args))))))
  (test-assert "duplicate page id errors identify every source path"
    (and (string-contains message "cat-0001")
         (string-contains message "trees/math/cat-0001.grv")
         (string-contains message "trees/logic/cat-0001.grv"))))

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
           (car (render-forest-rss
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
                 #:page-path
                 (lambda (id) (string-append "notes/" id ".html"))
                 #:url-prefix "/published/"))
       (feed (artifact-content
               (car (render-forest-rss
                      forest #:site-url "https://notes.example" #:routes routes))))
       (sitemap (artifact-content
                  (car (render-forest-sitemap
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
           (car (render-forest-rss
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
                     "notes/a&b.grv"
                     #:metadata '((updated . "2026-09-27")))
                   (make-site-page
                     "plain" "Plain" (make-content-document '())
                     "notes/plain.grv"))
             #:sources '("notes/a&b.grv" "notes/plain.grv"))))
       (artifact
         (car (render-forest-sitemap
                forest
                #:site-url "https://notes.example/"
                #:sitemap-path "meta/sitemap.xml"
                #:routes
                (make-html-routes
                  #:page-path
                  (lambda (id) (string-append "notes/" id ".html"))
                  #:index-path "home/index.html"))))
       (content (artifact-content artifact)))
  (test-equal "sitemap renderer supports a configured artifact path"
    "meta/sitemap.xml"
    (artifact-path artifact))
  (test-equal "sitemap depends on every site source"
    '("notes/a&b.grv" "notes/plain.grv")
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
    (render-forest-sitemap
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
                 "alpha.grv")
               (make-site-page "beta" "Beta" (make-content-document '())
                               "beta.grv")))))
       (routes
         (make-text-routes
           #:index-path "index.txt"
           #:page-path
           (lambda (id) (string-append "notes/" id ".md"))))
       (artifacts (render-forest-text forest #:routes routes))
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

(test-end "forester site")

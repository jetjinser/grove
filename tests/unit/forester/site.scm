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
             "@subtree{@title{Inline section}@paragraph{Owned text. @link[\"root\"]{Home}.}}")))
       (forest (make-forest (make-site (list root))))
       (html (forest->html forest #:mathjax-src #f))
       (text (forest->text forest))
       (rss (artifact-content
              (car (render-forest-rss forest
                                      #:site-url "https://notes.example"))))
       (sitemap (artifact-content
                  (car (render-forest-sitemap
                         forest #:site-url "https://notes.example"))))
       (inline (forest-page forest "unstable-0001")))
  (test-equal "inline subtrees become independently addressable trees"
    '("root" "unstable-0001")
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
                     "class=\"forest-tree-block inline-subtree\""))
  (test-assert "inline trees render as initially open disclosure blocks"
    (string-contains (assoc-ref html "root") "<details open=\"open\">"))
  (test-assert "inline tree title is a native disclosure control"
    (string-contains (assoc-ref html "root") "<summary>"))
  (test-assert "tree blocks include hover styling"
    (string-contains (assoc-ref html "root") ".forest-tree-block:hover"))
  (test-assert "fragment navigation opens containing disclosures"
    (and (string-contains (assoc-ref html "root") "hashchange")
         (string-contains (assoc-ref html "root") "node.open = true")
         (string-contains (assoc-ref html "root") "target.scrollIntoView()")))
  (test-assert "HTML index lists file-backed trees but omits inline subtrees"
    (and (string-contains (assoc-ref html "index") "Root")
         (not (string-contains (assoc-ref html "index") "Inline section"))))
  (test-assert "subtrees receive standalone HTML routes"
    (string-contains (assoc-ref html "unstable-0001") "Owned text."))
  (test-assert "subtrees receive standalone text routes"
    (string-contains (assoc-ref text "unstable-0001") "Owned text."))
  (test-assert "text index lists file-backed trees but omits inline subtrees"
    (and (string-contains (assoc-ref text "index") "Root")
         (not (string-contains (assoc-ref text "index") "Inline section"))))
  (test-assert "sitemap includes addressable inline subtrees"
    (string-contains sitemap "/unstable-0001.html"))
  (test-assert "RSS omits inline subtrees as standalone feed entries"
    (and (string-contains rss "<title>Root</title>")
         (not (string-contains rss "<title>Inline section</title>"))))
  (test-equal "references inside a subtree belong to that subtree"
    '("unstable-0001")
    (map forest-reference-source-page-id
         (forest-incoming-references forest "root"))))

(test-error "generated subtree IDs cannot collide with file page IDs" #t
  (make-forest
    (make-site
      (list (make-site-page "unstable-0001" "File page"
                            (make-content-document '()))
            (make-site-page
              "root" "Root"
              (evaluate-document
                "@subtree{@title{Inline}}"))))))

(let* ((root
         (make-site-page
           "numbering" "Numbering"
           (evaluate-document
             "@subtree{@title{Outer} @subtree{@title{Nested}}} @subtree{@title{Sibling}}")))
       (forest (make-forest (make-site (list root))))
       (nested-html (assoc-ref (forest->html forest #:mathjax-src #f)
                               "numbering"))
       (make-forest-pages
         (lambda () (map site-page-id
                         (forest-pages (make-forest (make-site (list root)))))))
       (expected '("numbering" "unstable-0001" "unstable-0002"
                   "unstable-0003")))
  (test-equal "subtree IDs use preorder and sibling source order"
    expected (make-forest-pages))
  (test-equal "subtree IDs reset deterministically for each analysis"
    expected (make-forest-pages))
  (test-equal "nested subtrees each render as an open disclosure"
    3
    (let loop ((start 0) (count 0))
      (let ((found (string-contains nested-html "<details open=\"open\">"
                                    start)))
        (if found
            (loop (+ found 1) (+ count 1))
            count))))
  (test-assert "nested subtree titles remain nested in source order"
    (< (string-contains nested-html "Outer")
       (string-contains nested-html "Nested"))))

(let* ((target
         (make-site-page
           "reusable" "Reusable note"
           (evaluate-document
             "@heading{Included section} @paragraph{Shared content. @link[\"destination\"]{Follow destination.}}")
           "trees/reusable.grv"))
       (destination
         (make-site-page
           "destination" "Destination"
           (evaluate-document "@paragraph{Destination page.}")))
       (host
         (make-site-page
           "host" "Host"
           (evaluate-document
             "Before. @transclude[\"reusable\"] After.")
           "trees/host.grv"))
       (forest (make-forest (make-site (list host target destination))))
       (html (forest->html forest #:mathjax-src #f))
       (text (forest->text forest))
       (transclusions (forest-outgoing-transclusions forest "host")))
  (test-equal "transclusions are indexed as a distinct tree relation"
    '("reusable")
    (map forest-transclusion-target-tree-id transclusions))
  (test-equal "transclusion records identify their source occurrence"
    '("transclude-1")
    (map forest-transclusion-source-id transclusions))
  (test-equal "incoming transclusions return every occurrence"
    1
    (length (forest-incoming-transclusions forest "reusable")))
  (test-equal "incoming transclusions map to their host trees"
    '("host")
    (map site-page-id (forest-transcluding-pages forest "reusable")))
  (test-equal "transclusions do not become ordinary link references"
    '()
    (forest-outgoing-references forest "host"))
  (test-assert "HTML inserts the target content at the call site"
    (let ((host-html (assoc-ref html "host")))
      (and (string-contains host-html
                            "class=\"forest-tree-block transclusion\"")
           (string-contains host-html "href=\"/reusable.html\"")
           (string-contains host-html "forest-tree-permalink")
           (string-contains host-html "Reusable note")
           (string-contains host-html "Shared content."))))
  (test-assert "text output inserts the target content"
    (and (string-contains (assoc-ref text "host") "Reusable note")
         (string-contains (assoc-ref text "host") "Shared content.")))
  (test-assert "host artifacts depend on transcluded tree sources"
    (member "trees/reusable.grv"
            (forest-page-source-paths forest host)
            string=?))
  (test-equal "links in transcluded content remain owned by the source tree"
    '("reusable")
    (map forest-reference-source-page-id
         (forest-incoming-references forest "destination")))
  (test-assert "transcluded HTML links keep their resolved destination"
    (string-contains (assoc-ref html "host")
                     "href=\"/destination.html\""))
  (test-assert "transcluded text links use the host route and destination"
    (string-contains (assoc-ref text "host") "destination.txt")))

(let* ((target
         (make-site-page
           "repeat-target" "Repeated target"
           (evaluate-document "@heading{Unique heading}")))
       (host
         (make-site-page
           "repeat-host" "Repeated host"
           (evaluate-document
             "@transclude[\"repeat-target\"] @transclude[\"repeat-target\"]")))
       (html (assoc-ref (forest->html
                          (make-forest (make-site (list host target)))
                          #:mathjax-src #f)
                        "repeat-host")))
  (test-assert "repeated transclusions receive distinct HTML anchor prefixes"
    (and (string-contains html "transcluded-transclude-1--unique-heading")
         (string-contains html "transcluded-transclude-2--unique-heading"))))

(test-error "transclusion cycles fail forest analysis" #t
  (make-forest
    (make-site
      (list (make-site-page
              "cycle" "Cycle"
              (evaluate-document "@transclude[\"cycle\"]"))))))

(let ((message
        (catch #t
          (lambda ()
            (make-forest
              (make-site
                (list (make-site-page
                        "cycle-root" "Cycle root"
                        (evaluate-document
                          "@subtree{@title{Child} @transclude[\"cycle-root\"]}")
                        "notes/cycle.grv"))))
            #f)
          (lambda (key . args) (format #f "~s" (cons key args))))))
  (test-assert "cycle diagnostic reports the full tree path and source"
    (and (string-contains message "cyclic Forester tree composition")
         (string-contains message "cycle-root (notes/cycle.grv)")
         (string-contains message "unstable-0001 (notes/cycle.grv)"))))

(let ((message
        (catch #t
          (lambda ()
            (make-forest
              (make-site
                (list (make-site-page
                        "host" "Host"
                        (evaluate-document "@transclude[\"missing\"]")
                        "notes/host.grv"))))
            #f)
          (lambda (key . args) (format #f "~s" (cons key args))))))
  (test-assert "unresolved transclusion diagnostic identifies tree, source, occurrence, target"
    (and (string-contains message "host")
         (string-contains message "notes/host.grv")
         (string-contains message "transclude-1")
         (string-contains message "missing"))))

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

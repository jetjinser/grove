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
  #:use-module ((forester composition) #:prefix composition:)
  #:use-module (forester relations)
  #:use-module (forester site)
  #:use-module (grove build)
  #:use-module (grove ir)
  #:use-module (grove render html)
  #:use-module (grove render html routes)
  #:use-module (grove output)
  #:use-module (grove relation dsl)
  #:use-module (grove site)
  #:use-module (grove style html))

(define environment (default-environment scribble))
(eval '(use-modules (forester commands)
                     (forester relations)
                     (forester site)
                     (grove site)
                     (grove relation dsl)
                     ((grove math notation) #:prefix notation:))
      environment)

(define (evaluate-document source)
  (scribble-evaluate-document (open-input-string source) environment))

(define algebra
  (make-site-page
    "algebra"
    "Algebra & limits"
    (evaluate-document
      "@paragraph{The @link[\"limits\"]{limits} are useful. @tex-math{x^2+y^2=z^2}}\n@tex-display-math{a^2+b^2=c^2}\n@paragraph{@image[\"images/algebra.svg\"]{An algebra diagram.}}")
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
            (map reference-source-page-id
                 (incoming-references forest id))
            equal?)))
    (filter (lambda (page)
              (member (site-page-id page) source-ids equal?))
            (site-pages (site forest)))))

(define (common-backlinks-by-incoming-references forest left-id right-id)
  (let ((left (map site-page-id
                   (backlinks-by-incoming-references forest left-id)))
        (right (map site-page-id
                    (backlinks-by-incoming-references forest right-id))))
    (filter (lambda (page)
              (and (member (site-page-id page) left equal?)
                   (member (site-page-id page) right equal?)))
            (site-pages (site forest)))))

(define (render-test-html forest . options)
  (let ((routes
          (make-html-routes
            #:home-page (site-page-id (car (pages forest))))))
    (apply site->html forest #:routes routes options)))

(test-begin "forester site")

(define (document-paragraph-texts document)
  (map (lambda (paragraph)
         (content-text-value
           (car (content-element-children paragraph))))
       (filter (lambda (element)
                 (and (content-element? element)
                      (eq? (content-element-kind element) 'paragraph)))
               (content-document-children document))))

(test-error "HTML routes require an explicit home page ID" #t
  (make-html-routes))

(let* ((document (evaluate-document "@lipsum{}"))
       (page (make-site-page "lipsum" "Lipsum" document))
       (forest (make-forest (make-site (list page))))
       (paragraph (car (content-document-children document)))
       (text (car (content-element-children paragraph)))
       (source (content-text-source text)))
  (test-equal "top-level @lipsum text becomes ordinary paragraphs"
    150
    (length (document-paragraph-texts document)))
  (test-equal "@lipsum paragraphs use paragraph IR"
    'paragraph
    (content-element-kind paragraph))
  (test-assert "@lipsum records its corpus as an IR source"
    source)
  (test-assert "@lipsum corpus participates in page cache dependencies"
    (member source (page-source-paths forest page) string=?))
  (test-assert "@lipsum provides the classic opening text"
    (string-prefix?
      "Lorem ipsum dolor sit amet, consectetuer adipiscing elit."
      (content-text-value text))))

(let* ((document (evaluate-document "@lipsum[1-3]{}"))
       (paragraphs (document-paragraph-texts document)))
  (test-assert "@lipsum selects a paragraph range"
    (and (= (length paragraphs) 3)
         (string-contains (car paragraphs) "Curabitur dictum gravida mauris.")
         (string-prefix? "Nam dui ligula" (cadr paragraphs))
         (string-prefix? "Nulla malesuada porttitor diam"
                         (caddr paragraphs)))))

(test-assert "@lipsum selects a single paragraph"
  (string-prefix?
    "Nam dui ligula"
    (car (document-paragraph-texts
           (evaluate-document "@lipsum[2]{}")))))

(test-error "@lipsum rejects reversed paragraph ranges" #t
  (evaluate-document "@lipsum[3-2]{}"))

(test-error "@lipsum rejects ranges beyond its corpus" #t
  (evaluate-document "@lipsum[1-151]{}"))

(test-equal "@lipsum without a range returns all 150 paragraphs"
  (document-paragraph-texts (evaluate-document "@lipsum[1-150]{}"))
  (document-paragraph-texts (evaluate-document "@lipsum{}")))

(let* ((destination (page-with-links "destination" '()))
       (source (page-with-links "source" '("destination" "destination")))
       (other-source (page-with-links "other-source" '("destination")))
       (unlinked (page-with-links "unlinked" '()))
       (forest
         (make-forest
           (make-site (list destination source other-source unlinked))))
       (backlinks (backlinks forest "destination")))
  (test-equal "relation backlinks preserve the old page ordering and uniqueness"
    '("source" "other-source")
    (map site-page-id backlinks))
  (test-equal "relation backlinks match the previous incoming-reference filter"
    (map site-page-id (backlinks-by-incoming-references forest "destination"))
    (map site-page-id backlinks))
  (test-equal "incoming-reference queries still preserve each link occurrence"
    3
    (length (incoming-references forest "destination"))))

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
       (common (common-backlinks forest "left" "right")))
  (test-equal "relational joins find pages linking to both targets in site order"
    '("shared-late" "shared-early")
    (map site-page-id common))
  (test-equal "common backlinks match the incoming-reference intersection"
    (map site-page-id
         (common-backlinks-by-incoming-references forest "left" "right"))
    (map site-page-id common))
  (test-equal "joining a target to itself gives its unique backlink pages"
    (map site-page-id (backlinks forest "left"))
    (map site-page-id (common-backlinks forest "left" "left"))))

(define (bfs-reachable-page-ids forest source-id)
  (let visit ((pending (list source-id)) (visited '()))
    (if (null? pending)
        (map site-page-id
             (filter (lambda (page)
                       (and (not (equal? (site-page-id page) source-id))
                            (member (site-page-id page) visited equal?)))
                     (site-pages (site forest))))
        (let ((current (car pending)))
          (if (member current visited equal?)
              (visit (cdr pending) visited)
              (visit
                (append
                  (cdr pending)
                  (map reference-target-page-id
                       (outgoing-references forest current)))
                (cons current visited)))))))

(let* ((isolated (page-with-links "isolated" '()))
       (hub (page-with-links "hub" '("left" "right")))
       (left (page-with-links "left" '("merge")))
       (right (page-with-links "right" '("merge")))
       (merge (page-with-links "merge" '("hub")))
       (forest (make-forest (make-site (list isolated hub left right merge)))))
  (test-equal "reachability includes the transitive closure once in site order"
    '("left" "right" "merge")
    (map site-page-id (reachable-pages forest "hub")))
  (test-equal "reachability excludes the source even when a cycle returns to it"
    '("hub" "left" "right")
    (map site-page-id (reachable-pages forest "merge")))
  (test-equal "isolated pages have no reachable pages"
    '()
    (reachable-pages forest "isolated"))
  (test-equal "relation reachability agrees with breadth-first traversal"
    (map (lambda (page)
           (cons (site-page-id page)
                 (bfs-reachable-page-ids forest (site-page-id page))))
         (site-pages (site forest)))
    (map (lambda (page)
           (cons (site-page-id page)
                 (map site-page-id
                      (reachable-pages forest (site-page-id page)))))
         (site-pages (site forest)))))

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
         (string-contains target-html "href=\"#overview-2\">Overview</a>")))
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

(let* ((root
         (make-site-page
           "root" "Root"
           (evaluate-document
             "@subtree{@title{Inline section}@paragraph{Owned text. @link[\"root\"]{Home}.}}")))
       (forest (make-forest (make-site (list root))))
       (html (render-test-html forest #:mathjax-src #f))
       (text (site->text forest))
       (rss (artifact-content
              (car (render-rss forest
                                      #:site-url "https://notes.example"))))
       (sitemap (artifact-content
                  (car (render-sitemap
                         forest #:site-url "https://notes.example"))))
       (inline (find-page forest "unstable-0")))
  (test-equal "inline subtrees become independently addressable trees"
    '("root" "unstable-0")
    (map site-page-id (pages forest)))
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
  (test-assert "inline subtree bodies omit the document wrapper label"
    (and (not (string-contains (assoc-ref html "root") ">article "))
         (string-contains (assoc-ref html "root") "Owned text.")))
  (test-assert "generated tree IDs use the local-ID display form"
    (string-contains (assoc-ref html "root") "[#0]"))
  (test-assert "inline trees render as initially open disclosure blocks"
    (string-contains (assoc-ref html "root") "<details open=\"open\">"))
  (test-assert "inline subtree can be initially collapsed"
    (let* ((collapsed-root
             (make-site-page
               "collapsed" "Collapsed"
               (evaluate-document
                 "@subtree[#:open? #f]{@title{Collapsed}@paragraph{Hidden until opened.}}")))
           (collapsed-html
             (assoc-ref (render-test-html
                          (make-forest (make-site (list collapsed-root)))
                          #:mathjax-src #f)
                        "collapsed")))
      (and (string-contains collapsed-html "<summary>")
           (not (string-contains collapsed-html "<details open=\"open\">"))
           (string-contains collapsed-html "Hidden until opened."))))
  (test-assert "host promotes addressable disclosures to indexed subtrees"
    (let* ((page
             (make-site-page
               "addressable" "Addressable"
               (make-content-document
                 (list (make-content-disclosure
                         "Generated steps"
                         (list (make-content-element
                                 'paragraph '("Step content.")))
                         #:open? #f #:addressable? #t)))))
           (forest (make-forest (make-site (list page))))
           (rendered (assoc-ref
                       (render-test-html forest #:mathjax-src #f)
                       "addressable")))
      (and (find-page forest "unstable-0")
           (string-contains rendered "class=\"forest-tree-block inline-subtree\"")
           (string-contains rendered "<summary>")
           (not (string-contains rendered "<details open=\"open\">")))))
  (test-assert "inline subtree can be explicitly set to open"
    (let* ((open-root
             (make-site-page
               "explicit-open" "Explicit open"
               (evaluate-document
                 "@subtree[#:open? #t]{@title{Open}}")))
           (open-html
             (assoc-ref (render-test-html
                          (make-forest (make-site (list open-root)))
                          #:mathjax-src #f)
                        "explicit-open")))
      (string-contains open-html "<details open=\"open\">")))
  (test-assert "inline tree title is a native disclosure control"
    (string-contains (assoc-ref html "root") "<summary>"))
  (test-assert "site->html embeds the Forester stylesheet"
    (string-contains (assoc-ref html "root")
                     ".forest-tree-block:hover"))
  (test-assert "tree disclosure markers are hidden while summaries stay clickable"
    (and (string-contains (assoc-ref html "root") "summary::marker")
         (string-contains (assoc-ref html "root") "summary::-webkit-details-marker")
         (string-contains (assoc-ref html "root") "summary { cursor: pointer")))
  (test-assert "fragment navigation opens containing disclosures"
    (and (string-contains (assoc-ref html "root") "hashchange")
         (string-contains (assoc-ref html "root")
                          "addEventListener('hashchange', revealFragment)")
         (string-contains (assoc-ref html "root") "node.open = true")
         (not (string-contains (assoc-ref html "root") "&quot;"))
         (string-contains (assoc-ref html "root") "target.scrollIntoView()")))
  (test-assert "HTML index redirects to the configured home tree"
    (and (string-contains (assoc-ref html "index")
                          "content=\"0;url=/root.html\"")
         (not (string-contains (assoc-ref html "index") "Inline section"))))
  (test-assert "subtrees receive standalone HTML routes"
    (string-contains (assoc-ref html "unstable-0") "Owned text."))
  (test-assert "subtrees receive standalone text routes"
    (string-contains (assoc-ref text "unstable-0") "Owned text."))
  (test-assert "text index lists file-backed trees but omits inline subtrees"
    (and (string-contains (assoc-ref text "index") "Root")
         (not (string-contains (assoc-ref text "index") "Inline section"))))
  (test-assert "sitemap includes addressable inline subtrees"
    (string-contains sitemap "/unstable-0.html"))
  (test-assert "RSS omits inline subtrees as standalone feed entries"
    (and (string-contains rss "<title>Root</title>")
         (not (string-contains rss "<title>Inline section</title>"))))
  (test-equal "references inside a subtree belong to that subtree"
    '("unstable-0")
    (map reference-source-page-id
         (incoming-references forest "root"))))

(test-error "generated subtree IDs cannot collide with file page IDs" #t
  (make-forest
    (make-site
      (list (make-site-page "unstable-0" "File page"
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
       (nested-html (assoc-ref (render-test-html forest #:mathjax-src #f)
                               "numbering"))
       (make-pages
         (lambda () (map site-page-id
                         (pages (make-forest (make-site (list root)))))))
       (expected '("numbering" "unstable-0" "unstable-1"
                   "unstable-2")))
  (test-equal "subtree IDs use preorder and sibling source order"
    expected (make-pages))
  (test-equal "subtree IDs reset deterministically for each analysis"
    expected (make-pages))
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

(let* ((root
         (make-site-page
           "root" "Root"
           (evaluate-document
             "@subtree[#:id \"stable-piece\"]{@title{Named piece} @paragraph{Reusable.}} @transclude[\"stable-piece\"]")))
       (forest (make-forest (make-site (list root))))
       (html (render-test-html forest #:mathjax-src #f))
       (changed-root
         (make-site-page
           "root" "Root"
           (evaluate-document
             "@subtree{@title{Earlier anonymous piece}} @subtree[#:id \"stable-piece\"]{@title{Named piece} @paragraph{Reusable.}} @transclude[\"stable-piece\"]")))
       (changed-forest (make-forest (make-site (list changed-root)))))
  (test-assert "subtrees can use an author-chosen address"
    (find-page forest "stable-piece"))
  (test-assert "named subtrees receive a standalone HTML route"
    (string-contains (assoc-ref html "stable-piece") "Reusable."))
  (test-equal "explicit subtree addresses are included in composition data"
    '("stable-piece")
    (map composition:transclusion-target-tree-id
         (outgoing-transclusions forest "root")))
  (test-assert "explicit subtree addresses survive insertion of anonymous trees"
    (find-page changed-forest "stable-piece"))
  (test-equal "anonymous subtree IDs remain generated around named trees"
    '("root" "unstable-0" "stable-piece")
    (map site-page-id (pages changed-forest))))

(test-error "explicit subtree IDs cannot collide with file page IDs" #t
  (make-forest
    (make-site
      (list (make-site-page "stable-piece" "File page"
                            (make-content-document '()))
            (make-site-page
              "root" "Root"
              (evaluate-document
                "@subtree[#:id \"stable-piece\"]{@title{Inline}}"))))))

(test-error "explicit subtree IDs cannot collide with other subtree IDs" #t
  (make-forest
    (make-site
      (list
        (make-site-page
          "root" "Root"
          (evaluate-document
            "@subtree[#:id \"shared\"]{@title{First}} @subtree[#:id \"shared\"]{@title{Second}}"))))))

(test-error "explicit subtree IDs reject the fragment separator" #t
  (evaluate-document
    "@subtree[#:id \"part#section\"]{@title{Invalid}}"))

(let* ((target
         (make-site-page
           "reusable" "Reusable note"
           (evaluate-document
             "@heading{Included section} @paragraph{Shared content. @link[\"destination\"]{Follow destination.}}")
           "plants/reusable.plant"))
       (destination
         (make-site-page
           "destination" "Destination"
           (evaluate-document "@paragraph{Destination page.}")))
       (host
         (make-site-page
           "host" "Host"
           (evaluate-document
             "Before. @transclude[\"reusable\"] After.")
           "plants/host.plant"))
       (forest (make-forest (make-site (list host target destination))))
       (html (render-test-html forest #:mathjax-src #f))
       (text (site->text forest))
       (transclusions (outgoing-transclusions forest "host")))
  (test-equal "transclusions are indexed as a distinct tree relation"
    '("reusable")
       (map composition:transclusion-target-tree-id transclusions))
  (test-equal "transclusion records identify their source occurrence"
    '("transclude-1")
    (map composition:transclusion-source-id transclusions))
  (test-equal "incoming transclusions return every occurrence"
    1
    (length (incoming-transclusions forest "reusable")))
  (test-equal "incoming transclusions map to their host trees"
    '("host")
    (map site-page-id (transcluding-pages forest "reusable")))
  (test-assert "default HTML backmatter shows transclusion context"
    (and (string-contains (assoc-ref html "reusable")
                          "class=\"backmatter context\"")
         (string-contains (assoc-ref html "reusable") "<h2>Context</h2>")
         (string-contains (assoc-ref html "reusable")
                          "href=\"/host.html\">Host</a>")))
  (test-assert "default text backmatter shows transclusion context"
    (and (string-contains (assoc-ref text "reusable") "Context:")
         (string-contains (assoc-ref text "reusable") "Host (host.txt)")))
  (test-assert "context sources are included in target artifact dependencies"
    (member "plants/host.plant" (page-source-paths forest target) string=?))
  (test-assert "backmatter sections can be disabled"
    (and (not (string-contains
                (assoc-ref (render-test-html forest #:backmatter '())
                           "reusable")
                "class=\"backmatter"))
         (not (string-contains
                (assoc-ref (render-test-html forest #:backmatter '())
                           "reusable")
                "class=\"backmatter backlinks\""))
         (not (string-contains
                (assoc-ref (site->text forest #:backmatter '()) "reusable")
                "Context:"))))
  (test-error "backmatter rejects unknown sections" #t
    (render-test-html forest #:backmatter '(related)))
  (test-error "backmatter rejects duplicate sections" #t
    (site->text forest #:backmatter '(context context)))
  (test-error "backmatter labels reject unknown sections" #t
    (site->text forest #:backmatter-labels '((related . "Related"))))
  (test-error "backmatter labels reject empty text" #t
    (site->text forest #:backmatter-labels '((context . ""))))
  (test-equal "transclusions do not become ordinary link references"
    '()
    (outgoing-references forest "host"))
  (test-assert "HTML inserts the target content at the call site"
    (let ((host-html (assoc-ref html "host")))
      (and (string-contains host-html
                            "class=\"forest-tree-block transclusion\"")
           (string-contains host-html "href=\"/reusable.html\"")
           (string-contains host-html "class=\"forest-tree-id\"")
           (string-contains host-html "[reusable]")
           (not (string-contains host-html "↗"))
           (string-contains host-html "Reusable note")
           (string-contains host-html "Shared content."))))
  (test-assert "text output inserts the target content"
    (and (string-contains (assoc-ref text "host") "Reusable note")
         (string-contains (assoc-ref text "host") "Shared content.")))
  (test-assert "host artifacts depend on transcluded tree sources"
    (member "plants/reusable.plant"
            (page-source-paths forest host)
            string=?))
  (test-equal "links in transcluded content remain owned by the source tree"
    '("reusable")
    (map reference-source-page-id
         (incoming-references forest "destination")))
  (test-assert "transcluded HTML links keep their resolved destination"
    (string-contains (assoc-ref html "host")
                     "href=\"/destination.html\""))
  (test-assert "transcluded text links use the host route and destination"
    (string-contains (assoc-ref text "host") "destination.txt")))

(let* ((source
         (make-site-page
           "source" "Source"
           (evaluate-document "@subtree{@title{Inline section}}")
           "plants/source.plant"))
       (host
         (make-site-page
           "host" "Host"
           (evaluate-document "@transclude[\"unstable-0\"]")
           "plants/host.plant"))
       (forest (make-forest (make-site (list source host))))
       (host-html (assoc-ref (render-test-html forest #:mathjax-src #f) "host")))
  (test-assert "transclusions display generated IDs in local-ID form"
    (and (string-contains host-html "data-tree-id=\"unstable-0\"")
         (string-contains host-html "href=\"/unstable-0.html\"")
         (string-contains host-html "[#0]"))))

(let* ((target
         (make-site-page
           "repeat-target" "Repeated target"
           (evaluate-document "@heading{Unique heading}")))
       (host
         (make-site-page
           "repeat-host" "Repeated host"
           (evaluate-document
             "@transclude[\"repeat-target\"] @transclude[\"repeat-target\"]")))
       (html (assoc-ref (render-test-html
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
                        "notes/cycle.plant"))))
            #f)
          (lambda (key . args) (format #f "~s" (cons key args))))))
  (test-assert "cycle diagnostic reports the full tree path and source"
    (and (string-contains message "cyclic Forester tree composition")
         (string-contains message "cycle-root (notes/cycle.plant)")
         (string-contains message "unstable-0 (notes/cycle.plant)"))))

(let ((message
        (catch #t
          (lambda ()
            (make-forest
              (make-site
                (list (make-site-page
                        "host" "Host"
                        (evaluate-document "@transclude[\"missing\"]")
                        "notes/host.plant"))))
            #f)
          (lambda (key . args) (format #f "~s" (cons key args))))))
  (test-assert "unresolved transclusion diagnostic identifies tree, source, occurrence, target"
    (and (string-contains message "host")
         (string-contains message "notes/host.plant")
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
                        "notes/broken.plant"))))
            #f)
          (lambda (key . args) (format #f "~s" (cons key args))))))
  (test-assert "unresolved-link errors identify source page and target"
    (and (string-contains message "notes/broken.plant")
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
                      "plants/math/cat-0001.plant")
                    (make-site-page
                      "same" "Second" (evaluate-document "second")
                      "plants/logic/cat-0001.plant")))
            #f)
          (lambda (key . args) (format #f "~s" (cons key args))))))
  (test-assert "duplicate page id errors identify every source path"
    (and (string-contains message "cat-0001")
         (string-contains message "plants/math/cat-0001.plant")
         (string-contains message "plants/logic/cat-0001.plant"))))

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

(let* ((plain-paragraph
         (make-content-element
           'paragraph (list "A short note about 范畴论.")))
       (math-paragraph
         (make-content-element
           'paragraph
           (list "Formula: "
                 (make-content-element
                   'math (list (make-content-layout 'math "x^2")))
                 ".")))
       (link-node
         (make-content-element
           'link (list "The target page")
           #:properties '((target . "target"))))
       (tag-node
         (make-content-element
           'tag (list "Tutorial")
           #:properties '((id . "tutorial"))))
       (taxon-node
         (make-content-element
           'taxon (list "Mathematics")
           #:properties '((id . "mathematics"))))
       (subtree-node
         (make-content-element
           'subtree
           (list (make-content-element 'paragraph (list "Nested note.")))
           #:properties '((tree-id . "query-subtree")
                          (title . "Query subtree"))
           #:flow-kind 'block))
       (article
         (make-site-page
           "query-article" "Query article"
           (make-content-document
             (list plain-paragraph math-paragraph link-node tag-node
                   taxon-node subtree-node))
           "query-article.plant"))
       (target
         (make-site-page "target" "Target" (make-content-document '())
                         "target.plant"))
       (forest (make-forest (make-site (list article target))))
       (nodes (content-node-relation forest))
       (math-paragraph-ids
         (query (node-id)
           (math-containing-paragrapho nodes node-id)))
       (plain-matches
         (query (node-id page-id text)
           (plain-text-paragrapho nodes node-id)
           (shorter-than-o nodes node-id 120)
           (contains-texto nodes node-id "范畴论")
           (nodeo nodes
                  (page-id page-id)
                  (text text))))
       (tutorial-pages
         (query (page-id label)
           (topic-membero nodes "tutorial" page-id label)))
       (math-pages
         (query (page-id label)
           (topic-membero nodes "mathematics" page-id label)))
       (link-node-ids
         (query (node-id)
           (nodeo nodes (node-id node-id) (kind 'link))))
       (subtree-node-ids
         (query (node-id)
           (nodeo nodes (node-id node-id) (kind 'subtree)))))
  (test-equal "plain paragraphs and their text are derived from page IR"
    '("query-article" "query-subtree")
    (delete-duplicates (map cadr plain-matches) string=?))
  (test-assert "content queries can search the derived paragraph text"
    (string-contains (caddar plain-matches) "范畴论"))
  (test-equal "paragraphs containing math are found through IR ancestry"
    1
    (length math-paragraph-ids))
  (test-equal "the math query identifies the original paragraph IR node"
    'paragraph
    (content-element-kind
      (content-node forest (car math-paragraph-ids))))
  (test-equal "tag IR is queryable as a page topic"
    '(("query-article" "Tutorial"))
    tutorial-pages)
  (test-equal "taxon IR is queryable through the same topic relation"
    '(("query-article" "Mathematics"))
    math-pages)
  (test-equal "link nodes and their target properties remain in content IR"
    '("target")
    (query (target-id)
      (node-propertyo nodes (car link-node-ids) 'target target-id)))
  (test-equal "named subtree nodes are queryable as content IR"
    '("query-subtree")
    (query (tree-id)
      (node-propertyo nodes (car subtree-node-ids) 'tree-id tree-id)))
  (test-equal "subtree body paragraphs belong to their named tree"
    '("query-subtree")
    (delete-duplicates
      (map cadr
           (query (node-id page-id)
             (plain-text-paragrapho nodes node-id)
             (nodeo nodes (node-id node-id) (page-id page-id))
             (contains-texto nodes node-id "Nested note")))
      string=?)))

(define source-projection-math
  (make-content-element
    'math
    (list
      (make-content-element
        'math-sup
        (list (make-content-element 'math-row (list "x"))
              (make-content-element 'math-row (list "2"))))
      (make-content-element 'math-token (list "+")
                            #:properties '((token-kind . mo)))
      (make-content-element
        'math-frac
        (list (make-content-element 'math-row (list "a"))
              (make-content-element 'math-row (list "b")))))))

(let* ((location '((file . "query-article.plant") (line . 7) (column . 3)))
       (paragraph
         (content-with-source-location
           (make-content-element
             'paragraph (list "Formula: " source-projection-math "."))
           location))
       (page
         (make-site-page "query-article" "Query article"
                         (make-content-document (list paragraph))
                         "query-article.plant"))
       (forest (make-forest (make-site (list page))))
       (nodes (content-node-relation forest)))
  (test-equal "math text projection keeps powers and fractions legible"
    '("Formula: x^(2) + (a)/(b).")
    (query (text)
      (fresh (node-id)
        (math-containing-paragrapho nodes node-id)
        (nodeo nodes (node-id node-id) (text text)))))
(test-equal "source-locationo returns the paragraph's source position"
    '(("query-article.plant" 7 3))
    (query (file line column)
      (fresh (node-id)
        (math-containing-paragrapho nodes node-id)
        (source-locationo nodes node-id file line column)))))

(let* ((catalog-document
         (evaluate-document
           (string-append
           "@(define render-query\n"
           "   (lambda (forest _page-id)\n"
           "     (let* ((nodes (content-node-relation forest))\n"
           "            (matches\n"
           "              (query (node-id page-id)\n"
           "                (plain-text-paragrapho nodes node-id)\n"
           "                (nodeo nodes (node-id node-id) (page-id page-id))\n"
           "                (contains-texto nodes node-id \"范畴论\"))))\n"
           "       (make-content-document\n"
           "         (map (lambda (match)\n"
           "                (let ((target (find-page forest (cadr match))))\n"
           "                  (make-content-element 'item\n"
           "                    (list (link (site-page-id target)\n"
           "                                (site-page-title target))\n"
           "                          \" — \" (content-node forest (car match))))))\n"
           "              matches)))))\n"
           "@forest-query[render-query]\n"
           "@paragraph{Authored link: @link[\"manual\"]{manual}.}")))
       (catalog
         (make-site-page "catalog" "Catalog" catalog-document
                         "catalog.plant"))
       (article
         (make-site-page "article" "Article"
           (evaluate-document "@paragraph{范畴论 appears in this note.}")
           "article.plant"))
       (manual (make-site-page "manual" "Manual" (make-content-document '())
                               "manual.plant"))
       (forest
         (make-forest
           (make-site (list catalog article manual)
             #:sources '("commands.scm"))))
       (resolved-catalog (find-page forest "catalog"))
       (resolved-children
         (content-document-children
           (site-page-document resolved-catalog)))
       (nodes (content-node-relation forest)))
  (test-assert "@forest-query runs after the complete Forest snapshot exists"
    (any (lambda (element)
           (and (content-element? element)
                (eq? (content-element-kind element) 'item)))
         resolved-children))
  (test-equal "query-generated links do not enter the reference relation"
    '("manual")
    (map reference-target-page-id (outgoing-references forest "catalog")))
  (test-equal "query-generated links do not create backlinks"
    '()
    (incoming-references forest "article"))
  (test-equal "query-generated content is absent from the source node snapshot"
    '("article")
    (map cadr
      (query (node-id page-id)
      (plain-text-paragrapho nodes node-id)
      (nodeo nodes (node-id node-id) (page-id page-id))
      (contains-texto nodes node-id "范畴论")
      (== page-id "article"))))
  (test-assert "query pages depend on every Forest source for caching"
    (every (lambda (source)
            (member source (page-source-paths forest resolved-catalog) string=?))
      '("catalog.plant" "article.plant" "manual.plant" "commands.scm")))
  (let ((html (assoc-ref (render-test-html forest) "catalog")))
  (test-assert "query-generated links render without consuming authored link IDs"
    (and (string-contains html "href=\"/article.html\">Article</a>")
         (string-contains html "id=\"ref-1\" href=\"/manual.html\">manual</a>")))
  (let* ((routes (make-text-routes))
         (text (assoc-ref (site->text forest #:routes routes) "catalog")))
    (test-assert "query-generated links render in text output"
      (string-contains text "Article [article.txt]")))))

(let ((message
        (catch #t
          (lambda ()
            (make-forest
              (make-site
                (list
                  (make-site-page "query" "Query"
                    (make-content-document
                      (list
                        (make-content-element
                          'forest-query '()
                          #:properties
                          `((query . ,(lambda (_forest _page-id)
                                        (make-content-document
                                          (list
                                            (make-content-element
                                              'transclude '()
                                              #:properties
                                              '((target . "other")))))))))))
                    "query.plant"))))
            #f)
          (lambda (key . args) (format #f "~s" (cons key args))))))
  (test-assert "query output cannot mutate Forest composition recursively"
    (string-contains message
      "Forester query at query.plant returned unsupported transclude content")))

(test-end "forester site")

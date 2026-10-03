;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester composition)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (forester render html)
  #:use-module (forester render rss)
  #:use-module (forester render sitemap)
  #:use-module (forester render text)
  #:use-module ((forester composition) #:prefix composition:)
  #:use-module (forester relations)
  #:use-module (forester site)
  #:use-module (grove build)
  #:use-module (grove ir)
  #:use-module (grove render html routes)
  #:use-module (grove output)
  #:use-module (grove relation dsl)
  #:use-module (grove site)
  #:use-module (grove style html)
  #:use-module (tests unit forester fixtures))

(test-begin "forester composition")
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
             "@paragraph{Included content} @paragraph{Shared content. @link[\"destination\"]{Follow destination.}}")
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
    (render-test-html forest #:backmatter '(unknown)))
  (test-error "backmatter rejects duplicate sections" #t
    (site->text forest #:backmatter '(context context)))
  (test-error "backmatter labels reject unknown sections" #t
    (site->text forest #:backmatter-labels '((unknown . "Unknown"))))
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

(let* ((citation
         (make-site-page "citation" "A citation"
           (evaluate-document "@taxon[\"Reference\"]{Reference}")
           "plants/citation.plant"))
       (idea
         (make-site-page "idea" "A related idea"
           (evaluate-document "@paragraph{An idea.}")
           "plants/idea.plant"))
       (support
         (make-site-page "support" "Supporting note"
           (evaluate-document
             "@paragraph{See @link[\"citation\"]{the citation}.}")
           "plants/support.plant"))
       (source
         (make-site-page "source" "Source"
           (evaluate-document
             "@paragraph{@link[\"idea\"]{the idea}.} @transclude[\"support\"]")
           "plants/source.plant"))
       (forest (make-forest (make-site (list source support citation idea))))
       (html (render-test-html forest #:mathjax-src #f))
       (text (site->text forest)))
  (test-equal "Related excludes pages in the Reference taxon"
    '("idea")
    (map site-page-id (related-pages forest "source")))
  (test-assert "HTML Related lists linked non-reference pages"
    (and (string-contains (assoc-ref html "source") "<h2>Related</h2>")
         (string-contains (assoc-ref html "source")
                          "href=\"/idea.html\">A related idea</a>")))
  (test-assert "text Related lists linked non-reference pages"
    (and (string-contains (assoc-ref text "source") "Related:")
         (string-contains (assoc-ref text "source") "A related idea (idea.txt)")))
  (test-equal "References includes citations from transcluded content"
    '("citation")
    (map site-page-id (reference-pages forest "source")))
  (test-assert "HTML References shows the transcluded tree's citation"
    (and (string-contains (assoc-ref html "source") "<h2>References</h2>")
         (string-contains (assoc-ref html "source")
                          "href=\"/citation.html\">A citation</a>")))
  (test-assert "text References shows the transcluded tree's citation"
    (and (string-contains (assoc-ref text "source") "References:")
         (string-contains (assoc-ref text "source")
                          "A citation (citation.txt)")))
  (test-assert "related targets are included in source artifact dependencies"
    (and (member "plants/idea.plant" (page-source-paths forest source) string=?)
         (member "plants/citation.plant" (page-source-paths forest source)
                 string=?)))
  (test-equal "Related can use a project-specific reference taxon ID"
    '("idea")
    (map site-page-id
      (related-pages forest "source" "Bibliography")))
  (test-error "empty reference taxon IDs are rejected" #t
    (site->text forest #:reference-taxon-id "")))

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
           (evaluate-document "@paragraph{@link[\"repeat-target\"]{Self reference}}")))
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
    (and (string-contains html "tree-1--ref-1")
         (string-contains html "tree-2--ref-1"))))

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


(let* ((forest
          (make-forest
            (make-site
              (list (make-site-page "frontmatter" "Root"
                      (evaluate-document
                        "@subtree[#:id \"child\"]{@title{Child}@summary{Child summary}@published{2026-01-01}@tag[\"nested\"]{Nested}@taxon[\"Lemma\"]{Lemma}@paragraph{Child body.}}" "frontmatter.plant"))))))
       (child (find-page forest "child")))
  (test-equal "subtree uses root frontmatter extraction"
    "Child summary" (assq-ref (site-page-metadata child) 'summary))
  (test-equal "subtree dates are stored as frontmatter"
    "2026-01-01" (assq-ref (site-page-metadata child) 'published))
  (test-equal "subtree declaration retains source location"
    1 (assq-ref (assq-ref (site-page-metadata child) 'source-location) 'line))
  (test-equal "nested topics belong only to their tree"
    '("child") (map site-page-id (topic-pages forest "nested")))
  (test-equal "direct definition is retained"
    '(("frontmatter" "child")) (subtree-definitions forest)))

(test-error "subtree frontmatter validates dates" #t
  (make-forest
    (make-site (list (make-site-page "invalid" "Invalid"
      (evaluate-document
        "@subtree{@title{Child}@published{yesterday}}"))))))

(test-end "forester composition")

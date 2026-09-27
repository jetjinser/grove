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

(test-begin "forester site")

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
         (string-contains algebra-html "href=\"/limits.html\">Limits</a>")))
  (test-assert "reverse links point back to their source"
    (string-contains limits-html "href=\"/algebra.html\">Algebra &amp; limits</a>"))
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

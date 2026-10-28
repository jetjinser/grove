;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester render)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (forester render html)
  #:use-module (forester render context)
  #:use-module (forester render gemtext)
  #:use-module (forester render rss)
  #:use-module (forester render sitemap)
  #:use-module (forester render text)
  #:use-module (forester render text routes)
  #:use-module (forester site)
  #:use-module (forester style html)
  #:use-module (grove build)
  #:use-module (grove highlight)
  #:use-module (grove private renderer)
  #:use-module (grove ir)
  #:use-module (grove render html)
  #:use-module (grove render context)
  #:use-module (grove render html presentation)
  #:use-module (grove render text)
  #:use-module (grove render rules)
  #:use-module (grove render html routes)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (grove style html element)
  #:use-module (grove output)
  #:use-module (grove i18n)
  #:use-module (grove site)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (tests unit forester fixtures))

(test-begin "forester renderers")

(let* ((forest
         (make-forest
           (make-site
             (list (make-site-page "guide-en" "Guide EN"
                                   (make-content-document '())
                                   #:metadata '((translation . "guide")))
                   (make-site-page "notes" "Notes"
                                   (make-content-document '())
                                   #:metadata '((translation . "notes")))
                   (make-site-page "guide-zh" "Guide ZH"
                                   (make-content-document '())
                                   #:metadata
                                   `((translation . ,(string-copy "guide"))))))))
       (groups (derive-translation-groups (pages forest))))
  (test-equal "translation groups follow first occurrence and site page order"
    '(("guide" "guide-en" "guide-zh")
      ("notes" "notes"))
    (map (lambda (group)
           (cons (car group) (map site-page-id (cdr group))))
         groups))
  (test-equal "site render context derives translation groups by default"
    groups
    (render-context-ref (make-site-render-context forest)
                        'translation-groups))
  (test-equal "site render context can explicitly omit translation groups"
    '()
    (render-context-ref
      (make-site-render-context forest #:translation-groups '())
      'translation-groups))
  (test-equal "site render context accepts explicit page groups"
    groups
    (render-context-ref
      (make-site-render-context forest #:translation-groups groups)
      'translation-groups))
  (test-error "site context rejects malformed translation groups"
    #t
    (make-site-render-context forest #:translation-groups '((42 . ()))))
  (test-error "site context rejects duplicate translation group keys"
    #t
    (make-site-render-context forest
      #:translation-groups '(("guide" . ()) ("guide" . ()))))
  (test-error "site context rejects non-page translation group members"
    #t
    (make-site-render-context forest
      #:translation-groups '(("guide" . (not-a-page)))))
  (test-error "site context rejects non-boolean language switches"
    #t
    (make-site-render-context forest #:hreflang? 'enabled))
  (test-error "site context rejects malformed site URLs"
    #t
    (make-site-render-context forest #:site-url "https:///path"))
  (test-error "site context rejects deployment paths in site origins"
    #t
    (make-site-render-context forest
      #:site-url "https://example.org/notes/"))
  (test-error "site context rejects empty Open Graph image URLs"
    #t
    (make-site-render-context forest #:og-image ""))
  (test-error "site context rejects non-string Open Graph alt text"
    #t
    (make-site-render-context forest #:og-image-alt 42))
  (test-error "site context validates renderer locale options"
    #t
    (make-site-render-context forest #:locale 'not-a-locale))
  (test-error "site context validates default locale type"
    #t
    (make-site-render-context forest #:default-locale 42))
  (test-error "site context rejects an empty default locale"
    #t
    (make-site-render-context forest #:default-locale ""))
  (test-error "site context validates message catalogs"
    #t
    (make-site-render-context forest #:messages 'not-a-catalog)))

(let ((message
        (catch #t
          (lambda ()
            (make-site-render-context #f)
            #f)
          (lambda (_ . arguments)
            (and (pair? arguments) (format #f "~s" arguments))))))
  (test-assert "site render context validates the forest before deriving defaults"
    (and message
         (string-contains message "render context requires a forest"))))

(define (backmatter-messages backlink-label)
  (make-message-catalog
    `(("en" . ((backmatter-backlinks . ,backlink-label))))))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (artifacts (render-gemtext forest))
       (index (artifact-content
                (find (lambda (artifact)
                        (string=? (artifact-path artifact) "index.gmi"))
                      artifacts)))
       (algebra-gemtext (artifact-content
                          (find (lambda (artifact)
                                  (string=? (artifact-path artifact)
                                            "algebra.gmi"))
                                artifacts)))
       (limits-gemtext (artifact-content
                         (find (lambda (artifact)
                                 (string=? (artifact-path artifact)
                                           "limits.gmi"))
                               artifacts))))
  (test-assert "Gemtext renderer creates a capsule index and page artifacts"
    (equal? '("index.gmi" "algebra.gmi" "limits.gmi")
            (map artifact-path artifacts)))
  (test-assert "Gemtext index links to each page"
    (and (string-prefix? "# Site\n" index)
         (string-contains index "=> algebra.gmi Algebra & limits")
         (string-contains index "=> limits.gmi Limits")))
  (test-assert "Gemtext page titles use headings and keep prose around links"
    (and (string-prefix? "# Algebra & limits\n" algebra-gemtext)
         (string-contains algebra-gemtext "=> index.gmi Home")
         (string-contains algebra-gemtext "=> limits.gmi limits")
         (string-contains algebra-gemtext "The limits are useful.")))
  (test-assert "Gemtext pages include configured backlink sections"
    (and (string-contains limits-gemtext "## Backlinks")
         (string-contains limits-gemtext "=> algebra.gmi Algebra & limits")))
  (test-assert "Gemtext backmatter options control sections and labels"
    (let ((page (cdr (assoc "limits"
                       (site->gemtext
                         forest
                         #:options
                         (make-gemtext-options
                           #:backmatter '(backlinks)
                           #:messages (backmatter-messages "Linked from")))))))
      (and (string-contains page "## Linked from")
           (not (string-contains page "## Related")))))
  (test-assert "Gemtext accepts renderer rules"
    (let* ((rules (make-render-rules
                    (list (cons 'paragraph
                                (lambda (context element _payload next)
                                  "CUSTOM\n")))))
           (page (cdr (assoc "algebra"
                              (site->gemtext
                                forest
                                #:options
                                (make-gemtext-options #:rules rules))))))
      (string-contains page "CUSTOM")))
  (test-assert "Gemtext artifacts retain Forest source dependencies"
    (equal? (source-paths forest) (artifact-sources (car artifacts)))))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (routes (make-text-routes
                 #:page-path (lambda (_id) "same.txt"))))
  (test-error "text renderer rejects colliding page routes" #t
    (render-text forest #:options (make-text-options #:routes routes)))
  (test-error "Gemtext renderer rejects colliding page routes" #t
    (render-gemtext forest #:options (make-gemtext-options #:routes routes))))

(let* ((source
         (make-site-page
           "source" "Source"
           (make-content-document
             (list
               (make-content-element
                 'paragraph
                 (list "Before "
                       (make-content-element 'link '("the limit")
                             #:properties '((target . "limits")))
                       " after. Run "
                       (make-content-element 'code '("grove"))
                       " now."))
               (make-content-element 'item '("A list item") #:flow-kind 'block)
               (make-content-element 'blockquote '("A quoted line.")
                 #:flow-kind 'block)
               (make-content-element
                 'paragraph
                 (list "See "
                       (make-content-element 'image '("A diagram")
                         #:properties '((source . "assets/diagram one.svg?view=full")))
                       " soon."))
               (make-content-element 'code-block
                 (list (make-content-text "(+ 1 2)"))
                 #:properties '((language . scheme)) #:flow-kind 'block)))))
       (forest (make-forest (make-site (list source algebra limits))))
       (page (cdr (assoc "source" (site->gemtext forest)))))
  (test-assert "Gemtext converts inline references to standalone link lines"
    (and (string-contains page "Before ")
         (string-contains page "=> limits.gmi the limit")
         (string-contains page " after. Run grove now.")
         (not (string-contains page "`grove`"))))
  (test-assert "Gemtext uses native list and preformatted code syntax"
    (and (string-contains page "* A list item")
         (string-contains page "> A quoted line.")
         (string-contains page "``` scheme\n(+ 1 2)\n```")))
  (test-assert "Gemtext preserves image alt text and links to encoded local assets"
    (and (string-contains page "See A diagram soon.")
         (string-contains page
           "=> assets/diagram%20one.svg?view=full A diagram"))))

(let* ((source
         (make-site-page "source" "Source"
           (make-content-document
             (list (make-content-element 'paragraph
                     (list (make-content-element 'image '("Remote image")
                             #:properties '((source . "http://images.example/pic.png")))))))))
       (forest (make-forest (make-site (list source))))
       (page (cdr (assoc "source" (site->gemtext forest)))))
  (test-assert "Gemtext preserves arbitrary absolute URI schemes for assets"
    (string-contains page "=> http://images.example/pic.png Remote image")))

(let* ((page
         (make-site-page "literal" "Literal syntax"
           (make-content-document
             (list (make-content-element 'paragraph
                     (list "# This is prose\n=> not a link\n* not a list\n> not a quote\n```"))))))
       (forest (make-forest (make-site (list page))))
       (output (cdr (assoc "literal" (site->gemtext forest)))))
  (test-assert "Gemtext keeps markup-like source lines as ordinary text"
    (and (string-contains output "\n # This is prose")
         (string-contains output "\n => not a link")
         (string-contains output "\n * not a list")
         (string-contains output "\n > not a quote")
         (string-contains output "\n ```"))))

(test-error "Gemtext reports code blocks containing fence lines" #t
  (render-gemtext
    (make-forest
      (make-site
        (list
          (make-site-page "bad-code" "Bad code"
            (make-content-document
              (list (make-content-element 'code-block
                      (list (make-content-text "before\n```\nafter")))))))))))
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
  (test-assert "metadata items are declarative presentation data"
    (string-contains
      (assoc-ref
        (render-test-html
          (make-forest (make-site (list algebra limits)))
          #:mathjax-src #f
          #:presentation
          (make-html-presentation
              #:metadata-items
              '(("algebra" . ((li (@ (class "meta-item")) "declared"))))))
        "algebra")
      "<div class=\"metadata\"><ul><li class=\"meta-item\">declared</li></ul></div>"))
  (test-assert "at-exp link resolves to a page"
    (string-contains algebra-html "href=\"/limits.html\">limits</a>"))
  (test-assert "standalone pages use the open tree disclosure"
    (and (string-contains algebra-html
                          "class=\"forest-tree-block page-tree\" data-tree-id=\"algebra\"")
         (string-contains algebra-html
                          "<details open=\"open\"><summary><h1 class=\"forest-tree-title\"><span>Algebra &amp; limits</span>")
         (string-contains algebra-html "class=\"forest-tree-content\"><p>")))
  (test-assert "page title and body share the collapsible tree"
    (and (not (string-contains algebra-html
                              "</header><h1>Algebra &amp; limits"))
         (string-contains algebra-html "id=\"tree-root-algebra\"")))
  (test-assert "links produce reverse links"
    (and (string-contains algebra-html "class=\"backmatter backlinks\"")
         (string-contains algebra-html
                          "data-tree-id=\"limits\"")
         (string-contains algebra-html "<span>Limits</span>")
         (string-contains algebra-html "[limits]")))
  (test-assert "reverse links identify their source by title, not alt"
    (and (string-contains limits-html
                          "<span>Algebra &amp; limits</span>")
         (not (string-contains limits-html "<span>limits</span>"))))
  (test-assert "backlinks have a semantic heading"
    (string-contains limits-html "<h2>Backlinks</h2>"))
  (test-assert "backmatter headings use the renderer message catalog"
    (and (string-contains
           (assoc-ref (render-test-html
                        (make-forest (make-site (list algebra limits)))
                        #:messages (backmatter-messages "Link sources"))
                      "limits")
           "<h2>Link sources</h2>")
         (string-contains
           (assoc-ref (site->text
                        (make-forest (make-site (list algebra limits)))
                        #:options
                        (make-text-options
                          #:messages (backmatter-messages "Link sources")))
                      "limits")
           "Link sources:")))
  (test-assert "inline math keeps TeX delimiters"
    (string-contains algebra-html "\\(x^2+y^2=z^2\\)"))
  (test-assert "display math keeps TeX delimiters"
    (string-contains algebra-html "\\[a^2+b^2=c^2 \\tag{1}\\]"))
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

(let* ((forest (make-forest (make-site (list algebra limits))))
       (rules (make-render-rules
                (list (cons 'paragraph
                            (lambda (context element _payload next)
                              (if (render-context-ref context 'page)
                                  "HASPAGE\n" "NOPAGE\n")))))))
  (test-assert "text rules see the current page context"
    (string-contains
      (assoc-ref (site->text forest
                            #:options (make-text-options #:rules rules))
                 "algebra")
      "HASPAGE")))

(let* ((math (make-content-element 'math (list "x")
                                   #:properties '((tex? . #t))))
       (rules (make-render-rules
                (list (cons 'math
                            (lambda (context element _payload next)
                              "MATH-RULE"))))))
  (test-equal "text rules can override math"
    "MATH-RULE"
    (content->text (make-content-document (list math)) #:rules rules))
  (test-equal "text math rules can delegate to the built-in renderer"
    "$x$"
    (content->text
      (make-content-document (list math))
      #:rules
      (make-render-rules
        (list (cons 'math
                    (lambda (context element _payload next)
                      (next))))))))

(let ((rules (make-render-rules
               (list (cons 'paragraph
                           (lambda (context element _payload next) "X"))))))
  (test-assert "custom text rules disable caching without a rules id"
    (procedure?
      (make-text-renderer
        #:options (make-text-options #:rules rules))))
  (test-assert "text rules with an id stay cacheable"
    (cached-renderer?
      (make-text-renderer
        #:options (make-text-options #:rules rules)
        #:rules-id 'v1)))
  (test-assert "text renderer can disable its cache explicitly"
    (procedure? (make-text-renderer #:cache? #f))))

(let ((rules (make-render-rules
               (list (cons 'paragraph
                           (lambda (context element _payload next) "X"))))))
  (test-assert "custom HTML rules disable caching without a rules id"
    (procedure?
      (make-html-renderer
        #:options
        (make-html-options
          #:presentation (make-html-presentation #:rules rules)))))
  (test-assert "HTML rules with an id stay cacheable"
    (cached-renderer?
      (make-html-renderer
        #:options
        (make-html-options
          #:presentation (make-html-presentation #:rules rules))
        #:rules-id 'v1)))
  (test-assert "HTML renderer can disable its cache explicitly"
    (procedure? (make-html-renderer #:cache? #f)))
  (test-assert "custom gemtext rules disable caching without a rules id"
    (procedure?
      (make-gemtext-renderer
        #:options (make-gemtext-options #:rules rules))))
  (test-assert "gemtext rules with an id stay cacheable"
    (cached-renderer?
      (make-gemtext-renderer
        #:options (make-gemtext-options #:rules rules)
        #:rules-id 'v1)))
  (test-assert "Gemtext renderer can disable its cache explicitly"
    (procedure? (make-gemtext-renderer #:cache? #f))))

(let ((rules (make-render-rules '())))
  (test-assert "explicit empty rules keep all stock renderers cacheable"
    (and (cached-renderer?
           (make-html-renderer
             #:options
             (make-html-options
               #:presentation (make-html-presentation #:rules rules))))
         (cached-renderer?
           (make-text-renderer #:options (make-text-options #:rules rules)))
         (cached-renderer?
           (make-gemtext-renderer
             #:options (make-gemtext-options #:rules rules))))))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (rules-id (string-copy "rules-v1"))
       (renderers
         (list (make-html-renderer #:rules-id rules-id)
               (make-text-renderer #:rules-id rules-id)
               (make-gemtext-renderer #:rules-id rules-id)))
       (before (renderer-configurations forest renderers)))
  (string-set! rules-id 0 (integer->char 88))
  (test-equal "renderer rules IDs are snapshotted at construction"
    before
    (renderer-configurations forest renderers)))

(test-assert "declarative HTML presentation remains cacheable"
  (cached-renderer?
    (make-html-renderer
      #:options
      (make-html-options
        #:presentation
        (make-html-presentation
                     #:title-decorations
                     '(("algebra" . ((span "Algebra")))))))))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (renderer-configuration-for
        (lambda (content sources)
          (let* ((style
                   (compose-html-styles
                     default-html-style
                     (make-html-style
                       #:stylesheets
                       (list (make-artifact "custom.css" content
                                            #:sources sources)))))
                 (renderer
                   (make-html-renderer
                     #:options
                     (make-html-options
                       #:presentation
                       (make-html-presentation #:style style)))))
            (renderer-configurations forest (list renderer))))))
  (test-equal "source-backed HTML style changes keep renderer identity stable"
    (renderer-configuration-for "body { color: red; }" '("/tmp/custom.css"))
    (renderer-configuration-for "body { color: blue; }" '("/tmp/custom.css")))
  (test-assert "inline HTML style content participates in renderer identity"
    (not (equal? (renderer-configuration-for "body { color: red; }" '())
                 (renderer-configuration-for "body { color: blue; }" '())))))

(let* ((forest (make-forest (make-site (list algebra limits)
                                       #:home "algebra")))
       (renderer-configurations-for
        (lambda (messages)
          (renderer-configurations
            forest
            (list (make-html-renderer
                    #:options
                    (make-html-options #:messages messages)
                    #:rules-id 'message-test)
                  (make-text-renderer
                    #:options (make-text-options #:messages messages))
                  (make-gemtext-renderer
                    #:options (make-gemtext-options #:messages messages)))))))
  (test-assert "backmatter message changes invalidate renderer cache configurations"
    (not (equal? (renderer-configurations-for
                   (backmatter-messages "First label"))
                 (renderer-configurations-for
                   (backmatter-messages "Second label"))))))

(test-error "HTML options reject a missing routes value at construction"
  #t
  (make-html-options #:routes #f))
(test-error "text options reject invalid backmatter at construction" #t
  (make-text-options #:backmatter '(context context)))
(test-error "Gemtext options reject invalid backmatter at construction" #t
  (make-gemtext-options #:backmatter '(context context)))
(test-error "Gemtext renderer rejects invalid cache flags at construction" #t
  (make-gemtext-renderer #:cache? 'yes))
(test-error "HTML renderer rejects invalid cache flags at construction" #t
  (make-html-renderer #:cache? 'yes))
(test-error "text renderer rejects invalid cache flags at construction" #t
  (make-text-renderer #:cache? 'yes))
(test-error "renderer options reject invalid message catalogs at construction" #t
  (make-text-options #:messages 'not-a-catalog))
(test-error "Gemtext options reject invalid message catalogs at construction" #t
  (make-gemtext-options #:messages 'not-a-catalog))
(test-error "renderer options reject empty locale tags at construction" #t
  (make-text-options #:locale ""))
(test-error "Gemtext options reject empty locale tags at construction" #t
  (make-gemtext-options #:locale ""))
(test-error "HTML options reject malformed root attributes at construction" #t
  (make-html-options #:html-attributes '((lang . 42))))
(test-error "HTML options reject duplicate root attributes at construction" #t
  (make-html-options #:html-attributes '((lang . "en") (lang . "zh"))))
(test-error "HTML options reject invalid side-lane kinds at construction" #t
  (make-html-options #:side-lane '(sidenote "figure")))
(test-error "HTML options reject invalid settings at construction" #t
  (make-html-options #:settings 'not-site-settings))
(test-error "HTML options reject malformed site URLs" #t
  (make-html-options #:site-url "https:///path"))
(test-error "HTML options reject deployment paths in site origins" #t
  (make-html-options #:site-url "https://example.org/notes/"))
(test-error "HTML options reject invalid default locales at construction" #t
  (make-html-options #:default-locale ""))
(test-error "HTML options reject invalid Open Graph image values" #t
  (make-html-options #:og-image 42))
(let* ((backmatter (list 'context))
       (options (make-text-options #:backmatter backmatter))
       (forest (make-forest (make-site (list algebra limits)))))
  (set-car! backmatter 'invalid-section)
  (test-assert "text options snapshot caller-owned backmatter lists"
    (list? (site->text forest #:options options))))
(let ((backmatter (cons 'context '())))
  (set-cdr! backmatter backmatter)
  (test-error "text options reject circular backmatter"
    #t
    (make-text-options #:backmatter backmatter)))
(let* ((attributes (list (cons 'lang "en")))
       (options
         (make-html-options
           #:routes (make-html-routes #:home-page "algebra")
           #:html-attributes attributes))
       (forest (make-forest (make-site (list algebra limits)))))
  (set-cdr! (car attributes) "zh")
  (test-assert "HTML options snapshot caller-owned attribute pairs"
    (string-contains
      (cdr (assoc "algebra" (site->html forest #:options options)))
      "lang=\"en\"")))
(let* ((mathjax-src (string-copy "assets/mathjax.js"))
       (options
         (make-html-options
           #:routes (make-html-routes #:home-page "algebra")
           #:mathjax-src mathjax-src))
       (forest (make-forest (make-site (list algebra limits)))))
  (string-set! mathjax-src 0 #\X)
  (test-assert "HTML options snapshot their MathJax source"
    (string-contains
      (cdr (assoc "algebra" (site->html forest #:options options)))
      "src=\"/assets/mathjax.js\"")))
(test-error "HTML renderer rejects unstable rules IDs" #t
  (make-html-renderer #:rules-id '(unstable value)))
(test-error "text renderer rejects unstable rules IDs" #t
  (make-text-renderer #:rules-id '(unstable value)))
(test-error "Gemtext renderer rejects unstable rules IDs" #t
  (make-gemtext-renderer #:rules-id '(unstable value)))
(test-error "site->html rejects a value that is not HTML options" #t
  (site->html #f #:options #f))
(test-error "render-html rejects a value that is not HTML options" #t
  (render-html #f #:options #f))
(test-error "make-html-renderer rejects a value that is not HTML options" #t
  (make-html-renderer #:options #f))
(let ((highlighter (cdr (assq 'scheme (default-highlighters)))))
  (test-error "HTML options reject duplicate highlighter keys" #t
    (make-html-options
      #:highlighters (list (cons 'scheme highlighter)
                           (cons 'scheme highlighter)))))
(test-error "site->text rejects a value that is not text options" #t
  (site->text #f #:options #f))
(test-error "render-text rejects a value that is not text options" #t
  (render-text #f #:options #f))
(test-error "render-text rejects a value that is not a forest" #t
  (render-text #f))
(test-error "make-text-renderer rejects a value that is not text options" #t
  (make-text-renderer #:options #f))
(test-error "site->gemtext rejects a value that is not Gemtext options" #t
  (site->gemtext #f #:options #f))
(test-error "render-gemtext rejects a value that is not Gemtext options" #t
  (render-gemtext #f #:options #f))
(test-error "make-gemtext-renderer rejects a value that is not Gemtext options" #t
  (make-gemtext-renderer #:options #f))

(let* ((forest (make-forest (make-site (list algebra limits)
                                       #:home "algebra")))
       (artifacts (render-html forest #:options (make-html-options))))
  (test-assert "HTML options derive the default route from the site home"
    (find (lambda (artifact)
            (string=? (artifact-path artifact) "index.html"))
          artifacts)))

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

(let* ((target
          (make-site-page "target" "Target"
            (evaluate-document
              "@subtree[#:id \"overview\"]{@title{Overview}@paragraph{Body}@subtree[#:id \"details\"]{@title{Details}@paragraph{Nested body}}}")))
       (source
          (make-site-page "source" "Source"
            (evaluate-document "@paragraph{@link[\"overview\"]{the overview}}")))
       (forest (make-forest (make-site (list target source))))
       (html (render-test-html forest))
       (text (site->text forest)))
  (let ((reference (car (outgoing-references forest "source"))))
    (test-assert "tree reference queries return typed reference values"
      (reference? reference))
    (test-equal "tree references preserve source, identity and label"
      '("source" "ref-1" "overview" "the overview")
      (list (reference-source-page-id reference) (reference-source-id reference)
            (reference-target-page-id reference) (reference-label reference))))
  (test-assert "tree directory follows nested display structure"
    (and (string-contains (assoc-ref html "target") "href=\"#overview\">Overview</a>")
         (string-contains (assoc-ref html "target") "href=\"#details\">Details</a>")))
  (test-assert "title levels follow display depth"
    (and (string-contains (assoc-ref html "target") "<h2 class=\"forest-tree-title\"><span>Overview</span>")
         (string-contains (assoc-ref html "target") "<h3 class=\"forest-tree-title\"><span>Details</span>")))
  (test-assert "tree references use independent routes"
    (and (string-contains (assoc-ref html "source") "href=\"/overview.html\">the overview</a>")
         (string-contains (assoc-ref text "source") "overview.txt")))
  (test-assert "backlinks belong to the referenced tree"
    (and (string-contains (assoc-ref html "overview")
                          "data-tree-id=\"source\"")
         (string-contains (assoc-ref html "overview")
                          "<span>Source</span>")
         (null? (incoming-references forest "target"))))
  (test-assert "text directory and body share occurrence anchors"
    (and (string-contains (assoc-ref text "target") "- Overview [#overview]")
         (string-contains (assoc-ref text "target") "  - Details [#details]")))
  (test-equal "tree link queries use the same identities"
    '("overview") (links forest "source")))

(let* ((theorems
         (make-site-page "theorems" "Theorems"
           (evaluate-document
             (string-append
               "@subtree[#:id \"theorem-3\"]{@title{Theorem 3}@paragraph{Statement.}}\n"
               "@subtree[#:id \"lemma-9\" #:anchor \"lemma-nine\"]{@title{Lemma 9}@paragraph{Proof.}}"))))
       (notes
         (make-site-page "notes" "Notes"
           (evaluate-document
             (string-append
               "@transclude[\"theorem-3\" #:anchor \"theorem-3-note\"]\n"
               "@transclude[\"lemma-9\"]\n"
               "@paragraph{See @link[\"theorems#theorem-3\"]{the theorem}.}"))))
       (forest (make-forest (make-site (list notes theorems))))
       (html (render-test-html forest)))
  (test-assert "explicit subtree ids anchor their host occurrence"
    (and (string-contains (assoc-ref html "theorems")
                          "data-tree-id=\"theorem-3\" id=\"theorem-3\"")
         (not (string-contains (assoc-ref html "theorems") "id=\"tree-1\""))
         (not (string-contains (assoc-ref html "theorems") "<details id="))))
  (test-assert "subtree anchors override the tree id default"
    (and (string-contains (assoc-ref html "theorems")
                          "data-tree-id=\"lemma-9\" id=\"lemma-nine\"")
         (not (string-contains (assoc-ref html "theorems")
                               " id=\"lemma-9\""))))
  (test-assert "unique transclusions anchor with the tree id"
    (and (string-contains (assoc-ref html "notes")
                          "data-tree-id=\"theorem-3\" id=\"theorem-3-note\"")
         (string-contains (assoc-ref html "notes")
                          "data-tree-id=\"lemma-9\" id=\"lemma-9\"")))
  (test-assert "backmatter entry outlines the citing tree"
    ;;     The entry panel carries the citing tree's outline, linking
    ;;     into the real page's sections; the fetch script upgrades it
    ;;     to full content on first expand.  Notes is the home page in
    ;;     these routes, so compute its URL instead of assuming it.
    (let* ((routes (make-html-routes #:home-page "notes"))
           (notes-url (html-page-url routes "notes"))
           (html (assoc-ref html "theorems")))
      (and (string-contains html "class=\"tree-outline\"")
           (string-contains html
             (string-append "href=\"" notes-url "#theorem-3-note\""))
           (string-contains html
             (string-append "href=\"" notes-url "#lemma-9\""))
           (string-contains html "data-tree-id=\"notes\""))))
  (test-assert "anchor links route through the host page with their fragment"
    (string-contains (assoc-ref html "notes")
                     "href=\"/theorems.html#theorem-3\"")))

(let* ((definitions
         (make-site-page "definitions" "Definitions"
           (evaluate-document
             "@subtree[#:id \"theorem-3\"]{@title{Theorem 3}@paragraph{Statement.}}")))
       (scratch
         (make-site-page "scratch" "Scratch"
           (evaluate-document "@subtree{@title{Loose}@paragraph{Body.}}")))
       (forest (make-forest (make-site (list scratch definitions))))
       (html (render-test-html forest)))
  (test-assert "unnamed subtrees anchor with their unstable id"
    (string-contains (assoc-ref html "scratch")
                     "data-tree-id=\"unstable-0\" id=\"unstable-0\"")))

(let* ((definitions
         (make-site-page "definitions" "Definitions"
           (evaluate-document
             "@subtree[#:id \"theorem-3\"]{@title{Theorem 3}@paragraph{Statement.}}")))
       (host
         (make-site-page "host" "Host"
           (evaluate-document
             (string-append
               "@transclude[\"theorem-3\"]\n"
               "@transclude[\"theorem-3\"]\n"
               "@paragraph{See @link[\"host#theorem-3\"]{here}.}"))))
       (port (open-output-string))
       (forest
         (parameterize ((current-warning-port port))
           (make-forest (make-site (list host definitions)))))
       (html (render-test-html forest))
       (message (get-output-string port)))
  (test-assert "repeated occurrences fall back to position anchors and warn"
    (and (string-contains (assoc-ref html "host") "id=\"tree-1\"")
         (string-contains (assoc-ref html "host") "id=\"tree-2\"")
         (not (string-contains (assoc-ref html "host")
                               "data-tree-id=\"theorem-3\" id=\"theorem-3\""))
         (string-contains message "does not resolve in page host"))))

(let* ((target
         (make-site-page "styled-target" "Styled target"
           (evaluate-document "@paragraph{Inner body}")))
       (host
         (make-site-page "styled-host" "Styled host"
           (make-content-document
             (list (make-content-element
                     'transclude '()
                     #:properties '((target . "styled-target"))
                     #:style (merge-content-style
                               (make-content-style 'aside-box)
                               (make-content-style #f
                                 (list (make-html-attributes
                                         '((data-tone . "quiet"))))))
                     #:flow-kind 'block)))))
       (forest (make-forest (make-site (list host target))))
       (html (assoc-ref (render-test-html forest) "styled-host")))
  (test-assert "tree transclusions attach their style to the container"
    (string-contains html
      "class=\"forest-tree-block transclusion aside-box\" data-tree-id=\"styled-target\" id=\"styled-target\" data-tone=\"quiet\""))
  (test-error "tree container styles cannot set an id" #t
    (make-forest
      (make-site
        (list (make-site-page "host2" "Host2"
                (make-content-document
                  (list (make-content-element
                          'transclude '()
                          #:properties '((target . "styled-target"))
                          #:style (make-content-style #f
                                    (list (make-html-attributes '((id . "mine")))))
                          #:flow-kind 'block)))
                #:source target))))))

(let* ((equations
         (make-site-page "equations" "Equations"
           (evaluate-document
             (string-append
               "@tex-display-math{a^2}\n"
               "@tex-display-math[#:id \"named\"]{b^2}\n"
               "@tex-display-math{c^2 \\tag{9}}"))))
       (chaptered
         (make-site-page "chaptered" "Chaptered"
           (evaluate-document "@tex-display-math{x}")
           #:metadata '((chapter . 2))))
       (rendered (render-test-html
                   (make-forest (make-site (list equations chaptered)))
                   #:mathjax-src #f))
       (html (assoc-ref rendered "equations")))
  (test-assert "display equations render build numbers and generated ids"
    (and (string-contains html "\\[a^2 \\tag{1}\\]")
         (string-contains html "id=\"unstable-eq-1\"")
         (string-contains html "\\[b^2 \\tag{2}\\]")
         (string-contains html "id=\"named\"")
         (not (string-contains html "id=\"unstable-eq-2\""))
         (string-contains html "\\[c^2 \\tag{9}\\]")
         (string-contains html "id=\"unstable-eq-3\"")
         (not (string-contains html "\\tag{3}"))))
  (test-assert "declared chapters prefix equation numbers"
    (string-contains (assoc-ref rendered "chaptered")
                     "\\[x \\tag{2.1}\\]")))

(test-error "a raw # in a link target is rejected (fragments are separate)" #t
  (make-forest
    (make-site (list (make-site-page "target" "Target" (make-content-document '()))
                    (make-site-page "source" "Source"
                      (make-content-document
                        (list (make-content-element 'link '("missing")
                                #:properties '((target . "target#heading"))))))))))

(let* ((forest (make-forest (make-site (list algebra limits)
                                       #:title "Linked Notes"
                                       #:description "Connected notes on math.")))
       (artifacts
         (render-model
           forest
           (list (make-html-renderer
                   #:options
                   (make-html-options
                     #:routes (make-html-routes #:home-page "algebra")))
                 (lambda (value)
                   (render-rss
                     value
                     #:site-url "https://notes.example/"
                     #:routes (make-html-routes
                                #:home-page "algebra"
                                #:page-path
                                (lambda (id)
                                  (string-append "notes/" id ".html")))))
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
                                         "index.html"))
                             artifacts)))
       (algebra-text (artifact-content
                       (find (lambda (artifact)
                               (string=? (artifact-path artifact)
                                         "text/algebra.txt"))
                             artifacts))))
  (test-assert "HTML, text, and RSS renderers compose into one artifact set"
    (and (member "index.html" paths)
         (not (member "algebra.html" paths))
         (member "limits.html" paths)
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
               #:options
               (make-html-options
                 #:routes (make-html-routes #:home-page "algebra")
                 #:presentation
                 (make-html-presentation
                   #:style (compose-html-styles default-html-style
                             (make-html-style
                               #:stylesheets (list custom)))))))
           (stylesheet-paths (map artifact-path (take styled-artifacts 3)))
           (styled-index
             (artifact-content
               (find (lambda (artifact)
                       (string=? (artifact-path artifact) "index.html"))
                     styled-artifacts)))
           (base-position (string-contains styled-index "forester.css"))
           (custom-position (string-contains styled-index "custom.css")))
      (and (equal? stylesheet-paths '("forester.css" "highlight.css" "custom.css"))
           base-position
           custom-position
           (< base-position custom-position))))
  (test-assert "RSS emits escaped metadata and absolute routed links"
    (and (string-contains feed "<title>Algebra &amp; limits</title>")
         (string-contains feed
           "https://notes.example/index.html")
         (string-contains feed "https://notes.example/notes/limits.html")
         (string-contains feed "&amp;")
         (string-contains feed
           "<description>Connected notes on math.</description>")))
  (test-assert "HTML index directly renders its configured home page"
    (and (string-contains algebra-html "Algebra &amp; limits")
         (not (string-contains algebra-html "http-equiv=\"refresh\""))))
  (test-assert "HTML pages prefer their own summary"
    (string-contains algebra-html
      "<meta name=\"description\" content=\"Algebra notes and mathematical ideas.\""))
  (test-assert "HTML pages fall back to the site description"
    (string-contains limits-html
      "<meta name=\"description\" content=\"Connected notes on math.\""))
  (test-assert "HTML page titles expose their stable tree ID"
    (and (string-contains algebra-html "[algebra]</a>")
         (string-contains algebra-html "href=\"/index.html\"")))
  (test-assert "HTML renderer produces standalone HTML artifacts"
    (string-contains algebra-html "<html lang=\"en\">"))
  (test-assert "text renderer produces independent text artifacts"
    (string-contains algebra-text "are useful")))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (routes (make-html-routes #:home-page "algebra"))
       (artifacts (render-html forest
                              #:options (make-html-options #:routes routes)))
       (index (artifact-content
                (find (lambda (artifact)
                        (string=? (artifact-path artifact) "index.html"))
                      artifacts))))
  (test-assert "configured home page is rendered directly at the index path"
    (and (string-contains index "Algebra &amp; limits")
         (string-contains index "The ")
         (not (string-contains index "http-equiv=\"refresh\""))
         (equal? '("index.html" "limits.html")
                 (map artifact-path
                      (filter (lambda (artifact)
                                (string-suffix? ".html" (artifact-path artifact)))
                              artifacts)))))
  (test-error "configured HTML home page must exist" #t
    (render-html forest
      #:options
      (make-html-options
        #:routes (make-html-routes #:home-page "missing"))))
  (test-error "non-home HTML routes cannot collide with the index" #t
    (render-html forest
      #:options
      (make-html-options
        #:routes (make-html-routes
                   #:home-page "algebra"
                   #:page-path (lambda (_id) "index.html"))))))

(let* ((html (render-test-html (make-forest (make-site (list algebra limits)))))
       (limits-html (assoc-ref html "limits")))
  (test-assert "pages without mathematics do not load MathJax"
    (not (string-contains limits-html "mathjax"))))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (html (site->html
               forest
               #:options
               (make-html-options
                 #:routes (make-html-routes
                            #:home-page "algebra"
                            #:page-path
                            (lambda (id)
                              (string-append "notes/" id ".html")))
                 #:mathjax-src "assets/mathjax.js")))
       (algebra-html (assoc-ref html "algebra")))
  (test-assert "projects can provide a local MathJax script"
    (string-contains algebra-html "src=\"/assets/mathjax.js\"")))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (html (render-test-html
               forest #:mathjax-src "HTTPS://cdn.example.org/mathjax.js")))
  (test-assert "external URL schemes are case-insensitive"
    (string-contains
      (assoc-ref html "algebra")
      "src=\"HTTPS://cdn.example.org/mathjax.js\"")))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (html (render-test-html forest #:mathjax-src #f)))
  (test-assert "projects can disable browser-side math rendering"
    (not (string-contains (assoc-ref html "algebra") "mathjax"))))

(let* ((page
         (make-site-page
           "preview" "Preview"
           (make-content-document
             (list
               (make-content-element 'paragraph '("Details"))
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
           feed "https://notes.example/published/index.html")
         (string-contains
           feed "https://notes.example/published/notes/limits.html")))
  (test-assert "sitemap absolute links include the HTML URL prefix"
    (and (string-contains
           sitemap "https://notes.example/published/notes/limits.html")
         (string-contains
           sitemap "https://notes.example/published/index.html")
         (not (string-contains
                sitemap "https://notes.example/published/notes/algebra.html")))))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (feed (artifact-content
               (car (render-rss forest #:site-url "https://notes.example"))))
       (styled-feed
         (artifact-content
           (car (render-rss forest
                  #:site-url "https://notes.example"
                  #:stylesheet-url "feed.xsl?theme=light&view=full"))))
       (css-feed
         (artifact-content
           (car (render-rss forest
                  #:site-url "https://notes.example"
                  #:stylesheet-url "/feed.css"
                  #:stylesheet-type "text/css")))))
  (test-assert "RSS stylesheet is omitted by default"
    (not (string-contains feed "xml-stylesheet")))
  (test-assert "RSS can link an escaped XSL stylesheet"
    (string-contains styled-feed
      (string-append
        "<?xml-stylesheet type=\"text/xsl\" "
        "href=\"feed.xsl?theme=light&amp;view=full\"?>\n<rss")))
  (test-assert "RSS can link a CSS stylesheet"
    (string-contains css-feed
      "<?xml-stylesheet type=\"text/css\" href=\"/feed.css\"?>"))
  (test-error "RSS stylesheet URLs cannot contain whitespace" #t
    (render-rss forest #:site-url "https://notes.example"
      #:stylesheet-url "feed style.xsl"))
  (test-error "RSS stylesheet types cannot contain whitespace" #t
    (render-rss forest #:site-url "https://notes.example"
      #:stylesheet-url "feed.css" #:stylesheet-type "text/css; bad")))

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
         (< older-position undated-position)))
  (test-error "RSS rejects an invalid published date in a constructed forest"
    #t
    (render-rss
      (make-forest
        (make-site
          (list (make-site-page
                  "invalid-published" "Invalid published date"
                  (make-content-document '())
                  #:metadata '((published . "2026-02-31"))))))
      #:site-url "https://notes.example"))
  (test-error "RSS rejects an invalid updated date in a constructed forest"
    #t
    (render-rss
      (make-forest
        (make-site
          (list (make-site-page
                  "invalid-updated" "Invalid updated date"
                  (make-content-document '())
                  #:metadata '((updated . "2026-99-01"))))))
      #:site-url "https://notes.example")))

(let* ((forest
         (make-forest
           (make-site
             (list (make-site-page
                     "a&b" "A and B" (make-content-document '())
                     #:source "notes/a&b.plant"
                     #:metadata '((updated . "2026-09-27")))
                   (make-site-page
                     "plain" "Plain" (make-content-document '())
                     #:source "notes/plain.plant"))
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
  (test-assert "sitemap lists each canonical page route once"
    (and (string-contains
           content
           "<loc>https://notes.example/home/index.html</loc>")
         (not (string-contains content "notes/a&amp;b.html"))
         (string-contains
           content
           "<loc>https://notes.example/notes/plain.html</loc>\n  </url>")))
  (test-error "sitemap rejects invalid updated dates" #t
    (render-sitemap
      (make-forest
        (make-site
          (list (make-site-page
                  "invalid" "Invalid" (make-content-document '())
                  #:metadata '((updated . "2026-02-31"))))))
      #:site-url "https://notes.example"
      #:routes (make-html-routes #:home-page "invalid"))))

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
                 #:source "alpha.plant")
               (make-site-page "beta" "Beta" (make-content-document '())
                               #:source "beta.plant")))))
       (routes
         (make-text-routes
           #:index-path "index.txt"
           #:page-path
           (lambda (id) (string-append "notes/" id ".md"))))
       (artifacts (render-text forest
                              #:options (make-text-options #:routes routes)))
       (index (artifact-content (car artifacts)))
       (alpha (artifact-content (cadr artifacts)))
       (delegated-alpha
         (artifact-content
           (cadr
             (render-text forest
               #:options
               (make-text-options
                 #:routes routes
                 #:rules
                 (make-render-rules
                   (list (cons 'link
                               (lambda (_context _element _payload next)
                                 (next)))))))))))
  (test-equal "text routes configure the index and page output paths"
    '("index.txt" "notes/alpha.md" "notes/beta.md")
    (map artifact-path artifacts))
  (test-assert "text links resolve relative to the source page route"
    (string-contains alpha "Beta [beta.md]"))
  (test-equal "delegating rules preserve Forester's stock link handler"
    alpha delegated-alpha)
  (test-assert "nested text pages link back to the configured index"
    (string-contains alpha "Home: ../index.txt"))
  (test-assert "text index references configured page routes"
    (string-contains index "Alpha (notes/alpha.md)")))

(let* ((forest
         (make-forest
           (make-site
             (list (make-site-page "index" "Index page"
                                   (make-content-document '()))))))
       (routes
         (make-text-routes
           #:index-path "index.txt"
           #:page-path (lambda (id) (string-append "trees/" id ".txt"))))
       (text (site->text forest #:options (make-text-options #:routes routes)))
       (gemtext
         (site->gemtext forest
           #:options (make-gemtext-options #:routes routes))))
  (test-assert "site->text distinguishes the aggregate index from page ID index"
    (and (string-contains (assoc-ref text 'index) "Index page")
         (string-contains (assoc-ref text "index") "Index page")
         (not (string=? (assoc-ref text 'index) (assoc-ref text "index")))))
  (test-assert "site->gemtext distinguishes the aggregate index from page ID index"
    (and (string-contains (assoc-ref gemtext 'index) "Index page")
         (string-contains (assoc-ref gemtext "index") "Index page")
         (not (string=? (assoc-ref gemtext 'index)
                        (assoc-ref gemtext "index"))))))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (text-artifacts
         (render-text forest
           #:options (make-text-options #:prefix "text")))
       (gemtext-artifacts
         (render-gemtext forest
           #:options (make-gemtext-options #:prefix "gemini"))))
  (test-equal "render-text applies its configured output prefix"
    '("text/index.txt" "text/algebra.txt" "text/limits.txt")
    (map artifact-path text-artifacts))
  (test-equal "render-gemtext applies its configured output prefix"
    '("gemini/index.gmi" "gemini/algebra.gmi" "gemini/limits.gmi")
    (map artifact-path gemtext-artifacts)))


(let* ((forest (make-forest (make-site (list algebra limits))))
       (routes (make-html-routes #:home-page "algebra")))
  (for-each
    (lambda (name)
      (let* ((style (make-html-style
                      #:stylesheets (list (make-artifact name "body { color: teal; }"))))
             (artifacts (render-model
                          forest
                          (list (make-html-renderer
                                  #:options
                                  (make-html-options
                                    #:routes routes
                                    #:presentation
                                    (make-html-presentation #:style style))))))
             (html (artifact-content
                     (find (lambda (artifact) (string=? (artifact-path artifact) "index.html"))
                           artifacts))))
        (test-equal "explicit style is used without filename-based composition"
          (list name)
          (filter (lambda (path) (string-suffix? ".css" path))
                  (map artifact-path artifacts)))
        (test-assert "explicit style controls linked stylesheet contents"
          (and (string-contains html (string-append "href=\"/" name "\""))
               (string=? "body { color: teal; }" (artifact-content (car artifacts)))))
        (test-assert "explicit style controls inline stylesheet contents too"
          (let ((html (assoc-ref
                        (site->html forest
                                    #:options
                                    (make-html-options
                                      #:routes routes
                                      #:presentation
                                      (make-html-presentation #:style style)))
                        "algebra")))
            (and (string-contains html "body { color: teal; }")
                 (not (string-contains html ".forest-tree-block {")))))))
    '("theme.css" "forester.css"))
  (test-assert "an empty style opts out of default stylesheets"
    (not (any (lambda (artifact) (string-suffix? ".css" (artifact-path artifact)))
              (render-html forest
                           #:options
                           (make-html-options
                             #:routes routes
                             #:presentation
                             (make-html-presentation
                               #:style (make-html-style))))))))

(let* ((target
         (make-site-page "target" "Target"
           (evaluate-document
             "@subtree[#:id \"overview\"]{@title{Overview}@paragraph{Body}}")))
       (forest (make-forest (make-site (list target))))
       (routes (make-html-routes #:home-page "target"))
       (html (assoc-ref
               (site->html
                 forest
                 #:options
                 (make-html-options
                   #:routes routes
                   #:presentation
                   (make-html-presentation
                       #:title-decorations
                       '(("overview" . ((span (@ (class "taxon")) "overview. ")
                                         (span (@ (class "source")) "?")))))))
               "target")))
  (test-assert "title-decoration decorates tree headings"
    (string-contains html
      "<h2 class=\"forest-tree-title\"><span class=\"taxon\">overview. </span><span class=\"source\">?</span><span>Overview</span>"))
  (test-assert "title-decoration reads the page through the forest"
    (string-contains html
      "<span class=\"source\">?</span>"))
  (test-assert "title-decoration decorates the table of contents"
    (string-contains html
      "href=\"#overview\"><span class=\"taxon\">overview. </span><span class=\"source\">?</span>Overview</a>")))

(let* ((annotated
         (make-site-page "annotated" "Annotated"
           (evaluate-document "@paragraph{Body}")
           #:metadata '((summary . "A summary"))))
       (forest (make-forest (make-site (list annotated) #:home "annotated")))
       (html (assoc-ref
               (site->html
                 forest
                 #:options
                 (make-html-options
                   #:routes (make-html-routes)
                   #:presentation
                   (make-html-presentation
                       #:title-decorations
                       '(("annotated" . ((span (@ (class "summary")) "S")))))))
               "annotated")))
  (test-assert "title-decoration reads page metadata through the forest"
    (string-contains html "<span class=\"summary\">S</span>")))

(let* ((forest (make-forest (make-site (list algebra limits))))
       (routes (make-html-routes #:home-page "algebra"))
       (html (site->html
               forest
               #:options
               (make-html-options
                 #:routes routes
                 #:html-attributes '((lang . "zh-CN"))
                 #:presentation
                 (make-html-presentation
                     #:head '((link (@ (rel "alternate") (href "/feed.xml"))))
                     #:site-nav '((div (@ (class "controls")) "Controls")))
                 #:article? #t
                 #:backmatter-details? #t)))
       (algebra-html (assoc-ref html "algebra"))
       (limits-html (assoc-ref html "limits")))
  (test-assert "html attributes set document language"
    (string-contains algebra-html "<html lang=\"zh-CN\">"))
  (test-assert "presentation head appends nodes to the document head"
    (and (string-contains algebra-html "rel=\"alternate\"")
         (string-contains algebra-html "href=\"/feed.xml\"")))
  (test-assert "presentation navigation appends controls to the site navigation"
    (string-contains algebra-html
      "<div class=\"controls\">Controls</div></nav>"))
  (test-assert "article option wraps the page tree and backmatter"
    (and (string-contains algebra-html
           "<article><section class=\"forest-tree-block page-tree\"")
         (string-contains algebra-html "</article>")))
  (test-assert "backmatter-details option makes backmatter collapsible"
    (string-contains limits-html
      "<aside class=\"backmatter forest-tree-block backlinks\"><details open=\"open\"><summary><h2 class=\"forest-tree-title\">Backlinks</h2>"))
  (test-assert "declarative home link replaces the default navigation link"
    (let ((custom (assoc-ref
                    (site->html
                      forest
                      #:options
                      (make-html-options
                        #:routes routes
                        #:presentation
                        (make-html-presentation
                            #:home-link
                            (make-html-home-link '("Home")
                              #:attributes '((class . "brand"))))))
                    "algebra")))
      (and (string-contains custom "<a class=\"brand\" href=\"/index.html\">Home</a></nav>")
           (not (string-contains custom "Linked Notes</a></nav>"))))))

(let* ((text (lambda (value) (make-content-text value)))
       (target (make-site-page "target" "Target"
                 (make-content-document
                   (list (make-content-element 'paragraph (list (text "Body")))))))
       (source (make-site-page "source" "Source"
                 (make-content-document
                   (list (make-content-element 'paragraph
                           (list (make-content-element 'link (list (text "there"))
                                   #:properties '((target . "target")
                                                  (fragment . "section")))))))))
       (forest (make-forest (make-site (list source target)
                                       #:title "T" #:home "source")))
       (source-html (assoc-ref (render-test-html forest) "source"))
       (source-gemtext (assoc-ref (site->gemtext forest) "source"))
       (source-text (assoc-ref (site->text forest) "source")))
  (test-assert "HTML links carry the fragment"
    (string-contains source-html "href=\"/target.html#section\">there</a>"))
  (test-assert "Gemtext links carry the fragment"
    (string-contains source-gemtext "target.gmi#section"))
  (test-assert "text links carry the fragment"
    (string-contains source-text "target.txt#section")))

(let* ((text (lambda (value) (make-content-text value)))
       (target (make-site-page "target" "Target"
                 (make-content-document
                   (list (make-content-element 'paragraph (list (text "Body")))))))
       (query-link (make-content-element 'query-link (list (text "there"))
                     #:properties '((target . "target") (fragment . "section"))))
       (source (make-site-page "source" "Source"
                 (make-content-document
                   (list (make-content-element 'paragraph (list query-link))))))
       (forest (make-forest (make-site (list source target)
                                       #:title "T" #:home "source")))
       (source-html (assoc-ref (render-test-html forest) "source"))
       (source-gemtext (assoc-ref (site->gemtext forest) "source")))
  (test-assert "HTML query links carry the fragment"
    (string-contains source-html "href=\"/target.html#section\">there</a>"))
  (test-assert "Gemtext query links carry the fragment"
    (string-contains source-gemtext "target.gmi#section")))

(let* ((text (lambda (value) (make-content-text value)))
       (target (make-site-page "target" "Target"
                 (make-content-document
                   (list (make-content-element 'paragraph (list (text "Body")))))))
       (source (make-site-page "source" "Source"
                 (make-content-document
                   (list (make-content-element 'paragraph
                           (list (text "See ")
                                 (make-content-element 'link (list (text "a"))
                                   #:properties '((target . "target") (fragment . "one")))
                                 (text " and ")
                                 (make-content-element 'link (list (text "b"))
                                   #:properties '((target . "target") (fragment . "two")))))))))
       (forest (make-forest (make-site (list source target)
                                       #:title "T" #:home "source"))))
  (test-equal "fragment links to one page keep a single backlink"
    '("source") (map site-page-id (backlinks forest "target"))))

(let* ((page
         (make-site-page "annotated" "Annotated"
           (make-content-document
             (list (make-content-element
                     'paragraph
                     (list "Body with "
                           (make-content-element 'sidenote
                             (list "A marginal note."))
                           "."))))))
       (forest (make-forest (make-site (list page)))))
  (test-assert "side-lane lifts declared kinds into a margin lane"
    (let ((html (assoc-ref
                  (site->html
                    forest
                    #:options
                    (make-html-options
                      #:routes (make-html-routes #:home-page "annotated")
                      #:side-lane '(sidenote)))
                  "annotated")))
      (and (string-contains html "class=\"margin-lane\"")
           (string-contains html "A marginal note.")
           (string-contains html "class=\"margin-ref\"")
           (string-contains html "id=\"margin-1\""))))
  (test-assert "the margin lane is opt-in"
    (let ((html (assoc-ref
                  (site->html
                    forest
                    #:options
                    (make-html-options
                      #:routes (make-html-routes #:home-page "annotated")))
                  "annotated")))
      ;;     The default stylesheet (inlined by site->html) names the
      ;;     margin classes, so probe the emitted markup, not the CSS.
      (and (not (string-contains html "class=\"margin-lane\""))
           (not (string-contains html "class=\"margin-ref\""))))))

(let* ((dfn-element
         (lambda (anchor children)
           ;;     The vocabulary shape make-element produces: a span
           ;;     whose style carries the tag and whose properties
           ;;     declare the anchor.
           (make-content-element
             'span children
             #:properties `((anchor . ,anchor))
             #:style (merge-content-style
                       (make-content-style #f (list (make-html-tag "span")))
                       (make-content-style #f (list (make-html-tag "dfn")))))))
       (text (lambda (value) (make-content-text value)))
       (glossary
         (make-site-page "glossary" "Glossary"
           (make-content-document
             (list (make-content-element 'paragraph
                     (list (dfn-element "term-tree" (list (text "tree")))))
                   (make-content-element 'paragraph
                     (list (dfn-element "term-loom" (list (text "loom")))))))))
       (chapter
         (make-site-page "ch01" "Chapter"
           (make-content-document
             (list (make-content-element 'paragraph
                     (list (dfn-element "term-loom" (list (text "织机")))
                           " uses the "
                           (make-content-element 'link (list (text "glossary"))
                             #:properties '((target . "glossary")
                                            (fragment . "term-tree")))
                           " and the "
                           (make-content-element 'link (list (text "ghost"))
                             #:properties '((target . "glossary")
                                            (fragment . "term-ghost")))
                           " with a marked "
                           (make-content-element 'link '()
                             #:properties '((target . "glossary")
                                            (term . "term-tree")))
                           " and a marked "
                           (make-content-element 'query-link
                             (list (text "ghost mention"))
                             #:properties '((term . "term-ghost")))))))))
       (forest (make-forest (make-site (list glossary chapter)
                                       #:title "T" #:home "glossary")))
       (chapter-html
         (assoc-ref (site->html
                      forest
                      #:options
                      (make-html-options
                        #:routes (make-html-routes
                                   #:home-page "glossary")))
                    "ch01")))
  (test-equal "term definitions collect dfn occurrences across pages"
    '(("glossary" "term-loom" "loom")
      ("ch01" "term-loom" "织机"))
    (term-definitions forest "term-loom"))
  (test-equal "terms defined once are not conflicts"
    '() (filter (lambda (entry) (string=? (car entry) "term-tree"))
                (term-definition-conflicts forest)))
  (test-equal "term definition conflicts report host, page, and both displays"
    '(("term-loom" ("glossary" "Glossary" "glossary" "term-loom" "loom")
                  ("ch01" "Chapter" "ch01" "term-loom" "织机")))
    (term-definition-conflicts forest))
  (test-equal "unresolved fragment links are the definition backlog"
    '(("term-ghost" (("ch01" "Chapter" "ch01"))))
    (undefined-term-mentions forest))
  (test-assert "a resolved marked mention renders the canonical display"
    ;;     Glossary is the site home, so its route is /index.html.
    (string-contains chapter-html
                     "href=\"/index.html#term-tree\">tree</a>"))
  (test-assert "an undefined marked mention renders without a link"
    (string-contains chapter-html
                     "class=\"missing-term\">ghost mention</span>")))

(let* ((dfn (lambda (anchor children)
              (make-content-element
                'span children
                #:properties `((anchor . ,anchor))
                #:style (make-content-style #f (list (make-html-tag "dfn"))))))
       (text (lambda (value) (make-content-text value)))
       (article
         (make-site-page "article" "Article"
           (make-content-document
             (list (make-content-element 'paragraph
                     (list (dfn "term-sub" (list (text "root term")))))
                   (make-content-element 'subtree
                     (list (make-content-element 'paragraph
                             (list (dfn "term-sub"
                                       (list (text "subtree term"))))))
                     #:properties '((tree-id . "named-sub")
                                    (title . "Named Sub")))))))
       (forest (make-forest (make-site (list article)
                                       #:title "T" #:home "article"))))
  (test-equal "an in-subtree definition names its owning subtree"
    '(("article" "term-sub" "root term")
      ("named-sub" "term-sub" "subtree term"))
    (term-definitions forest "term-sub"))
  (test-equal "conflict projections resolve the subtree's host article"
    '(("term-sub" ("article" "Article" "article" "term-sub" "root term")
                 ("article" "Article" "named-sub" "term-sub" "subtree term")))
    (term-definition-conflicts forest)))


(let* ((forest (make-forest (make-site (list algebra limits))))
       (feed (artifact-content
               (car (render-rss forest #:site-url "https://notes.example"
                                #:language "zh-Hans")))))
  (test-assert "RSS declares its language"
    (string-contains feed "<language>zh-Hans</language>")))
(let* ((forest (make-forest (make-site (list algebra limits))))
       (routes (make-html-routes #:home-page "algebra")))
  (test-assert "renderer entry points accept options values"
    (and (pair? (site->html
                  forest
                  #:options
                  (make-html-options #:routes routes #:mathjax-src #f)))
         (pair? (site->text
                  forest
                  #:options (make-text-options #:locale "en")))
         (pair? (site->gemtext
                  forest
                  #:options (make-gemtext-options #:locale "en"))))))

(let* ((doi "doi:10.1146/annurev.fluid.29.1.123")
       (page (make-site-page "paper" "A paper"
               (evaluate-document "@paragraph{A short note.}")
               #:metadata `((describes . (,doi)))))
       (forest (make-forest (make-site (list page))))
       (html (assoc-ref
               (site->html forest
                 #:options (make-html-options
                             #:routes (make-html-routes #:home-page "paper")
                             #:mathjax-src #f))
               "paper"))
       (text (assoc-ref (site->text forest) "paper"))
       (gemtext (assoc-ref (site->gemtext forest) "paper")))
  (test-assert "HTML renders doi URIs as resolver links"
    (and (string-contains html "class=\"describes\"")
         (string-contains html "href=\"https://doi.org/10.1146/annurev.fluid.29.1.123\"")
         (string-contains html "doi:10.1146/annurev.fluid.29.1.123")))
  (test-assert "text and Gemtext expose described entity links"
    (and (string-contains text "https://doi.org/10.1146/annurev.fluid.29.1.123")
         (string-contains gemtext "=> https://doi.org/10.1146/annurev.fluid.29.1.123 doi:10.1146/annurev.fluid.29.1.123"))))

(let* ((page (make-site-page "paper" "A paper"
               (evaluate-document "@paragraph{A short note.}")
               #:metadata '((describes . ("https:")))))
       (forest (make-forest (make-site (list page))))
       (html (assoc-ref
               (site->html forest
                 #:options (make-html-options
                             #:routes (make-html-routes #:home-page "paper")
                             #:mathjax-src #f))
               "paper")))
  (test-assert "HTML does not link hostless HTTP(S) entity identifiers"
    (and (string-contains html "<code>https:</code>")
         (not (string-contains html "href=\"https:\"")))))

(test-end "forester renderers")

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
  #:use-module (forester input scribble)
  #:use-module (grove build)
  #:use-module (grove ir)
  #:use-module (grove render html)
  #:use-module (grove render html routes)
  #:use-module (grove output)
  #:use-module (grove relation dsl)
  #:use-module (tests unit forester fixtures)
  #:use-module (grove site)
  #:use-module (grove project)
  #:use-module (grove private project timing)
  #:use-module (grove style html))

(test-begin "forester site")

(let* ((phases '())
       (page (make-site-page "measured" "Measured"
                            (evaluate-document "A paragraph.")))
       (forest (make-forest (make-site (list page))
                 #:timings (lambda (phase seconds)
                             (set! phases (cons (cons phase seconds) phases))))))
  (test-equal "timing observation preserves the page"
    "Measured" (site-page-title (find-page forest "measured")))
  (test-equal "forest timings follow analysis order"
    '(identify-trees extract-trees occurrence-anchors equation-numbers
      references transclusions reference-index term-definitions page-index
      described-entities
      content composition anchors term-mentions queries)
    (map car (reverse phases)))
  (test-assert "forest timings report nonnegative seconds"
    (every (lambda (entry) (>= (cdr entry) 0)) phases)))

(let* ((phases '())
       (page (make-site-page "measured" "Measured"
                            (evaluate-document "A paragraph.")))
       (forest
         (parameterize ((current-analysis-timings
                          (lambda (phase seconds)
                            (set! phases (cons (cons phase seconds) phases)))))
           (make-forest (make-site (list page))))))
  (test-equal "analysis timings flow through the parameter hook"
    '(identify-trees extract-trees occurrence-anchors equation-numbers
      references transclusions reference-index term-definitions page-index
      described-entities
      content composition anchors term-mentions queries)
    (map car (reverse phases))))

(define (document-paragraph-texts document)
  (map (lambda (paragraph)
         (content-text-value
           (car (content-element-children paragraph))))
       (filter (lambda (element)
                 (and (content-element? element)
                      (eq? (content-element-kind element) 'paragraph)))
               (content-document-children document))))

(test-error "HTML routes reject an empty home page ID" #t
  (make-html-routes #:home-page ""))

(let* ((document (evaluate-document "@lipsum{}"))
       (page (make-site-page "lipsum" "Lipsum" document))
       (forest (make-forest (make-site (list page) #:home "lipsum")))
       (artifacts (render-html forest
                              #:options
                              (make-html-options
                                #:routes (make-html-routes)))))
  (test-assert "HTML routes derive the home page from the site"
    (find (lambda (artifact)
            (string=? "index.html" (artifact-path artifact)))
          artifacts)))

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

(test-error "duplicate occurrence anchors fail site analysis" #t
  (make-forest
    (make-site
      (list
        (make-site-page "defs" "Defs"
          (evaluate-document
            "@subtree[#:id \"theorem-3\"]{@title{Theorem 3}@paragraph{Statement.}}"))
        (make-site-page "notes" "Notes"
          (evaluate-document
            (string-append
              "@transclude[\"theorem-3\" #:anchor \"same\"]\n"
              "@transclude[\"theorem-3\" #:anchor \"same\"]")))))))

(test-error "subtree anchors and math IDs share one namespace" #t
  (make-forest
    (make-site
      (list (make-site-page "collide" "Collide"
              (evaluate-document
                (string-append
                  "@tex-math[#:id \"theorem-3\"]{x}\n"
                  "@subtree[#:id \"theorem-3\"]{@title{Theorem 3}@paragraph{Statement.}}")))))))

(test-error "occurrence anchors reject invalid anchor IDs" #t
  (evaluate-document "@transclude[\"theorem-3\" #:anchor \"bad anchor\"]"))

(test-error "auto anchors join the duplicate-anchor namespace" #t
  (make-forest
    (make-site
      (list
        (make-site-page "defs" "Defs"
          (evaluate-document
            "@subtree[#:id \"y\"]{@title{Y}@paragraph{b}}"))
        (make-site-page "host" "Host"
          (evaluate-document
            (string-append
              "@tex-math[#:id \"y\"]{a}\n"
              "@transclude[\"y\"]")))))))

(let* ((known-document
         (evaluate-document
           (string-append
             "@tex-display-math[#:id \"eq-1\"]{a}\n"
             "@subtree[#:id \"theorem-1\"]{@title{T}@paragraph{b}}\n"
             "@paragraph{See @link[\"notes#eq-1\"]{eq}, @link[\"notes#theorem-1\"]{t},"
             " and @link[\"notes#tree-root-notes\"]{root}.}")))
       (port (open-output-string))
       (forest
         (parameterize ((current-warning-port port))
           (make-forest
             (make-site (list (make-site-page "notes" "Notes" known-document))))))
       (message (get-output-string port)))
  (test-assert "declared anchors accept link fragments silently"
    (string-null? message)))

(let* ((stale-document
         (evaluate-document
           (string-append
             "@tex-display-math[#:id \"eq-1\"]{a}\n"
             "@paragraph{See @link[\"notes#eq-2\"]{eq} and @link[\"notes#tree-1\"]{first}.}")))
       (port (open-output-string))
       (forest
         (parameterize ((current-warning-port port))
           (make-forest
             (make-site (list (make-site-page "notes" "Notes" stale-document))))))
       (message (get-output-string port)))
  (test-assert "unknown fragments warn with fragment and target"
    (and (string-contains message "eq-2")
         (string-contains message "notes")
         (string-contains message "tree-1"))))

(let* ((container
         (evaluate-document
           (string-append
             "@subtree[#:id \"chapter\"]{@title{Chapter}\n"
             " @subtree[#:id \"section\"]{@title{Section}\n"
             "  @tex-display-math[#:id \"eq-1\"]{a}}}\n"
             "@paragraph{See @link[\"book#eq-1\"]{the equation}.}")))
       (port (open-output-string))
       (forest
         (parameterize ((current-warning-port port))
           (make-forest
             (make-site (list (make-site-page "book" "Book"
                                 container))))))
       (message (get-output-string port)))
  (test-assert "a container page's registry covers its subtree anchors"
    (string-null? message)))

(let* ((numbered-document
         (evaluate-document
           (string-append
             "@tex-display-math[#:id \"named\"]{a}\n"
             "@tex-display-math{b}\n"
             "@paragraph{See @link[\"notes#unstable-eq-2\"]{b} and"
             " @link[\"notes#unstable-eq-9\"]{missing}.}")))
       (port (open-output-string))
       (forest
         (parameterize ((current-warning-port port))
           (make-forest
             (make-site
               (list (make-site-page "notes" "Notes" numbered-document))))))
       (message (get-output-string port)))
  (test-assert "generated equation ids join the registry"
    (and (string-contains message "unstable-eq-9")
         (not (string-contains message "unstable-eq-2")))))

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
           'tag (list "tutorial")
           #:properties '((id . "tutorial"))))
       (taxon-node
         (make-content-element
           'taxon (list "mathematics")
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
    '(("query-article" "tutorial"))
    tutorial-pages)
  (test-equal "taxon IR is queryable through the same topic relation"
    '(("query-article" "mathematics"))
    math-pages)
  (test-equal "topic-pages resolves a taxon topic to its pages"
    '("query-article")
    (map site-page-id (topic-pages forest "mathematics" 'taxon)))
  (test-equal "repeated topic-pages calls return the memoized pages"
    '("query-article")
    (map site-page-id (topic-pages forest "mathematics" 'taxon)))
  (test-equal "topic-pages answers absent topics with an empty list"
    '()
    (topic-pages forest "nonexistent-topic"))
  (test-equal "lookup-page resolves known ids and misses unknown ones"
    (list "target" #f)
    (list (and (lookup-page forest "target")
               (site-page-id (lookup-page forest "target")))
          (lookup-page forest "no-such-page")))
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
         (text (assoc-ref (site->text forest
                                      #:options
                                      (make-text-options #:routes routes))
                          "catalog")))
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
                                              'subtree '()
                                              #:properties
                                              '((target . "other")))))))))))
                    "query.plant"))))
            #f)
          (lambda (key . args) (format #f "~s" (cons key args))))))
  (test-assert "query output cannot mutate Forest composition recursively"
    (string-contains message
      "Forester query at query.plant returned unsupported subtree content")))

(define (query-transclusion-page id target)
  (make-site-page id id
    (make-content-document
      (list (make-content-element 'forest-query '()
        #:properties `((query . ,(lambda (_forest _id)
          (make-content-document
            (list (make-content-element 'transclude '()
                    #:properties `((target . ,target)))))))))))))

(let* ((target (make-site-page "query-target" "Existing tree"
                 (evaluate-document "@paragraph{Existing body}")))
       (host (query-transclusion-page "query-host" "query-target"))
       (forest (make-forest (make-site (list host target))))
       (html (assoc-ref (render-test-html forest) "query-host")))
  (test-assert "queries can display an existing tree"
    (and (string-contains html "Existing body")
         (string-contains html "href=\"#tree-1\">Existing tree</a>")))
  (test-equal "query tree displays do not change authored composition"
    '() (incoming-transclusions forest "query-target"))
  (test-assert "query tree displays render in text"
    (string-contains (assoc-ref (site->text forest) "query-host") "Existing body")))

(test-error "query tree displays reject unknown identities" #t
  (make-forest (make-site (list (query-transclusion-page "query-host" "missing")))))

(test-error "query tree displays reject direct cycles" #t
  (make-forest (make-site (list (query-transclusion-page "query-host" "query-host")))))

(test-error "query tree displays reject cycles through authored composition" #t
  (make-forest
    (make-site
      (list (query-transclusion-page "query-host" "query-target")
            (make-site-page "query-target" "Target"
              (evaluate-document "@transclude[\"query-host\"]"))))))

(test-error "duplicate explicit math IDs in a page are rejected" #t
  (make-forest
    (make-site
      (list (make-site-page "dup" "Duplicate"
              (evaluate-document
                "@tex-display-math[#:id \"same\"]{x}
                 @tex-display-math[#:id \"same\"]{y}"))))))

(test-assert "distinct math IDs are accepted"
  (forest?
    (make-forest
      (make-site
        (list (make-site-page "ids" "IDs"
                (evaluate-document
                  "@tex-display-math[#:id \"one\"]{x}
                   @tex-display-math[#:id \"two\"]{y}")))))))

(let* ((page
         (make-site-page "chars" "Äbc"
           (evaluate-document
             "@paragraph{One two. @link[\"chars\"]{Ägain}}")))
       (forest (make-forest (make-site (list page)))))
  (test-equal "used codepoints collect titles and document text"
    (string->list " .OabceginotwÄ")
    (used-codepoints forest)))

(let* ((page (document->tree-page
               "roache"
               (evaluate-document
                 "@title{Quantification of Uncertainty}
                  @describes{doi:10.1146/annurev.fluid.29.1.123}
                  @paragraph{A useful paper.}")
               "roache.plant")))
  (test-equal "describes frontmatter is retained as URI metadata"
    '("doi:10.1146/annurev.fluid.29.1.123")
    (assq-ref (site-page-metadata page) 'describes))
  (test-equal "describes frontmatter is removed from rendered body"
    '(paragraph)
    (map content-element-kind
         (content-document-children (site-page-document page)))))

(test-error "describes requires an absolute URI" #t
  (document->tree-page "bad"
    (evaluate-document "@title{Bad}@describes{relative-id}") "bad.plant"))

(test-error "describes rejects duplicate URI declarations on one page" #t
  (document->tree-page "bad"
    (evaluate-document
      "@title{Bad}@describes{doi:10.1146/x}@describes{doi:10.1146/x}")
    "bad.plant"))

(test-end "forester site")

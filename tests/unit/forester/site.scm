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
  #:use-module (tests unit forester fixtures)
  #:use-module (grove site)
  #:use-module (grove style html))

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

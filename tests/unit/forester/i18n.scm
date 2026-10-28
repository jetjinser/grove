;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester i18n)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (system base language)
  #:use-module (language scribble spec)
  #:use-module (grove i18n)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render html)
  #:use-module (grove render text)
  #:use-module (grove site)
  #:use-module (grove style)
  #:use-module (grove style html)
  #:use-module (grove style html element)
  #:use-module (forester i18n)
  #:use-module (forester commands)
  #:use-module (forester render gemtext)
  #:use-module (forester render text)
  #:use-module (forester site)
  #:use-module (forester style i18n))

(test-begin "forester i18n")

(test-assert "bilingual lane grouping stays behind the authoring API"
  (not (module-variable
         (module-public-interface (resolve-module '(forester i18n)))
         'pair-translations)))

;;; Language lanes

(define english (lang "en" "The grove is a small forest."))
(define chinese (lang "zh-CN" "树林是一片小小的森林。"))

(test-equal "lang records its language and direction"
  '("en" . "ltr")
  (cons (content-element-property english 'lang)
        (content-element-property english 'dir)))
(test-equal "lang decodes prose into a paragraph"
  'paragraph
  (content-element-kind (car (content-element-children english))))
(test-equal "lang is a block element"
  'block (content-element-flow-kind english))
(test-assert "lang recognizes its own lane elements" (bilingual-lane? english))
(test-error "lang rejects unknown text directions" #t (lang "en" #:dir "up" "x"))
(test-error "lang rejects invalid language tags" #t (lang "!!" "x"))

;;; Pairing

(define bilingual-block (bilingual #:mode 'parallel english chinese))

(test-equal "bilingual groups lanes into one pair"
  1 (length (content-element-children bilingual-block)))
(test-equal "bilingual preserves lane order"
  '("en" "zh-Hans")
  (map (lambda (lane) (content-element-property lane 'lang))
       (content-element-children
         (car (content-element-children bilingual-block)))))

(test-equal "a repeated language starts a new pair"
  2
  (length
    (content-element-children
      (bilingual (lang "en" "One.") (lang "zh" "一。")
                 (lang "en" "Two.") (lang "zh" "二。")))))

(test-equal "distinct languages share one group"
  1
  (length
    (content-element-children
      (bilingual (lang "en" "One.") (lang "zh" "一。") (lang "fr" "Un.")))))

(test-error "bilingual rejects loose content" #t
  (bilingual (lang "en" "One.")
             (make-content-element 'paragraph '("Loose"))))
(test-error "bilingual requires at least one lane" #t (bilingual))

;;; Folded (reader-first) mode

(define (pair-children block)
  (content-element-children (car (content-element-children block))))

(define (find-content-kind node kind)
  (cond
    ((content-element? node)
     (if (eq? (content-element-kind node) kind)
         node
         (any (lambda (child) (find-content-kind child kind))
              (content-element-children node))))
    ((content-document? node)
     (any (lambda (child) (find-content-kind child kind))
          (content-document-children node)))
    (else #f)))

(define (chip-text summary)
  (let ((chip (find-content-kind summary 'bilingual-more)))
    (and chip (content-text-value (car (content-element-children chip))))))

(let* ((pair (car (content-element-children (bilingual english chinese))))
       (kids (content-element-children pair))
       (reader (car kids))
       (details (cadr kids))
       (summary (car (content-element-children details)))
       (folded (cdr (content-element-children details))))
  (test-equal "fold keeps the pair container"
    'bilingual-pair (content-element-kind pair))
  (test-equal "the reader lane stays visible"
    'bilingual-lane (content-element-kind reader))
  (test-equal "the last lane is the default reader"
    "zh-Hans" (content-element-property reader 'lang))
  (test-equal "the folded lanes carry their language"
    '("en")
    (map (lambda (lane) (content-element-property lane 'lang))
         (filter bilingual-lane? folded)))
  (test-equal "the summary hints at the folded language"
    "en" (chip-text summary))
  (test-equal "the source lanes are in a disclosure"
    'details (content-element-kind details)))

(test-equal "source labels replace the folded-language hint"
  "原文"
  (let* ((pair (car (content-element-children
                      (bilingual #:reader "en" #:source-label "原文"
                        english chinese))))
         (details (cadr (content-element-children pair)))
         (summary (car (content-element-children details))))
    (chip-text summary)))

(let* ((pair (car (content-element-children
                    (bilingual #:reader "en" english chinese))))
       (kids (content-element-children pair)))
  (test-equal "an explicit reader selects the visible lane"
    "en" (content-element-property (car kids) 'lang))
  (test-equal "the explicit reader folds the other language"
    '("zh-Hans")
    (map (lambda (lane) (content-element-property lane 'lang))
         (filter bilingual-lane?
                 (cdr (content-element-children (cadr kids)))))))

(test-equal "the reader matches a more specific lane tag"
  "zh-Hans"
  (content-element-property
    (car (content-element-children
           (car (content-element-children
                  (bilingual #:reader "zh" english chinese)))))
    'lang))

(test-error "an unmatched reader is an error"
  #t (bilingual #:reader "fr" english chinese))

(test-equal "a region alias selects the canonical lane"
  "zh-Hans"
  (content-element-property
    (car (content-element-children
           (car (content-element-children
                  (bilingual #:reader "zh-CN" english chinese)))))
    'lang))

(test-equal "parallel mode keeps every lane visible"
  2 (length (pair-children
              (bilingual #:mode 'parallel english chinese))))

(test-error "bilingual rejects unknown modes" #t
  (bilingual #:mode 'side english))
(test-error "bilingual source labels must be strings or #f" #t
  (bilingual #:mode 'parallel #:source-label 42 english chinese))

(test-assert "folded bilingual keeps the reader lane visible"
  (let ((html (sxml->html
                (content->sxml
                  (make-content-document
                    (list (bilingual #:reader "en" english chinese)))))))
    (and (string-contains html "data-mode=\"fold\"")
         (string-contains html "<div class=\"bilingual-pair\">")
         (string-contains html
           "<div class=\"bilingual-lane\" lang=\"en\" dir=\"ltr\"")
         (string-contains html "<details class=\"bilingual-source\">")
         (string-contains html
           "<summary><span class=\"bilingual-more\">zh-Hans</span></summary>"))))

(test-equal "text output keeps the reader lane and marks folded lanes"
  "[en] The grove is a small forest.\n[zh-Hans] 树林是一片小小的森林。\n\n"
  (content->text
    (make-content-document (list (bilingual #:reader "en" english chinese)))
    #:rules bilingual-text-rules))

;;; HTML presentation

(test-equal "bilingual lowers to nested, language-tagged HTML"
  '(article
     (div (@ (class "bilingual"))
       (div (@ (class "bilingual-pair"))
         (div (@ (class "bilingual-lane") (lang "en") (dir "ltr"))
           (p "The grove is a small forest."))
         (div (@ (class "bilingual-lane") (lang "zh-Hans") (dir "ltr"))
           (p "树林是一片小小的森林。")))))
  (content->sxml (make-content-document (list bilingual-block))))

(test-assert "serialized HTML marks each lane's language"
  (let ((html (sxml->html
                (content->sxml (make-content-document (list bilingual-block))))))
    (and (string-contains html "lang=\"en\" dir=\"ltr\"")
         (string-contains html "lang=\"zh-Hans\" dir=\"ltr\""))))

(test-equal "foreign terms keep their language inline"
  '(span (@ (class "foreign-term") (lang "fr") (dir "ltr")) "une forêt")
  (content->sxml (term "fr" "une forêt")))

(test-equal "a user style composes with the lane's language attributes"
  ;;     The style's own class precedes the lane's: class order carries no
  ;;     meaning to CSS, and the style name is emitted first.
  '(div (@ (class "note bilingual-lane") (lang "en") (dir "ltr")) (p "Text"))
  (content->sxml (lang "en" #:style 'note "Text")))

(test-equal "ARIA roles use explicit HTML style attributes"
  '(div (@ (role "note") (class "bilingual-lane")
           (lang "en") (dir "ltr"))
     (p "Text"))
  (content->sxml
    (lang "en"
      #:style (make-content-style #f
                (list (make-html-attributes '((role . "note")))))
      "Text")))

;;; Text presentation

(test-equal "text output marks each lane"
  "[en] The grove is a small forest.\n[zh-Hans] 树林是一片小小的森林。\n\n"
  (content->text (make-content-document (list bilingual-block))
                 #:rules bilingual-text-rules))

(test-equal "a plain summary renders unlabeled"
  "Derivation"
  (content->text
    (make-content-document
      (list (make-content-element 'summary (list "Derivation")
                                  #:flow-kind 'block)))
    #:rules bilingual-text-rules))

(test-equal "a summary carrying a language keeps its label"
  "[en] The grove is a small forest."
  (content->text
    (make-content-document
      (list (make-content-element
              'summary (list "The grove is a small forest.")
              #:properties (list (cons 'lang "en") (cons 'dir "ltr")))))
    #:rules bilingual-text-rules))

;;; Built-in renderers

(define bilingual-page
  (make-site-page "hello" "Hello"
    (make-content-document (list bilingual-block))))
(define bilingual-forest
  (make-forest (make-site (list bilingual-page) #:home "hello")))

(test-assert "the built-in text renderer marks each lane"
  (let ((text (assoc-ref (site->text bilingual-forest) "hello")))
    (and (string-contains text "[en] The grove is a small forest.")
         (string-contains text "[zh-Hans] 树林是一片小小的森林。"))))

(test-assert "the built-in Gemtext renderer marks each lane"
  (let ((gemtext (assoc-ref (site->gemtext bilingual-forest) "hello")))
    (and (string-contains gemtext "[en] The grove is a small forest.")
         (string-contains gemtext "[zh-Hans] 树林是一片小小的森林。"))))

;;; Scribble integration

(define i18n-environment
  (let ((environment (default-environment scribble)))
    (eval '(use-modules (forester i18n) (grove ir)) environment)
    environment))

(define (evaluate-i18n source)
  (scribble-evaluate-document (open-input-string source) i18n-environment))

(let* ((document (evaluate-i18n
                   "@bilingual[#:mode 'parallel]{ @lang[\"en\"]{Hello.} @lang[\"zh-CN\"]{你好。} }"))
       (block (car (content-document-children document)))
       (pair (car (content-element-children block))))
  (test-equal "@bilingual lowers through the Scribble reader"
    'bilingual (content-element-kind block))
  (test-equal "the reader preserves lane order and language"
    '("en" "zh-Hans")
    (map (lambda (lane) (content-element-property lane 'lang))
         (content-element-children pair)))
  (test-equal "the reader decodes lane prose into paragraphs"
    '(paragraph paragraph)
    (map content-element-kind
         (map (lambda (lane) (car (content-element-children lane)))
              (content-element-children pair)))))

;;; Style

(test-equal "the bilingual stylesheet is an installable artifact"
  "i18n.css"
  (artifact-path (car (html-style-stylesheets default-bilingual-style))))
(test-equal "the default bilingual style exposes its stylesheet" 1
  (length (html-style-stylesheets default-bilingual-style)))


(test-assert "text output localizes interface labels"
  (let ((text (assoc-ref
                (site->text bilingual-forest
                            #:options (make-text-options #:locale "zh-Hans"))
                "hello")))
    (and (string-contains text "首页")
         (not (string-contains text "Home")))))

(test-equal "the project reader locale selects the visible lane"
  "zh-Hans"
  (parameterize ((current-reader-locale "zh-Hans"))
    (content-element-property
      (car (content-element-children
             (car (content-element-children (bilingual english chinese)))))
      'lang)))
(test-equal "the built-in bi uses the project reader locale"
  "zh-Hans"
  (parameterize ((current-reader-locale "zh-Hans"))
    (content-element-property
      (car (content-element-children
             (car (content-element-children (bi english chinese)))))
      'lang)))
(test-end "forester i18n")

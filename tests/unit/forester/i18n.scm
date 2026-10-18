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
  #:use-module (grove style html)
  #:use-module (forester i18n)
  #:use-module (forester render gemtext)
  #:use-module (forester render text)
  #:use-module (forester site)
  #:use-module (forester style i18n))

(test-begin "forester i18n")

;;; Locales

(let ((locale (make-locale "zh-Hant")))
  (test-equal "locales keep their authored tag" "zh-Hant" (locale-tag locale))
  (test-equal "locales expose their primary language" "zh" (locale-lang locale))
  (test-equal "Chinese locales are left-to-right" "ltr" (locale-dir locale)))

(test-equal "Arabic locales request right-to-left direction"
  "rtl" (locale-dir (make-locale "ar-EG")))

(test-equal "locale HTML attributes carry language and direction"
  '((lang . "he") (dir . "rtl"))
  (locale-html-attributes (make-locale "he")))

(test-assert "locale matching accepts a more specific tag"
  (locale-matches? "zh" "zh-CN"))
(test-assert "locale matching is case-insensitive"
  (locale-matches? "ZH" "zh-CN"))
(test-assert "locale matching rejects a different region"
  (not (locale-matches? "zh-CN" "zh-TW")))

(test-equal "a right-to-left script overrides a left-to-right language"
  "rtl" (locale-dir (make-locale "az-Arab")))
(test-equal "a right-to-left script applies to a language without a default"
  "rtl" (locale-dir (make-locale "ks-Arab")))
(test-equal "a left-to-right script overrides a right-to-left language"
  "ltr" (locale-dir (make-locale "ku-Latn")))
(test-equal "common scripts resolve left-to-right"
  "ltr" (locale-dir (make-locale "sr-Latn")))

(test-error "locale tags reject spaces" #t (make-locale "not a tag"))
(test-error "locale tags reject empty strings" #t (make-locale ""))
(test-error "locale tags reject non-strings" #t (make-locale 'en))

;;; Message catalogs

(define catalog
  (make-message-catalog
    '(("en" . ((contents . "Contents") (home . "Home")))
      ("zh-CN" . ((contents . "目录") (home . "首页"))))
    #:default "en"))

(test-equal "messages resolve for a known locale"
  "目录" (message catalog 'contents "zh-CN"))
(test-equal "messages fall back to the default locale"
  "Contents" (message catalog 'contents "fr"))
(test-equal "unknown keys fall back to their name"
  "notes" (message catalog 'notes "en"))
(test-equal "messages use the catalog default locale by default"
  "Home" (message catalog 'home))
(test-error "catalogs reject duplicate message keys" #t
  (make-message-catalog '(("en" . ((home . "Home") (home . "Start"))))))
(test-error "catalogs reject non-symbol message keys" #t
  (make-message-catalog '(("en" . (("home" . "Home"))))))
(test-error "catalogs reject symbol locale keys" #t
  (make-message-catalog '((en . ((home . "Home"))))))
(test-error "message keys must be symbols" #t (message catalog "home"))
(test-error "message rejects non-catalogs" #t (message '() 'home))

(define script-catalog
  (make-message-catalog
    '(("zh-Hant" . ((home . "首頁")))
      ("en" . ((home . "Home"))))
    #:default "en"))
(test-equal "messages fall back through shorter locale prefixes"
  "首頁" (message script-catalog 'home "zh-Hant-CN"))
(test-equal "a more specific catalog entry does not answer a shorter request"
  "Home" (message script-catalog 'home "zh-CN"))

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
  '("en" "zh-CN")
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
       (summary (car (content-element-children pair)))
       (folded (cdr (content-element-children pair))))
  (test-equal "fold is the default bilingual mode"
    'details (content-element-kind pair))
  (test-equal "the reader lane is the disclosure summary"
    'summary (content-element-kind summary))
  (test-equal "the last lane is the default reader"
    "zh-CN" (content-element-property summary 'lang))
  (test-equal "the folded lanes carry their language"
    '("en")
    (map (lambda (lane) (content-element-property lane 'lang))
         (filter bilingual-lane? folded)))
  (test-equal "the summary hints at the folded language"
    "en" (chip-text summary))
  (test-assert "the hint trails the reader text on the same line"
    (let ((paragraph (car (content-element-children summary))))
      (eq? 'bilingual-more
           (content-element-kind (last (content-element-children paragraph)))))))

(let* ((pair (car (content-element-children
                    (bilingual #:reader "en" english chinese))))
       (kids (content-element-children pair)))
  (test-equal "an explicit reader selects the visible lane"
    "en" (content-element-property (car kids) 'lang))
  (test-equal "the explicit reader folds the other language"
    '("zh-CN")
    (map (lambda (lane) (content-element-property lane 'lang))
         (filter bilingual-lane? (cdr kids)))))

(test-equal "the reader matches a more specific lane tag"
  "zh-CN"
  (content-element-property
    (car (content-element-children
           (car (content-element-children
                  (bilingual #:reader "zh" english chinese)))))
    'lang))

(test-equal "an unmatched reader keeps the group parallel"
  2 (length (pair-children (bilingual #:reader "fr" english chinese))))

(test-equal "parallel mode keeps every lane visible"
  2 (length (pair-children
              (bilingual #:mode 'parallel english chinese))))

(test-error "bilingual rejects unknown modes" #t
  (bilingual #:mode 'side english))

(test-assert "folded bilingual makes the reader lane the disclosure"
  (let ((html (sxml->html
                (content->sxml
                  (make-content-document
                    (list (bilingual #:reader "en" english chinese)))))))
    (and (string-contains html "data-mode=\"fold\"")
         (string-contains html "<details class=\"bilingual-pair\">")
         (string-contains html
           "<summary class=\"bilingual-lane\" lang=\"en\" dir=\"ltr\"")
         (string-contains html "class=\"bilingual-more\">zh-CN"))))

(test-equal "text output keeps the reader lane and marks folded lanes"
  "[en] The grove is a small forest.\n[zh-CN] 树林是一片小小的森林。\n"
  (content->text
    (make-content-document (list (bilingual #:reader "en" english chinese)))
    #:handlers bilingual-text-handlers))

;;; HTML presentation

(test-equal "bilingual lowers to nested, language-tagged HTML"
  '(article
     (div (@ (class "bilingual"))
       (div (@ (class "bilingual-pair"))
         (div (@ (class "bilingual-lane") (lang "en") (dir "ltr"))
           (p "The grove is a small forest."))
         (div (@ (class "bilingual-lane") (lang "zh-CN") (dir "ltr"))
           (p "树林是一片小小的森林。")))))
  (content->sxml (make-content-document (list bilingual-block))))

(test-assert "serialized HTML marks each lane's language"
  (let ((html (sxml->html
                (content->sxml (make-content-document (list bilingual-block))))))
    (and (string-contains html "lang=\"en\" dir=\"ltr\"")
         (string-contains html "lang=\"zh-CN\" dir=\"ltr\""))))

(test-equal "foreign terms keep their language inline"
  '(span (@ (class "foreign-term") (lang "fr") (dir "ltr")) "une forêt")
  (content->sxml (term "fr" "une forêt")))

(test-equal "a user style composes with the lane's language attributes"
  ;;     The style's own class precedes the lane's: class order carries no
  ;;     meaning to CSS, and the style name is emitted first.
  '(div (@ (class "note bilingual-lane") (lang "en") (dir "ltr")) (p "Text"))
  (content->sxml (lang "en" #:style 'note "Text")))

;;; Text presentation

(test-equal "text output marks each lane"
  "[en] The grove is a small forest.\n[zh-CN] 树林是一片小小的森林。\n\n"
  (content->text (make-content-document (list bilingual-block))
                 #:handlers bilingual-text-handlers))

(test-equal "a plain summary renders unlabeled"
  "Derivation"
  (content->text
    (make-content-document
      (list (make-content-element 'summary (list "Derivation")
                                  #:flow-kind 'block)))
    #:handlers bilingual-text-handlers))

(test-equal "a summary carrying a language keeps its label"
  "[en] The grove is a small forest."
  (content->text
    (make-content-document
      (list (make-content-element
              'summary (list "The grove is a small forest.")
              #:properties (list (cons 'lang "en") (cons 'dir "ltr")))))
    #:handlers bilingual-text-handlers))

;;; Built-in renderers

(define bilingual-page
  (make-site-page "hello" "Hello"
    (make-content-document (list bilingual-block))))
(define bilingual-forest
  (make-forest (make-site (list bilingual-page) #:home "hello")))

(test-assert "the built-in text renderer marks each lane"
  (let ((text (assoc-ref (site->text bilingual-forest) "hello")))
    (and (string-contains text "[en] The grove is a small forest.")
         (string-contains text "[zh-CN] 树林是一片小小的森林。"))))

(test-assert "the built-in Gemtext renderer marks each lane"
  (let ((gemtext (assoc-ref (site->gemtext bilingual-forest) "hello")))
    (and (string-contains gemtext "[en] The grove is a small forest.")
         (string-contains gemtext "[zh-CN] 树林是一片小小的森林。"))))

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
    '("en" "zh-CN")
    (map (lambda (lane) (content-element-property lane 'lang))
         (content-element-children pair)))
  (test-equal "the reader decodes lane prose into paragraphs"
    '(paragraph paragraph)
    (map content-element-kind
         (map (lambda (lane) (car (content-element-children lane)))
              (content-element-children pair)))))

;;; Style

(test-equal "the bilingual stylesheet is an installable artifact"
  "i18n.css" (artifact-path bilingual-stylesheet))
(test-equal "the default bilingual style exposes its stylesheet"
  (list bilingual-stylesheet)
  (html-style-stylesheets default-bilingual-style))

(test-end "forester i18n")

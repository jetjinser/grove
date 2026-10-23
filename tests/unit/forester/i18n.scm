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
  #:use-module (forester commands)
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

;;; Matching keys

(test-equal "matching keys fold case across the full Unicode range"
  (match-key "STRASSE") (match-key "Straße"))
(test-equal "matching keys compose canonically equivalent spellings"
  (match-key "café")
  (match-key (string-append "cafe" (string (integer->char #x301)))))
(test-equal "matching keys keep the folded canonical spelling"
  "strasse" (match-key "Straße"))
(test-error "matching keys require strings" #t (match-key 'en))

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
    #:handlers bilingual-text-handlers))

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

;;; Text presentation

(test-equal "text output marks each lane"
  "[en] The grove is a small forest.\n[zh-Hans] 树林是一片小小的森林。\n\n"
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
  "i18n.css" (artifact-path bilingual-stylesheet))
(test-equal "the default bilingual style exposes its stylesheet"
  (list bilingual-stylesheet)
  (html-style-stylesheets default-bilingual-style))


;;; Formatting

(test-equal "English long dates"
  "September 27, 2026" (format-date "en" "2026-09-27" #:style 'long))
(test-equal "English medium dates"
  "Sep 27, 2026" (format-date "en" "2026-09-27"))
(test-equal "Chinese dates"
  "2026年9月27日" (format-date "zh-Hans" "2026-09-27"))
(test-equal "unsupported locales keep the ISO date"
  "2026-09-27" (format-date "sw" "2026-09-27"))
(test-equal "unparseable dates pass through"
  "not-a-date" (format-date "en" "not-a-date"))
(test-equal "numbers group by threes"
  "1,234,567" (format-number "en" 1234567))
(test-equal "negative numbers keep their sign"
  "-1,234" (format-number "en" -1234))
(test-equal "English singular"
  "~a item" (plural "en" 1 '((one . "~a item") (other . "~a items"))))
(test-equal "English plural"
  "~a items" (plural "en" 2 '((one . "~a item") (other . "~a items"))))
(test-equal "Chinese has a single plural form"
  "~a 条" (plural "zh-Hans" 1 '((one . "~a 条") (other . "~a 条"))))

(test-assert "text output localizes chrome labels"
  (let ((text (assoc-ref
                (site->text bilingual-forest
                            #:options (make-text-options #:locale "zh-Hans"))
                "hello")))
    (and (string-contains text "首页")
         (not (string-contains text "Home")))))

;;; Sorting

(test-equal "sort keys fold case and accents"
  "strasse" (locale-sort-key "Straße"))
(test-equal "strings sort by their folded keys"
  '("apple" "Straße" "Zebra" "éclair")
  (sort-strings '("Straße" "apple" "Zebra" "éclair")))
(test-equal "equal keys keep their input order"
  '("Straße" "STRASSE")
  (sort-strings '("Straße" "STRASSE")))

;;; Locale-aware collation

(test-equal "glibc locale names follow the primary language"
  "fr_FR.UTF-8" (glibc-locale-name "fr-CA"))
(test-equal "glibc locale names cover traditional Chinese"
  "zh_TW.UTF-8" (glibc-locale-name "zh-Hant"))
(test-equal "glibc locale names cover Chinese"
  "zh_CN.UTF-8" (glibc-locale-name "zh-Hans"))
(test-equal "unsupported locales have no glibc name"
  #f (glibc-locale-name "xx"))
(test-equal "French collation keeps accents with their base letter"
  '("éclair" "Zebra")
  (sort-strings '("Zebra" "éclair") #:locale "fr"))
(test-equal "German collation keeps umlauts with their base letter"
  '("Äpfel" "Zebra")
  (sort-strings '("Zebra" "Äpfel") #:locale "de"))
(test-equal "Chinese collation uses pinyin order"
  '("阿" "波")
  (sort-strings '("波" "阿") #:locale "zh-Hans"))
(test-equal "unsupported locales fall back to folded code points"
  '("Zebra" "éclair")
  (sort-strings '("Zebra" "éclair") #:locale "xx"))
(test-equal "a custom comparator overrides the locale"
  '("b" "a")
  (sort-strings '("a" "b") #:compare (lambda (a b) (string>? a b))))
(test-equal "locale sorting keeps equal keys stable"
  '("Straße" "STRASSE")
  (sort-strings '("Straße" "STRASSE") #:locale "de"))

;;; Open Graph locales

(test-equal "Open Graph locale tags use language_TERRITORY"
  "en_US" (og-locale-tag "en"))
(test-equal "Open Graph locales honor a region subtag"
  "fr_CA" (og-locale-tag "fr-CA"))
(test-equal "Open Graph locales map Chinese scripts"
  (list "zh_CN" "zh_TW")
  (list (og-locale-tag "zh-Hans") (og-locale-tag "zh-Hant")))

(test-equal "German long dates"
  "27. September 2026" (format-date "de" "2026-09-27" #:style 'long))
(test-equal "German medium dates"
  "27. Sept. 2026" (format-date "de" "2026-09-27"))
(test-equal "French long dates"
  "27 septembre 2026" (format-date "fr" "2026-09-27" #:style 'long))
(test-equal "French medium dates"
  "27 sept. 2026" (format-date "fr" "2026-09-27"))
(test-equal "Russian plural categories"
  '("one" "few" "many" "many")
  (map (lambda (n)
         (plural "ru" n '((one . "one") (few . "few") (many . "many"))))
       '(1 2 5 11)))
(test-equal "Polish plural categories"
  '("one" "few" "many" "many")
  (map (lambda (n)
         (plural "pl" n '((one . "one") (few . "few") (many . "many"))))
       '(1 2 5 12)))
(test-equal "Arabic plural categories"
  '("zero" "one" "two" "few" "many" "other")
  (map (lambda (n)
         (plural "ar" n '((zero . "zero") (one . "one") (two . "two")
                          (few . "few") (many . "many") (other . "other"))))
       '(0 1 2 3 11 100)))

(test-equal "format-number passes non-integers through"
  "1.5" (format-number "en" 1.5))
(test-equal "format-number can skip grouping"
  "1234" (format-number "en" 1234 #:group? #f))
(test-equal "unsupported Open Graph locales keep the language"
  "sw" (og-locale-tag "sw"))
(test-equal "German short dates"
  "2026-09-27" (format-date "de" "2026-09-27" #:style 'short))
(test-equal "French short dates"
  "2026-09-27" (format-date "fr" "2026-09-27" #:style 'short))
(test-equal "plural falls back to other"
  "other" (plural "ru" 5 '((other . "other"))))
(test-equal "plural falls back to the empty string"
  "" (plural "ru" 5 '()))

(test-equal "generic Chinese tags fall back to the zh catalog"
  "站点导航" (message default-message-catalog 'nav-site "zh-CN"))
(test-equal "Traditional Chinese has its own catalog"
  "網站導覽" (message default-message-catalog 'nav-site "zh-Hant"))

(test-equal "zh-CN canonicalizes to zh-Hans"
  "zh-Hans" (locale-tag (make-locale "zh-CN")))
(test-equal "zh-TW canonicalizes to zh-Hant"
  "zh-Hant" (locale-tag (make-locale "zh-TW")))
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

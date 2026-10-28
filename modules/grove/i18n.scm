;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove i18n)
  #:use-module (grove private alist)
  #:use-module (grove private procedure)
  #:use-module (grove private string-fun)
  #:use-module (grove private snapshot)
  #:use-module (grove unicode)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 i18n) #:prefix glibc:)
  #:export (locale?
            make-locale
            locale-tag
            locale-lang
            locale-dir
            locale-html-attributes
            locale-matches?
            message-catalog?
            make-message-catalog
            message-catalog-default
            message-catalog-entries
            default-message-catalog
            merge-message-catalogs
            message
            message-format
            iso-date?
            format-date
            format-number
            plural
            locale-collation-comparator
            current-reader-locale
            sort-strings))

;; Grove's i18n primitives are language tags and built-in interface messages.
;; They carry no document content; translated trees and their presentation live
;; elsewhere.

(define rtl-languages
  '("ar" "arc" "ckb" "dv" "fa" "he" "khw" "ku" "nqo" "prs" "ps" "sd" "syr"
    "ug" "ur" "yi"))

;; Direction is a property of the script, not just the language. A script
;; subtag (four letters, title case) overrides the language defaults, so that
;; `ku-Latn' is left-to-right even though Kurdish has right-to-left varieties,
;; and `az-Arab' is right-to-left even though Azerbaijani is usually Latin.
(define rtl-scripts
  '("Adlm" "Arab" "Avst" "Hebr" "Mand" "Mani" "Mend" "Narb" "Nkoo" "Ougr"
    "Phli" "Phlp" "Prti" "Rohg" "Samr" "Sarb" "Syrc" "Thaa" "Yezi"))

(define ltr-scripts
  '("Armn" "Beng" "Cyrl" "Deva" "Ethi" "Geor" "Grek" "Gujr" "Guru" "Hang"
    "Hani" "Hans" "Hant" "Hira" "Jpan" "Kana" "Khmr" "Knda" "Kore" "Laoo"
    "Latn" "Mlym" "Mong" "Mymr" "Sinh" "Taml" "Telu" "Thai" "Tibt"))

(define (ascii-alphanumeric? char)
  (or (ascii-alpha? char) (ascii-digit? char)))

(define (subtag-matches? subtag predicate min-length max-length)
  (and (<= min-length (string-length subtag) max-length)
       (string-every predicate subtag)))

(define (language-subtag? subtag)
  (or (subtag-matches? subtag ascii-alpha? 2 3)
      (subtag-matches? subtag ascii-alpha? 4 4)
      (subtag-matches? subtag ascii-alpha? 5 8)))

(define (extlang-subtag? subtag)
  (subtag-matches? subtag ascii-alpha? 3 3))

(define (script-subtag? subtag)
  (subtag-matches? subtag ascii-alpha? 4 4))

(define (region-subtag? subtag)
  (or (subtag-matches? subtag ascii-alpha? 2 2)
      (subtag-matches? subtag ascii-digit? 3 3)))

(define (variant-subtag? subtag)
  (or (subtag-matches? subtag ascii-alphanumeric? 5 8)
      (and (subtag-matches? subtag ascii-alphanumeric? 4 4)
           (ascii-digit? (string-ref subtag 0)))))

(define grandfathered-tags
  '("en-gb-oed" "i-ami" "i-bnn" "i-default" "i-enochian" "i-hak"
    "i-klingon" "i-lux" "i-mingo" "i-navajo" "i-pwn" "i-tao" "i-tay"
    "i-tsu" "sgn-be-fr" "sgn-be-nl" "sgn-ch-de" "art-lojban"
    "cel-gaulish" "no-bok" "no-nyn" "zh-guoyu" "zh-hakka" "zh-min"
    "zh-min-nan" "zh-xiang"))

(define (parse-locale-tag tag)
  "Parse the structural BCP 47 grammar without consulting the IANA registry.
Return language, extlangs, script, region, variants, extensions, and private
use subtags, or #f when TAG is not well-formed."
  (and (non-empty-string? tag)
       (let ((subtags (string-split tag #\-)))
         (and (every (lambda (subtag)
                       (subtag-matches? subtag ascii-alphanumeric? 1 8))
                     subtags)
              (let ((lower (map string-downcase subtags)))
                (cond
                 ((member (string-join lower "-") grandfathered-tags)
                  (list (car lower) '() #f #f '() '() '()))
                 ((equal? (car lower) "x")
                  (and (pair? (cdr lower))
                       (list "x" '() #f #f '() '() (cdr lower))))
                 ((not (language-subtag? (car lower))) #f)
                 (else
                  (let* ((language (car lower))
                         (rest (cdr lower))
                         (extlangs (if (<= (string-length language) 3)
                                       (take-while extlang-subtag? rest)
                                       '()))
                         (rest (drop rest (length extlangs)))
                         (script (and (pair? rest) (script-subtag? (car rest))
                                      (car rest)))
                         (rest (if script (cdr rest) rest))
                         (region (and (pair? rest) (region-subtag? (car rest))
                                      (car rest)))
                         (rest (if region (cdr rest) rest))
                         (variants (take-while variant-subtag? rest))
                         (rest (drop rest (length variants))))
                    (and (= (length variants) (length (delete-duplicates variants)))
                     (let extension-loop ((rest rest) (seen '()) (extensions '()))
                      (cond
                       ((null? rest)
                        (list language extlangs script region variants
                              (reverse extensions) '()))
                       ((and (string=? (car rest) "x") (pair? (cdr rest)))
                        (list language extlangs script region variants
                              (reverse extensions) (cdr rest)))
                       ((and (= (string-length (car rest)) 1)
                             (not (string=? (car rest) "x")))
                        (let* ((singleton (car rest))
                               (tail (cdr rest))
                               (values (take-while
                                        (lambda (part)
                                          (>= (string-length part) 2))
                                        tail)))
                          (and (pair? values)
                               (not (member singleton seen))
                               (extension-loop
                                (drop tail (length values))
                                (cons singleton seen)
                                (cons (cons singleton values) extensions)))))
                       (else #f))))))))))))

(define (valid-locale-tag? tag)
  (and (string? tag) (if (parse-locale-tag tag) #t #f)))

(define (canonical-extensions extensions)
  (append-map (lambda (extension)
                (map string-downcase extension))
              (sort extensions
                    (lambda (left right)
                      (string<? (car left) (car right))))))

(define-record-type <locale>
  (%make-locale tag lang dir)
  locale?
  (tag locale-tag/raw)
  (lang locale-lang/raw)
  (dir locale-dir/raw))

(define (locale-tag locale)
  (string-copy (locale-tag/raw locale)))

(define (locale-lang locale)
  (string-copy (locale-lang/raw locale)))

(define (locale-dir locale)
  (string-copy (locale-dir/raw locale)))

(define (resolve-direction script lang)
  "Derive a text direction from a script subtag, then from the language."
  (cond
   ((and script (member (string-titlecase script) rtl-scripts)) "rtl")
   ((and script (member (string-titlecase script) ltr-scripts)) "ltr")
   ((member lang rtl-languages) "rtl")
   (else "ltr")))

(define (canonical-locale-tag tag)
  "Return TAG with BCP 47 casing and Grove's Chinese aliases.

Language subtags are lowercased, scripts title-cased, and regions
uppercased.  Chinese takes a script and drops the redundant region:
zh-CN/zh-SG/zh-MY and bare zh become zh-Hans, while zh-TW/zh-HK/zh-MO
become zh-Hant."
  (unless (string? tag)
    (error "locale tag must be a string" tag))
  (let* ((tag (string-trim-both tag))
         (subtags (string-split tag #\-))
         (lower-tag (string-join (map string-downcase subtags) "-"))
         (parsed (parse-locale-tag tag))
         (lang (list-ref parsed 0))
         (extlangs (list-ref parsed 1))
         (script (list-ref parsed 2))
         (region (list-ref parsed 3))
         (variants (list-ref parsed 4))
         (extensions (list-ref parsed 5))
         (private-use (list-ref parsed 6)))
    (cond
     ((member lower-tag grandfathered-tags) lower-tag)
     ((string=? lang "x")
      (string-join (cons "x" private-use) "-"))
     ((string=? lang "zh")
        (let ((script (or (and script (string-titlecase script))
                          (if (member (and region (string-upcase region))
                                      '("TW" "HK" "MO"))
                              "Hant"
                              "Hans"))))
          (string-join
           (append (list lang)
                   (map string-downcase extlangs)
                   (list script)
                   (map string-downcase variants)
                   (canonical-extensions extensions)
                   (if (pair? private-use) (cons "x" private-use) '()))
           "-")))
     (else
      (string-join
         (append (list lang)
                 (map string-downcase extlangs)
                 (if script (list (string-titlecase script)) '())
                 (if region (list (if (string-every ascii-digit? region)
                                      region
                                      (string-upcase region))) '())
                 (map string-downcase variants)
                 (canonical-extensions extensions)
                 (if (pair? private-use) (cons "x" private-use) '()))
         "-")))))

(define (make-locale tag)
  "Normalize a BCP 47-style language tag into a locale with a text direction."
  (unless (string? tag)
    (error "locale tag must be a string" tag))
  (let ((tag (string-trim-both tag)))
    (unless (valid-locale-tag? tag)
      (error "locale tag must be a well-formed BCP 47 language tag" tag))
    (let* ((tag (canonical-locale-tag tag))
           (parsed (parse-locale-tag tag))
           (lang (car parsed))
           (dir (resolve-direction (list-ref parsed 2) lang)))
      (%make-locale tag lang dir))))

(define (locale-html-attributes locale)
  "Return the `lang' and `dir' HTML attributes for a locale."
  (unless (locale? locale)
    (error "locale-html-attributes requires a locale" locale))
  `((lang . ,(locale-tag locale))
    (dir . ,(locale-dir locale))))

(define (locale-matches? reader tag)
  "True when TAG is READER or a more specific tag beneath it.

Tags are canonicalized first, so `zh-CN' and `zh-Hans' compare equal
and a `zh-Hans' reader matches `zh-Hans-CN'."
  (let ((reader (locale-tag (make-locale reader)))
        (tag (locale-tag (make-locale tag))))
    (or (string-ci=? reader tag)
        (string-prefix-ci? (string-append reader "-") tag))))

(define-record-type <message-catalog>
  (%make-message-catalog default entries)
  message-catalog?
  (default message-catalog-default/raw)
  (entries message-catalog-entries/raw))

(define (message-catalog-default catalog)
  (string-copy (message-catalog-default/raw catalog)))

(define (message-catalog-entries catalog)
  (snapshot-data (message-catalog-entries/raw catalog)))

(define (valid-messages? messages)
  (and (list? messages)
       (every (match-lambda
                ((key . value) (and (symbol? key) (string? value)))
                (_ #f))
              messages)
       (let loop ((remaining messages) (seen '()))
         (or (null? remaining)
             (and (not (memq (caar remaining) seen))
                  (loop (cdr remaining) (cons (caar remaining) seen)))))))

(define (valid-message-entries? entries)
  (and (list? entries)
       (every (match-lambda
                ((locale . messages)
                 (and (non-empty-string? locale) (valid-messages? messages)))
                (_ #f))
              entries)))

(define (catalog-locale-key tag)
  (let ((locale (make-locale tag)))
    ;; Keep generic Chinese available as a fallback after zh-Hans/zh-Hant,
    ;; while canonicalizing every authored tag, including language-only tags.
    (if (string=? (locale-tag locale) "zh-Hans")
        (let ((parts (string-split (string-trim-both tag) #\-)))
          (if (= (length parts) 1) "zh" (locale-tag locale)))
        (locale-tag locale))))

(define (unique-catalog-locales? entries)
  (let loop ((remaining entries) (seen '()))
    (or (null? remaining)
        (let ((locale (caar remaining)))
          (and (not (member locale seen string=?))
               (loop (cdr remaining) (cons locale seen)))))))

(define* (make-message-catalog entries #:key (default "en"))
  "Map locale tags to symbol-keyed messages, falling back to DEFAULT."
  (unless (valid-message-entries? entries)
    (error "message catalog requires locale-tagged message lists" entries))
  (unless (non-empty-string? default)
    (error "message catalog default locale must be a non-empty string" default))
  (let ((entries (snapshot-data
                   (map (lambda (entry)
                          (cons (catalog-locale-key (car entry)) (cdr entry)))
                        entries)))
        (default (string-copy (catalog-locale-key default))))
    (unless (unique-catalog-locales? entries)
      (error "message catalog contains duplicate canonical locale tags"
             entries))
    (%make-message-catalog default entries)))

(define (locale-fallbacks tag)
  "List TAG and its progressively shorter prefixes, most specific first.
So `zh-Hant-CN' falls back through `zh-Hant' to `zh'."
  (let loop ((remaining tag) (collected '()))
    (cond
      ((string-null? remaining) (reverse collected))
      ((string-rindex remaining #\-)
       => (lambda (dash)
            (loop (substring remaining 0 dash) (cons remaining collected))))
      (else (reverse (cons remaining collected))))))

(define* (message catalog key #:optional (locale (message-catalog-default catalog)))
  "Look up KEY in CATALOG for LOCALE, its shorter prefixes, the default locale,
then the key name."
  (unless (message-catalog? catalog)
    (error "message requires a message catalog" catalog))
  (unless (symbol? key)
    (error "message key must be a symbol" key))
  (let* ((locale (locale-tag
                   (if (locale? locale) locale (make-locale locale))))
         (entries (message-catalog-entries/raw catalog))
         (lookup (lambda (tags)
                   (any (lambda (tag)
                          (let ((messages (assoc-ref entries tag)))
                            (and messages (assq-ref messages key))))
                        tags))))
    (or (lookup (locale-fallbacks locale))
        (lookup (locale-fallbacks (message-catalog-default/raw catalog)))
        (symbol->string key))))

;; Interface messages for renderer-owned navigation, skip links, tree labels,
;; metadata, and backmatter headings. Projects extend the catalog through the
;; renderer's `#:messages' option.
(define default-message-catalog
  (make-message-catalog
    '(("en" . ((nav-site . "Site")
               (nav-contents . "Contents")
               (skip-to-content . "Skip to content")
               (home . "Home")
               (language . "Language")
               (tree-open-page . "Open tree ~a as a page")
               (metadata-updated . "updated ~a")
               (backmatter-context . "Context")
               (backmatter-references . "References")
               (backmatter-backlinks . "Backlinks")
               (backmatter-related . "Related")
               (describes . "Describes")))
      ("zh-Hans" . ((nav-site . "站点导航")
                    (nav-contents . "目录")
                    (skip-to-content . "跳到正文")
                    (home . "首页")
                    (language . "语言")
                    (tree-open-page . "将树 ~a 作为页面打开")
                    (metadata-updated . "更新于 ~a")
                    (backmatter-context . "上下文")
                    (backmatter-references . "参考文献")
                    (backmatter-backlinks . "反向链接")
                    (backmatter-related . "相关")
                    (describes . "描述的实体")))
      ("zh-Hant" . ((nav-site . "網站導覽")
                    (nav-contents . "目錄")
                    (skip-to-content . "跳至內容")
                    (home . "首頁")
                    (language . "語言")
                    (tree-open-page . "將樹 ~a 作為頁面開啟")
                    (metadata-updated . "更新於 ~a")
                    (backmatter-context . "脈絡")
                    (backmatter-references . "參考文獻")
                    (backmatter-backlinks . "反向連結")
                    (backmatter-related . "相關")
                    (describes . "描述的實體")))
      ("zh" . ((nav-site . "站点导航")
               (nav-contents . "目录")
               (skip-to-content . "跳到正文")
               (home . "首页")
               (language . "语言")
               (tree-open-page . "将树 ~a 作为页面打开")
               (metadata-updated . "更新于 ~a")
               (backmatter-context . "上下文")
               (backmatter-references . "参考文献")
               (backmatter-backlinks . "反向链接")
               (backmatter-related . "相关")
               (describes . "描述的实体"))))))

(define* (message-format catalog key arguments
                          #:optional (locale (message-catalog-default catalog)))
  "Look up KEY in CATALOG for LOCALE and format its message with ARGUMENTS."
  (unless (list? arguments)
    (error "message-format requires an argument list" arguments))
  (apply simple-format #f (message catalog key locale) arguments))

(define (merge-messages base override)
  (append override
          (filter (lambda (entry)
                    (not (assq (car entry) override)))
                  base)))

(define (merge-message-catalogs base override)
  "Layer OVERRIDE over BASE per locale and key, so a partial project
catalog keeps the built-in messages for everything it does not name."
  (unless (and (message-catalog? base) (message-catalog? override))
    (error "merge-message-catalogs requires two message catalogs" base override))
  (let* ((base-entries (message-catalog-entries/raw base))
         (override-entries (message-catalog-entries/raw override))
         (locales (delete-duplicates
                    (append (map car base-entries)
                            (map car override-entries))
                    string=?)))
    (make-message-catalog
      (map (lambda (locale)
             (cons locale
                   (merge-messages (or (assoc-ref base-entries locale) '())
                                   (or (assoc-ref override-entries locale) '()))))
           locales)
      #:default (message-catalog-default/raw override))))

(define english-months
  '("January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(define german-months
  '("Januar" "Februar" "März" "April" "Mai" "Juni"
    "Juli" "August" "September" "Oktober" "November" "Dezember"))

(define german-months-short
  '("Jan." "Feb." "März" "Apr." "Mai" "Juni"
    "Juli" "Aug." "Sept." "Okt." "Nov." "Dez."))

(define french-months
  '("janvier" "février" "mars" "avril" "mai" "juin"
    "juillet" "août" "septembre" "octobre" "novembre" "décembre"))

(define french-months-short
  '("janv." "févr." "mars" "avr." "mai" "juin"
    "juil." "août" "sept." "oct." "nov." "déc."))

(define (iso-date year month day)
  (format #f "~4,'0d-~2,'0d-~2,'0d" year month day))

(define (parse-iso-date text)
  (and (string? text)
       (= (string-length text) 10)
       (char=? (string-ref text 4) #\-)
       (char=? (string-ref text 7) #\-)
       (let ((year-text (substring text 0 4))
             (month-text (substring text 5 7))
             (day-text (substring text 8 10)))
         (and (string-every char-set:digit year-text)
              (string-every char-set:digit month-text)
              (string-every char-set:digit day-text)
              (let* ((year (string->number year-text))
                     (month (string->number month-text))
                     (day (string->number day-text))
                     (leap? (and (= (modulo year 4) 0)
                                 (or (not (= (modulo year 100) 0))
                                     (= (modulo year 400) 0))))
                     (days-in-month
                      (and (<= 1 month 12)
                           (list-ref '(31 28 31 30 31 30 31 31 30 31 30 31)
                                     (1- month))))
                     (maximum-day
                      (and days-in-month
                           (if (and (= month 2) leap?)
                               (1+ days-in-month)
                               days-in-month))))
                (and (<= 1 year 9999)
                     maximum-day
                     (<= 1 day maximum-day)
                     (list year month day)))))))

(define (iso-date? value)
  "Return true when VALUE is a valid Gregorian date in YYYY-MM-DD form."
  (and (parse-iso-date value) #t))

(define* (format-date locale date #:key (style 'medium))
  "Format an ISO YYYY-MM-DD DATE for LOCALE.  Unparseable dates and
locales without date data return DATE unchanged."
  (unless (memq style '(short medium long))
    (error "date style must be short, medium, or long" style))
  (let* ((locale (if (locale? locale) locale (make-locale locale)))
         (parts (parse-iso-date date)))
    (if (not parts)
        date
        (let* ((lang (locale-lang locale))
               (year (car parts))
               (month (cadr parts))
               (day (caddr parts)))
          (cond
            ((string=? lang "zh")
             (if (eq? style 'short)
                 (iso-date year month day)
                 (format #f "~d年~d月~d日" year month day)))
            ((string=? lang "en")
             (cond
               ((eq? style 'short) (iso-date year month day))
               ((eq? style 'long)
                (format #f "~a ~d, ~d"
                        (list-ref english-months (1- month)) day year))
               (else
                (format #f "~a ~d, ~d"
                        (substring (list-ref english-months (1- month)) 0 3)
                        day year))))
            ((string=? lang "de")
             (cond
               ((eq? style 'short) (iso-date year month day))
               ((eq? style 'long)
                (format #f "~d. ~a ~d"
                        day (list-ref german-months (1- month)) year))
               (else
                (format #f "~d. ~a ~d"
                        day (list-ref german-months-short (1- month)) year))))
            ((string=? lang "fr")
             (cond
               ((eq? style 'short) (iso-date year month day))
               ((eq? style 'long)
                (format #f "~d ~a ~d"
                        day (list-ref french-months (1- month)) year))
               (else
                (format #f "~d ~a ~d"
                        day (list-ref french-months-short (1- month)) year))))
            (else date))))))
(define (group-digits digits)
  (let loop ((end (string-length digits)) (acc '()))
    (if (<= end 3)
        (string-concatenate (cons (substring digits 0 end) acc))
        (loop (- end 3)
              (cons (string-append "," (substring digits (- end 3) end)) acc)))))

(define* (format-number locale number #:key (group? #t))
  "Format an integer NUMBER with LOCALE's digit grouping.  Other number
types pass through number->string.  When locale data is unavailable, use
comma-separated groups of three digits."
  (let ((locale (if (locale? locale) locale (make-locale locale))))
    (if (not (and group? (integer? number)))
        (number->string number)
        (or (let ((glibc-locale (make-glibc-locale locale LC_NUMERIC)))
              (and glibc-locale
                   (not (string-null?
                         (glibc:locale-thousands-separator glibc-locale)))
                   (false-if-exception
                     (format #f "~:h" number glibc-locale))))
            (let* ((text (number->string number))
                   (negative? (string-prefix? "-" text))
                   (digits (if negative? (substring text 1) text)))
              (if negative?
                  (string-append "-" (group-digits digits))
                  (group-digits digits)))))))

(define (plural-category lang count)
  (let ((n (abs count))
        (m10 (modulo (abs count) 10))
        (m100 (modulo (abs count) 100)))
    (cond
      ((string=? lang "zh") 'other)
      ((string=? lang "en") (if (= n 1) 'one 'other))
      ((string=? lang "ru")
       (cond ((and (= m10 1) (not (= m100 11))) 'one)
             ((and (>= m10 2) (<= m10 4)
                   (not (and (>= m100 12) (<= m100 14)))) 'few)
             (else 'many)))
      ((string=? lang "pl")
       (cond ((= n 1) 'one)
             ((and (>= m10 2) (<= m10 4)
                   (not (and (>= m100 12) (<= m100 14)))) 'few)
             (else 'many)))
      ((string=? lang "ar")
       (cond ((= n 0) 'zero)
             ((= n 1) 'one)
             ((= n 2) 'two)
             ((and (>= n 3) (<= n 10)) 'few)
             ((and (>= n 11) (<= n 99)) 'many)
             (else 'other)))
      (else (if (= n 1) 'one 'other)))))

(define plural-categories '(zero one two few many other))

(define (valid-plural-forms? forms)
  (and (list? forms)
       (every (match-lambda
                ((category . text)
                 (and (memq category plural-categories)
                      (string? text)))
                (_ #f))
              forms)
       (unique-symbol-key-alist? forms)))

(define* (plural locale count forms)
  "Choose the plural form for COUNT from FORMS, an alist of CLDR
categories (`zero', `one', `two', `few', `many', or `other'). COUNT must be an
integer. The caller formats the chosen string."
  (unless (integer? count)
    (error "plural count must be an integer" count))
  (unless (valid-plural-forms? forms)
    (error "plural forms must map unique CLDR categories to strings" forms))
  (let* ((locale (if (locale? locale) locale (make-locale locale)))
         (category (plural-category (locale-lang locale) count)))
    (or (assq-ref forms category)
        (assq-ref forms 'other)
        (assq-ref forms 'one)
        "")))

;; glibc locale names for the languages whose collation Grove opts into.
;; Regional variants fall back to the primary language's locale.
(define glibc-locale-names
  '(("en" . "en_US.UTF-8")
    ("fr" . "fr_FR.UTF-8")
    ("de" . "de_DE.UTF-8")
    ("es" . "es_ES.UTF-8")
    ("it" . "it_IT.UTF-8")
    ("pt" . "pt_PT.UTF-8")
    ("nl" . "nl_NL.UTF-8")
    ("sv" . "sv_SE.UTF-8")
    ("da" . "da_DK.UTF-8")
    ("nb" . "nb_NO.UTF-8")
    ("fi" . "fi_FI.UTF-8")
    ("pl" . "pl_PL.UTF-8")
    ("cs" . "cs_CZ.UTF-8")
    ("tr" . "tr_TR.UTF-8")
    ("ru" . "ru_RU.UTF-8")
    ("el" . "el_GR.UTF-8")
    ("ja" . "ja_JP.UTF-8")
    ("ko" . "ko_KR.UTF-8")
    ("zh-Hans" . "zh_CN.UTF-8")
    ("zh-Hant" . "zh_TW.UTF-8")
    ("zh" . "zh_CN.UTF-8")))

(define* (glibc-locale-name locale)
  "Return the glibc locale name for LOCALE, or #f when unsupported.

LOCALE is a tag string or a locale object.  Regional variants fall back
to their primary language, so `fr-CA' uses the `fr_FR' locale data."
  (let* ((locale (if (locale? locale) locale (make-locale locale)))
         (tag (locale-tag locale))
         (lang (locale-lang locale)))
    (or (assoc-ref glibc-locale-names tag)
        (assoc-ref glibc-locale-names lang))))

(define (make-glibc-locale locale category)
  (let ((name (glibc-locale-name locale)))
    (and name
         (false-if-exception
           (glibc:make-locale (list category) name)))))

(define* (locale-collation-comparator locale)
  "Return a caseless collation comparator for LOCALE, or #f.

The comparator uses the platform's locale collation data (glibc's
ISO 14651 tables, not a full UCA/CLDR implementation).  It returns #f
when the locale is unsupported or its data is missing, so callers can
fall back to folded code-point order."
  (let ((loc (make-glibc-locale locale LC_ALL)))
    (and loc
         (false-if-exception
           ;; A missing locale silently behaves like C, where uppercase sorts
           ;; first; use that as the probe.
           (and (glibc:string-locale<? "a" "B" loc)
                (lambda (a b)
                  (let ((ka (canonical-caseless-key a))
                        (kb (canonical-caseless-key b)))
                    (cond
                      ((string=? ka kb) #f)
                      ((glibc:string-locale<? ka kb loc) #t)
                      (else #f)))))))))

(define (folded-order a b)
  (let ((ka (canonical-caseless-key a))
        (kb (canonical-caseless-key b)))
    (and (not (string=? ka kb))
         (string<? ka kb))))

(define* (sort-strings strings #:key (locale #f) (compare #f))
  "Stably sort STRINGS by their canonical caseless match keys.

With LOCALE, use the platform's collation data when it is available and
fall back to folded code-point order otherwise.  COMPARE, a two-argument
predicate, overrides both.  Two strings with equal keys keep their input
order."
  (unless (and (list? strings) (every string? strings))
    (error "sort-strings requires a list of strings" strings))
  (unless (or (not compare) (procedure-accepts-arguments? compare 2))
    (error "sort-strings comparator must accept two strings or be #f" compare))
  (sort strings
        (or compare
            (and locale (locale-collation-comparator locale))
            folded-order)))

(define current-reader-locale (make-parameter #f))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove i18n)
  #:use-module (grove string-fun)
  #:use-module ((scheme char) #:select (string-foldcase))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 unicode)
  #:use-module ((ice-9 i18n) #:prefix glibc:)
  #:export (locale?
            make-locale
            locale-tag
            locale-lang
            locale-dir
            locale-html-attributes
            locale-matches?
            match-key
            message-catalog?
            make-message-catalog
            message-catalog-default
            message-catalog-entries
            default-message-catalog
            merge-message-catalogs
            message
            message-format
            format-date
            format-number
            plural
            locale-sort-key
            glibc-locale-name
            locale-collation-comparator
            og-locale-tag
            sort-strings))

;; Grove's i18n primitives are language tags and chrome messages. They carry no
;; document content; translated trees and their presentation live elsewhere.

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

(define (script-subtag subtags)
  (find (lambda (subtag)
          (and (= (string-length subtag) 4)
               (string-every char-set:letter subtag)))
        subtags))

(define (valid-locale-tag? tag)
  (and (non-empty-string? tag)
       (let ((subtags (string-split tag #\-)))
         (and (every (lambda (subtag)
                       (and (not (string-null? subtag))
                            (string-every char-set:letter+digit subtag)))
                     subtags)
              (let ((primary (car subtags)))
                (and (<= (string-length primary) 8)
                     (>= (string-length primary) 1)
                     (string-every char-set:letter primary)))))))

(define-record-type <locale>
  (%make-locale tag lang dir)
  locale?
  (tag locale-tag)
  (lang locale-lang)
  (dir locale-dir))

(define (resolve-direction subtags lang)
  "Derive a text direction from a script subtag, then from the language."
  (let ((script (script-subtag subtags)))
    (cond
      ((and script (member (string-titlecase script) rtl-scripts)) "rtl")
      ((and script (member (string-titlecase script) ltr-scripts)) "ltr")
      ((member lang rtl-languages) "rtl")
      (else "ltr"))))

(define (make-locale tag)
  "Normalize a BCP 47-style language tag into a locale with a text direction."
  (unless (string? tag)
    (error "locale tag must be a string" tag))
  (let ((tag (string-trim-both tag)))
    (unless (valid-locale-tag? tag)
      (error "locale tag must be a BCP 47-style language tag" tag))
    (let* ((subtags (string-split tag #\-))
           (lang (string-downcase (car subtags)))
           (dir (resolve-direction subtags lang)))
      (%make-locale tag lang dir))))

(define (locale-html-attributes locale)
  "Return the `lang' and `dir' HTML attributes for a locale."
  (unless (locale? locale)
    (error "locale-html-attributes requires a locale" locale))
  `((lang . ,(locale-tag locale))
    (dir . ,(locale-dir locale))))

(define (locale-matches? reader tag)
  "True when TAG is READER or a more specific tag beneath it.
So a `zh' reader matches `zh-CN', but `zh-CN' does not match `zh-TW'."
  (unless (and (string? reader) (string? tag))
    (error "locale-matches? requires two language tags" reader tag))
  (or (string-ci=? reader tag)
      (string-prefix-ci? (string-append reader "-") tag)))

(define (match-key text)
  "Return TEXT as a canonical caseless key for identity matching.

Case folding and canonical composition make `STRASSE', `Straße', and a
decomposed `e' plus combining acute compare equal to their canonical
spellings."
  (unless (string? text)
    (error "match-key requires a string" text))
  (string-normalize-nfc (string-foldcase (string-normalize-nfd text))))

(define-record-type <message-catalog>
  (%make-message-catalog default entries)
  message-catalog?
  (default message-catalog-default)
  (entries message-catalog-entries))

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

(define* (make-message-catalog entries #:key (default "en"))
  "Map locale tags to symbol-keyed messages, falling back to DEFAULT."
  (unless (valid-message-entries? entries)
    (error "message catalog requires locale-tagged message lists" entries))
  (unless (non-empty-string? default)
    (error "message catalog default locale must be a non-empty string" default))
  (%make-message-catalog default entries))

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
  (let* ((entries (message-catalog-entries catalog))
         (lookup (lambda (tags)
                   (any (lambda (tag)
                          (let ((messages (assoc-ref entries tag)))
                            (and messages (assq-ref messages key))))
                        tags))))
    (or (lookup (locale-fallbacks locale))
        (lookup (locale-fallbacks (message-catalog-default catalog)))
        (symbol->string key))))

;; Chrome messages for the renderers' navigation, skip link, tree labels,
;; metadata, and backmatter headings.  Projects extend the catalog through
;; the renderer's `#:messages' option.
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
               (backmatter-related . "Related")))
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
                    (backmatter-related . "相关")))
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
                    (backmatter-related . "相關")))
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
               (backmatter-related . "相关"))))))

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
  (let* ((base-entries (message-catalog-entries base))
         (override-entries (message-catalog-entries override))
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
      #:default (message-catalog-default override))))

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
       (let ((parts (string-split text #\-)))
         (and (= (length parts) 3)
              (every (lambda (part)
                       (and (not (string-null? part))
                            (string-every char-set:digit part)))
                     parts)
              (map string->number parts)))))

(define* (format-date locale date #:key (style 'medium))
  "Format an ISO YYYY-MM-DD DATE for LOCALE.  Unparseable dates and
locales without date data return DATE unchanged."
  (let ((parts (parse-iso-date date)))
    (if (not parts)
        date
        (let* ((locale (if (locale? locale) locale (make-locale locale)))
               (lang (locale-lang locale))
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
  "Format an integer NUMBER with the locale's digit grouping.  Other
number types pass through number->string."
  (if (not (integer? number))
      (number->string number)
      (let* ((text (number->string number))
             (negative? (string-prefix? "-" text))
             (digits (if negative? (substring text 1) text))
             (digits (if group? (group-digits digits) digits)))
        (if negative? (string-append "-" digits) digits))))

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
(define* (plural locale count forms)
  "Choose the plural form for COUNT from FORMS, an alist of CLDR
categories such as one and other.  The caller formats the chosen string."
  (let* ((locale (if (locale? locale) locale (make-locale locale)))
         (category (plural-category (locale-lang locale) count)))
    (or (assq-ref forms category)
        (assq-ref forms 'other)
        (assq-ref forms 'one)
        "")))

(define (locale-sort-key text)
  "Return TEXT's collation key: its canonical caseless form.

The key is locale-independent, so ordering is deterministic across
machines.  It is not a UCA collation key: accented letters sort by code
point rather than by language-tailored rules."
  (match-key text))

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
to their primary language, so `fr-CA' uses the `fr_FR' collation."
  (let* ((locale (if (locale? locale) locale (make-locale locale)))
         (tag (locale-tag locale))
         (lang (locale-lang locale)))
    (or (assoc-ref glibc-locale-names tag)
        (assoc-ref glibc-locale-names lang))))

(define* (locale-collation-comparator locale)
  "Return a caseless collation comparator for LOCALE, or #f.

The comparator uses the platform's locale collation data (glibc's
ISO 14651 tables, not a full UCA/CLDR implementation).  It returns #f
when the locale is unsupported or its data is missing, so callers can
fall back to folded code-point order."
  (let ((name (glibc-locale-name locale)))
    (and name
         (false-if-exception
           (let ((loc (glibc:make-locale (list LC_ALL) name)))
             ;;     A missing locale silently behaves like C, where
             ;;     uppercase sorts first; use that as the probe.
             (and (glibc:string-locale<? "a" "B" loc)
                  (lambda (a b)
                    (let ((ka (match-key a))
                          (kb (match-key b)))
                      (cond
                        ((string=? ka kb) #f)
                        ((glibc:string-locale<? ka kb loc) #t)
                        (else #f))))))))))

(define (folded-order a b)
  (let ((ka (locale-sort-key a))
        (kb (locale-sort-key b)))
    (and (not (string=? ka kb))
         (string<? ka kb))))

(define* (sort-strings strings #:key (locale #f) (compare #f))
  "Stably sort STRINGS by locale-sort-key.

With LOCALE, use the platform's collation data when it is available and
fall back to folded code-point order otherwise.  COMPARE, a two-argument
predicate, overrides both.  Two strings with equal keys keep their input
order."
  (sort strings
        (or compare
            (and locale (locale-collation-comparator locale))
            folded-order)))

(define (og-locale-tag locale)
  "Return the Open Graph locale tag (language_TERRITORY) for LOCALE.

A region subtag wins (`fr-CA' -> `fr_CA'); Chinese script subtags map to
`zh_CN' and `zh_TW'; otherwise the glibc locale name supplies the
territory, and an unknown tag falls back to its language subtag."
  (let* ((locale (if (locale? locale) locale (make-locale locale)))
         (tag (locale-tag locale))
         (parts (string-split tag #\-))
         (lang (car parts))
         (second (if (>= (length parts) 2) (cadr parts) #f))
         (third (if (>= (length parts) 3) (caddr parts) #f))
         (script? (lambda (part)
                    (and part (= (string-length part) 4)
                         (string-every char-set:letter part))))
         (region? (lambda (part)
                    (and part (= (string-length part) 2)
                         (string-every char-set:letter part))))
         (script (and (script? second) second))
         (region (cond ((region? second) second)
                       ((region? third) third)
                       (else #f))))
    (cond
      (region (string-append lang "_" region))
      ((and (string=? lang "zh") (equal? script "Hant")) "zh_TW")
      ((string=? lang "zh") "zh_CN")
      ((glibc-locale-name tag)
       (let* ((name (glibc-locale-name tag))
              (dot (string-index name #\.)))
         (if dot (substring name 0 dot) name)))
      (else lang))))

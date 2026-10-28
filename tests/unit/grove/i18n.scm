;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove i18n)
  #:use-module (srfi srfi-64)
  #:use-module (grove i18n)
  #:use-module (grove unicode))

(test-begin "grove i18n")

(test-assert "system locale names stay behind the i18n API"
  (not (module-variable
         (module-public-interface (resolve-module '(grove i18n)))
         'glibc-locale-name)))


;;; Locales

(let ((locale (make-locale "zh-Hant")))
  (test-equal "locales keep their authored tag" "zh-Hant" (locale-tag locale))
  (test-equal "locales expose their primary language" "zh" (locale-lang locale))
  (test-equal "Chinese locales are left-to-right" "ltr" (locale-dir locale)))

(let* ((locale (make-locale "ar-EG"))
       (tag (locale-tag locale))
       (language (locale-lang locale))
       (direction (locale-dir locale)))
  (string-set! tag 0 (integer->char 88))
  (string-set! language 0 (integer->char 88))
  (string-set! direction 0 (integer->char 88))
  (test-equal "locale accessors return independent strings"
    '("ar-EG" "ar" "rtl")
    (list (locale-tag locale) (locale-lang locale) (locale-dir locale))))

(test-equal "Arabic locales request right-to-left direction"
  "rtl" (locale-dir (make-locale "ar-EG")))

(test-equal "locale HTML attributes carry language and direction"
  '((lang . "he") (dir . "rtl"))
  (locale-html-attributes (make-locale "he")))

(test-assert "locale matching accepts a more specific tag"
  (locale-matches? "zh" "zh-CN"))
(test-assert "locale matching canonicalizes equivalent tags"
  (locale-matches? "zh-CN" "zh-Hans"))
(test-assert "locale matching is case-insensitive"
  (locale-matches? "ZH" "zh-CN"))
(test-assert "locale matching rejects a different region"
  (not (locale-matches? "zh-CN" "zh-TW")))
(test-error "locale matching rejects malformed language tags" #t
  (locale-matches? "not a tag" "en"))
(test-error "locale matching rejects non-string language tags" #t
  (locale-matches? 'en "en"))

(test-equal "a right-to-left script overrides a left-to-right language"
  "rtl" (locale-dir (make-locale "az-Arab")))
(test-equal "a right-to-left script applies to a language without a default"
  "rtl" (locale-dir (make-locale "ks-Arab")))
(test-equal "a left-to-right script overrides a right-to-left language"
  "ltr" (locale-dir (make-locale "ku-Latn")))
(test-equal "common scripts resolve left-to-right"
  "ltr" (locale-dir (make-locale "sr-Latn")))

(test-equal "Unicode locale extensions do not become script subtags"
  "en-u-nu-latn" (locale-tag (make-locale "en-u-nu-Latn")))
(test-equal "extensions preserve their position after the language core"
  "en-Latn-US-u-ca-gregory"
  (locale-tag (make-locale "en-Latn-US-u-ca-gregory")))
(test-equal "extension sequences use canonical singleton order"
  "en-t-foo-u-ca-gregory"
  (locale-tag (make-locale "en-u-ca-gregory-t-foo")))
(test-equal "extension scripts do not affect text direction"
  "ltr" (locale-dir (make-locale "en-u-nu-Arab")))
(test-equal "private-use subtags are normalized as private use"
  "en-x-us" (locale-tag (make-locale "en-x-US")))
(test-equal "private-use-only tags keep one private-use marker"
  "x-foo" (locale-tag (make-locale "X-Foo")))
(test-equal "numeric regions are not confused with variants"
  "es-419" (locale-tag (make-locale "es-419")))
(test-equal "grandfathered tags are preserved"
  "i-default" (locale-tag (make-locale "I-DEFAULT")))
(test-equal "Chinese canonicalization retains extended language subtags"
  "zh-cmn-Hans" (locale-tag (make-locale "ZH-cmn")))

(test-error "locale tags reject spaces" #t (make-locale "not a tag"))
(test-error "locale tags reject empty strings" #t (make-locale ""))
(test-error "locale tags reject non-strings" #t (make-locale 'en))
(test-error "locale tags reject incomplete extensions" #t
  (make-locale "en-u"))
(test-error "locale tags reject duplicate extension singletons" #t
  (make-locale "en-u-ca-a-foo-u-nu-latn"))
(test-error "locale tags reject duplicate variants" #t
  (make-locale "sl-rozaj-rozaj"))
(test-error "locale tags reject malformed variants" #t
  (make-locale "en-rozaj-abc"))

;;; Matching keys

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
(test-error "message lookup rejects invalid locale tags" #t
  (message catalog 'home "not a locale"))
(test-equal "unknown keys fall back to their name"
  "notes" (message catalog 'notes "en"))
(test-equal "messages use the catalog default locale by default"
  "Home" (message catalog 'home))
(test-equal "catalog locale tags canonicalize language-only tags"
  "Accueil"
  (message (make-message-catalog
             '(("FR" . ((home . "Accueil"))))
             #:default "FR")
           'home "fr"))
(test-error "catalogs reject locale aliases that canonicalize to the same tag" #t
  (make-message-catalog
    '(("zh-CN" . ((home . "首页")))
      ("zh-Hans" . ((home . "首頁"))))))
(test-error "catalogs reject invalid locale tags" #t
  (make-message-catalog '(("not a locale" . ((home . "Home"))))))
(test-error "catalogs reject duplicate message keys" #t
  (make-message-catalog '(("en" . ((home . "Home") (home . "Start"))))))
(test-error "catalogs reject non-symbol message keys" #t
  (make-message-catalog '(("en" . (("home" . "Home"))))))
(test-error "catalogs reject symbol locale keys" #t
  (make-message-catalog '((en . ((home . "Home"))))))
(test-error "message keys must be symbols" #t (message catalog "home"))
(test-error "message rejects non-catalogs" #t (message '() 'home))

(let* ((messages (list (cons 'home "Home")))
       (catalog (make-message-catalog (list (cons "en" messages)))))
  (set-cdr! (car messages) "Changed")
  (test-equal "catalogs snapshot caller-owned message lists"
    "Home" (message catalog 'home))
  (let ((entries (message-catalog-entries catalog)))
    (set-cdr! (car (cdar entries)) "Changed copy"))
  (test-equal "catalog accessors do not expose mutable internal entries"
    "Home" (message catalog 'home))
  (let ((default (message-catalog-default catalog)))
    (string-set! default 0 #\x))
  (test-equal "catalog accessors do not expose mutable internal tags"
    "en" (message-catalog-default catalog)))

(define script-catalog
  (make-message-catalog
    '(("zh-Hant" . ((home . "首頁")))
      ("en" . ((home . "Home"))))
    #:default "en"))
(test-equal "messages fall back through shorter locale prefixes"
  "首頁" (message script-catalog 'home "zh-Hant-CN"))
(test-equal "a more specific catalog entry does not answer a shorter request"
  "Home" (message script-catalog 'home "zh-CN"))

;;; Formatting

(test-assert "ISO date predicate accepts valid leap days"
  (iso-date? "2024-02-29"))
(test-assert "ISO date predicate rejects impossible calendar dates"
  (not (iso-date? "1900-02-29")))
(test-assert "ISO date predicate requires a string"
  (not (iso-date? #f)))
(test-equal "English long dates"
  "September 27, 2026" (format-date "en" "2026-09-27" #:style 'long))
(test-equal "English medium dates"
  "Sep 27, 2026" (format-date "en" "2026-09-27"))
(test-equal "Chinese dates"
  "2026年9月27日" (format-date "zh-Hans" "2026-09-27"))
(test-equal "Gregorian leap day is accepted"
  "Feb 29, 2024" (format-date "en" "2024-02-29"))
(test-equal "century years follow Gregorian leap rules"
  (list "1900-02-29" "Feb 29, 2000")
  (list (format-date "en" "1900-02-29")
        (format-date "en" "2000-02-29")))
(test-equal "impossible calendar dates pass through"
  (list "2026-02-29" "2026-04-31" "2026-13-01" "2026-00-10")
  (map (lambda (date) (format-date "en" date))
       '("2026-02-29" "2026-04-31" "2026-13-01" "2026-00-10")))
(test-equal "dates require the documented YYYY-MM-DD shape"
  "2026-9-27" (format-date "en" "2026-9-27"))
(test-error "date styles are validated even for unparseable input"
  #t (format-date "en" "not-a-date" #:style 'compact))
(test-equal "unsupported locales keep the ISO date"
  "2026-09-27" (format-date "sw" "2026-09-27"))
(test-equal "unparseable dates pass through"
  "not-a-date" (format-date "en" "not-a-date"))
(test-equal "numbers group by threes"
  "1,234,567" (format-number "en" 1234567))
(test-equal "negative numbers keep their sign"
  "-1,234" (format-number "en" -1234))
(test-equal "number grouping follows the requested locale"
  "1.234.567" (format-number "de" 1234567))
(test-equal "unsupported number locales use deterministic grouping"
  "1,234,567" (format-number "sw" 1234567))
(test-equal "English singular"
  "~a item" (plural "en" 1 '((one . "~a item") (other . "~a items"))))
(test-equal "English plural"
  "~a items" (plural "en" 2 '((one . "~a item") (other . "~a items"))))
(test-equal "Chinese has a single plural form"
  "~a 条" (plural "zh-Hans" 1 '((one . "~a 条") (other . "~a 条"))))

;;; Sorting

(test-assert "Unicode exposes canonical caseless matching"
  (let ((unicode (module-public-interface (resolve-module '(grove unicode))))
        (i18n (module-public-interface (resolve-module '(grove i18n)))))
    (and (module-variable unicode 'canonical-caseless-key)
         (not (module-variable i18n 'canonical-caseless-key))
         (not (module-variable i18n 'locale-sort-key)))))
(test-equal "strings sort by their folded keys"
  '("apple" "Straße" "Zebra" "éclair")
  (sort-strings '("Straße" "apple" "Zebra" "éclair")))
(test-equal "equal keys keep their input order"
  '("Straße" "STRASSE")
  (sort-strings '("Straße" "STRASSE")))
(test-error "string sorting requires a list"
  #t (sort-strings "not a list"))
(test-error "string sorting rejects non-string elements"
  #t (sort-strings '("valid" 42)))
(test-error "string sorting requires a comparator procedure"
  #t (sort-strings '("a" "b") #:compare 'descending))
(test-error "string sorting comparators must accept two strings"
  #t (sort-strings '("a" "b") #:compare (lambda (_) #t)))

;;; Locale-aware collation

(test-assert "supported locale data provides a reusable comparator"
  (procedure? (locale-collation-comparator "fr-CA")))
(test-equal "unsupported locale data has no platform comparator"
  #f (locale-collation-comparator "xx"))
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

;;; Locale formatting and default catalog

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
(test-equal "German short dates"
  "2026-09-27" (format-date "de" "2026-09-27" #:style 'short))
(test-equal "French short dates"
  "2026-09-27" (format-date "fr" "2026-09-27" #:style 'short))
(test-equal "plural falls back to other"
  "other" (plural "ru" 5 '((other . "other"))))
(test-equal "plural falls back to the empty string"
  "" (plural "ru" 5 '()))
(test-error "plural requires an integer count"
  #t (plural "en" 1.5 '((one . "one") (other . "other"))))
(test-error "plural rejects unknown categories"
  #t (plural "en" 1 '((singular . "one"))))
(test-error "plural rejects duplicate categories"
  #t (plural "en" 1 '((one . "one") (one . "another"))))
(test-error "plural requires string forms"
  #t (plural "en" 1 '((one . 1))))

(test-equal "generic Chinese tags fall back to the zh catalog"
  "站点导航" (message default-message-catalog 'nav-site "zh-CN"))
(test-equal "Traditional Chinese has its own catalog"
  "網站導覽" (message default-message-catalog 'nav-site "zh-Hant"))

(test-equal "zh-CN canonicalizes to zh-Hans"
  "zh-Hans" (locale-tag (make-locale "zh-CN")))
(test-equal "zh-TW canonicalizes to zh-Hant"
  "zh-Hant" (locale-tag (make-locale "zh-TW")))

(test-end "grove i18n")

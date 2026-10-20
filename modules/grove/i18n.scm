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
            plural))

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
                 (format #f "~4,'0d-~2,'0d-~2,'0d" year month day)
                 (format #f "~d年~d月~d日" year month day)))
            ((string=? lang "en")
             (cond
               ((eq? style 'short)
                (format #f "~4,'0d-~2,'0d-~2,'0d" year month day))
               ((eq? style 'long)
                (format #f "~a ~d, ~d"
                        (list-ref english-months (1- month)) day year))
               (else
                (format #f "~a ~d, ~d"
                        (substring (list-ref english-months (1- month)) 0 3)
                        day year))))
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
  (cond
    ((string=? lang "zh") 'other)
    ((string=? lang "en") (if (= count 1) 'one 'other))
    (else (if (= count 1) 'one 'other))))

(define* (plural locale count forms)
  "Choose the plural form for COUNT from FORMS, an alist of CLDR
categories such as one and other.  The caller formats the chosen string."
  (let* ((locale (if (locale? locale) locale (make-locale locale)))
         (category (plural-category (locale-lang locale) count)))
    (or (assq-ref forms category)
        (assq-ref forms 'other)
        (assq-ref forms 'one)
        "")))

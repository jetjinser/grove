;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove i18n)
  #:use-module (grove string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 match)
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
            message))

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

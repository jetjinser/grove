;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit grove string)
  #:use-module (srfi srfi-64)
  #:use-module (grove private string-fun)
  #:use-module (grove unicode))

(test-begin "grove string and Unicode helpers")

(test-assert "non-empty-string? accepts non-empty strings"
  (non-empty-string? "grove"))
(test-assert "non-empty-string? rejects the empty string"
  (not (non-empty-string? "")))
(test-assert "non-empty-string? rejects non-strings"
  (not (non-empty-string? #f)))

(test-equal "caseless keys fold case across the full Unicode range"
  (canonical-caseless-key "STRASSE")
  (canonical-caseless-key "Straße"))
(test-equal "caseless keys compose canonically equivalent spellings"
  (canonical-caseless-key "café")
  (canonical-caseless-key
    (string-append "cafe" (string (integer->char #x301)))))
(test-equal "caseless keys keep the folded canonical spelling"
  "strasse" (canonical-caseless-key "Straße"))
(test-error "caseless keys require strings" #t
  (canonical-caseless-key 'en))

(test-equal "grapheme-count handles empty text" 0
  (grapheme-count ""))
(test-equal "grapheme-count counts ordinary characters" 5
  (grapheme-count "grove"))
(test-equal "grapheme-count keeps combining marks with their base" 1
  (grapheme-count "è̤"))
(test-equal "grapheme-count handles canonically composed accents" 1
  (grapheme-count "é"))
(test-equal "grapheme-count keeps emoji ZWJ sequences together" 1
  (grapheme-count "👩‍💻"))
(test-equal "grapheme-count pairs regional indicators into flags" 2
  (grapheme-count "🇺🇸🇫🇷"))
(test-equal "grapheme-count keeps emoji skin-tone modifiers together" 1
  (grapheme-count "👍🏽"))
(test-equal "grapheme-count keeps keycap variation sequences together" 1
  (grapheme-count "1️⃣"))
(test-error "grapheme-count rejects non-strings" #t
  (grapheme-count #f))

(test-end "grove string and Unicode helpers")

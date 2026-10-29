;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
#|
   Test cases for escaped (percent-encoded data)

   Most general procedure: encode-string

   The first two sets of non-property test cases are via Ivan Raikov.
|#

(test-begin "direct encoding and decoding")

(test-equal "Encode all four byte lengths in a single call"
            "a%20b%C3%A9c%E2%99%82d%F0%9F%98%8Ee"
            (encode-string "a béc♂d😎e" (char-set-difference char-set:full char-set:uri)))

(test-equal "Decode everything except ♂ (U+2642, encoded as %E2%99%82)"
            "a béc%E2%99%82d😎e"
            (decode-string "a%20b%C3%A9c%E2%99%82d%F0%9F%98%8Ee"
                           (char-set-complement (char-set #\♂))))

(test-equal "Direct encoding treats percent as character"
            "foo%2520bar"
            (encode-string "foo%20bar" (char-set-difference char-set:full char-set:uri)))

(test-equal "Escaped percent round trip during direct decoding"
            "foo%20bar"
            (decode-string "foo%2520bar" char-set:full))

(test-end "direct encoding and decoding")



(test-begin "overlong percent-encoded sequences in decode-string")

(define overlong-strings
  `(["overlong 2-byte encoding of U+0020 (space)"  "a%C0%A0b"]
    ["overlong 3-byte encoding of U+0020 (space)"  "a%E0%80%A0b"]
    ["overlong 4-byte encoding of U+0020 (space)"  "a%E0%80%80%A0b"]
    ["overlong 3-byte encoding of U+00E9 (é)"      "a%E0%80%A9b"]
    ["incomplete 2-byte sequence (missing continuation byte)"           "a%C0b"]
    ["incomplete 3-byte sequence (missing third byte for ♂ U+2642)"     "a%E2%99"]
    ["incomplete 4-byte sequence (missing fourth byte for 😎 U+1F60E)" "a%F0%9F%98"]
    ["non-encoded character where a continuation byte is expected"      "a%F0%9F%98x"]
    ["UTF-8 continuation byte without a leading byte"                   "a%A9"]))

(define run-string-decode
  (match-lambda
    [((desc src))
     (test-assert desc
                  (decode-string src char-set:full))]))

(test-expect-fail 9)

(for-each run-string-decode overlong-strings)

(test-end)

(test-begin "overlong percent-encoded sequences in URI string-encoding")

(define (run-string-conversion lab template from-string)
  (match-lambda
    [((desc src))
     (let ([formatted (format template src)])
       (test-assert (format "~a parse fails on ~a" lab desc)
                    (from-string formatted)))]))

(test-expect-fail 180)

(for-each (run-string-conversion "URI user component" "foo://~a@" string->uri) overlong-strings)
(for-each (run-string-conversion "URI reference user component" "//~a@" string->uri) overlong-strings)
(for-each (run-string-conversion "URI host component" "foo://~a/" string->uri) overlong-strings)
(for-each (run-string-conversion "URI reference host component" "//~a/" string->uri) overlong-strings)
(for-each (run-string-conversion "URI path component" "foo:~a/" string->uri) overlong-strings)
(for-each (run-string-conversion "URI reference path component" "~a/" string->uri) overlong-strings)
(for-each (run-string-conversion "URI query component" "foo:?~a" string->uri) overlong-strings)
(for-each (run-string-conversion "URI query component" "?~a" string->uri) overlong-strings)
(for-each (run-string-conversion "URI query component" "foo:#~a" string->uri) overlong-strings)
(for-each (run-string-conversion "URI query component" "#~a" string->uri) overlong-strings)

(for-each (run-string-conversion "IRI user component" "foo://~a@" string->iri) overlong-strings)
(for-each (run-string-conversion "IRI reference user component" "//~a@" string->iri) overlong-strings)
(for-each (run-string-conversion "IRI host component" "foo://~a/" string->iri) overlong-strings)
(for-each (run-string-conversion "IRI reference host component" "//~a/" string->iri) overlong-strings)
(for-each (run-string-conversion "IRI path component" "foo:~a/" string->iri) overlong-strings)
(for-each (run-string-conversion "IRI reference path component" "~a/" string->iri) overlong-strings)
(for-each (run-string-conversion "IRI query component" "foo:?~a" string->iri) overlong-strings)
(for-each (run-string-conversion "IRI query component" "?~a" string->iri) overlong-strings)
(for-each (run-string-conversion "IRI query component" "foo:#~a" string->iri) overlong-strings)
(for-each (run-string-conversion "IRI query component" "#~a" string->iri) overlong-strings)

(test-end)

(define sample-iri (string->iri "http://example.org/some/where/place?x#a"))
(define sample-iri-ref (string->iri "//example.org/some/where/place?x#a"))
(define sample-uri (string->uri "http://example.org/some/where/place?x#a"))
(define sample-uri-ref (string->uri "//example.org/some/where/place?x#a"))

(define (reserved-clear-property ident sub upd cset)
  ;; IRI component character sets can include almost every Unicode scalar
  ;; value.  Test representative legal characters rather than expanding the
  ;; full set into a multi-megabyte string.
  (define candidates '(#\a #\! #\$ #\~ #\é #\東 #\😎))
  (define target-clear
    (filter (lambda (char) (char-set-contains? cset char)) candidates))
  (define string-repr (list->string target-clear))
  (define updated (upd ident string-repr))
  (string=? (sub updated)
            string-repr))

;; The first scheme character must be alphabetic.  Charset iteration order is
;; implementation-dependent, so arrange one alphabetic character first and
;; test every remaining scheme character after it.
(define (scheme-clear-property ident sub upd)
  (let* ((characters (char-set->list char-set:scheme))
         (first (find char-alphabetic? characters))
         (ordered (cons first (delete first characters)))
         (value (list->string ordered)))
    (string=? (sub (upd ident value)) value)))

(define (reserved-escaped-property ident sub upd cset)
  ;; The reference property enumerates the complement of each component
  ;; charset.  That set contains nearly every Unicode scalar value, which is
  ;; impractical to materialize on Guile.  Exercise representative forbidden
  ;; characters from ASCII, BMP, and supplementary planes instead.
  (define candidates '(#\space #\" #\< #\> #\[ #\\ #\] #\^ #\` #\{ #\| #\} #\é #\😎 #\nul))
  (define target-escaped
    (filter (lambda (char) (char-set-contains? cset char)) candidates))
  (define string-repr
    (encode-string (list->string target-escaped) cset))
  (define updated (upd ident string-repr))
  (string=? (sub updated)
            string-repr))

(test-begin "URI scheme")
(test-assert "Scheme character set not encoded"
             (scheme-clear-property sample-uri uri-scheme update-uri-scheme))
(test-expect-fail 11)
(test-assert "Relative reference cannot have scheme set"
             (update-uri-scheme sample-uri-ref "urn"))

(test-assert (update-uri-scheme sample-uri "sch%20eme"))
(test-assert (update-uri-scheme sample-uri "%20scheme"))
(test-assert (update-uri-scheme sample-uri "scheme%20"))
(test-assert (update-uri-scheme sample-uri "£dosh"))
(test-assert (update-uri-scheme sample-uri "crêpes"))

(test-assert (update-uri-scheme sample-uri-ref "sch%20eme"))
(test-assert (update-uri-scheme sample-uri-ref "%20scheme"))
(test-assert (update-uri-scheme sample-uri-ref "scheme%20"))
(test-assert (update-uri-scheme sample-uri-ref "£dosh"))
(test-assert (update-uri-scheme sample-uri-ref "crêpes"))
(test-end "URI scheme")

;;;

(test-begin "IRI scheme")
(test-assert "Scheme character set not encoded"
             (scheme-clear-property sample-iri iri-scheme update-iri-scheme))
(test-expect-fail 11)
(test-assert "Relative reference cannot have scheme set"
             (update-iri-scheme sample-iri-ref "urn"))

(test-assert (update-iri-scheme sample-iri "sch%20eme"))
(test-assert (update-iri-scheme sample-iri "%20scheme"))
(test-assert (update-iri-scheme sample-iri "scheme%20"))
(test-assert (update-iri-scheme sample-iri "£dosh"))
(test-assert (update-iri-scheme sample-iri "crêpes"))

(test-assert (update-iri-scheme sample-iri-ref "sch%20eme"))
(test-assert (update-iri-scheme sample-iri-ref "%20scheme"))
(test-assert (update-iri-scheme sample-iri-ref "scheme%20"))
(test-assert (update-iri-scheme sample-iri-ref "£dosh"))
(test-assert (update-iri-scheme sample-iri-ref "crêpes"))
(test-end "IRI scheme")

;;;

(test-begin "implicit encoding in URI user setters")

(test-assert "Reserved characters not encoded updating non-relative user"
             (reserved-clear-property sample-uri uri-user update-uri-user
                                      (char-set-difference char-set:uri-userinfo char-set:reserved)))

(test-assert "Reserved characters not encoded updating relative user"
             (reserved-clear-property sample-uri-ref uri-user update-uri-user
                                      (char-set-difference char-set:uri-userinfo char-set:reserved)))

(test-assert "Other permissible characters not encoded updating non-relative user"
             (reserved-clear-property sample-uri uri-user update-uri-user
                                      char-set:uri-userinfo))

(test-assert "Other permissible characters not encoded updating non-relative user"
             (reserved-clear-property sample-uri-ref uri-user update-uri-user
                                      char-set:uri-userinfo))

(test-assert "Encode any other illegal character upadating non-relative user"
             (reserved-escaped-property sample-uri uri-user update-uri-user
                                        (char-set-complement char-set:uri-userinfo)))

(test-assert "Encode any other illegal character updating relative user"
             (reserved-escaped-property sample-uri-ref uri-user update-uri-user
                                        (char-set-complement char-set:uri-userinfo)))

(test-end "implicit encoding in URI user setters")

;;;

(test-begin "implicit encoding in URI host reg-name setters")

(test-assert "Reserved characters not encoded updating non-relative host reg-name"
             (reserved-clear-property sample-uri uri-host update-uri-host
                                      (char-set-difference char-set:uri-reg-name char-set:reserved)))

(test-assert "Reserved characters not encoded updating relative host reg-name"
             (reserved-clear-property sample-uri-ref uri-host update-uri-host
                                      (char-set-difference char-set:uri-reg-name char-set:reserved)))

(test-assert "Other permissible characters not encoded updating non-relative host reg-name"
             (reserved-clear-property sample-uri uri-host update-uri-host
                                      char-set:uri-reg-name))

(test-assert "Other permissible characters not encoded updating relative host reg-name"
             (reserved-clear-property sample-uri-ref uri-host update-uri-host
                                      char-set:uri-reg-name))

(test-assert "Encode any other illegal character updating non-relative host reg-name"
             (reserved-escaped-property sample-uri uri-host update-uri-host
                                        (char-set-complement char-set:uri-reg-name)))

(test-assert "Encode any other illegal character updating relative host reg-name"
             (reserved-escaped-property sample-uri-ref uri-host update-uri-host
                                        (char-set-complement char-set:uri-reg-name)))

(test-end "implicit encoding in URI host reg-name setters")

;;;

(test-begin "implicit encoding in URI query reg-name setters")

(test-assert "Reserved characters not encoded updating non-relative query reg-name"
             (reserved-clear-property sample-uri uri-query update-uri-query
                                      (char-set-difference char-set:uri-query char-set:reserved)))

(test-assert "Reserved characters not encoded updating relative query reg-name"
             (reserved-clear-property sample-uri-ref uri-query update-uri-query
                                      (char-set-difference char-set:uri-query char-set:reserved)))

(test-assert "Other permissible characters not encoded updating non-relative query reg-name"
             (reserved-clear-property  sample-uri uri-query update-uri-query
                                       char-set:uri-query))

(test-assert "Other permissible characters not encoded updating relative query reg-name"
             (reserved-clear-property  sample-uri-ref uri-query update-uri-query
                                       char-set:uri-query))

(test-assert "Encode any other illegal character updating non-relative query reg-name"
             (reserved-escaped-property sample-uri uri-query update-uri-query
                                        (char-set-complement char-set:uri-query)))

(test-assert "Encode any other illegal character updating relative query reg-name"
             (reserved-escaped-property sample-uri-ref uri-query update-uri-query
                                        (char-set-complement char-set:uri-query)))

(test-end "implicit encoding in URI query reg-name setters")

;;;

(test-begin "implicit encoding in URI fragment reg-name setters")

(test-assert "Reserved characters not encoded updating non-relative fragment reg-name"
             (reserved-clear-property sample-uri uri-fragment update-uri-fragment
                                      (char-set-difference char-set:uri-fragment char-set:reserved)))

(test-assert "Reserved characters not encoded updating relative fragment reg-name"
             (reserved-clear-property sample-uri-ref uri-fragment update-uri-fragment
                                      (char-set-difference char-set:uri-fragment char-set:reserved)))

(test-assert "Other permissible characters not encoded updating non-relative fragment reg-name"
             (reserved-clear-property sample-uri uri-fragment update-uri-fragment
                                      char-set:uri-fragment))

(test-assert "Other permissible characters not encoded updating relative fragment reg-name"
             (reserved-clear-property sample-uri-ref uri-fragment update-uri-fragment
                                      char-set:uri-fragment))

(test-assert "Encode any other illegal character updating non-relative fragment reg-name"
             (reserved-escaped-property sample-uri uri-fragment update-uri-fragment
                                        (char-set-complement char-set:uri-fragment)))

(test-assert "Encode any other illegal character updating relative fragment reg-name"
             (reserved-escaped-property sample-uri-ref uri-fragment update-uri-fragment
                                        (char-set-complement char-set:uri-fragment)))

(test-end "implicit encoding in URI fragment reg-name setters")

;;;

(test-begin "implicit encoding in IRI user setters")

(test-assert "Reserved characters not encoded updating non-relative user"
             (reserved-clear-property sample-iri iri-user update-iri-user
                                      (char-set-difference char-set:iri-userinfo char-set:reserved)))

(test-assert "Reserved characters not encoded updating relative user"
             (reserved-clear-property sample-iri-ref iri-user update-iri-user
                                      (char-set-difference char-set:iri-userinfo char-set:reserved)))

(test-assert "Other permissible characters not encoded updating non-relative user"
             (reserved-clear-property sample-iri iri-user update-iri-user
                                      char-set:iri-userinfo))

(test-assert "Other permissible characters not encoded updating relative user"
             (reserved-clear-property sample-iri-ref iri-user update-iri-user
                                      char-set:iri-userinfo))

(test-assert "Encode any other illegal character updating non-relative user"
             (reserved-escaped-property sample-iri iri-user update-iri-user
                                        (char-set-complement char-set:iri-userinfo)))

(test-assert "Encode any other illegal character updating relative user"
             (reserved-escaped-property sample-iri-ref iri-user update-iri-user
                                        (char-set-complement char-set:iri-userinfo)))

(test-end "implicit encoding in IRI user setters")

;;;

(test-begin "implicit encoding in IRI host reg-name setters")

(test-assert "Reserved characters not encoded updating non-relative host reg-name"
             (reserved-clear-property sample-iri iri-host update-iri-host
                                      (char-set-difference char-set:iri-reg-name char-set:reserved)))

(test-assert "Reserved characters not encoded updating relative host reg-name"
             (reserved-clear-property sample-iri-ref iri-host update-iri-host
                                      (char-set-difference char-set:iri-reg-name char-set:reserved)))

(test-assert "Other permissible characters not encoded updating non-relative host reg-name"
             (reserved-clear-property sample-iri iri-host update-iri-host
                                      char-set:iri-reg-name))

(test-assert "Other permissible characters not encoded updating relative host reg-name"
             (reserved-clear-property sample-iri-ref iri-host update-iri-host
                                      char-set:iri-reg-name))

(test-assert "Encode any other illegal character updating non-relative host reg-name"
             (reserved-escaped-property sample-iri iri-host update-iri-host
                                        (char-set-complement char-set:iri-reg-name)))

(test-assert "Encode any other illegal character updating relative host reg-name"
             (reserved-escaped-property sample-iri-ref iri-host update-iri-host
                                        (char-set-complement char-set:iri-reg-name)))

(test-end "implicit encoding in IRI host reg-name setters")

;;;

(test-begin "implicit encoding in IRI query reg-name setters")

(test-assert "Reserved characters not encoded updating non-relative query"
             (reserved-clear-property sample-iri iri-query update-iri-query
                                      (char-set-difference char-set:iri-query char-set:reserved)))

(test-assert "Reserved characters not encoded updating relative query"
             (reserved-clear-property sample-iri-ref iri-query update-iri-query
                                      (char-set-difference char-set:iri-query char-set:reserved)))

(test-assert "Other permissible characters not encoded updating non-relative query"
             (reserved-clear-property sample-iri iri-query update-iri-query
                                      char-set:iri-query))

(test-assert "Other permissible characters not encoded updating relative query"
             (reserved-clear-property sample-iri-ref iri-query update-iri-query
                                      char-set:iri-query))

(test-assert "Encode any other illegal character updating non-relative query "
             (reserved-escaped-property sample-iri iri-query update-iri-query
                                        (char-set-complement char-set:iri-query)))

(test-assert "Encode any other illegal character updating relative query"
             (reserved-escaped-property sample-iri-ref iri-query update-iri-query
                                        (char-set-complement char-set:iri-query)))

(test-end "implicit encoding in IRI query reg-name setters")

;;;

(test-begin "implicit encoding in IRI fragment reg-name setters")

(test-assert "Reserved characters not encoded updating non-relative fragment"
             (reserved-clear-property sample-iri iri-fragment update-iri-fragment
                                      (char-set-difference char-set:iri-fragment char-set:reserved)))

(test-assert "Reserved characters not encoded updating relative fragment"
             (reserved-clear-property sample-iri-ref iri-fragment update-iri-fragment
                                      (char-set-difference char-set:iri-fragment char-set:reserved)))

(test-assert "Other permissible characters not encoded updating non-relative fragment"
             (reserved-clear-property sample-iri iri-fragment update-iri-fragment
                                      char-set:iri-fragment))

(test-assert "Other permissible characters not encoded updating relative fragment"
             (reserved-clear-property sample-iri-ref iri-fragment update-iri-fragment
                                      char-set:iri-fragment))

(test-assert "Encode any other illegal character updating non-relative fragment"
             (reserved-escaped-property sample-iri iri-fragment update-iri-fragment
                                        (char-set-complement char-set:iri-fragment)))

(test-assert "Encode any other illegal character updating relative fragment"
             (reserved-escaped-property sample-iri-ref iri-fragment update-iri-fragment
                                        (char-set-complement char-set:iri-fragment)))

(test-end "implicit encoding in IRI fragment reg-name setters")

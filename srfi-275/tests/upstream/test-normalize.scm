;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(define uri-case-normalization
  `(;; Mixed-case scheme and hostname are rendered lower-case.
    ["All lower-case preserved"        "http://example.org/ex#test" "http://example.org/ex#test" ,normalize-uri-case]
    ["Mixed-case scheme to lower-case" "HttP://example.org/ex#test" "http://example.org/ex#test" ,normalize-uri-case]
    ["Mixed-case user preserved"       "http://MySelf@example.org/Examp#test" "http://MySelf@example.org/Examp#test" ,normalize-uri-case]
    ["Mixed-case host to lower-case"   "http://Example.ORG/ex#test"           "http://example.org/ex#test"           ,normalize-uri-case]
    ["Mixed case path preserved"       "http://example.org/Examp#test"        "http://example.org/Examp#test"        ,normalize-uri-case]
    ["Mixed case query preserved"      "http://example.org/examp?Qua#test"    "http://example.org/examp?Qua#test"    ,normalize-uri-case]
    ["Mixed case fragment preserved"   "http://example.org/examp#TeSt"        "http://example.org/examp#TeSt"        ,normalize-uri-case]
    ["User percent-encodings to upper-case"     "http://%2e@%2E%2E%2E/some/where/place" "http://%2E@%2E%2E%2E/some/where/place" ,normalize-uri-case]
    ["Host percent-encodings to upper-case"     "http://%2e%2E%2e/some/where/place"     "http://%2E%2E%2E/some/where/place"     ,normalize-uri-case]
    ["Path percent-encodings to upper-case"     "http://myname@example.org/%2e/%2E/%2e"       "http://myname@example.org/%2E/%2E/%2E"       ,normalize-uri-case]
    ["Query percent-encodings to upper-case"    "http://myname@example.org/%2E/%2E/%2E?%2e"   "http://myname@example.org/%2E/%2E/%2E?%2E"   ,normalize-uri-case]
    ["Fragment percent-encodings to upper-case" "http://myname@example.org/%2E/%2E/%2E#%2e"   "http://myname@example.org/%2E/%2E/%2E#%2E"   ,normalize-uri-case]))

(define uri-escaped-normalization
  `(;; Reserved character(s) in the clear are not escaped (!)
    ["User reserved character not escaped"     "http://my!name@example.org/ex#test"   "http://my!name@example.org/ex#test"   ,normalize-uri-escape]
    ["Host reserved character not escaped"     "http://myname@!example.org/ex#test"   "http://myname@!example.org/ex#test"   ,normalize-uri-escape]
    ["Path reserved character not escaped"     "http://myname@example.org/ex!#test"   "http://myname@example.org/ex!#test"   ,normalize-uri-escape]
    ["Query reserved character not escaped"    "http://myname@example.org/ex?!a#test" "http://myname@example.org/ex?!a#test" ,normalize-uri-escape]
    ["Fragment reserved character not escaped" "http://myname@example.org/ex?a#!test" "http://myname@example.org/ex?a#!test" ,normalize-uri-escape]
    ;; Reserved character(s) escaped are not decoded (@)
    ["User reserved escape not decoded"     "http://my%40name@example.org/ex#test"   "http://my%40name@example.org/ex#test"   ,normalize-uri-escape]
    ["Host reserved escape not decoded"     "http://myname@ex%40ample.org/ex#test"   "http://myname@ex%40ample.org/ex#test"   ,normalize-uri-escape]
    ["Path reserved escape not decoded"     "http://myname@example.org/e%40x#test"   "http://myname@example.org/e%40x#test"   ,normalize-uri-escape]
    ["Query reserved escape not decoded"    "http://myname@example.org/ex?a%40#test" "http://myname@example.org/ex?a%40#test" ,normalize-uri-escape]
    ["Fragment reserved escape not decoded" "http://myname@example.org/ex?a#t%40est" "http://myname@example.org/ex?a#t%40est" ,normalize-uri-escape]
    ;; Other permissible character(s) escaped are decoded
    ["User permissible escape decoded"     "http://my%2Ename@example.org/ex?a#test"        "http://my.name@example.org/ex?a#test"      ,normalize-uri-escape]
    ["Host permissible escape decoded"     "http://myname@example%2Eorg/ex?a#test"         "http://myname@example.org/ex?a#test"       ,normalize-uri-escape]
    ["Path permissible escape decoded"     "http://myname@example.org/misc%2Etxt#test"     "http://myname@example.org/misc.txt#test"   ,normalize-uri-escape]
    ["Query permissible escape decoded"    "http://myname@example.org/misc.txt?%2E%2E%2E"  "http://myname@example.org/misc.txt?..."    ,normalize-uri-escape]
    ["Fragment permissible escape decoded" "http://myname@example.org/misc.txt#line%31%30" "http://myname@example.org/misc.txt#line10" ,normalize-uri-escape]))
(define uri-escaped-normalization/ge
  `(;; Illegal characters are escaped
    ["User illegal characters are escaped" "http://dosh£@crepes.example.org"      "http://dosh%C2%A3@crepes.example.org"     ,normalize-uri-escape]
    ["Host illegal characters are escaped" "http://crêpes.example.org"            "http://cr%C3%AApes.example.org"           ,normalize-uri-escape]
    ["Path illegal characters are escaped" "http://crepes.example.org/in/Rhône"  "http://crepes.example.org/in/Rh%C3%B4ne" ,normalize-uri-escape]
    ["Query illegal characters are escaped" "http://crepes.example.org/in/Rennes?Dim.‥Sam."  "http://crepes.example.org/in/Rennes?Dim.%E2%80%A5Sam." ,normalize-uri-escape]
    ["Fragment illegal characters are escaped" "http://crepes.example.org/in/Rennes#L'Étage"  "http://crepes.example.org/in/Rennes#L'%C3%89tage" ,normalize-uri-escape]))

(define uri-path-normalization
  `(;; Path without dotted segments is unchanged
    ["Absolute path without dotted segments unchanged"  "http://example.org/some/where/place" "http://example.org/some/where/place" ,normalize-uri-path]
    ["No authority absolute path without dotted segments unchanged"   "urn:/some/where/place" "urn:/some/where/place" ,normalize-uri-path]
    ["Relative path without dotted segments unchanged"                "urn:some/where/place"  "urn:some/where/place"  ,normalize-uri-path]
    ;; Single-dot segments only eliminates the dotted segments
    ["Absolute path eliminates single dotted segments"  "urn:/some/./where/././place/./"  "urn:/some/where/place/" ,normalize-uri-path]
    ["Relative path eliminates single dotted segments"  "urn:some/./where/././place/./"   "urn:some/where/place/"  ,normalize-uri-path]
    ;; Empty segments are treated identically to non-empty segments
    ["Absolute path empty segments treated like non-empty"  "urn:/some//where//place//" "urn:/some//where//place//" ,normalize-uri-path]
    ["Relative path empty segments treated like non-empty"  "urn:some//where//place//"  "urn:some//where//place//"  ,normalize-uri-path]
    ;; Relative references are not path normalized
    ["Single leading slash not normalized"      "/"  "/"   ,normalize-uri-path]
    ["Multiple leading slashes not normalized"  "//" "//"  ,normalize-uri-path]
    ["Absolute path reference not normalized (double-dot)"    "/a/b/../../c"    "/a/b/../../c"    ,normalize-uri-path]
    ["Absolute path reference not normalized (single-dot)"    "/a/b/././c"      "/a/b/././c"      ,normalize-uri-path]
    ["Absolute path reference not normalized (mixed dotted)"  "/a/b/../c/././d" "/a/b/../c/././d" ,normalize-uri-path]
    ["Relative path reference not normalized (double-dot)"    "a/b/../../c"     "a/b/../../c"     ,normalize-uri-path]
    ["Relative path reference not normalized (single-dot)"    "a/b/././c"       "a/b/././c"       ,normalize-uri-path]
    ["Relative path reference not normalized (mixed dotted)"  "a/b/../c/././d"  "a/b/../c/././d"  ,normalize-uri-path]
    ["1-segment path reference with leading dot not normalized"         "./def"     "./def"     ,normalize-uri-path]
    ["1-segment path reference with leading dot not normalized (colon)" "./abc:def" "./abc:def" ,normalize-uri-path]
    ["Additional relative path case not normalized"  "../../abc/./def" "../../abc/./def" ,normalize-uri-path]
    ;; Normalisation may not preserve relative path, in a non-relative URI
    ["Relative path must not be normalized to an absolute path" "foo:a/b/../.././../../e" "foo:e" ,normalize-uri-path]
    ;; Additional test cases from Webkit
    ["Empty segments eliminated like non-empty (1)"  "http://example.com////../.."       "http://example.com//"        ,normalize-uri-path]
    ["Empty segments eliminated like non-empty (2)"  "http://example.com/foo/bar//../.." "http://example.com/foo/"     ,normalize-uri-path]
    ["Empty segments eliminated like non-empty (3)"  "http://example.com/foo/bar//.."    "http://example.com/foo/bar/" ,normalize-uri-path]
    ;; From Haskell network-uri
    ["General test cases (1)"  "http://example/a/b/../../c"  "http://example/c"      ,normalize-uri-path]
    ["General test cases (2)"  "http://example/a/b/c/../../" "http://example/a/"     ,normalize-uri-path]
    ["General test cases (3)"  "http://example/a/b/c/./"     "http://example/a/b/c/" ,normalize-uri-path]
    ["General test cases (4)"  "http://example/a/b/c/.././"  "http://example/a/b/"   ,normalize-uri-path]
    ["General test cases (5)"  "http://example/a/b/c/d/../../../../e"   "http://example/e" ,normalize-uri-path]
    ["General test cases (6)"  "http://example/a/b/c/d/../.././../../e" "http://example/e" ,normalize-uri-path]
    ["General test cases (7)"  "http://example/a/b/../.././../../e"     "http://example/e" ,normalize-uri-path]))

(define (make-uri-case->iri-case norm)
  (lambda (cs)
    (match cs
      [(list desc action result op)
       (let ([desc (string-append desc " (IRI)")])
         `(,desc ,action ,result ,norm))]
      [_
       (error #f "internal error converting from URI case to IRI case" cs)])))

;; Reuse the URI test cases for IRIs:
(define iri-case-normalization
  (map (make-uri-case->iri-case normalize-iri-case) uri-case-normalization))
(define iri-case-normalization/extra
  `(;; Only U.S. ASCII has a canonical lower-case representation
    ["Host non-U.S. ASCII is case-sensitive" "http://CRÊPES.example.org" "http://crÊpes.example.org" ,normalize-iri-case]))

(define iri-escaped-normalization
  (map (make-uri-case->iri-case normalize-iri-escape) uri-escaped-normalization))

(define iri-escaped-normalization/extra
  `(;; Other permissible character(s) escaped are decoded
    ["User permissible escape decoded"   "http://dosh%C2%A3@crepes.example.org" "http://dosh£@crepes.example.org" ,normalize-iri-escape]
    ["Host permissible escape decoded"    "http://cr%C3%AApes.example.org"      "http://crêpes.example.org"       ,normalize-iri-escape]
    ["Path permissible characters are decoded" "http://crepes.example.org/in/Rennes/Cr%C3%AAperie" "http://crepes.example.org/in/Rennes/Crêperie"   ,normalize-uri-escape]
    ["Query permissible characters are decoded" "http://crepes.example.org/in/Rennes?Dim.%E2%80%A5Sam." "http://crepes.example.org/in/Rennes?Dim.‥Sam."   ,normalize-iri-escape]
    ["Fragment permissible characters are decoded" "http://crepes.example.org/in/Rennes#L'%C3%89tage" "http://crepes.example.org/in/Rennes#L'Étage"   ,normalize-iri-escape]
    ["Wholly escaped wholly decoded"        "https://en.wiktionary.org/wiki/%E1%BF%AC%CF%8C%CE%B4%CE%BF%CF%82" "https://en.wiktionary.org/wiki/Ῥόδος" ,normalize-iri-escape]
    ["Partially normalized wholly decoded"  "https://example.org/ceol/%C3%89irigh'sCuirOrtDoChuid%C3%89adaigh" "https://example.org/ceol/Éirigh'sCuirOrtDoChuidÉadaigh" ,normalize-iri-escape]
    ["Wholly normalized not reencoded"       "https://en.wiktionary.org/wiki/Ῥόδος" "https://en.wiktionary.org/wiki/Ῥόδος" ,normalize-iri-escape]
    ["Repeated normalization is indepodent" "https://en.wiktionary.org/wiki/Ῥόδος"  "https://en.wiktionary.org/wiki/Ῥόδος" ,(compose normalize-iri-escape normalize-iri-escape)]))

(define iri-path-normalization
  (map (make-uri-case->iri-case normalize-iri-path) uri-path-normalization))

(define (string->uri/ge st)
  (let ([src (string->iri st)])
    (if (relative-uri? st)
        (make-raw-relative-uri ; no scheme
         (relative-reference-raw-user src)
         (relative-reference-raw-host src)
         (relative-reference-port src)
         (relative-reference-path src)
         (relative-reference-raw-query src)
         (relative-reference-raw-fragment src))
        (make-raw-uri (resource-identifier-raw-scheme src)
         (resource-identifier-raw-user src)
         (resource-identifier-raw-host src)
         (resource-identifier-port src)
         (resource-identifier-path src)
         (resource-identifier-raw-query src)
         (resource-identifier-raw-fragment src)))))

(define (make-test-case-of ->res =?)
  (lambda (cs)
    (match cs
      [(list desc action result norm)
       (test-assert desc
        (let* ([acted-on (norm (->res action))]
               [expecting (->res result)])
          (=? acted-on expecting)))]
      [_
       (error #f "malformed test case" cs)])))

(define uri-test-case-of (make-test-case-of string->uri uri=?))
(define uri-test-case-of/ge (make-test-case-of string->uri/ge uri=?))
(define iri-test-case-of (make-test-case-of string->iri iri=?))

(test-begin "RFC 3986 URI case normalization")
(for-each uri-test-case-of uri-case-normalization)
(test-end "RFC 3986 URI case normalization")

(test-begin "RFC 3987 IRI case normalization")
(for-each iri-test-case-of iri-case-normalization)
(for-each iri-test-case-of iri-case-normalization/extra)
(test-end "RFC 3987 IRI case normalization")

(test-begin "RFC 3986 URI escaped normalization")
(for-each uri-test-case-of uri-escaped-normalization)
(for-each uri-test-case-of/ge uri-escaped-normalization/ge)
(test-end "RFC 3986 URI escaped normalization")

(test-begin "RFC 3987 IRI escaped normalization")
(for-each iri-test-case-of iri-escaped-normalization)
(test-end "RFC 3987 IRI escaped normalization")

(test-begin "RFC 3986 URI path segment normalization")
(for-each uri-test-case-of uri-path-normalization)
(test-end "RFC 3986 URI path segment normalization")
(test-begin "RFC 3987 IRI path segment normalization")
(for-each iri-test-case-of iri-path-normalization)
(test-end "RFC 3987 IRI path segment normalization")

#|
   Normalisation test cases are tested slightly differently
|#
(define uri-to-iri-normalization
  `(["Wholly escaped path normalized" "https://en.wiktionary.org/wiki/%E1%BF%AC%CF%8C%CE%B4%CE%BF%CF%82" "https://en.wiktionary.org/wiki/Ῥόδος"]
    ["Partially escaped path normalized" "https://example.org/ceol/%C3%89irigh'sCuirOrtDoChuid%C3%89adaigh" "https://example.org/ceol/Éirigh'sCuirOrtDoChuidÉadaigh"]))

(define iri-to-uri-normalization
  `(["Wholly escapable path escaped" "https://en.wiktionary.org/wiki/Ῥόδος" "https://en.wiktionary.org/wiki/%E1%BF%AC%CF%8C%CE%B4%CE%BF%CF%82"]
    ["Partialy escapable path escaped" "https://example.org/ceol/Éirigh'sCuirOrtDoChuidÉadaigh" "https://example.org/ceol/%C3%89irigh'sCuirOrtDoChuid%C3%89adaigh"]))

(define (make-conversion-case-of ->action ->result conv =?)
  (lambda (cs)
    (match cs
      [(list desc action result)
       (test-assert desc
         (let* ([acted-on (conv (->action action))]
                [expecting (->result result)])
           (=? acted-on expecting)))]
      [_
       (error #f "malformed test case" cs)])))

(define uri-to-iri-case-of (make-conversion-case-of string->uri string->iri uri->iri iri=?))
(define iri-to-uri-case-of (make-conversion-case-of string->iri string->uri iri->uri uri=?))

(test-begin "URI to IRI")
(for-each uri-to-iri-case-of uri-to-iri-normalization)
(test-end "URI to IRI")

(test-begin "IRI to URI")
(for-each iri-to-uri-case-of iri-to-uri-normalization)
(test-end "IRI to URI")

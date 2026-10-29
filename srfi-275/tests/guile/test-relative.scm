;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(define turtle-common-cases
  '("g:h"
    "g"
    "./g"
    "g/"
    "/g"
    "//g"
    "?y"
    "g?y"
    "#s"
    "g#s"
    "g?y#s"
    ";x"
    "g;x"
    "g;x?y#s"
    ""
    "."
    "./"
    ".."
    "../"
    "../g"
    "../.."
    "../../"
    "../../g"
    "../../../g"
    "../../../../g"
    "/./g"
    "/../g"
    "g."
    ".g"
    "g.."
    "..g"
    "./../g"
    "./g/."
    "g/./h"
    "g/../h"
    "g;x=1/./y"
    "g;x=1/../y"
    "g?y/./x"
    "g?y/../x"
    "g#s/./x"
    "g#s/../x"))

(define turtle-base01 (string->iri"http://a/bb/ccc/d;p?q"))
(define turtle-expected01
  '("g:h"
    "http://a/bb/ccc/g"
    "http://a/bb/ccc/g"
    "http://a/bb/ccc/g/"
    "http://a/g"
    "http://g"
    "http://a/bb/ccc/d;p?y"
    "http://a/bb/ccc/g?y"
    "http://a/bb/ccc/d;p?q#s"
    "http://a/bb/ccc/g#s"
    "http://a/bb/ccc/g?y#s"
    "http://a/bb/ccc/;x"
    "http://a/bb/ccc/g;x"
    "http://a/bb/ccc/g;x?y#s"
    "http://a/bb/ccc/d;p?q"
    "http://a/bb/ccc/"
    "http://a/bb/ccc/"
    "http://a/bb/"
    "http://a/bb/"
    "http://a/bb/g"
    "http://a/"
    "http://a/"
    "http://a/g"
    "http://a/g"
    "http://a/g"
    "http://a/g"
    "http://a/g"
    "http://a/bb/ccc/g."
    "http://a/bb/ccc/.g"
    "http://a/bb/ccc/g.."
    "http://a/bb/ccc/..g"
    "http://a/bb/g"
    "http://a/bb/ccc/g/"
    "http://a/bb/ccc/g/h"
    "http://a/bb/ccc/h"
    "http://a/bb/ccc/g;x=1/y"
    "http://a/bb/ccc/y"
    "http://a/bb/ccc/g?y/./x"
    "http://a/bb/ccc/g?y/../x"
    "http://a/bb/ccc/g#s/./x"
    "http://a/bb/ccc/g#s/../x"))

(define turtle-base02 (string->iri"http://a/bb/ccc/d/"))
(define turtle-expected02
  '("g:h"
    "http://a/bb/ccc/d/g"
    "http://a/bb/ccc/d/g"
    "http://a/bb/ccc/d/g/"
    "http://a/g"
    "http://g"
    "http://a/bb/ccc/d/?y"
    "http://a/bb/ccc/d/g?y"
    "http://a/bb/ccc/d/#s"
    "http://a/bb/ccc/d/g#s"
    "http://a/bb/ccc/d/g?y#s"
    "http://a/bb/ccc/d/;x"
    "http://a/bb/ccc/d/g;x"
    "http://a/bb/ccc/d/g;x?y#s"
    "http://a/bb/ccc/d/"
    "http://a/bb/ccc/d/"
    "http://a/bb/ccc/d/"
    "http://a/bb/ccc/"
    "http://a/bb/ccc/"
    "http://a/bb/ccc/g"
    "http://a/bb/"
    "http://a/bb/"
    "http://a/bb/g"
    "http://a/g"
    "http://a/g"
    "http://a/g"
    "http://a/g"
    "http://a/bb/ccc/d/g."
    "http://a/bb/ccc/d/.g"
    "http://a/bb/ccc/d/g.."
    "http://a/bb/ccc/d/..g"
    "http://a/bb/ccc/g"
    "http://a/bb/ccc/d/g/"
    "http://a/bb/ccc/d/g/h"
    "http://a/bb/ccc/d/h"
    "http://a/bb/ccc/d/g;x=1/y"
    "http://a/bb/ccc/d/y"
    "http://a/bb/ccc/d/g?y/./x"
    "http://a/bb/ccc/d/g?y/../x"
    "http://a/bb/ccc/d/g#s/./x"
    "http://a/bb/ccc/d/g#s/../x"))

(define turtle-cases07 (append turtle-common-cases '("http:g")))
(define turtle-base07 (string->iri"file:///a/bb/ccc/d;p?q"))
(define turtle-expected07
  '("g:h"
    "file:///a/bb/ccc/g"
    "file:///a/bb/ccc/g"
    "file:///a/bb/ccc/g/"
    "file:///g"
    "file://g"
    "file:///a/bb/ccc/d;p?y"
    "file:///a/bb/ccc/g?y"
    "file:///a/bb/ccc/d;p?q#s"
    "file:///a/bb/ccc/g#s"
    "file:///a/bb/ccc/g?y#s"
    "file:///a/bb/ccc/;x"
    "file:///a/bb/ccc/g;x"
    "file:///a/bb/ccc/g;x?y#s"
    "file:///a/bb/ccc/d;p?q"
    "file:///a/bb/ccc/"
    "file:///a/bb/ccc/"
    "file:///a/bb/"
    "file:///a/bb/"
    "file:///a/bb/g"
    "file:///a/"
    "file:///a/"
    "file:///a/g"
    "file:///g"
    "file:///g"
    "file:///g"
    "file:///g"
    "file:///a/bb/ccc/g."
    "file:///a/bb/ccc/.g"
    "file:///a/bb/ccc/g.."
    "file:///a/bb/ccc/..g"
    "file:///a/bb/g"
    "file:///a/bb/ccc/g/"
    "file:///a/bb/ccc/g/h"
    "file:///a/bb/ccc/h"
    "file:///a/bb/ccc/g;x=1/y"
    "file:///a/bb/ccc/y"
    "file:///a/bb/ccc/g?y/./x"
    "file:///a/bb/ccc/g?y/../x"
    "file:///a/bb/ccc/g#s/./x"
    "file:///a/bb/ccc/g#s/../x"
    "http:g"))

;;;;

(define turtle-base08a (string->iri"http://abc/def/ghi"))
(define turtle-cases08a '("."
                   ".?a=b"
                   ".#a=b"
                   ".."
                   "..?a=b"
                   "..#a=b"))
(define turtle-expected08a '("http://abc/def/"
                      "http://abc/def/?a=b"
                      "http://abc/def/#a=b"
                      "http://abc/"
                      "http://abc/?a=b"
                      "http://abc/#a=b"))

(define turtle-base08b (string->iri"http://ab//de//ghi"))
(define turtle-cases08b '("xyz" "./xyz" "../xyz"))
(define turtle-expected08b '("http://ab//de//xyz"
                      "http://ab//de//xyz"
                      "http://ab//de/xyz"))

(define turtle-base08c (string->iri"http://abc/d:f/ghi"))
(define turtle-cases08c '("xyz"
                   "./xyz"
                   "../xyz"))
(define turtle-expected08c '("http://abc/d:f/xyz"
                      "http://abc/d:f/xyz"
                      "http://abc/xyz"))

(define (make-run-manual-case base-iri)
  (lambda (in out)
    (let ([expanded-input
           (resolve-iri base-iri (string->iri in))]
          [out-as-is (string->iri out)])
      (test-assert (format "~s <> ~s -> ~s" base-iri in out)
                   (iri=? expanded-input out-as-is)))))

(define (run-cases base-iri test-cases expected)
  (unless (null? test-cases)
    (for-each (make-run-manual-case base-iri)
              test-cases
              expected)))

(test-begin "RFC 3987 relative reference resolution (RDF and Turtle test cases)")

(run-cases turtle-base01 turtle-common-cases turtle-expected01)
(run-cases turtle-base02 turtle-common-cases turtle-expected02)
(run-cases turtle-base07 turtle-cases07 turtle-expected07)
(run-cases turtle-base08a turtle-cases08a turtle-expected08a)
(run-cases turtle-base08b turtle-cases08b turtle-expected08b)
(run-cases turtle-base08c turtle-cases08c turtle-expected08c)

(test-end "RFC 3987 relative reference resolution (RDF and Turtle test cases)")

(define swap-cases
  '(;; test cases from Python URI implementation
    ["foo:xyz" "bar:abc" "bar:abc"]
    ["http://example/x/y/z" "../abc" "http://example/x/abc"]
    ["http://example2/x/y/z" "//example/x/abc" "http://example/x/abc"]
    ["http://ex/x/y/z" "../r" "http://ex/x/r"]

    ["http://ex/x/y" "q/r" "http://ex/x/q/r"]
    ["http://ex/x/y" "q/r#s" "http://ex/x/q/r#s"]
    ["http://ex/x/y" "q/r#s/t" "http://ex/x/q/r#s/t"]
    ["http://ex/x/y" "ftp://ex/x/q/r" "ftp://ex/x/q/r"]
    ["http://ex/x/y" "y" "http://ex/x/y"]
    ["http://ex/x/y/" "." "http://ex/x/y/"]
    ["http://ex/x/y/pdq" "pdq" "http://ex/x/y/pdq"]
    ["http://ex/x/y/" "z/" "http://ex/x/y/z/"]
    ["file:/swap/test/animal.rdf" "animal.rdf#Animal" "file:/swap/test/animal.rdf#Animal"]
    ["file:/e/x/y/z" "../abc" "file:/e/x/abc"]
    ["file:/example2/x/y/z" "../../../example/x/abc" "file:/example/x/abc"]
    ["file:/ex/x/y/z" "../r" "file:/ex/x/r"]
    ["file:/ex/x/y/z" "../../../r" "file:/r"]
    ["file:/ex/x/y" "q/r" "file:/ex/x/q/r"]
    ["file:/ex/x/y" "q/r#s" "file:/ex/x/q/r#s"]
    ["file:/ex/x/y" "q/r#" "file:/ex/x/q/r#"]
    ["file:/ex/x/y" "q/r#s/t" "file:/ex/x/q/r#s/t"]
    ["file:/ex/x/y" "ftp://ex/x/q/r" "ftp://ex/x/q/r"]
    ["file:/ex/x/y" "y" "file:/ex/x/y"]
    ["file:/ex/x/y/" "." "file:/ex/x/y/"]
    ["file:/ex/x/y/pdq" "pdq" "file:/ex/x/y/pdq"]
    ["file:/ex/x/y/" "z/" "file:/ex/x/y/z/"]
    ["file:/devel/WWW/2000/10/swap/test/reluri-1.n3" "//meetings.example.com/cal#m1" "file://meetings.example.com/cal#m1"]
    ["file:/home/connolly/w3ccvs/WWW/2000/10/swap/test/reluri-1.n3" "//meetings.example.com/cal#m1" "file://meetings.example.com/cal#m1"]
    ["file:/some/dir/foo" ".#blort" "file:/some/dir/#blort"]
    ["file:/some/dir/foo" ".#" "file:/some/dir/#"]
    ;; Graham Klyne https://lists.w3.org/Archives/Public/uri/2003Feb/0016.html
    ["http://example/x/y%2Fz" "abc" "http://example/x/abc"]
    ["http://example/x/y/z" "../../x%2Fabc" "http://example/x%2Fabc"]
    ["http://example/x/y%2Fz" "../x%2Fabc" "http://example/x%2Fabc"]
    ["http://example/x%2Fy/z" "abc" "http://example/x%2Fy/abc"]
    #| Ryan Lee |#
    ["http://example/x/abc.efg" "." "http://example/x/"]))

#|
   The following extra cases were lifted directly form Haskell's network-uri.
   I identified these when I was looking for comprehensive test cases, and as far as
   I can tell, these cases are largely accounted for by the previous tests. The main
   point of difference with network-uri is in the final two test cases, which are
   explicitly part of the SRFI document's test suite.
|#
(define hs-network-uri-nonrel-cases
  `(;; Check handling of queries and fragments with non-relative paths
    ["mailto:local1@domain1?query1" "local2@domain2" "mailto:local2@domain2"]
    ["mailto:local1@domain1" "local2@domain2?query2" "mailto:local2@domain2?query2"]
    ["mailto:local1@domain1?query1" "local2@domain2?query2" "mailto:local2@domain2?query2"]
    ["mailto:local@domain?query1" "?query2" "mailto:local@domain?query2"]
    ["mailto:?query1" "local@domain?query2" "mailto:local@domain?query2"]
    ["mailto:local@domain?query1" "?query2" "mailto:local@domain?query2"]
    ["foo:bar" "http://example/a/b?c/../d" "http://example/a/b?c/../d"]
    ["foo:bar" "http://example/a/b#c/../d" "http://example/a/b#c/../d"]))

(define hs-network-uri-invertible-cases
  '(#| I'm quoting a comment in the Haskell network-uri test file verbatim:

       "These (78-81) are some awkward test cases thrown up by a question on the URI list:
        http://lists.w3.org/Archives/Public/uri/2005Jul/0013
        Note that RFC 3986 discards path segments after the final '/' only when merging two
        paths - otherwise the final segment in the base URI is maintained.  This leads to
        difficulty in constructing a reversible relativeTo/relativeFrom pair of functions."
    |#
    ["http://www.example.com/data/limit/.." "test.xml" "http://www.example.com/data/limit/test.xml"]
    ["file:/some/dir/foo" "./#blort" "file:/some/dir/#blort"]
    ["file:/some/dir/foo" "./#" "file:/some/dir/#"]
    ["file:/some/dir/.." "./#blort" "file:/some/dir/#blort"]
    ;; testRelative91 = testRelSplit "testRelative91" "http://example.org/base/uri" "http:this" "this"
    ["http://example.org/base/uri" "http:this" "http:this"]
    ["http:base" "http:this" "http:this"]
    ["f://example.org/base/a" "b/c//d/e" "f://example.org/base/b/c//d/e"]
    ["mid:m@example.ord/c@example.org" "m2@example.ord/c2@example.org" "mid:m@example.ord/m2@example.ord/c2@example.org"]
    ["file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/" "mini1.xml" "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/mini1.xml"]
    ["foo:a/y/z" "../b/c" "foo:a/b/c"]))

(define all-hs-network-uri-cases
  (append hs-network-uri-nonrel-cases hs-network-uri-invertible-cases))

(define run-misc
  (match-lambda
    [((base ref expect))
      (let ([base/iri (string->iri base)]
            [ref/iri (string->iri ref)]
            [expect/iri (string->iri expect)]
            [label (format "<~a> ++ <~a> => <~a>"
                           base
                           ref
                           expect)])
        (test-assert label
          (iri=? expect/iri
                 (resolve-iri base/iri ref/iri))))]))

(test-begin "RFC 3987 relative reference resolution (miscellaneous test cases)")
(for-each run-misc swap-cases)
(for-each run-misc all-hs-network-uri-cases)
(test-end "RFC 3987 relative reference resolution (miscellaneous test cases)")

(define abnormal-cases
  '(;; Over-deep `..' traversal is clamped at root, not an error:
    ["http://a/b/c/d;p?q" "../../../g"    "http://a/g"]
    ["http://a/b/c/d;p?q" "../../../../g" "http://a/g"]
    ["http://a/b/c/d;p?q" "../../../.."   "http://a/"]
    ["http://a/b/c/d;p?q" "../../../../"  "http://a/"]
    ;; Dotted segments in absolute paths are removed:
    ["http://a/b/c/d;p?q" "/./g"  "http://a/g"]
    ["http://a/b/c/d;p?q" "/../g" "http://a/g"]
    ;; These are NOT dot-segments; literal names, not traversal:
    ["http://a/b/c/d;p?q" "g.." "http://a/b/c/g.."]
    ["http://a/b/c/d;p?q" "..g" "http://a/b/c/..g"]
    ;; Dots do not affect query or fragment:
    ["http://a/b/c/d;p?q" "g?y/./x"  "http://a/b/c/g?y/./x"]
    ["http://a/b/c/d;p?q" "g?y/../x" "http://a/b/c/g?y/../x"]
    ["http://a/b/c/d;p?q" "g#s/./x"  "http://a/b/c/g#s/./x"]
    ["http://a/b/c/d;p?q" "g#s/../x" "http://a/b/c/g#s/../x"]
    #| The following two test cases test what happens if we normalize
       a path to be empty. Specifically an empty path can never be parsed
       as an empty internal representation (zero-length u16vector), so this
       must be normalized to #f in order to parse the equivalent <foo:>
       string representation to the same IRI record. |#
    ["foo:a" "." "foo:"]
    ["foo:a" ".." "foo:"]
    #|
       The following two normalization cases are numbered testRelative94 and testRelative99
       in Haskell network-uri test file. The string representation <f://g> is given as result
       of both.

       I don't think this is tenable, because this is parsed with `f' as scheme component and
       `g' as host, because `://' always separates a scheme and authority component. I believe
       this is a limitation of a string representation of URIs: there's no way to preserve
       the separation after normalization.

       RFC 3986 section 4.2 explicitly emphasises that `./some/path' is semantically equivalent
       to the bare, undotted path, and to use that for disambiguation of relative references
       with colons. Sufficiently robust normalization should thus make sure not to normalize a
       path which would have a different meaning when represented as a string and then re-parsed.
       That is, prefix such a path with `./'!

       In these two examples, relative reference resolution would elicit the path `//g'. There
       is no authority component defined, hence that path is invalid for the parser (since it
       has an empty segment with a leading slash, only admitted by the `path-abempty' production)
       Although this path /would/ be valid if we wrote e.g. <f:////g> (empty registered name
       component), the empty registered name portion would not be produced by the relative
       reference resolution or normalization procedure as described in RFC 3986.
    |#
    ["f:/a" ".//g" "f:.///g"]
    ["f:/a/" "..//g" "f:.///g"]))

(test-begin "RFC 3987 relative reference resolution (abnormal test cases)")
(for-each run-misc abnormal-cases)
(test-end "RFC 3987 relative reference resolution (abnormal test cases)")

(define run-relativize
  (match-lambda
    [((base reference resolved))
     (let* ([base-iri (string->iri base)]
            [resolved-iri (string->iri resolved)]
            [expected-ref (string->iri reference)]
            [elicited-ref (relativize-iri base-iri resolved-iri)])
       (test-assert
        (format "~s > ~s    => ~s\n" base-iri resolved-iri expected-ref)
        (iri=? elicited-ref expected-ref)))]))

(define run-relativize-inversion
  (match-lambda
    [((base reference resolved))
     (let* ([base-iri (string->iri base)]
            [resolved-iri (string->iri resolved)]
            [expected-ref (string->iri reference)])
       (test-assert (format "relativize . resolve base(~s), resolved(~s), expected(~s)"
                            base resolved-iri expected-ref)
         (iri=? (relativize-iri base-iri (resolve-iri base-iri expected-ref))
                expected-ref))
       (test-assert (format "resolve . relativize base(~s), resolved(~s), expected(~s)"
                            base resolved-iri expected-ref)
         (iri=? (resolve-iri base-iri (relativize-iri base-iri resolved-iri))
                resolved-iri)))]))

(define chicken-uri-generic-cases
  `(;; Chicken uri-generic `reverse-extra-cases'
    ["http://a/b/c/d;p?q" "d;p" "http://a/b/c/d;p?q"]
    ["http://a/b/c/d;p?q" "e" "http://a/b/c/e"]
    ["http://a/b/c/d;p?q" "." "http://a/b/c/"]
    ["http://a/b/c/d;p?q" "../e" "http://a/b/e"]
    ["http://a/b/c/d;p?q" "../c" "http://a/b/c"] ;; Slightly weird: dir in base, file in target
    ["http://a/b/c/d;p?q" ".." "http://a/b/"]
    ["http://a/b/c/d;p?q" "../.." "http://a/"] ;; or "/", but that's not convenient
    ["http://a/b/c/d;p?q" "//a" "http://a"] ; No relative representation possible
    ["http://a/b/c/d;p?q" "//b" "http://b"]
    ["http://a/b/c/d;p?q" "//b/" "http://b/"]
    ["http://a/b/c/d;p?q" "//b/c" "http://b/c"]
    ["http://a/b/c/d;p?q" "ftp://a/b/c/d;p?q" "ftp://a/b/c/d;p?q"]
    ["http://a/b/c/d;p?q" "ftp://x/y/z;a?b" "ftp://x/y/z;a?b"]))

(test-begin "non-RFC relativize-iri")

#|
   Need to test these two tricky cases explicitly:
|#
(test-assert "extract-iri-reference <f:/a> ++ <f:.///g> => </g> (equivalent to <.//g>)"
  (iri=? (relativize-iri (string->iri "f:/a")
                         (string->iri "f:.///g"))
         (string->iri "/g")))

(test-assert "extract-iri-reference <f:/a/> ++ <f:.///g> => <..//g>)"
  (iri=? (relativize-iri (string->iri "f:/a/")
                         (string->iri "f:.///g"))
         (string->iri "..//g")))

(for-each run-relativize swap-cases)
(for-each run-relativize chicken-uri-generic-cases)

(test-end)

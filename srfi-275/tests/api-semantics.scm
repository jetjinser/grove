;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;; SPDX-License-Identifier: MIT

(use-modules (srfi :275 iri)
             (srfi :275 normalize)
             (srfi :275 path)
             (srfi :275 uri))

(define (check name condition)
  (unless condition
    (error "SRFI 275 API contract failed" name)))

(define (raises? thunk)
  (catch #t
    (lambda () (thunk) #f)
    (lambda _ #t)))

(check "path equality preserves absolute versus relative"
       (not (path=? (string->path "a/b")
                    (string->path "/a/b"))))

(check "relative path constructor does not duplicate its first segment"
       (string=? "./scheme:part/next"
                 (path->string
                  (list->relative-path '("scheme:part" "next") #t))))

(check "URI serialization preserves an absent authority"
       (string=? "scheme:path"
                 (uri->string (string->uri "scheme:path"))))
(check "URI serialization preserves an explicitly empty authority"
       (string=? "file:///path"
                 (uri->string (string->uri "file:///path"))))

(check "URI resolution returns a URI"
       (uri? (resolve-uri (string->uri "https://example.org/a/b")
                          (string->uri "../c"))))

(check "IRI round trip preserves Unicode text"
       (string=? "https://例え.テスト/资料?q=é#章"
                 (iri->string
                  (string->iri "https://例え.テスト/资料?q=é#章"))))

(check "URI parser requires percent-encoded non-ASCII data"
       (raises? (lambda () (string->uri "https://example.org/资料"))))
(check "IRI-to-URI percent-encodes a Unicode path as UTF-8"
       (string=? "https://example.org/%E8%B5%84%E6%96%99"
                 (uri->string
                  (iri->uri (string->iri "https://example.org/资料")))))
(check "generic IRI-to-URI mapping does not perform IDNA"
       (string=? "https://%E4%BE%8B%E3%81%88.%E3%83%86%E3%82%B9%E3%83%88/"
                 (uri->string
                  (iri->uri (string->iri "https://例え.テスト/")))))
(check "IRI-to-URI encodes supplementary-plane characters as UTF-8"
       (string=? "https://example.org/%F0%9F%98%80"
                 (uri->string
                  (iri->uri (string->iri "https://example.org/😀")))))

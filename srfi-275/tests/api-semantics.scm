;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;; SPDX-License-Identifier: MIT

(use-modules (srfi :275 iri)
             (srfi :275 normalize)
             (srfi :275 path)
             (srfi :275 uri))

(define (check name condition)
  (unless condition
    (error "SRFI 275 API contract failed" name)))

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

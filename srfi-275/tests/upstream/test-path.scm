; SPDX-FileCopyrightText: 2026 Duncan Guthrie
;
; SPDX-License-Identifier: MIT

#!r6rs

(test-begin "path definitions")

(define test-path0 (empty-path))
(define test-path1 (empty-absolute-path))
(define test-path2 (string->path "some/where/place"))
(define test-path3 (string->path "some:thing/where/place"))
(define test-path4 (string->path "some/where:there/place"))
(define test-path5 (string->path "/some/where/place"))
(define test-path6 (string->path "//some/where/place"))

(test-assert "empty path"
             (empty-path? test-path0))
(test-assert "absolute path with no segments not considered empty"
             (not (empty-path? test-path1)))

(test-assert "all absolute paths considered absolute"
             (for-all absolute-path? `(,test-path1 ,test-path5 ,test-path6)))
(test-assert "no relative path considered absolute"
             (not (exists absolute-path? `(,test-path0 ,test-path2 ,test-path3 ,test-path4))))

(test-assert "all relative paths considered relative"
             (for-all relative-path? `(,test-path0 ,test-path2 ,test-path3)))
(test-assert "no absolute paths considered relative"
             (not (exists relative-path? `(,test-path1 ,test-path5 ,test-path6))))

(test-assert "all relative paths without colons in initial segment are RFC 3986 path-noscheme"
             (for-all rfc3987-path-noscheme? `(,test-path2 ,test-path4)))
(test-assert "no absolute paths or paths with colons in initial segment are RFC 3986 path-noscheme"
             (not (exists rfc3987-path-noscheme? `(,test-path0 ,test-path3 ,test-path5 ,test-path6))))

(test-assert "all absolute paths without empty initial segment are RFC 3986 path-absolute"
             (rfc3987-path-absolute? test-path5))
(test-assert "no relative path or absolute path with empty initial segment are RFC 3986 path-absolute"
             (not (exists rfc3987-path-absolute? `(,test-path0 ,test-path1 ,test-path2 ,test-path3 ,test-path4 ,test-path6))))

(test-end)



(test-begin "path construction")

(test-assert "build a relative path from segments"
 (path=? (build-path "some" "where" "place")
         (string->path "some/where/place")))

(test-expect-fail 2)
(test-assert "reject path with empty initial segment"
             (build-path "" "some" "where" "place"))
(test-assert "reject path with empty initial segment, empty base"
             (build-path (empty-path) "" "some" "where" "place"))

(test-assert "build an absolute path from segments"
 (path=? (build-path (empty-absolute-path) "some" "where" "place")
         (string->path "/some/where/place")))

(test-assert "build an absolute path with leading double slash"
 (path=? (build-path (empty-absolute-path) "" "some" "where" "place")
         (string->path "//some/where/place")))

(test-equal "escape slash in relative path segment string"
            '("a%2Fb" "c%2Fd" "e%2Ff")
            (path-segments (build-path "a/b" "c/d" "e/f")))

(test-equal "escape slash in absolute path segment string"
            '("a%2Fb" "c%2Fd" "e%2Ff")
            (path-segments (build-path "a/b" "c/d" "e/f")))

(test-end)



(test-begin "path serialisation")

(test-equal "serialise empty path"
            "" (path->string test-path0))
(test-equal "serialise absolute path with no segments"
            "/" (path->string test-path1))
(test-equal "serialise relative path"
            "some/where/place" (path->string test-path2))
(test-equal "serialise relative path with colon in initial segment"
            "some:thing/where/place" (path->string test-path3))
(test-equal "serialise relative path with colon in non-initial segment"
            "some/where:there/place" (path->string test-path4))
(test-equal "serialise absolute path"
            "/some/where/place" (path->string test-path5))
(test-equal "serialise absolute path with empty initial segment"
            "//some/where/place" (path->string test-path6))

(test-end)



(test-begin "path segments")

(test-equal '("" "bar") (path-segments (uri-path (string->uri "//foo//bar"))))
(test-equal '("" "" "bar") (path-segments (uri-path (string->uri "//foo///bar"))))
(test-equal '("foo" "" "bar") (path-segments (uri-path (string->uri "/foo//bar"))))
(test-equal '("foo" "" "bar") (path-segments (uri-path (string->uri "foo//bar"))))

(test-end)

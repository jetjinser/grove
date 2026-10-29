;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs
#|
   Tests for in-place setters, namely with respect to preserving internal consistency
   with respect to setting authority components and path.
|#

(test-begin "set URI user")
#|
   User cases:
   a) Nil authority (all #f). Update to non-nil.
   b) Nil authority. Update user to nil. No effect.
   c) Non-nil user, nil host/port. Target user is nil. Expect user to be nil; hostname "".
   d) Non-nil user, non-nil host. Target user is nil. Expect user to be nil; hostname same
   e) Non-nil user, non-nil port. Target user is nil. Expect user to be nil; port same
   f) path-abempty, set authority to nil. Expect error.
   g) path-rootless, set authority. Expect error.

   With respect to (f) and (g), path-abempty generalises path-absolute, so setting the
   authority component for a path-absolute path (like in (a)) would succeed.
|#
(define sample-user-uri0 (string->uri "urn:/some/where/place"))
(define sample-user-uri1 (string->uri "urn:/some/where/place"))
(define sample-user-uri2 (string->uri "urn://user@"))
(define sample-user-uri3 (string->uri "urn://user@example.org"))
(define sample-user-uri4 (string->uri "urn://user@:80"))
(define sample-user-uri5 (string->uri "urn://user@//some/where/place"))
(define sample-user-uri6 (string->uri "urn:some/where/place"))

(test-assert "Persistence of update URI user"
 (and (equal? "persistent"
              (uri-user (update-uri-user sample-user-uri0 "persistent")))
      (not    (uri-user sample-user-uri0))))

(test-assert "URI nil authority, update user to non-nil"
  (match-values (uri-authority (update-uri-user sample-user-uri0 "user"))
    [("user" #f #f) #t]
    [(_ _ _)        #f]))

(test-assert "URI nil authority, update user to nil"
  (match-values (uri-authority (update-uri-user sample-user-uri1 #f))
    [(#f #f #f) #t]
    [(_ _ _)    #f]))

(test-assert "URI non-nil user, nil host/port, update user to nil"
  (match-values (uri-authority (update-uri-user sample-user-uri2 #f))
    [(#f #f #f) #t]
    [(_ _ _)    #f]))

(test-assert "URI non-nil user, non-nil host, nil port, update user to nil"
  (match-values (uri-authority (update-uri-user sample-user-uri3 #f))
    [(#f "example.org" #f) #t]
    [(_ _ _)               #f]))


(test-assert "URI non-nil user, non-nil port, nil host, update user to nil"
  (match-values (uri-authority (update-uri-user sample-user-uri4 #f))
    [(#f #f 80) #t]
    [(_ _ _)    #f]))

(test-expect-fail 2)
(test-assert (update-uri-user sample-user-uri5 #f))
(test-assert (update-uri-user sample-user-uri6 "user"))

(test-end "set URI user")

(test-begin "set IRI user")

(define sample-user-iri0 (string->iri "urn:/some/where/place"))
(define sample-user-iri1 (string->iri "urn:/some/where/place"))
(define sample-user-iri2 (string->iri "urn://user@"))
(define sample-user-iri3 (string->iri "urn://user@example.org"))
(define sample-user-iri4 (string->iri "urn://user@:80"))
(define sample-user-iri5 (string->iri "urn://user@//some/where/place"))
(define sample-user-iri6 (string->iri "urn:some/where/place"))

(test-assert "Persistence of update IRI user"
 (and (equal? "persistent"
              (iri-user (update-iri-user sample-user-iri0 "persistent")))
      (not    (iri-user sample-user-iri0))))

(test-assert "IRI nil authority, update user to non-nil"
  (match-values (iri-authority (update-iri-user sample-user-iri0 "user"))
    [("user" #f #f) #t]
    [(_ _ _)        #f]))

(test-assert "IRI nil authority, update user to nil"
  (match-values (iri-authority (update-iri-user sample-user-iri1 #f))
    [(#f #f #f) #t]
    [(_ _ _)    #f]))

(test-assert "IRI non-nil user, nil host/port, update user to nil"
  (match-values (iri-authority (update-iri-user sample-user-iri2 #f))
    [(#f #f #f) #t]
    [(_ _ _)    #f]))

(test-assert "IRI non-nil user, non-nil host, nil port, update user to nil"
  (match-values (iri-authority (update-iri-user sample-user-iri3 #f))
    [(#f "example.org" #f) #t]
    [(_ _ _)               #f]))


(test-assert "IRI non-nil user, non-nil port, nil host, update user to nil"
  (match-values (iri-authority (update-iri-user sample-user-iri4 #f))
    [(#f #f 80) #t]
    [(_ _ _)    #f]))

(test-expect-fail 2)
(test-assert (update-iri-user sample-user-iri5 #f))
(test-assert (update-iri-user sample-user-iri6 "user"))

(test-end "set IRI user")

;;;

(test-begin "set URI host")
#|
   Host cases:
   a) Nil authority (all #f). Update host to non-nil
   b) Nil authority. Update host to nil. No effect.
   c) Non-nil host, nil user/port. Target host is nil. Expect hostname to be "".
   d) Non-nil host, non-nil user, nil port. Target host is nil. Expect hostname to be nil.
   e) Non-nil host, non-nil port, nil user. Target host is nil. Expect hostname to be nil.
   f) path-abempty, set authority to nil. Expect error.
   g) path-rootless, set authority. Expect error.
|#
(define sample-host-uri0 (string->uri "urn:/some/where/place"))
(define sample-host-uri1 (string->uri "urn:/some/where/place"))
(define sample-host-uri2 (string->uri "urn://"))
(define sample-host-uri3 (string->uri "urn://user@example.org"))
(define sample-host-uri4 (string->uri "urn://example.org:80"))
(define sample-host-uri5 (string->uri "urn://example.org//some/where/place"))
(define sample-host-uri6 (string->uri "urn:some/where/place"))

(test-assert "Persistence of update URI host"
 (and (equal? "persistent"
              (uri-host (update-uri-host sample-user-uri0 "persistent")))
      (not    (uri-host sample-user-uri0))))

(test-assert "URI nil authority, update host to non-nil"
  (match-values (uri-authority (update-uri-host sample-host-uri0 "example.org"))
    [(#f "example.org" #f) #t]
    [(_ _ _)               #f]))

(test-assert "URI nil authority, update host to nil"
  (match-values (uri-authority (update-uri-host sample-host-uri1 #f))
    [(#f #f #f) #t]
    [(_ _ _)    #f]))


(test-assert "URI non-nil host, nil user/port, update host to nil"
  (match-values (uri-authority (update-uri-host sample-host-uri2 #f))
    [(#f #f #f) #t]
    [(_ _ _)    #f]))


(test-assert "URI non-nil host, non-nil user, nil port, update host to nil"
  (match-values (uri-authority (update-uri-host sample-host-uri3 #f))
    [("user" #f #f) #t]
    [(_ _ _)        #f]))


(test-assert "URI non-nil host, non-nil port, nil user, upadte host to nil"
  (match-values (uri-authority (update-uri-host sample-host-uri4 #f))
    [(#f #f 80) #t]
    [(_ _ _)    #f]))

(test-expect-fail 2)
(test-assert (update-uri-host sample-host-uri5 #f))
(test-assert (update-uri-host sample-host-uri6 "example.org"))

(test-end "set URI host")

(test-begin "set IRI host")

(define sample-host-iri0 (string->iri "urn:/some/where/place"))
(define sample-host-iri1 (string->iri "urn:/some/where/place"))
(define sample-host-iri2 (string->iri "urn://"))
(define sample-host-iri3 (string->iri "urn://user@example.org"))
(define sample-host-iri4 (string->iri "urn://example.org:80"))
(define sample-host-iri5 (string->iri "urn://example.org//some/where/place"))
(define sample-host-iri6 (string->iri "urn:some/where/place"))

(test-assert "Persistence of update IRI host"
 (and (equal? "persistent"
              (iri-host (update-iri-host sample-user-iri0 "persistent")))
      (not    (iri-host sample-user-iri0))))

(test-assert "IRI nil authority, update host to non-nil"
  (match-values (iri-authority (update-iri-host sample-host-iri0 "example.org"))
    [(#f "example.org" #f) #t]
    [(_ _ _)               #f]))

(test-assert "IRI nil authority, update host to nil"
  (match-values (iri-authority (update-iri-host sample-host-iri1 #f))
    [(#f #f #f) #t]
    [(_ _ _)    #f]))


(test-assert "IRI non-nil host, nil user/port, update host to nil"
  (match-values (iri-authority (update-iri-host sample-host-iri2 #f))
    [(#f #f #f) #t]
    [(_ _ _)    #f]))


(test-assert "IRI non-nil host, non-nil user, nil port, update host to nil"
  (match-values (iri-authority (update-iri-host sample-host-iri3 #f))
    [("user" #f #f) #t]
    [(_ _ _)        #f]))


(test-assert "IRI non-nil host, non-nil port, nil user, upadte host to nil"
  (match-values (iri-authority (update-iri-host sample-host-iri4 #f))
    [(#f #f 80) #t]
    [(_ _ _)    #f]))

(test-expect-fail 2)
(test-assert (update-iri-host sample-host-iri5 #f))
(test-assert (update-iri-host sample-host-iri6 "example.org"))

(test-end "set IRI host")

;;;

(test-begin "set URI port")
#|
   Port cases:
   a) Nil authority (all #f). Update port to non-nil.
   b) Nil authority. Update port to nil. No effect.
   c) Non-nil port, nil host/user. Target port is nil. Expect hostname to be "".
   d) Non-nil port, non-nil user, nil host. Target port is nil. Expect hostname to be nil.
   e) Non-nil port, non-nil host, nil user. Target port is nil. Expect hostname to be non-nil.
|#
(define sample-port-uri0 (string->uri "urn:/some/where/place"))
(define sample-port-uri1 (string->uri "urn:/some/where/place"))
(define sample-port-uri2 (string->uri "urn://:80"))
(define sample-port-uri3 (string->uri "urn://user@:80"))
(define sample-port-uri4 (string->uri "urn://example.org:80"))
(define sample-port-uri5 (string->uri "urn://:80//some/where/place"))
(define sample-port-uri6 (string->uri "urn:some/where/place"))

(test-assert "Persistence of update URI port"
 (and (equal? 8080
              (uri-port (update-uri-port sample-port-uri0 8080)))
      (not    (uri-port sample-port-uri0))))


(test-assert "URI nil authority, update port to non-nil"
  (match-values (uri-authority (update-uri-port sample-port-uri0 80))
    [(#f #f 80) #t]
    [(_ _ _)    #f]))


(test-assert "URI nil authority, update port to nil. No effect."
  (match-values (uri-authority (update-uri-port sample-port-uri1 #f))
    [(#f #f #f) #t]
    [(_ _ _)    #f]))


(test-assert "URI non-nil port, nil host/user, update port to nil"
  (match-values (uri-authority (update-uri-port sample-port-uri2 #f))
    [(#f #f #f) #t]
    [(_ _ _)    #f]))

(test-assert "URI non-nil port, non-nil user, nil host, update port to nil"
  (match-values (uri-authority (update-uri-port sample-port-uri3 #f))
    [("user" #f #f) #t]
    [(_ _ _)        #f]))

(test-assert "URI non-nil port, nil user, non-nil host, update port to nil"
  (match-values (uri-authority (update-uri-port sample-port-uri4 #f))
    [(#f "example.org" #f) #t]
    [(_ _ _)               #f]))

(test-expect-fail 2)
(test-assert (update-uri-port sample-port-uri5 #f))
(test-assert (update-uri-port sample-port-uri6 19))

(test-end "set URI port")

(test-begin "set IRI port")
#|
   Port cases:
   a) Nil authority (all #f). Update port to non-nil.
   b) Nil authority. Update port to nil. No effect.
   c) Non-nil port, nil host/user. Target port is nil. Expect hostname to be "".
   d) Non-nil port, non-nil user, nil host. Target port is nil. Expect hostname to be nil.
   e) Non-nil port, non-nil host, nil user. Target port is nil. Expect hostname to be non-nil.
|#
(define sample-port-iri0 (string->iri "urn:/some/where/place"))
(define sample-port-iri1 (string->iri "urn:/some/where/place"))
(define sample-port-iri2 (string->iri "urn://:80"))
(define sample-port-iri3 (string->iri "urn://user@:80"))
(define sample-port-iri4 (string->iri "urn://example.org:80"))
(define sample-port-iri5 (string->iri "urn://:80//some/where/place"))
(define sample-port-iri6 (string->iri "urn:some/where/place"))

(test-assert "Persistence of update IRI port"
 (and (equal? 8080
              (iri-port (update-iri-port sample-port-iri0 8080)))
      (not    (iri-port sample-port-iri0))))


(test-assert "IRI nil authority, update port to non-nil"
  (match-values (iri-authority (update-iri-port sample-port-iri0 80))
    [(#f #f 80) #t]
    [(_ _ _)    #f]))


(test-assert "IRI nil authority, update port to nil. No effect."
  (match-values (iri-authority (update-iri-port sample-port-iri1 #f))
    [(#f #f #f) #t]
    [(_ _ _)    #f]))


(test-assert "IRI non-nil port, nil host/user, update port to nil"
  (match-values (iri-authority (update-iri-port sample-port-iri2 #f))
    [(#f #f #f) #t]
    [(_ _ _)    #f]))

(test-assert "IRI non-nil port, non-nil user, nil host, update port to nil"
  (match-values (iri-authority (update-iri-port sample-port-iri3 #f))
    [("user" #f #f) #t]
    [(_ _ _)        #f]))

(test-assert "IRI non-nil port, nil user, non-nil host, update port to nil"
  (match-values (iri-authority (update-iri-port sample-port-iri4 #f))
    [(#f "example.org" #f) #t]
    [(_ _ _)               #f]))

(test-expect-fail 2)
(test-assert (update-iri-port sample-port-iri5 #f))
(test-assert (update-iri-port sample-port-iri6 19))

(test-end "set IRI port")

;;;

(test-begin "set URI path")
#| In these test cases, we consider abempty paths which start with a double slash.
   path-absolute paths restrict this to a single slash.

   a) Authority is set, path is abempty. Set path to absolute. Expect success.
   b) Authority is set, path is abempty. Set path to empty. Expect success.
   c) Authority is set, path is abempty. Set path to noscheme. Expect failure.
   d) Authority is nil, path is absolute. Set path to abempty. Expect failure.
   e) Authority is nil, path is noscheme. Set path to abempty. Expect failure.
   f) Authority is nil, path is empty. Set path to abempty. Expect failure.
|#

(define sample-path-uri0 (string->uri "urn://example.org//some/where/place"))
(define sample-path-uri1 (string->uri "urn://example.org//some/where/place"))
(define sample-path-uri2 (string->uri "urn://example.org//some/where/place"))
(define sample-path-uri3 (string->uri "urn:/some/where/place"))
(define sample-path-uri4 (string->uri "urn:some/where/place"))
(define sample-path-uri5 (string->uri "urn:"))

(define path-absolute1 (build-path (empty-absolute-path) "some" "where" "place"))
(define path-empty (empty-path))
(define path-absolute2 (build-path (empty-absolute-path) "some" "where" "else"))
(define path-noscheme (build-path "some" "where" "place"))
(define path-abempty (build-path (empty-absolute-path) "" "some" "where" "place"))

(test-assert "Persistence of update URI path"
  (and (not (equal? (update-uri-path sample-path-uri0 path-absolute2)
                    sample-path-uri0))
       (path=? (uri-path sample-path-uri0)
               path-abempty)))

(test-assert (path=? path-absolute1 (uri-path (update-uri-path sample-path-uri1 path-absolute1))))
(test-assert (path=? path-empty (uri-path (update-uri-path sample-path-uri0 path-empty))))

(test-expect-fail 4)
(test-assert (update-uri-path sample-path-uri2 path-noscheme))
(test-assert (update-uri-path sample-path-uri3 path-abempty))
(test-assert (update-uri-path sample-path-uri4 path-abempty))
(test-assert (update-uri-path sample-path-uri5 path-abempty))

#|
   Test cases for character encoding of non-URI characters
|#
(define base-uri0 (string->uri "urn:/"))

(define test-uri-path0a (string->path "crêpes/in/Rhône"))
(define test-uri-path0b (string->path "/crêpes/in/Rhône"))
(define test-uri-path1a (string->path "The/Köln/Concert"))
(define test-uri-path1b (string->path "/The/Köln/Concert"))

(test-equal "escape relative path non-URI segment (1)"
            '("cr%C3%AApes" "in" "Rh%C3%B4ne")
            (path-segments (uri-path (update-uri-path base-uri0 test-uri-path0a))))
(test-equal "escape absolute path non-URI segment (1)"
            '("cr%C3%AApes" "in" "Rh%C3%B4ne")
            (path-segments (uri-path (update-uri-path base-uri0 test-uri-path0b))))
(test-equal "escape relative path non-URI segment (2)"
            '("The" "K%C3%B6ln" "Concert")
            (path-segments (uri-path (update-uri-path base-uri0 test-uri-path1a))))
(test-equal "escape absolute path non-URI segment (2)"
            '("The" "K%C3%B6ln" "Concert")
            (path-segments (uri-path (update-uri-path base-uri0 test-uri-path1b))))

(test-end "set URI path")

;;;

(test-begin "set IRI path")

(define sample-path-iri0 (string->iri "urn://example.org//some/where/place"))
(define sample-path-iri1 (string->iri "urn://example.org//some/where/place"))
(define sample-path-iri2 (string->iri "urn://example.org//some/where/place"))
(define sample-path-iri3 (string->iri "urn:/some/where/place"))
(define sample-path-iri4 (string->iri "urn:some/where/place"))
(define sample-path-iri5 (string->iri "urn:"))

(define path-absolute1 (build-path (empty-absolute-path) "some" "where" "place"))
(define path-empty (empty-path))
(define path-absolute2 (build-path (empty-absolute-path) "some" "where" "else"))
(define path-noscheme (build-path "some" "where" "place"))
(define path-abempty (build-path (empty-absolute-path) "" "some" "where" "place"))

(test-assert "Persistence of update IRI path"
  (and (not (equal? (update-iri-path sample-path-iri0 path-absolute2)
                    sample-path-iri0))
       (path=? (iri-path sample-path-iri0)
               path-abempty)))

;; test case (a) again:
(test-assert (path=? path-absolute1 (iri-path (update-iri-path sample-path-iri1 path-absolute1))))
(test-assert (path=? path-empty (iri-path (update-iri-path sample-path-iri0 path-empty))))

(test-expect-fail 4)
(test-assert (update-iri-path sample-path-iri2 path-noscheme))
(test-assert (update-iri-path sample-path-iri3 path-abempty))
(test-assert (update-iri-path sample-path-iri4 path-abempty))
(test-assert (update-iri-path sample-path-iri5 path-abempty))

#|
   We start with the same test cases for URIs, but expect that they are
   not percent-encoded as they are permissible in IRIs.

   Then, we try the same trick with the U+202E (right to left override),
   which is not permitted within an IRI.
|#
(define base-iri0 (string->iri "urn:/"))

(define test-iri-path0a (string->path "crêpes/in/Rhône"))
(define test-iri-path0b (string->path "/crêpes/in/Rhône"))
(define test-iri-path1a (string->path "The/Köln/Concert"))
(define test-iri-path1b (string->path "/The/Köln/Concert"))

(test-equal "non-escape in IRI relative path (1)"
            '("crêpes" "in" "Rhône")
            (path-segments (iri-path (update-iri-path base-iri0 test-iri-path0a))))
(test-equal "non-escape in IRI absolute path (1)"
            '("crêpes" "in" "Rhône")
            (path-segments (iri-path (update-iri-path base-iri0 test-iri-path0b))))
(test-equal "non-escape in IRI relative path (2)"
            '("The" "Köln" "Concert")
            (path-segments (iri-path (update-iri-path base-iri0 test-iri-path1a))))
(test-equal "non-escape in IRI absolute path (2)"
            '("The" "Köln" "Concert")
            (path-segments (iri-path (update-iri-path base-iri0 test-iri-path1b))))

(define test-iri-path2-input
  (list (string #\a (integer->char #x202E) #\b)
        (string #\c (integer->char #x202E) #\d)
        (string #\e (integer->char #x202E) #\f)))

(define test-iri-path2a (apply build-path (empty-path) test-iri-path2-input))
(define test-iri-path2b (apply build-path (empty-absolute-path) test-iri-path2-input))

(test-equal "escape relative path non-IRI segment"
            '("a%E2%80%AEb" "c%E2%80%AEd" "e%E2%80%AEf")
            (path-segments (iri-path (update-iri-path base-iri0 test-iri-path2a))))

(test-equal "escape relative path non-IRI segment"
            '("a%E2%80%AEb" "c%E2%80%AEd" "e%E2%80%AEf")
            (path-segments (iri-path (update-iri-path base-iri0 test-iri-path2b))))

(test-end "set IRI path")

;;;

(test-begin "set URI path segment")
#|
   a) Authority is set, path is abempty. Make it absolute by setting first segment. Expect success.
   b) Authority is nil, path is absolute. Set path to abempty by unsetting first segment. Expect failure.
   c) Authority is nil, path is rootless. Make it noscheme by eliminating colon. Expect success.
   d) Authority is nil, path is noscheme. Make it rootless by adding colon. Expect failure.
|#

(define inplace-path-uri6 (string->uri "urn://example.org//some/where/place"))
(define inplace-path-uri7 (string->uri "urn:/some/where/place"))
(define inplace-path-uri8 (string->uri "urn:some:thing/where/place"))
(define inplace-path-uri9 (string->uri "some/where/place"))

(test-assert
 (let ([res6 (uri-path-segment-update inplace-path-uri6 0 "ini")])
   (equal? "ini" (uri-path-segment-ref res6 0) )))

(test-expect-fail 1)
(test-assert (uri-path-segment-update inplace-path-uri7 0 ""))

(test-assert
 (let ([res8 (uri-path-segment-update inplace-path-uri8 0 "something")])
   (equal? "something" (uri-path-segment-ref res8 0))))

(test-expect-fail 1)
(test-assert (uri-path-segment-update inplace-path-uri9 0 "some:thing"))

(test-end "set URI path segment")

(test-begin "set IRI path segment")

(define inplace-path-iri6 (string->iri "urn://example.org//some/where/place"))
(define inplace-path-iri7 (string->iri "urn:/some/where/place"))
(define inplace-path-iri8 (string->iri "urn:some:thing/where/place"))
(define inplace-path-iri9 (string->iri "some/where/place"))

(test-assert
 (let ([res6 (iri-path-segment-update inplace-path-iri6 0 "ini")])
   (equal? "ini" (iri-path-segment-ref res6 0) )))

(test-expect-fail 1)
(test-assert (iri-path-segment-update inplace-path-iri7 0 ""))

(test-assert
 (let ([res8 (iri-path-segment-update inplace-path-iri8 0 "something")])
   (equal? "something" (iri-path-segment-ref res8 0))))

(test-expect-fail 1)
(test-assert (iri-path-segment-update inplace-path-iri9 0 "some:thing"))

(test-end "set IRI path segment")

;;;

(test-begin "set URI query")

(define inplace-query-uri0 (string->uri "urn://example.org/some/where/place"))
(define inplace-query-uri1 (string->uri "//example.org/some/where/place"))

(test-assert "Persistence of update URI query"
  (and (equal? "query" (uri-query (update-uri-query inplace-query-uri0 "query")))
       (not (uri-query inplace-query-uri0))))

(test-equal "query" (uri-query (update-uri-query inplace-query-uri0 "query")))
(test-equal "query" (uri-query (update-uri-query inplace-query-uri1 "query")))

(test-end "set URI query")

(test-begin "set IRI query")

(define inplace-query-iri0 (string->iri "urn://example.org/some/where/place"))
(define inplace-query-iri1 (string->iri "//example.org/some/where/place"))

(test-assert "Persistence of update IRI query"
  (and (equal? "query" (iri-query (update-iri-query inplace-query-iri0 "query")))
       (not (iri-query inplace-query-iri0))))

(test-equal "query" (iri-query (update-iri-query inplace-query-iri0 "query")))
(test-equal "query" (iri-query (update-iri-query inplace-query-iri1 "query")))

(test-end "set IRI query")

;;;

(test-begin "set URI fragment")

(define inplace-fragment-uri0 (string->uri "urn://example.org/some/where/place"))
(define inplace-fragment-uri1 (string->uri "//example.org/some/where/place"))

(test-assert "Persistence of update URI fragment"
  (and (equal? "fragment" (uri-fragment (update-uri-fragment inplace-fragment-uri0 "fragment")))
       (not (uri-fragment inplace-fragment-uri0))))

(test-equal "fragment" (uri-fragment (update-uri-fragment inplace-fragment-uri0 "fragment")))
(test-equal "fragment" (uri-fragment (update-uri-fragment inplace-fragment-uri1 "fragment")))

(test-end "set URI fragment")

(test-begin "set IRI fragment")

(define inplace-fragment-iri0 (string->iri "urn://example.org/some/where/place"))
(define inplace-fragment-iri1 (string->iri "//example.org/some/where/place"))

(test-assert "Persistence of update IRI fragment"
  (and (equal? "fragment" (iri-fragment (update-iri-fragment inplace-fragment-iri0 "fragment")))
       (not (iri-fragment inplace-fragment-iri0))))

(test-equal "fragment" (iri-fragment (update-iri-fragment inplace-fragment-iri0 "fragment")))
(test-equal "fragment" (iri-fragment (update-iri-fragment inplace-fragment-iri1 "fragment")))

(test-end "set IRI fragment")

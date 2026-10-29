; SPDX-FileCopyrightText: 2026 Duncan Guthrie
;
; SPDX-License-Identifier: MIT

#!r6rs

(test-begin "split username and password")

(define expected-user+pass?
  (case-lambda
   [(str user pass)
    (expected-user+pass? (compose uri-username+password string->uri)
                         str user pass)]
   [(op str user pass)
    (call-with-values (lambda () (op str))
      (lambda (user* pass*)
        (and (equal? user user*) (equal? pass pass*))))]))

(test-assert (expected-user+pass? "//foo:bar:qux@host"   "foo"       "bar:qux"))
(test-assert (expected-user+pass? "//foo%3Abar:qux@host" "foo%3Abar" "qux"))
(test-assert (expected-user+pass? "//@host"    ""    #f))
(test-assert (expected-user+pass? "//"         #f    #f))
(test-assert (expected-user+pass? "//foo@host" "foo" #f))

(test-end "split username and password")

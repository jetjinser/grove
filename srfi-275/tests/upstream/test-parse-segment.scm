;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
#!r6rs

(define ip-address-segments
  `(;; :: alone
    ["IPv6 address (:: alone)"  "http://[::]" "http" #f "[::]" #f "" #f #f]
    ;; Full address of hex eight segments
    ["IPv6 address (eight segments)" "http://[AAAA:BBBB:CCCC:DDDD:EEEE:FFFF:1111:2222]" "http" #f "[AAAA:BBBB:CCCC:DDDD:EEEE:FFFF:1111:2222]" #f "" #f #f]
    ["IPv6 address (six segments : IPv4)" "http://[AAAA:BBBB:CCCC:DDDD:EEEE:FFFF:10.0.12.255]" "http" #f "[AAAA:BBBB:CCCC:DDDD:EEEE:FFFF:10.0.12.255]" #f "" #f #f]
    ;; :: exactly seven hex segments
    ["IPv6 address (seven segments)" "http://[::BBBB:CCCC:DDDD:EEEE:FFFF:1111:2222]" "http" #f "[::BBBB:CCCC:DDDD:EEEE:FFFF:1111:2222]" #f "" #f #f]
    ["IPv6 address (five segments : IPv4)" "http://[::BBBB:CCCC:DDDD:EEEE:FFFF:10.0.12.255]" "http" #f "[::BBBB:CCCC:DDDD:EEEE:FFFF:10.0.12.255]" #f "" #f #f]
    ;; at most one hex segment :: exactly six segments
    ["IPv6 address (zero :: six segments)" "http://[::CCCC:DDDD:EEEE:FFFF:1111:2222]"     "http" #f "[::CCCC:DDDD:EEEE:FFFF:1111:2222]"     #f "" #f #f]
    ["IPv6 address (one :: six segments)"  "http://[AAAA::CCCC:DDDD:EEEE:FFFF:1111:2222]" "http" #f "[AAAA::CCCC:DDDD:EEEE:FFFF:1111:2222]" #f "" #f #f]
    ["IPv6 address (zero :: four segments : IPv4)" "http://[::CCCC:DDDD:EEEE:FFFF:10.0.12.255]"     "http" #f "[::CCCC:DDDD:EEEE:FFFF:10.0.12.255]"     #f "" #f #f]
    ["IPv6 address (one :: four segments : IPv4)"  "http://[AAAA::CCCC:DDDD:EEEE:FFFF:10.0.12.255]" "http" #f "[AAAA::CCCC:DDDD:EEEE:FFFF:10.0.12.255]" #f "" #f #f]
    ;; at most two hex segments :: exactly five hex segments
    ["IPv6 address (zero :: five segments)" "http://[::DDDD:EEEE:FFFF:1111:2222]"          "http" #f "[::DDDD:EEEE:FFFF:1111:2222]"          #f "" #f #f]
    ["IPv6 address (one :: five segments)"  "http://[AAAA::DDDD:EEEE:FFFF:1111:2222]"      "http" #f "[AAAA::DDDD:EEEE:FFFF:1111:2222]"      #f "" #f #f]
    ["IPv6 address (two :: five segments)"  "http://[AAAA:BBBB::DDDD:EEEE:FFFF:1111:2222]" "http" #f "[AAAA:BBBB::DDDD:EEEE:FFFF:1111:2222]" #f "" #f #f]
    ["IPv6 address (zero :: three segments : IPv4)" "http://[::DDDD:EEEE:FFFF:10.0.12.255]"          "http" #f "[::DDDD:EEEE:FFFF:10.0.12.255]"          #f "" #f #f]
    ["IPv6 address (one :: three segments : IPv4)"  "http://[AAAA::DDDD:EEEE:FFFF:10.0.12.255]"      "http" #f "[AAAA::DDDD:EEEE:FFFF:10.0.12.255]"      #f "" #f #f]
    ["IPv6 address (two :: three segments : IPv4)"  "http://[AAAA:BBBB::DDDD:EEEE:FFFF:10.0.12.255]" "http" #f "[AAAA:BBBB::DDDD:EEEE:FFFF:10.0.12.255]" #f "" #f #f]
    ;; at most three hex segments :: exactly four hex segments
    ["IPv6 address (zero :: four segments)"  "http://[::EEEE:FFFF:1111:2222]"               "http" #f "[::EEEE:FFFF:1111:2222]"               #f "" #f #f]
    ["IPv6 address (one :: four segments)"   "http://[AAAA::EEEE:FFFF:1111:2222]"           "http" #f "[AAAA::EEEE:FFFF:1111:2222]"           #f "" #f #f]
    ["IPv6 address (two :: four segments)"   "http://[AAAA:BBBB::EEEE:FFFF:1111:2222]"      "http" #f "[AAAA:BBBB::EEEE:FFFF:1111:2222]"      #f "" #f #f]
    ["IPv6 address (three :: four segments)" "http://[AAAA:BBBB:CCCC::EEEE:FFFF:1111:2222]" "http" #f "[AAAA:BBBB:CCCC::EEEE:FFFF:1111:2222]" #f "" #f #f]
    ["IPv6 address (zero :: two segments : IPv4)"  "http://[::EEEE:FFFF:10.0.12.255]"               "http" #f "[::EEEE:FFFF:10.0.12.255]"               #f "" #f #f]
    ["IPv6 address (one :: two segments : IPv4)"   "http://[AAAA::EEEE:FFFF:10.0.12.255]"           "http" #f "[AAAA::EEEE:FFFF:10.0.12.255]"           #f "" #f #f]
    ["IPv6 address (two :: two segments : IPv4)"   "http://[AAAA:BBBB::EEEE:FFFF:10.0.12.255]"      "http" #f "[AAAA:BBBB::EEEE:FFFF:10.0.12.255]"      #f "" #f #f]
    ["IPv6 address (three :: two segments : IPv4)" "http://[AAAA:BBBB:CCCC::EEEE:FFFF:10.0.12.255]" "http" #f "[AAAA:BBBB:CCCC::EEEE:FFFF:10.0.12.255]" #f "" #f #f]
    ;; at most four hex segments :: exactly three hex segments
    ["IPv6 address (zero :: three segments)"  "http://[::FFFF:1111:2222]"                    "http" #f "[::FFFF:1111:2222]"                    #f "" #f #f]
    ["IPv6 address (one :: three segments)"   "http://[AAAA::FFFF:1111:2222]"                "http" #f "[AAAA::FFFF:1111:2222]"                #f "" #f #f]
    ["IPv6 address (two :: three segments)"   "http://[AAAA:BBBB::FFFF:1111:2222]"           "http" #f "[AAAA:BBBB::FFFF:1111:2222]"           #f "" #f #f]
    ["IPv6 address (three :: three segments)" "http://[AAAA:BBBB:CCCC::FFFF:1111:2222]"      "http" #f "[AAAA:BBBB:CCCC::FFFF:1111:2222]"      #f "" #f #f]
    ["IPv6 address (four :: three segments)"  "http://[AAAA:BBBB:CCCC:DDDD::FFFF:1111:2222]" "http" #f "[AAAA:BBBB:CCCC:DDDD::FFFF:1111:2222]" #f "" #f #f]
    ["IPv6 address (zero :: one segment : IPv4)"  "http://[::FFFF:10.0.12.255]"                    "http" #f "[::FFFF:10.0.12.255]"                    #f "" #f #f]
    ["IPv6 address (one :: one segment : IPv4)"   "http://[AAAA::FFFF:10.0.12.255]"                "http" #f "[AAAA::FFFF:10.0.12.255]"                #f "" #f #f]
    ["IPv6 address (two :: one segment : IPv4)"   "http://[AAAA:BBBB::FFFF:10.0.12.255]"           "http" #f "[AAAA:BBBB::FFFF:10.0.12.255]"           #f "" #f #f]
    ["IPv6 address (three :: one segment : IPv4)" "http://[AAAA:BBBB:CCCC::FFFF:10.0.12.255]"      "http" #f "[AAAA:BBBB:CCCC::FFFF:10.0.12.255]"      #f "" #f #f]
    ["IPv6 address (four :: one segment : IPv4)"  "http://[AAAA:BBBB:CCCC:DDDD::FFFF:10.0.12.255]" "http" #f "[AAAA:BBBB:CCCC:DDDD::FFFF:10.0.12.255]" #f "" #f #f]
    ;; at most five hex segments :: exactly two hex segments
    ["IPv6 address (zero :: two segments)"  "http://[::1111:2222]"                         "http" #f "[::1111:2222]"                         #f "" #f #f]
    ["IPv6 address (one :: two segments)"   "http://[AAAA::1111:2222]"                     "http" #f "[AAAA::1111:2222]"                     #f "" #f #f]
    ["IPv6 address (two :: two segments)"   "http://[AAAA:BBBB::1111:2222]"                "http" #f "[AAAA:BBBB::1111:2222]"                #f "" #f #f]
    ["IPv6 address (three :: two segments)" "http://[AAAA:BBBB:CCCC::1111:2222]"           "http" #f "[AAAA:BBBB:CCCC::1111:2222]"           #f "" #f #f]
    ["IPv6 address (four :: two segments)"  "http://[AAAA:BBBB:CCCC:DDDD::1111:2222]"      "http" #f "[AAAA:BBBB:CCCC:DDDD::1111:2222]"      #f "" #f #f]
    ["IPv6 address (five :: two segments)"  "http://[AAAA:BBBB:CCCC:DDDD:EEEE::1111:2222]" "http" #f "[AAAA:BBBB:CCCC:DDDD:EEEE::1111:2222]" #f "" #f #f]
    ["IPv6 address (zero :: IPv4)"  "http://[::10.0.12.255]"                         "http" #f "[::10.0.12.255]"                         #f "" #f #f]
    ["IPv6 address (one :: IPv4)"   "http://[AAAA::10.0.12.255]"                     "http" #f "[AAAA::10.0.12.255]"                     #f "" #f #f]
    ["IPv6 address (two :: IPv4)"   "http://[AAAA:BBBB::10.0.12.255]"                "http" #f "[AAAA:BBBB::10.0.12.255]"                #f "" #f #f]
    ["IPv6 address (three :: IPv4)" "http://[AAAA:BBBB:CCCC::10.0.12.255]"           "http" #f "[AAAA:BBBB:CCCC::10.0.12.255]"           #f "" #f #f]
    ["IPv6 address (four :: IPv4)"  "http://[AAAA:BBBB:CCCC:DDDD::10.0.12.255]"      "http" #f "[AAAA:BBBB:CCCC:DDDD::10.0.12.255]"      #f "" #f #f]
    ["IPv6 address (five :: IPv4)"  "http://[AAAA:BBBB:CCCC:DDDD:EEEE::10.0.12.255]" "http" #f "[AAAA:BBBB:CCCC:DDDD:EEEE::10.0.12.255]" #f "" #f #f]
    ;; at most six hex segments :: exactly one hex segment
    ["IPv6 address (zero :: one segment)"  "http://[::2222]"                              "http" #f "[::2222]"                              #f "" #f #f]
    ["IPv6 address (one :: one segment)"   "http://[AAAA::2222]"                          "http" #f "[AAAA::2222]"                          #f "" #f #f]
    ["IPv6 address (two :: one segment)"   "http://[AAAA:BBBB::2222]"                     "http" #f "[AAAA:BBBB::2222]"                     #f "" #f #f]
    ["IPv6 address (three :: one segment)" "http://[AAAA:BBBB:CCCC::2222]"                "http" #f "[AAAA:BBBB:CCCC::2222]"                #f "" #f #f]
    ["IPv6 address (four :: one segment)"  "http://[AAAA:BBBB:CCCC:DDDD::2222]"           "http" #f "[AAAA:BBBB:CCCC:DDDD::2222]"           #f "" #f #f]
    ["IPv6 address (five :: one segment)"  "http://[AAAA:BBBB:CCCC:DDDD:EEEE::2222]"      "http" #f "[AAAA:BBBB:CCCC:DDDD:EEEE::2222]"      #f "" #f #f]
    ["IPv6 address (six :: one segment)"   "http://[AAAA:BBBB:CCCC:DDDD:EEEE:FFFF::2222]" "http" #f "[AAAA:BBBB:CCCC:DDDD:EEEE:FFFF::2222]" #f "" #f #f]
    ;; positive cases suggested by Ivan Raikov
    ["IPv6 address (all-zeros unspecified address)" "http://[::]/" "http" #f "[::]" #f "/" #f #f]
    ["IPv6 address (loopback)"                      "http://[::1]/" "http" #f "[::1]" #f "/" #f #f]
    ["IPv6 address (trailing ::)"                   "http://[1::]/" "http" #f "[1::]" #f "/" #f #f]
    ["IPv6 address (trailing :: with prefix)"       "http://[2001:db8::]/" "http" #f "[2001:db8::]" #f "/" #f #f]
    ["IPv6 address (leading ::)"                    "http://[::2001:db8]/" "http" #f "[::2001:db8]" #f "/" #f #f]
    ["IPv6 address (IPv4-mapped)"                   "http://[::ffff:192.0.2.1]/"  "http" #f "[::ffff:192.0.2.1]" #f "/" #f #f]
    ["IPv6 address (IPv4-translated (RFC 6052))"    "http://[64:ff9b::192.0.2.1]/" "http" #f "[64:ff9b::192.0.2.1]" #f "/" #f #f]
    ;; IPv4 addresses
    ["IPv4 address (host only)"        "http://10.0.0.0/"   "http" #f  "10.0.0.0" #f "/" #f #f]
    ["IPv4 address (user+host)"        "http://a@10.0.0.0/" "http" "a" "10.0.0.0" #f "/" #f #f]
    ["IPv4 address (user+host+port)"   "http://a@10.0.0.0:8080/" "http" "a" "10.0.0.0" 8080 "/" #f #f]
    ["IPv4 address (max octets)"       "http://255.255.255.128/" "http" #f  "255.255.255.128" #f "/" #f #f]
    ["IPv4 address (min octets)"       "http://0.0.0.0/" "http" #f  "0.0.0.0" #f "/" #f #f]
    ["IPv4 address (mixed octets)"     "http://0.1.22.223/" "http" #f  "0.1.22.223" #f "/" #f #f]
    ;;;
    ))

(define relative-regular-segments
  '(;; Relative reference specific tests
    ["Empty URI"       ""     #f #f #f "" #f #f]
    ["Empty authority" "//"   #f "" #f "" #f #f]
    ["Empty user"      "//@"  "" #f #f "" #f #f]
    ["Empty port"      "//:"  #f "" #f "" #f #f]
    ["Empty query"     "?"    #f #f #f "" "" #f]
    ["Empty fragment"  "#"    #f #f #f "" #f ""]
    ["Path which looks like a hostname"   "example.org"            #f    #f #f "example.org"     #f #f]
    ["Path which looks like a URN"        "./urn:something"        #f    #f #f "./urn:something" #f #f]
    ["Relative reference, nil host"        "////g"    #f ""  #f "//g"     #f #f]
    ["Relative reference, path, nil host"  ".///g"    #f #f  #f ".///g"   #f #f]
    ["Relative reference, non-empty host"  "//g"      #f "g" #f ""       #f #f]
    ["Path which looks like a query"       "./p=q:r"  #f #f  #f "./p=q:r" #f #f]
    ["Relative reference, all components filled out"                 "//user@example.org:80/some/where/place?qua#ought" "user" "example.org" 80 "/some/where/place" "qua" "ought"]
    ["Relative reference, all components except user filled out"     "//example.org:80/some/where/place?qua#ought"      #f "example.org"     80 "/some/where/place" "qua" "ought"]
    ["Relative reference, all components except host filled out"     "//user@:80/some/where/place?qua#ought"            "user" #f            80 "/some/where/place" "qua" "ought"]
    ["Relative reference, all components except port filled out"     "//user@example.org:80/some/where/place?qua#ought" "user" "example.org" 80 "/some/where/place" "qua" "ought"]
    ["Relative reference, all components except path filled out"     "//user@example.org:80?qua#ought"                  "user" "example.org" 80 ""                  "qua" "ought"]
    ["Relative reference, all components except query filled out"    "//user@example.org:80/some/where/place#ought"     "user" "example.org" 80 "/some/where/place" #f    "ought"]
    ["Relative reference, all components except fragment filled out" "//user@example.org:80/some/where/place?qua"       "user" "example.org" 80 "/some/where/place" "qua" #f]
    ["Relative reference empty host, nil user/port"     "///some/where/place?qua#ought"  #f "" #f "/some/where/place" "qua" "ought"]
    ["Relative reference empty user, nil host/port"     "//@/some/where/place?qua#ought" "" #f #f "/some/where/place" "qua" "ought"]
    ["Relative reference empty port implies empty host" "//:/some/where/place?qua#ought" #f "" #f "/some/where/place" "qua" "ought"]))

(define non-relative-regular-segments
  `(["URN-like"                           "urn:something"   "urn"  #f    #f #f "something"       #f #f]
    ["URN-like, path looks like hostname" "urn:example.org" "urn"  #f    #f #f "example.org"     #f #f]
    ["User with colon segment"            "http://a:b@c:29" "http" "a:b" "c" 29 ""       #f #f]
    ["User-like component appears as path" "http::@c:29"    "http" #f    #f  #f ":@c:29" #f #f]
    ["Host-like component appears as user" "http://example.org:b@d/"   "http" "example.org:b" "d" #f "/" #f #f]
    ["Padded port as numeric value"        "http://example.org:000080" "http" #f    "example.org" 80 ""  #f #f]
    ["Query component with question mark"    "http://example.org/abcd?efgh?ijkl" "http" #f "example.org" #f "/abcd" "efgh?ijkl" #f]
    ["Fragment component with question mark" "http://example.org/abcd#efgh?ijkl" "http" #f "example.org" #f "/abcd" #f "efgh?ijkl"]
    ["Path where first segment looks like host" "http:///some/where/place" "http" #f "" #f "/some/where/place" #f #f]
    ;;;
    ["Scheme with nil host"          "foo:"      "foo" #f #f  #f ""      #f #f]
    ["Scheme with path, empty host"  "foo:////g" "foo" #f ""  #f "//g"   #f #f]
    ["Scheme with path, nil host"    "foo:.///g" "foo" #f #f  #f ".///g" #f #f]
    ["Scheme with non-empty host"    "foo://g"   "foo" #f "g" #f ""      #f #f]
    ["All components filled out"                 "http://user@example.org:80/some/where/place?qua#ought"  "user" "example.org" 80 "/some/where/place" "qua" "ought"]
    ["All components except user filled out"     "http://example.org:80/some/where/place?qua#ought"       #f "example.org"     80 "/some/where/place" "qua" "ought"]
    ["All components except host filled out"     "http://user@:80/some/where/place?qua#ought"             "user" #f            80 "/some/where/place" "qua" "ought"]
    ["All components except port filled out"     "http://user@example.org/some/where/place?qua#ought"     "user" "example.org" #f "/some/where/place" "qua" "ought"]
    ["All components except path filled out"     "http://user@example.org:80?qua#ought"                   "user" "example.org" 80 ""                  "qua" "ought"]
    ["All components except query filled out"    "http://user@example.org:80/some/where/place#ought"      "user" "example.org" 80 "/some/where/place" #f    "ought"]
    ["All components except fragment filled out" "http://user@example.org:80/some/where/place?qua"        "user" "example.org" 80 "/some/where/place" "qua" #f]
    ["Empty host, nil user/port"     "http:///some/where/place?qua#ought"   "http" #f "" #f "/some/where/place" "qua" "ought"]
    ["Empty user, nil host/port"     "http://@/some/where/place?qua#ought"  "http" "" #f #f "/some/where/place" "qua" "ought" ]
    ["Empty port implies empty host" "http://:/some/where/place?qua#ought"  "http" #f "" #f "/some/where/place" "qua" "ought"]))

(define absolute-regular-segments
  `(;; Largely duplicates `non-relative-regular-segments'
    ["Absolute, URN-like"                           "urn:something"   "urn"  #f    #f #f "something"       #f #f]
    ["Absolute, URN-like, path looks like hostname" "urn:example.org" "urn"  #f    #f #f "example.org"     #f #f]
    ["Absolute, user with colon segment"            "http://a:b@c:29" "http" "a:b" "c" 29 ""       #f #f]
    ["Absolute, user-like component appears as path" "http::@c:29"    "http" #f    #f  #f ":@c:29" #f #f]
    ["Absolute, host-like component appears as user" "http://example.org:b@d/"   "http" "example.org:b" "d" #f "/" #f #f]
    ["Absolute, padded port as numeric value"        "http://example.org:000080" "http" #f    "example.org" 80 ""  #f #f]
    ["Absolute, query component with question mark"    "http://example.org/abcd?efgh?ijkl" "http" #f "example.org" #f "/abcd" "efgh?ijkl" #f]
    ["Absolute, path where first segment looks like host" "http:///some/where/place" "http" #f "" #f "/some/where/place" #f #f]
    ;;;
    ["Absolute, scheme with nil host"          "foo:"      "foo" #f #f  #f ""      #f #f]
    ["Absolute, scheme with path, empty host"  "foo:////g" "foo" #f ""  #f "//g"   #f #f]
    ["Absolute, scheme with path, nil host"    "foo:.///g" "foo" #f #f  #f ".///g" #f #f]
    ["Absolute, scheme with non-empty host"    "foo://g"   "foo" #f "g" #f ""      #f #f]
    ["Absolute, all components filled out"                 "http://user@example.org:80/some/where/place?qua"  "user" "example.org" 80 "/some/where/place" "qua" #f]
    ["Absolute, all components except user filled out"     "http://example.org:80/some/where/place?qua"       #f "example.org"     80 "/some/where/place" "qua" #f]
    ["Absolute, all components except host filled out"     "http://user@:80/some/where/place?qua"             "user" #f            80 "/some/where/place" "qua" #f]
    ["Absolute, all components except port filled out"     "http://user@example.org/some/where/place?qua"     "user" "example.org" #f "/some/where/place" "qua" #f]
    ["Absolute, all components except path filled out"     "http://user@example.org:80?qua"                   "user" "example.org" 80 ""                  "qua" #f]
    ["Absolute, all components except query filled out"    "http://user@example.org:80/some/where/place"      "user" "example.org" 80 "/some/where/place" #f    #f]
    ["Absolute, empty host, nil user/port"     "http:///some/where/place?qua"   "http" #f "" #f "/some/where/place" "qua" #f]
    ["Absolute, empty user, nil host/port"     "http://@/some/where/place?qua"  "http" "" #f #f "/some/where/place" "qua" #f]
    ["Absolute, empty port implies empty host" "http://:/some/where/place?qua"  "http" #f "" #f "/some/where/place" "qua" #f]))

(define non-relative-escaped-segments
  `(["Completely percent-encoded" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79#%66%72%61%67%6D%65%6E%74"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" "%66%72%61%67%6D%65%6E%74"]
    ["Mixed percent-encoded user" "http://a%75%73%65%72:pass@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79#%66%72%61%67%6D%65%6E%74"
     "http" "a%75%73%65%72:pass" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" "%66%72%61%67%6D%65%6E%74"]
    ["Mixed percent-encoded path" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65?%71%75%65%72%79#%66%72%61%67%6D%65%6E%74"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65" "%71%75%65%72%79" "%66%72%61%67%6D%65%6E%74"]
    ["Mixed percent-encoded query" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?q%75e%72y#%66%72%61%67%6D%65%6E%74"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "q%75e%72y" "%66%72%61%67%6D%65%6E%74"]
    ["Mixed percent-encoded fragment" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79#fra%67%6D%65en%74"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" "fra%67%6D%65en%74"]
    ["Percent-encoded user only" "http://a%75%73%65%72:pass@example.org:8080/some/where/place?query#fragment"
     "http" "a%75%73%65%72:pass" "example.org" 8080 "/some/where/place" "query" "fragment"]
    ["Percent-encoded path only" "http://auser:pass@example.org:8080/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65?query#fragment"
     "http" "auser:pass" "example.org" 8080 "/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65" "query" "fragment"]
    ["Percent-encoded query only" "http://auser:pass@example.org:8080/some/where/place?q%75e%72y#fragment"
     "http" "auser:pass" "example.org" 8080 "/some/where/place" "q%75e%72y" "fragment"]
    ["Percent-encoded fragment only" "http://auser:pass@example.org:8080/some/where/place?query#fra%67%6D%65en%74"
     "http" "auser:pass" "example.org" 8080 "/some/where/place" "query" "fra%67%6D%65en%74"]))

(define relative-escaped-segments
  `(["Relative reference, Completely percent-encoded" "//%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79#%66%72%61%67%6D%65%6E%74"
     "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" "%66%72%61%67%6D%65%6E%74"]
    ["Relative reference, Mixed percent-encoded user" "//a%75%73%65%72:pass@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79#%66%72%61%67%6D%65%6E%74"
     "a%75%73%65%72:pass" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" "%66%72%61%67%6D%65%6E%74"]
    ["Relative reference, Mixed percent-encoded path" "//%75%73%65%72@example.org:8080/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65?%71%75%65%72%79#%66%72%61%67%6D%65%6E%74"
     "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65" "%71%75%65%72%79" "%66%72%61%67%6D%65%6E%74"]
    ["Relative reference, Mixed percent-encoded query" "//%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?q%75e%72y#%66%72%61%67%6D%65%6E%74"
     "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "q%75e%72y" "%66%72%61%67%6D%65%6E%74"]
    ["Relative reference, Mixed percent-encoded fragment" "//%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79#fra%67%6D%65en%74"
     "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" "fra%67%6D%65en%74"]
    ["Relative reference, Percent-encoded user only" "//a%75%73%65%72:pass@example.org:8080/some/where/place?query#fragment"
     "a%75%73%65%72:pass" "example.org" 8080 "/some/where/place" "query" "fragment"]
    ["Relative reference, Percent-encoded path only" "//auser:pass@example.org:8080/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65?query#fragment"
     "auser:pass" "example.org" 8080 "/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65" "query" "fragment"]
    ["Relative reference, Percent-encoded query only" "//auser:pass@example.org:8080/some/where/place?q%75e%72y#fragment"
     "auser:pass" "example.org" 8080 "/some/where/place" "q%75e%72y" "fragment"]
    ["Relative reference, Percent-encoded fragment only" "//auser:pass@example.org:8080/some/where/place?query#fra%67%6D%65en%74"
     "auser:pass" "example.org" 8080 "/some/where/place" "query" "fra%67%6D%65en%74"]))

(define absolute-escaped-segments
  `(;; Largely duplicates `non-relative-escaped-segments'
    ["Completely percent-encoded" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" #f]
    ["Mixed percent-encoded user" "http://a%75%73%65%72:pass@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79"
     "http" "a%75%73%65%72:pass" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" #f]
    ["Mixed percent-encoded path" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65?%71%75%65%72%79"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65" "%71%75%65%72%79" #f]
    ["Mixed percent-encoded query" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?q%75e%72y"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "q%75e%72y" #f]
    ["Mixed percent-encoded fragment" "http://%75%73%65%72@example.org:8080/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65?%71%75%65%72%79"
     "http" "%75%73%65%72" "example.org" 8080 "/%73%6F%6D%65%2F%77%68%65%72%65%2F%70%6C%61%63%65" "%71%75%65%72%79" #f]
    ["Percent-encoded user only" "http://a%75%73%65%72:pass@example.org:8080/some/where/place?query"
     "http" "a%75%73%65%72:pass" "example.org" 8080 "/some/where/place" "query" #f]
    ["Percent-encoded path only" "http://auser:pass@example.org:8080/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65?query"
     "http" "auser:pass" "example.org" 8080 "/%73%6F%6D%65/%77%68%65%72%65/%70%6C%61%63%65" "query" #f]
    ["Percent-encoded query only" "http://auser:pass@example.org:8080/some/where/place?q%75e%72y"
     "http" "auser:pass" "example.org" 8080 "/some/where/place" "q%75e%72y" #f]
    ["Percent-encoded fragment only" "http://auser:pass@example.org:8080/some/where/place?query"
     "http" "auser:pass" "example.org" 8080 "/some/where/place" "query" #f]))

(define (make-uri-test-case-of ->uri)
  (match-lambda
    [((list desc repr user host port path query fragment))
     (let* ([resource (->uri repr)]
            [result
             (and (equal? user (uri-user resource))
                  (equal? host (uri-host resource))
                  (equal? port (uri-port resource))
                  (equal? path (uri-path-string resource))
                  (equal? query (uri-query resource))
                  (equal? fragment (uri-fragment resource)))])
       (test-assert desc result))]
    [((list desc repr scheme user host port path query fragment))
     (let* ([resource (->uri repr)]
            [result
             (and (equal? scheme (uri-scheme resource))
                  (equal? user (uri-user resource))
                  (equal? host (uri-host resource))
                  (equal? port (uri-port resource))
                  (equal? path (uri-path-string resource))
                  (equal? query (uri-query resource))
                  (equal? fragment (uri-fragment resource)))])
       (test-assert desc result))]))

(define (make-iri-test-case-of ->iri)
  (match-lambda
    [((list desc repr user host port path query fragment))
     (let* ([resource (->iri repr)]
            [result
             (and (equal? user (iri-user resource))
                  (equal? host (iri-host resource))
                  (equal? port (iri-port resource))
                  (equal? path (iri-path-string resource))
                  (equal? query (iri-query resource))
                  (equal? fragment (iri-fragment resource)))])
       (test-assert desc result))]
    [((list desc repr scheme user host port path query fragment))
     (let* ([resource (->iri repr)]
            [result
             (and (equal? scheme (iri-scheme resource))
                  (equal? user (iri-user resource))
                  (equal? host (iri-host resource))
                  (equal? port (iri-port resource))
                  (equal? path (iri-path-string resource))
                  (equal? query (iri-query resource))
                  (equal? fragment (iri-fragment resource)))])
       (test-assert desc result))]))

(define all-regular-segments (append non-relative-regular-segments relative-regular-segments absolute-regular-segments))
(define all-escaped-segments (append non-relative-escaped-segments relative-escaped-segments absolute-escaped-segments))

(test-begin "RFC 3986 parse expected segments")
(for-each (make-uri-test-case-of string->uri) all-regular-segments)
(test-end "RFC 3986 parse expected segments")

(test-begin "RFC 3987 parse expected segments")
(for-each (make-iri-test-case-of string->iri) all-regular-segments)
(test-end "RFC 3987 parse expected segments")

(test-begin "RFC 3986 parse escaped expected segments")
(for-each (make-uri-test-case-of string->uri) all-escaped-segments)
(test-end "RFC 3986 parse escaped expected segments")

(test-begin "RFC 3987 parse escaped expected segments")
(for-each (make-iri-test-case-of string->iri) all-escaped-segments)
(test-end "RFC 3987 parse escaped expected segments")


(test-begin "RFC 3986 parse IP address expected segments")
    #;["IPv6 address (port, path, query, fragment)"   "http" #f "[2001:db8::1]" 8080 "/path" "query" "fragment"]

(test-assert "IPv6 address (port, path, query, fragment)"
  (let ([tgt (string->uri  "http://[2001:db8::1]:8080/path?query#fragment")])
    (and (equal? (uri-scheme tgt) "http")
         (equal? (uri-host tgt)   "[2001:db8::1]")
         (equal? (uri-port tgt)   8080)
         (equal? (uri-path-string tgt) "/path")
         (equal? (uri-query tgt) "query")
         (equal? (uri-fragment tgt) "fragment")
         (not (uri-user tgt)))))

(for-each (make-uri-test-case-of string->uri) ip-address-segments)
(for-each (make-uri-test-case-of string->rfc3986-absolute-uri) ip-address-segments)
(for-each (make-uri-test-case-of string->non-relative-uri) ip-address-segments)

(test-expect-fail 9)
(test-assert "reject more than one :: compression marker"
             (string->uri "http://[2001:db8:::1]/"))
(test-assert "reject nine groups (maximum is eight)"
             (string->uri "http://[2001:db8:a:b:c:d:e:f:1]/"))
(test-assert "reject seven groups with no :: (too few)"
             (string->uri "http://[2001:db8:a:b:c:d:e]/"))
(test-assert "reject group exceeds four hex digits"
             (string->uri "http://[2001:db8::fffff]/"))
(test-assert "reject same; leading zeros push group to five digits"
             (string->uri "http://[2001:00db8::0001]/"))
(test-assert "reject incomplete IPv4 suffix (three octets, not four)"
             (string->uri "http://[2001:db8::192.0.2]/"))
(test-assert "reject two separate :: markers"
             (string->uri "http://[2001:db8:a::b::c]/"))
(test-assert "reject only two groups, no ::"
             (string->uri "http://[2001:db8]/"))
(test-assert "reject unclosed bracket"
             (string->uri "http://[2001:db8"))

(test-end "RFC 3986 parse IP address expected segments")


(test-begin "RFC 3987 parse IP address expected segments")

(test-assert "IPv6 address (port, path, query, fragment)"
  (let ([tgt (string->iri  "http://[2001:db8::1]:8080/path?query#fragment")])
    (and (equal? (iri-scheme tgt) "http")
         (equal? (iri-host tgt)   "[2001:db8::1]")
         (equal? (iri-port tgt)   8080)
         (equal? (iri-path-string tgt) "/path")
         (equal? (iri-query tgt) "query")
         (equal? (iri-fragment tgt) "fragment")
         (not (iri-user tgt)))))

(for-each (make-uri-test-case-of string->uri) ip-address-segments)
(for-each (make-iri-test-case-of string->rfc3987-absolute-iri) ip-address-segments)
(for-each (make-iri-test-case-of string->non-relative-iri) ip-address-segments)

(test-expect-fail 9)
(test-assert "reject more than one :: compression marker"
             (string->iri "http://[2001:db8:::1]/"))
(test-assert "reject nine groups (maximum is eight)"
             (string->iri "http://[2001:db8:a:b:c:d:e:f:1]/"))
(test-assert "reject seven groups with no :: (too few)"
             (string->iri "http://[2001:db8:a:b:c:d:e]/"))
(test-assert "reject group exceeds four hex digits"
             (string->iri "http://[2001:db8::fffff]/"))
(test-assert "reject same; leading zeros push group to five digits"
             (string->iri "http://[2001:00db8::0001]/"))
(test-assert "reject incomplete IPv4 suffix (three octets, not four)"
             (string->iri "http://[2001:db8::192.0.2]/"))
(test-assert "reject two separate :: markers"
             (string->iri "http://[2001:db8:a::b::c]/"))
(test-assert "reject only two groups, no ::"
             (string->iri "http://[2001:db8]/"))
(test-assert "reject unclosed bracket"
             (string->iri "http://[2001:db8"))

(test-end "RFC 3987 parse IP address expected segments")


(test-begin "RFC 3986 parse absolute-URI production")

(for-each (make-uri-test-case-of string->rfc3986-absolute-uri) absolute-regular-segments)

(test-assert "string->rfc3986-absolute-uri accepts nil authority"
             (string->rfc3986-absolute-uri "urn:/some/where/place"))
(test-assert "string->rfc3986-absolute-uri accepts nil query"
             (string->rfc3986-absolute-uri "http://example.org/some/where/place"))
(test-assert "string->rfc3986-absolute-uri accepts fully-qualified URI minus fragment"
             (string->rfc3986-absolute-uri "http://example.org/some/where/place?a"))

(test-expect-fail 2)
(test-assert "string->rfc3986-absolute-uri rejects nil scheme"
             (string->rfc3986-absolute-uri "//example.org/some/where/place?a"))
(test-assert "string->rfc3986-absolute-uri rejects non-nil fragment"
             (string->rfc3986-absolute-uri "http://example.org/some/where/place?a#a"))

(test-end "RFC 3986 parse absolute-URI production")


(test-begin "RFC 3987 parse absolute-IRI production")

(for-each (make-iri-test-case-of string->rfc3987-absolute-iri) absolute-regular-segments)

(test-assert "string->rfc3987-absolute-iri accepts nil authority"
             (string->rfc3987-absolute-iri "urn:/some/where/place"))
(test-assert "string->rfc3987-absolute-iri accepts nil query"
             (string->rfc3987-absolute-iri "http://example.org/some/where/place"))
(test-assert "string->rfc3987-absolute-iri accepts fully-qualified IRI minus fragment"
             (string->rfc3987-absolute-iri "http://example.org/some/where/place?a"))

(test-expect-fail 2)
(test-assert "string->rfc3987-absolute-iri rejects nil scheme"
             (string->rfc3987-absolute-iri "//example.org/some/where/place?a"))
(test-assert "string->rfc3987-absolute-iri rejects non-nil fragment"
             (string->rfc3987-absolute-iri "http://example.org/some/where/place?a#a"))

(test-end "RFC 3987 parse absolute-IRI production")


(test-begin "RFC 3986 parse non-relative URI")

(for-each (make-uri-test-case-of string->non-relative-uri) non-relative-regular-segments)

(test-assert "string->non-relative-uri accepts non-nil fragment"
             (string->non-relative-uri "http://example.org/some/where/place?a#a"))

(test-expect-fail 1)
(test-assert "string->non-relative-uri rejects nil scheme"
             (string->non-relative-uri "//example.org/some/where/place?a"))

(test-end "RFC 3986 parse non-relative URI")

(test-begin "RFC 3986 parse non-relative IRI")

(for-each (make-iri-test-case-of string->non-relative-iri) non-relative-regular-segments)

(test-assert "string->non-relative-iri accepts non-nil fragment"
             (string->non-relative-iri "http://example.org/some/where/place?a#a"))

(test-expect-fail 1)
(test-assert "string->non-relative-iri rejects nil scheme"
             (string->non-relative-iri "//example.org/some/where/place?a"))

(test-end "RFC 3986 parse non-relative IRI")

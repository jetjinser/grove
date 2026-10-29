;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;; SPDX-License-Identifier: MIT

;; Guile's R6RS loader maps every (srfi :275 name) library name to this
;; aggregate module. Guile-native clients can import the granular modules.
(define-module (srfi srfi-275)
  #:use-module (srfi :275 iri)
  #:use-module (srfi :275 uri)
  #:use-module (srfi :275 normalize)
  #:use-module (srfi :275 path)
  #:use-module (srfi :275 utils)
  #:re-export (iri non-relative-iri? make-raw-iri
               relative-iri relative-iri? make-raw-relative-iri iri?
               rfc3987-absolute-iri? iri=? iri->string empty-iri
               get-iri get-non-relative-iri get-rfc3987-absolute-iri
               string->iri string->non-relative-iri
               string->rfc3987-absolute-iri iri-scheme update-iri-scheme
               iri-user update-iri-user iri-username+password
               iri-host update-iri-host iri-port update-iri-port
               iri-path update-iri-path iri-query update-iri-query
               iri-fragment update-iri-fragment iri-authority
               update-iri-authority char-set:iri-unreserved
               char-set:iri-private char-set:ucschar char-set:gen-delims
               char-set:sub-delims char-set:reserved char-set:iri
               char-set:scheme char-set:iri-userinfo char-set:iri-reg-name
               char-set:iri-segment char-set:iri-query char-set:iri-fragment
               iri-path-segment-ref iri-path-segment-update
               uri non-relative-uri? make-raw-uri relative-uri
               make-raw-relative-uri relative-uri? uri? uri=? empty-uri
               rfc3986-absolute-uri? uri->string get-uri
               get-non-relative-uri get-rfc3986-absolute-uri string->uri
               string->non-relative-uri string->rfc3986-absolute-uri
               uri-scheme update-uri-scheme uri-user update-uri-user
               uri-username+password uri-host update-uri-host uri-port
               update-uri-port uri-path update-uri-path uri-query
               update-uri-query uri-fragment update-uri-fragment
               uri-authority update-uri-authority char-set:uri-unreserved
               char-set:gen-delims char-set:sub-delims char-set:reserved
               char-set:uri char-set:scheme char-set:uri-userinfo
               char-set:uri-reg-name char-set:uri-segment char-set:uri-query
               char-set:uri-fragment uri-path-segment-ref
               uri-path-segment-update
               normalize-iri-case normalize-iri-escape normalize-iri-path
               normalize-iri normalize-uri-case normalize-uri-escape
               normalize-uri-path normalize-uri resolve-iri resolve-uri
               relativize-iri relativize-uri uri->iri iri->uri
               path? path-segments relative-path empty-path absolute-path
               empty-absolute-path path=? path-length path-ref path-update
               relative-path? absolute-path? empty-path?
               rfc3987-path-abempty? rfc3987-path-absolute?
               rfc3987-path-rootless? rfc3987-path-noscheme?
               path-string? path->string string->path
               string->absolute-path string->relative-path
               list->absolute-path list->relative-path remove-dot-segments
               merge-paths build-path char-set:path
               encode-string decode-string user-settable? host-settable?
               port-settable? path-settable?))

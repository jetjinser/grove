;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;; SPDX-License-Identifier: MIT

(define-module (srfi :275 iri)
  #:use-module (rnrs base)
  #:use-module (rnrs io simple)
  #:use-module ((srfi :275 internal iri)
                #:select (iri non-relative-iri? make-raw-iri
                          relative-iri relative-iri? make-raw-relative-iri
                          iri? iri=? empty-iri rfc3987-absolute-iri?
                          iri->string iri-scheme update-iri-scheme
                          iri-user update-iri-user iri-username+password
                          iri-host update-iri-host iri-port update-iri-port
                          iri-path update-iri-path iri-query update-iri-query
                          iri-fragment update-iri-fragment iri-authority
                          update-iri-authority iri-path-segment-ref
                          iri-path-segment-update))
  #:use-module ((srfi :275 internal iri) #:prefix raw:)
  #:use-module ((srfi :275 internal utils)
                #:select (char-set:iri-unreserved char-set:iri-private
                          char-set:ucschar char-set:gen-delims
                          char-set:sub-delims char-set:reserved char-set:iri
                          char-set:scheme char-set:iri-userinfo
                          char-set:iri-reg-name char-set:iri-segment
                          char-set:iri-query char-set:iri-fragment))
  #:re-export (iri non-relative-iri? make-raw-iri
               relative-iri relative-iri? make-raw-relative-iri
               iri? iri=? empty-iri rfc3987-absolute-iri?
               iri->string iri-scheme update-iri-scheme
               iri-user update-iri-user iri-username+password
               iri-host update-iri-host iri-port update-iri-port
               iri-path update-iri-path iri-query update-iri-query
               iri-fragment update-iri-fragment iri-authority
               update-iri-authority iri-path-segment-ref
               iri-path-segment-update char-set:iri-unreserved
               char-set:iri-private char-set:ucschar char-set:gen-delims
               char-set:sub-delims char-set:reserved char-set:iri
               char-set:scheme char-set:iri-userinfo char-set:iri-reg-name
               char-set:iri-segment char-set:iri-query char-set:iri-fragment)
  #:export (get-iri get-non-relative-iri get-rfc3987-absolute-iri
            string->iri string->non-relative-iri
            string->rfc3987-absolute-iri))

(define (get-iri port . stop-on)
  (raw:get-iri port (if (null? stop-on) (eof-object) (car stop-on)) 'get-iri))

(define (get-non-relative-iri port . stop-on)
  (raw:get-non-relative-iri
   port (if (null? stop-on) (eof-object) (car stop-on)) 'get-non-relative-iri))

(define (get-rfc3987-absolute-iri port . stop-on)
  (raw:get-rfc3987-absolute-iri
   port (if (null? stop-on) (eof-object) (car stop-on))
   'get-rfc3987-absolute-iri))

(define (string->iri value) (raw:string->iri value))
(define (string->non-relative-iri value) (raw:string->non-relative-iri value))
(define (string->rfc3987-absolute-iri value)
  (raw:string->rfc3987-absolute-iri value))

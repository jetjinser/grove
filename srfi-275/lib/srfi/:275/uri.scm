;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;; SPDX-License-Identifier: MIT

(define-module (srfi :275 uri)
  #:use-module (rnrs base)
  #:use-module (rnrs io simple)
  #:use-module ((srfi :275 internal uri)
                #:select (uri non-relative-uri? make-raw-uri
                          relative-uri relative-uri? make-raw-relative-uri
                          uri? uri=? empty-uri rfc3986-absolute-uri?
                          uri->string uri-scheme update-uri-scheme
                          uri-user update-uri-user uri-username+password
                          uri-host update-uri-host uri-port update-uri-port
                          uri-path update-uri-path uri-query update-uri-query
                          uri-fragment update-uri-fragment uri-authority
                          update-uri-authority uri-path-segment-ref
                          uri-path-segment-update))
  #:use-module ((srfi :275 internal utils)
                #:select (char-set:uri-unreserved char-set:gen-delims
                          char-set:sub-delims char-set:reserved
                          char-set:uri char-set:scheme char-set:uri-userinfo
                          char-set:uri-reg-name char-set:uri-segment
                          char-set:uri-query char-set:uri-fragment))
  #:use-module ((srfi :275 internal uri) #:prefix raw:)
  #:re-export (uri non-relative-uri? make-raw-uri
               relative-uri relative-uri? make-raw-relative-uri
               uri? uri=? empty-uri rfc3986-absolute-uri?
               uri->string uri-scheme update-uri-scheme
               uri-user update-uri-user uri-username+password
               uri-host update-uri-host uri-port update-uri-port
               uri-path update-uri-path uri-query update-uri-query
               uri-fragment update-uri-fragment uri-authority
               update-uri-authority char-set:uri-unreserved
               char-set:gen-delims char-set:sub-delims char-set:reserved
               char-set:uri char-set:scheme char-set:uri-userinfo
               char-set:uri-reg-name char-set:uri-segment
               char-set:uri-query char-set:uri-fragment
               uri-path-segment-ref uri-path-segment-update)
  #:export (get-uri get-non-relative-uri get-rfc3986-absolute-uri
            string->uri string->non-relative-uri
            string->rfc3986-absolute-uri))

(define (get-uri port . stop-on)
  (raw:get-uri port (if (null? stop-on) (eof-object) (car stop-on)) 'get-uri))

(define (get-non-relative-uri port . stop-on)
  (raw:get-non-relative-uri
   port (if (null? stop-on) (eof-object) (car stop-on)) 'get-non-relative-uri))

(define (get-rfc3986-absolute-uri port . stop-on)
  (raw:get-rfc3986-absolute-uri
   port (if (null? stop-on) (eof-object) (car stop-on))
   'get-rfc3986-absolute-uri))

(define (string->uri value) (raw:string->uri value))
(define (string->non-relative-uri value) (raw:string->non-relative-uri value))
(define (string->rfc3986-absolute-uri value)
  (raw:string->rfc3986-absolute-uri value))

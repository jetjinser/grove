;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
(define-module (srfi :275 internal utils)
  #:use-module (ice-9 match)
  #:use-module (rnrs base)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 format)
  #:use-module ((srfi srfi-1) #:select (drop fold-right memq))
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:use-module (rnrs arithmetic fixnums)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs conditions)
  #:use-module (rnrs exceptions)
  #:use-module (rnrs io ports)
  #:use-module (rnrs mutable-pairs)
  #:export (compose identity raise-overlong utf8->string/raise
          u8-list->char char->u8-list char->reverse-u8-list
          u8-list->string reverse-u8-list->string bytevector->reverse-u8-list
          reverse-u8-list->bytevector reverse-string->list reverse-list->u16vector u16vector-copy u16vector= u16vector-any u16vector-index u16vector-take u16vector-drop u16vector-empty?
          char-set:alpha char-set:numeric char-set:alphanumeric
          char-set:uri-unreserved char-set:iri-illegal-controls char-set:ucschar
          char-set:iri-unreserved char-set:iri-private char-set:gen-delims
          char-set:sub-delims char-set:reserved char-set:uri char-set:iri
          char-set:uri-pchar-unescaped char-set:iri-pchar-unescaped
          char-set:ipvfuture char-set:scheme char-set:uri-userinfo
          char-set:iri-userinfo char-set:uri-reg-name char-set:iri-reg-name
          char-set:uri-segment char-set:iri-segment char-set:uri-query
          char-set:iri-query char-set:uri-fragment char-set:iri-fragment char-set:full
          shift-percent-encoding unshift-percent-encoding shifted-percent-encoding?
          unshifted-percent-encoding? char-codepoint? percent-encoding->u8-list
          percent-encoding->reverse-u8-list percent-encoding->string append-buffer
          increment-port-position! last-port-position next with-eof stop-here?
          raise-fragment consume-escape consume-escapes consume-ipv4-address
          consume-ipv6-address consume-ipvfuture-address consume-ip-literal
          encode-raw-component decode-raw-component->string escape-utf8-octets
          reverse-escape-utf8-octets string->raw-scheme encode-string decode-string
          split-raw-userinfo split-userinfo list-set! list-index-right))

(define (errorf who fmt . args)
  (scm-error 'misc-error who (apply format #f fmt args) '() #f))
(define (assertion-violationf who fmt . args)
  (assertion-violation who (apply format #f fmt args)))
(define (void) (if #f #f))
(define char-set:full (char-set-complement (char-set)))
(define (reverse-list->string chars) (list->string (reverse chars)))
(define* (u16vector-copy v #:optional (start 0) (end (u16vector-length v)))
  (let ((copy (make-u16vector (- end start))))
    (let loop ((i start) (j 0))
      (when (< i end)
        (u16vector-set! copy j (u16vector-ref v i))
        (loop (+ i 1) (+ j 1))))
    copy))
(define (u16vector= a b)
  (and (= (u16vector-length a) (u16vector-length b))
       (let loop ((i 0))
         (or (= i (u16vector-length a))
             (and (= (u16vector-ref a i) (u16vector-ref b i))
                  (loop (+ i 1)))))))
(define (reverse-list->u16vector xs) (list->u16vector (reverse xs)))
(define (u16vector-empty? v) (zero? (u16vector-length v)))
(define (u16vector-any pred v)
  (let loop ((i 0))
    (and (< i (u16vector-length v))
         (or (pred (u16vector-ref v i)) (loop (+ i 1))))))
(define (u16vector-index pred v)
  (let loop ((i 0))
    (cond ((= i (u16vector-length v)) #f)
          ((pred (u16vector-ref v i)) i)
          (else (loop (+ i 1))))))
(define (u16vector-take v n) (u16vector-copy v 0 n))
(define (u16vector-drop v n) (u16vector-copy v n (u16vector-length v)))
  (define (identity x) x)

  (define (compose f . rest)
    (if (null? rest)
	f
	(let ([g (apply compose rest)])
	  (lambda args
	    (call-with-values (lambda () (apply g args)) f)))))

  (define (char->u8-list ch)
    (guard (ex [(and (who-condition? ex)
                     (eq? (condition-who ex) 'string))
                (assertion-violation 'char->u8-list
                                     "not a valid character"
                                     ch)])
      (bytevector->u8-list
       (string->utf8 (string ch)))))

  (define (char->reverse-u8-list ch)
    (guard (ex [(and (who-condition? ex)
                     (eq? (condition-who ex) 'string))
                (assertion-violation 'char->u8-list
                                     "not a valid character"
                                     ch)])
      (bytevector->reverse-u8-list
       (string->utf8 (string ch)))))

  (define (u8-list->char u8l)
    (guard (ex [(and (who-condition? ex)
                     (memq (condition-who ex) '(u8-list->bytevector fx=?)))
                (assertion-violation 'u8-list->char
                                     "not a valid list of UTF-8 octets for a single char"
                                     u8l)]
               [else
                (raise-continuable ex)])
      (let ([st (u8-list->string u8l)])
        (if (fx=? (string-length st) 1)
            (string-ref st 1)
            (assertion-violation 'u8-list->char
                                 "UTF-8 octets represented multiple characters"
                                 u8l)))))

  #|
     list index setter
  |#
  (define (valid-index? k)
    (and (fixnum? k)
         (not (fxnegative? k))))

  (define (list-set! lst k x)
    (if (not (valid-index? k))
        (assertion-violationf 'list-set! "~s is not a valid index for ~s" k lst)
        (let loop ([pool lst] [j 0])
          (cond [(null? pool)
                 (assertion-violationf 'list-set! "~s is not a valid index for ~s" k lst)]
                [(fx=? j k)
                 (set-car! pool x)]
                [else
                 (loop (cdr pool) (fx+ j 1))]))))

  (define (reverse+length lst)
    (let loop ([lst lst] [k 0] [acc '()])
      (if (null? lst)
          (values acc k)
          (loop (cdr lst) (fx+ k 1) (cons (car lst) acc)))))

  (define (list-index-right pred? lst)
    (let-values ([(rev len) (reverse+length lst)])
      (let loop ([rev rev] [k (fx- len 1)])
        (cond [(null? rev)       #f]
              [(pred? (car rev)) k]
              [else (loop (cdr rev) (fx- k 1))]))))

  #|
     Like utf8->string, but raising an error instead of silently using replacement character
  |#
  (define (raise-overlong rlist who k)
    (guard
     (ex [(and (who-condition? ex)
               (irritants-condition? ex)
               (eq? (condition-who ex) 'bytevector->string))
          (errorf who
                  "invalid UTF-8 encoding %~x in sequence of percent-encodings starting at position ~s"
                  (car (condition-irritants ex))
                  k)]
         [else
          (errorf 'raise-overlong
                  "internal error during conversion of sequence starting at position ~s"
                  k)])
     (let ([bv (reverse-u8-list->bytevector rlist)]
           [tx (make-transcoder (utf-8-codec)
                                (eol-style lf)
                                (error-handling-mode raise))])
       (bytevector->string bv tx))))

  (define (utf8->string/raise bv)
    (guard
     (ex [(and (who-condition? ex)
               (irritants-condition? ex)
               (eq? (condition-who ex) 'bytevector->string))
          (errorf 'utf8->string/raise
                  "invalid UTF-8 encoding %~x in sequence of percent-encodings"
                  (car (condition-irritants ex)))]
         [else
          (raise-continuable ex)])
     (let ([tx (make-transcoder (utf-8-codec)
                                (eol-style lf)
                                (error-handling-mode raise))])
       (bytevector->string bv tx))))


  #|
     convenient conversion procedures which accumulate in reverse
  |#
  (define (bytevector->reverse-u8-list bv)
    (let loop ([k 0] [acc '()])
      (if (fx=? k (bytevector-length bv))
          acc
          (loop (fx+ k 1)
                (cons (bytevector-u8-ref bv k) acc)))))

  (define (reverse-u8-list->bytevector rlist)
    (let* ([width (length rlist)]
           [slots (make-bytevector width)])
      (let loop ([in rlist] [k (fx- width 1)])
        (if (fxnegative? k)
            slots
            (begin (bytevector-u8-set! slots k (car in))
                   (loop (cdr in) (fx- k 1)))))))

  (define (reverse-string->list str)
    (let ([width (string-length str)])
      (let loop ([k 0] [acc '()])
        (if (fx=? k width)
            acc
            (loop (fx+ k 1) (cons (string-ref str k) acc))))))

  (define u8-list->string
    (compose utf8->string u8-list->bytevector))

  (define reverse-u8-list->string
    (compose utf8->string reverse-u8-list->bytevector))

  #|
  |#
  (define char-set:alpha (char-set-intersection char-set:letter char-set:ascii))
  (define char-set:numeric (char-set-intersection char-set:digit char-set:ascii))
  (define char-set:alphanumeric (char-set-union char-set:alpha char-set:numeric))

  (define char-set:uri-unreserved
    (char-set-union char-set:alpha char-set:numeric (char-set #\- #\. #\_ #\~)))

  (define char-set:iri-illegal-controls
    #| Excludes LRM, RLM, LRE, RLE, LRO, RLO, and PDF,
       a.k.a. U+200E LEFT-TO-RIGHT MARK,
              U+200F RIGHT-TO-LEFT MARK,
              U+202A LEFT-TO-RIGHT EMBEDDING,
              U+202B RIGHT-TO-LEFT EMBEDDING,
              U+202C POP DIRECTIONAL FORMATTING,
              U+202D LEFT-TO-RIGHT OVERRIDE,
              U+202E RIGHT-TO-LEFT OVERRIDE;
       as these affect visual presentation but are *impossible* to input
    |#
    (char-set-union (char-set (integer->char 8206) (integer->char 8207))
                    (ucs-range->char-set 8234 8239)))

  (define char-set:ucschar
    (char-set-union
     (ucs-range->char-set 160 55296)      ;; A0 D7FF
     (ucs-range->char-set 63744 64976)    ;; F900 FDCF
     (ucs-range->char-set 65008 65520)    ;; FDF0 FFEF
     (ucs-range->char-set 65536 131070)   ;; 10000 1FFFD
     (ucs-range->char-set 131072 196606)  ;; 20000 2FFFD
     (ucs-range->char-set 196608 262142)  ;; 30000 3FFFD
     (ucs-range->char-set 262144 327678)  ;; 40000 4FFFD
     (ucs-range->char-set 327680 393214)  ;; 50000 5FFFD
     (ucs-range->char-set 393216 458750)  ;; 60000 6FFFD
     (ucs-range->char-set 458752 524286)  ;; 70000 7FFFD
     (ucs-range->char-set 524288 589822)  ;; 80000 8FFFD
     (ucs-range->char-set 589824 655358)  ;; 90000 9FFFD
     (ucs-range->char-set 655360 720894)  ;; A0000 AFFFD
     (ucs-range->char-set 720896 786430)  ;; B0000 BFFFD
     (ucs-range->char-set 786432 851966)  ;; C0000 CFFFD
     (ucs-range->char-set 851968 917502)  ;; D0000 DFFFD
     (ucs-range->char-set 921600 983038)));; E1000 EFFFD

  (define char-set:iri-unreserved
    (char-set-difference
     (char-set-union char-set:uri-unreserved char-set:ucschar)
     char-set:iri-illegal-controls))

  (define char-set:iri-private
    (char-set-union (ucs-range->char-set 57344 63744)
                    (ucs-range->char-set 983040 1048574)
                    (ucs-range->char-set 1048576 1114110)))

  (define char-set:gen-delims (char-set #\: #\/ #\? #\# #\[ #\] #\@))
  (define char-set:sub-delims (char-set #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=))
  (define char-set:reserved (char-set-union char-set:gen-delims char-set:sub-delims))

  (define char-set:uri (char-set-union char-set:reserved char-set:uri-unreserved))
  (define char-set:iri (char-set-union char-set:reserved char-set:iri-unreserved char-set:iri-private))

  (define char-set:uri-pchar-unescaped (char-set-union char-set:uri-unreserved char-set:sub-delims (char-set #\: #\@)))
  (define char-set:iri-pchar-unescaped (char-set-union char-set:iri-unreserved char-set:sub-delims (char-set #\: #\@)))

  ;; The part after `vHHHH.' &c.
  (define char-set:ipvfuture (char-set-union char-set:uri-unreserved char-set:sub-delims (char-set #\:)))

  (define char-set:scheme (char-set-union char-set:alpha char-set:numeric (char-set #\+ #\- #\.)))

  (define char-set:uri-userinfo (char-set-union char-set:uri-unreserved char-set:sub-delims (char-set #\:)))
  (define char-set:iri-userinfo (char-set-union char-set:iri-unreserved char-set:sub-delims (char-set #\:)))

  (define char-set:uri-reg-name (char-set-union char-set:uri-unreserved char-set:sub-delims))
  (define char-set:iri-reg-name (char-set-union char-set:iri-unreserved char-set:sub-delims))

  (define char-set:uri-segment (char-set-copy char-set:uri-pchar-unescaped))
  (define char-set:iri-segment (char-set-copy char-set:iri-pchar-unescaped))

  (define char-set:uri-query (char-set-union char-set:uri-pchar-unescaped (char-set #\/ #\?)))
  (define char-set:iri-query (char-set-union char-set:iri-pchar-unescaped char-set:iri-private (char-set #\/ #\?)))

  (define char-set:uri-fragment (char-set-copy char-set:uri-query))
  (define char-set:iri-fragment (char-set-union char-set:iri-pchar-unescaped (char-set #\/ #\?)))

  #|
     Serialisations of mixed character / percent-encoded parts
  |#
  (define (unshifted-percent-encoding? x)
    (guard (ex [(and (who-condition? ex)
                     (eq? (condition-who ex) 'fx<=?))
                #f]
               [else
                (raise-continuable ex)])
      (fx<=? x 255)))

  (define (shifted-percent-encoding? x)
    (guard (ex [(and (who-condition? ex)
                     (eq? (condition-who ex) 'fx<=?))
                #f]
               [else
                (raise-continuable ex)])
      (fx<=? 256 x 511)))

  (define (char-codepoint? x)
    (unshifted-percent-encoding? x))

  (define (shift-percent-encoding %enc)
    (guard (ex [(and (who-condition? ex)
                     (eq? (condition-who ex) 'fx<=?))
                (assertion-violation 'shift-percent-encoding
                                     "not a fixnum"
                                     %enc)]
               [else
                (raise-continuable ex)])
      (if (fx<=? %enc 255)
          (fx+ %enc 256)
          (assertion-violation 'shift-percent-encoding
                               "not a fixnum between 0 and 255 inclusive"
                               %enc))))

  (define (unshift-percent-encoding %enc)
    (guard (ex [(and (who-condition? ex)
                     (eq? (condition-who ex) 'fx<=?))
                (assertion-violation 'shift-percent-encoding
                                     "not a fixnum"
                                     %enc)]
               [else
                (raise-continuable ex)])
      (if (fx<=? 256 %enc 511)
          (fx- %enc 256)
          (assertion-violation 'unshift-percent-encoding
                               "not a fixnum between 256 and 511 inclusive"
                               %enc))))

  (define (hex-digit n)
    (if (fx<? n 10)
        (fx+ 48 n)
        (fx+ 55 n)))

  (define (percent-encoding->u8-list %enc)
    (call-with-values (lambda () (fxdiv-and-mod %enc 16))
      (lambda (char0 char1)
        (list #x25 (hex-digit char0) (hex-digit char1)))))

  (define (percent-encoding->reverse-u8-list %enc)
    (call-with-values (lambda () (fxdiv-and-mod %enc 16))
      (lambda (char0 char1)
        (list (hex-digit char1) (hex-digit char0) #x25))))

  (define (percent-encoding->string %enc)
    (u8-list->string (percent-encoding->u8-list %enc)))

  (define (append-escape-chars ch acc)
    (guard (ex [(and (who-condition? ex)
                     (eq? (condition-who ex) 'string))
                (assertion-violation 'char->u8-list
                                     "not a valid character"
                                     ch)])
      (let ([octets (char->reverse-u8-list ch)])
        (fold-right
         (lambda (octet prev)
           (append (percent-encoding->reverse-u8-list octet)
                   prev))
         acc
         octets))))

  (define (append-buffer escaped acc who k decode-these)
    (string-fold
     (match-lambda*
       [((? (lambda (c) (char-set-contains? decode-these c)) curr) prev)
        (append (char->reverse-u8-list curr) prev)]
       [(curr prev)
        (append-escape-chars curr prev)])
     acc
     (raise-overlong escaped who k)))

  #|
     The following has two code paths. If no set of characters we're allowed to
     decode are provided, then we avoid grouping percent encodings. This is more
     efficient as there's no conversion or string/list manipulation.

     The utility procedure above has similar behaviour, which helps with prototyping.
  |#
  (define decode-raw-component->string
    (match-lambda*
      [(vec)
       (decode-raw-component->string vec 'decode-raw-component->string #f)]
      [(vec who)
       (decode-raw-component->string vec who #f)]
      [(vec who #f)
       (let loop ([k 0] [acc '()])
         (if (fx=? k (u16vector-length vec))
             (reverse-u8-list->string acc)
             (let ([codepoint (u16vector-ref vec k)])
               (cond
                [(fx<? 255 codepoint 512)
                 (let ([pct (percent-encoding->reverse-u8-list
                             (unshift-percent-encoding codepoint))])
                   (loop (fx+ k 1)
                         (append pct acc)))]
                [(fx<? codepoint 256)
                 (loop (fx+ k 1)
                       (cons codepoint acc))]
                [else
                 (errorf who
                         "octet ~a at position ~a did not represent a character (0-255) or percent-encoding (256-511)"
                         codepoint k)]))))]
      [(vec who decode-these)
       (let loop ([k 0] [escaped '()] [acc '()])
         (if (fx=? k (u16vector-length vec))
             (if (null? escaped)
                 (reverse-u8-list->string acc)
                 (reverse-u8-list->string (append-buffer escaped acc who k decode-these)))
             (let ([codepoint (u16vector-ref vec k)])
               (cond
                [(fx<? 255 codepoint 512)
                 (loop (fx+ k 1)
                       (cons (unshift-percent-encoding codepoint) escaped)
                       acc)]
                [(and (fx<? codepoint 256) (null? escaped))
                 (loop (fx+ k 1)
                       escaped
                       (cons codepoint acc))]
                [(fx<? codepoint 256)
                 (loop (fx+ k 1)
                       '(); clear the buffer
                       (cons codepoint
                             (append-buffer escaped acc who k decode-these)))]
                [else
                 (errorf who
                         "octet ~a at position ~a did not represent a character (0-255) or percent-encoding (256-511)"
                         codepoint k)]))))]))

  (define (escape-utf8-octets char)
    (map shift-percent-encoding (char->u8-list char)))

  (define (reverse-escape-utf8-octets char)
    (map shift-percent-encoding (char->reverse-u8-list char)))

  (define (encode-raw-component str escape-these)
    (list->u16vector
     (string-fold-right
      (match-lambda*
        [(#\% prev)
         (cons* #x25 #x32 #x35 prev)]
        [((? (lambda (c) (char-set-contains? escape-these c)) curr) prev)
         (append (escape-utf8-octets curr) prev)]
        [(curr prev)
         (append (char->u8-list curr) prev)])
      '()
      str)))

  (define string->raw-scheme
    (match-lambda*
      [("" par who)
       (errorf who
               "scheme ~s in identifier ~s was empty"
               "" par)]
      [((? string? str) par who)
       (match (string-ref str 0)
         [(? (lambda (c) (char-set-contains? char-set:alpha c)) ini)
          (let loop ([k 1]
                     [acc `(,(char->integer ini))])
            (if (fx=? k (string-length str))
                (reverse-list->u16vector acc)
                (match (string-ref str k)
                  [(? (lambda (c) (char-set-contains? char-set:scheme c)) curr)
                   (loop (fx+ k 1) (cons (char->integer curr) acc))]
                  [char
                   (errorf who
                           "char ~s at position ~s cannot be contained within scheme for identifier ~s"
                           char k par)])))]
         [ini
          (errorf who
                  "char ~s at position ~s cannot be contained within scheme for identifier ~s"
                  ini 0 par)])]
      [(nonstr _ who)
       (assertion-violationf who "scheme ~s was not a string" nonstr)]))

  (define encode-string
    (let ([default-esc (char-set-complement char-set:iri-unreserved)])
      (match-lambda*
        [(str)
         (encode-string str default-esc)]
        [((? string? str) (? char-set? escape-these))
         (decode-raw-component->string (encode-raw-component str escape-these) 'encode-string #f)]
        [(nonstr (? char-set?))
         (assertion-violationf 'encode-string "~s is not a string" nonstr)]
        [(_ noncset)
         (assertion-violationf 'encode-string "~s is not a character set" noncset)])))

  (define decode-string
    (match-lambda*
     [(str)
      (decode-string str char-set:full)]
     [((? string? str) (? char-set? decode-these))
      (let ([port (open-string-input-port str)])
        (let loop ([acc '()])
          (match (get-char port)
            [(? eof-object?)
             (decode-raw-component->string (reverse-list->u16vector acc)
                                           'decode-string
                                           decode-these)]
            [#\%
             (loop (append
                    (consume-escapes port 'string->uri-raw-user)
                    acc))]
            [char
             (loop (append (char->reverse-u8-list char) acc))])))]
     [(nonstr (? char-set?))
      (assertion-violationf 'decode-string "~s is not a string" nonstr)]
     [(_ noncset)
      (assertion-violationf 'decode-string "~s is not a character set" noncset)]))

  #|
     port-manipulation procedures and syntax
  |#
  (define (increment-port-position! port incr)
    (let ([new-position
           (fx+ (port-position port) incr)])
      (set-port-position! port new-position)))

  (define last-port-position
    (case-lambda
     [(port) (fx- (port-position port) 1)]
     [(port decr) (fx- (port-position port) decr)]))

  (define-syntax next
    (syntax-rules ()
      [(_ port body ...)
       (begin
         (increment-port-position! port 1)
         body ...)]
      [(_ (port incr) body ...)
       (begin
         (increment-port-position! port incr)
         body ...)]))

  (define-syntax with-eof
    (syntax-rules ()
      [(_ port stop-on body0 body* ...)
       (if (eof-object? stop-on)
           (begin
             body0 body* ...)
           (begin
             (set-port-position! port (last-port-position port))
             body0 body* ...))]))

  (define (stop-here? stop-on)
    (lambda (char/eof)
      (eq? char/eof stop-on)))

  (define (raise-fragment port who)
    (errorf who
            "char ~s indicated fragment at position ~s"
            #\# (last-port-position port)))

  #|
     Consume a percent encoding, or multiple.

     For this internal procedure, an additional procedure is required which
     checks whether a given sequence is valid. This is the mechanism by which
     overlong UTF-8 sequences are detected, _but_ this may be too strict, or
     unnecessary for non-UTF-8 encodings.
  |#
  (define (consume-escapes port who)
    (let ([start (last-port-position port)])
      (let loop ([acc '()])
        (let ([acc* (cons (consume-escape port) acc)])
          (match (lookahead-char port)
            [#\%
             (next port (loop acc*))]
            [char
             (guard
              (ex [(and (who-condition? ex)
                        (irritants-condition? ex)
                        (eq? (condition-who ex) 'bytevector->string))
                   (errorf 'get-uri
                           "invalid escape %~x in string of escapes after position ~a"
                           (car (condition-irritants ex))
                           start)])
              (raise-overlong acc* who start)
              (map shift-percent-encoding acc*))])))))

  (define (consume-escape port)
    (match (get-char port)
      [(? (lambda (c) (char-set-contains? char-set:hex-digit c)) char0)
       (match (get-char port)
         [(? (lambda (c) (char-set-contains? char-set:hex-digit c)) char1)
          (string->number (string char0 char1)
                          16)]
         [nonesc
          (errorf 'get-iri
                  "non-hexadecimal digit ~a in percent-encoding starting at ~a"
                  nonesc (last-port-position port 2))])]
      [nonesc
       (errorf 'get-iri
               "non-hexadecimal digit ~a in percent-encoding starting at ~a"
               nonesc (last-port-position port))]))

  #|
     IP addresses
  |#
  (define (dec-octet tally)
    (string->number (reverse-list->string tally)))

   (define (valid-dec-octet? tally)
     (let ([octet (dec-octet tally)])
       (and octet
            (fx<=? octet 255))))

   (define (consume-ipv4-address port acc stop-on who)
     (let loop ([octet 0] [k 0] [tally '()] [acc acc])
       (match (lookahead-char port)
         [(? (lambda (c) (char-set-contains? char-set:numeric c)) char)
          (if (fx<? k 3)
              (next port
                    (loop octet (fx+ k 1)
                          (cons char tally) (cons (char->integer char) acc)))
              (errorf who
                      "digit ~s at position ~s elicited IPv4 address with greater than three digits"
                      char (last-port-position port)))]
         [#\.
          (if (valid-dec-octet? tally)
              (next port
                    (loop (fx+ octet 1) 0 '() (cons #x2E acc)))
              (errorf who
                      "last digit ~s at position ~s elicited IPv4 address octet greater than 255"
                      (car tally) (last-port-position port)))]
         [char
          (cond [(and (fx=? octet 3) (valid-dec-octet? tally))
                 acc]
                [(and (fx=? octet 3) (not (null? tally)))
                 (errorf who
                         "last digit ~s at position ~s elicited IPv4 address octet greater than 255"
                         (car tally) (last-port-position port))]
                [((stop-here? stop-on) char)
                 (errorf who
                         "EOF object ~s indicated premature end to IPv4 address at position ~s"
                         char (last-port-position port))]
                [else
                 (errorf who
                         "char ~s at position ~s is not permissible within IPv4 address octet"
                         char (port-position port))])])))

   ;; raise-eof
   (define (raise-ipv6-premature-end port char who)
     (errorf who "EOF object ~s at position ~s implied premature end to IPv6 address" char (last-port-position port)))

   ;; raise-v4
   (define (raise-ipv6-ls32 port char who)
     (errorf who "digit ~s at position ~s elicited IPv6 LS32 v4 address with greater than three digits" char (last-port-position port)))

   ;; raise-n
   (define (raise-ipv6-n port prev-char who)
     (errorf who "char ~s at position ~s elicited more than 8 IPv6 address octets" prev-char (last-port-position port)))

   ;; raise-five
   (define (raise-ipv6-segment port char who)
     (errorf who "char ~s at position ~s constitutes more than four hexadecimal chars per segment" char (last-port-position port)))

   ;; raise-some
   (define (raise-ipv6-expect-end port char who)
     (errorf who "got char ~s but expected `]' at position ~s" char (last-port-position port)))

   ;; raise-hex
   (define (raise-ipv6-nonhex port char who)
     (errorf who "char ~s at position ~s is not hexadecimal" char (last-port-position port)))


   (define (consume-ipv6-address port acc stop-on who)
     (define (loop-ipv4-ls32 octet k tally acc)
       (match (lookahead-char port)
         [(? (lambda (c) (char-set-contains? char-set:numeric c)) char)
          (if (fx<? k 3)
              (next port
                    (loop-ipv4-ls32 octet (fx+ k 1)
                                    (cons char tally) (cons (char->integer char) acc)))
              (raise-ipv6-ls32 port char who))]
         [#\.
          (if (valid-dec-octet? tally)
              (next port (loop-ipv4-ls32 (fx+ octet 1) 0 '() (cons #x2E acc)))
              acc)]
         [char
          (cond [(and (fx=? octet 3) (valid-dec-octet? tally))  acc]
                [(and (fx=? octet 3) (not (null? tally))) (raise-ipv6-ls32 port char who)]
                [else (raise-ipv6-premature-end port char who)])]))
     (define (loop-post:: octet k acc)
       (if (fx>? octet 8)
           (raise-ipv6-n port (car acc) who)
           (match (get-char port)
             [#\]
              (if (fx>? octet 8) (raise-ipv6-n port (car acc) octet) (cons #x5D acc))]
             [(? (lambda (c) (char-set-contains? char-set:hex-digit c)) char)
              (if (fx=? k 4)
                  (raise-ipv6-segment port char who)
                  (if (fxzero? octet)
                      (loop-post:: (fx+ octet 1)
                                   (fx+ k 1) (cons (char->integer char) acc))
                      (loop-post:: octet
                                   (fx+ k 1) (cons (char->integer char) acc))))]
             [#\:
              (if (or (fx>? k 0) (fxzero? octet))
                  (match (lookahead-char port)
                    [#\:
                     (errorf who
                             "extreunous :: at position ~a"
                             (last-port-position port))]
                    [(? (lambda (c) (char-set-contains? char-set:hex-digit c)) char)
                     (loop-post:: (fx+ octet 1) 0 (cons #x3A acc))]
                    [(? (stop-here? stop-on))
                     (raise-ipv6-premature-end port stop-on who)]
                    [char
                     (raise-ipv6-nonhex port char who)])
                  (errorf 'get-uri
                          "expected at least one character per segment"))]
             [#\.
              (cond [(and (fx<=? octet 7); differs from pre-`::'
                          (fx<=? 1 k 3))
                     (set-port-position! port (fx- (port-position port) (fx+ 1 k)))
                     (let ([acc (loop-ipv4-ls32 0 0 '() (drop acc k))])
                       (match (get-char port)
                         [#\]  (cons #x5D acc)]
                         [char (raise-ipv6-expect-end port char who)]))]
                    [else
                     (errorf who
                             "char ~a at position ~a is not valid within first six IPv6 segments"
                             #\. (last-port-position port))])]
             [(? (stop-here? stop-on))
              (raise-ipv6-premature-end port stop-on who)]
             [char
              (raise-ipv6-nonhex port char who)])))
       (define (loop-pre:: octet k acc)
         (if (fx>? octet 8)
             (raise-ipv6-n port (car acc) who)
             (match (get-char port)
               [#\]
                (if (fx=? octet 8) (cons #x5D acc) (raise-ipv6-premature-end port #\] who))]
               [(? (lambda (c) (char-set-contains? char-set:hex-digit c)) char)
                (if (fx=? k 4)
                    (raise-ipv6-segment port char who)
                    (if (fxzero? octet)
                        (loop-pre:: (fx+ octet 1)
                                    (fx+ k 1) (cons (char->integer char) acc))
                        (loop-pre:: octet
                                    (fx+ k 1) (cons (char->integer char) acc))))]
               [#\:
                (if (or (fx>? k 0) (fxzero? octet))
                    (match (lookahead-char port)
                      [#\:
                       (next port
                             (loop-post:: octet 0 (cons* #x3A #x3A acc)))]
                      [(? (lambda (c) (char-set-contains? char-set:hex-digit c)))
                       (loop-pre:: (fx+ octet 1) 0 (cons #x3A acc))]
                      [(? (stop-here? stop-on))
                       (raise-ipv6-premature-end port stop-on who)]
                      [char
                       (raise-ipv6-nonhex port char who)])
                    (errorf 'get-uri
                            "expected at least one character per segment"))]
               [#\.
                (cond [(and (fx=? octet 7) (fx<=? 1 k 3))
                       (set-port-position! port (fx- (port-position port) (fx+ 1 k)))
                       (let ([acc (loop-ipv4-ls32 0 0 '() (drop acc k))])
                         (match (get-char port)
                           [#\]  (cons #x5D acc)]
                           [char (raise-ipv6-expect-end port char who)]))]
                      [else
                       (errorf 'get-uri
                               "char ~a at position ~a is not valid within first six IPv6 segments"
                               #\. (last-port-position port))])]
               [(? (stop-here? stop-on))
                (raise-ipv6-premature-end port stop-on who)]
               [char
                (raise-ipv6-nonhex port char who)])))
       ;;
       (loop-pre:: 0 0 acc))

   (define (consume-ipvfuture-address port acc stop-on who)
     (define (raise-eof c)
       (errorf who "EOF object ~s at position ~s implied premature end to IPvFuture address" c (last-port-position port)))
     (define (raise-char c lab)
       (errorf 'get-uri
               (format #f "char ~~s at position ~~s is not permissible within IPvFuture ~a portion" lab)
               c (last-port-position port)))
     (define (raise-n lab)
       (errorf 'get-uri
               (format #f "char ~~s at position ~~s indicated empty IPvFuture ~a portion" lab)
               #\. (last-port-position port)))
     ;;
     (define (loop-rest k acc)
       (match (get-char port)
         [#\]
          (if (fxzero? k)
              (raise-n "address")
              (cons #x5D acc))]
         [(? (lambda (c) (char-set-contains? char-set:ipvfuture c)) char)
          (loop-rest (fx+ k 1) (cons (char->integer char) acc))]
         [(? (stop-here? stop-on))
          (raise-eof stop-on)]
         [char (raise-char char "address")]))
     (define (loop-vsn k acc)
       (match (get-char port)
         [#\.
          (if (fxzero? k)
              (raise-n "version")
              (loop-rest 0 (cons #x2E acc)))]
         [(? (lambda (c) (char-set-contains? char-set:hex-digit c)) char)
          (loop-vsn (fx+ k 1) (cons (char->integer char) acc))]
         [(and (or #\] (? (stop-here? stop-on))) curr)
          (raise-eof curr)]
         [char (raise-char char "version")]))
     (loop-vsn 0 acc))

   (define (consume-ip-literal port stop-on who)
     (match (lookahead-char port)
       [#\]
        (errorf who
                "closing bracket ~s at position ~s implied empty IP literal"
                #\] (port-position port))]
       [#\v
        (next port
          (reverse-list->u16vector (consume-ipvfuture-address port '(#x76 #x5B) stop-on who)))]
       [(or #\: (? (lambda (c) (char-set-contains? char-set:hex-digit c))))
        (reverse-list->u16vector (consume-ipv6-address port '(#x5B) stop-on who))]

       [(? (stop-here? stop-on))
        (errorf who
                "EOF object ~s at position ~s indicated premature end"
                stop-on (port-position port))]
       [char
        (errorf who
                "char ~a at position ~a cannot be contained within IP literal"
                char (port-position port))]))

   (define (split-raw-userinfo seq who)
     (if (not seq)
         (values #f #f)
         (let ([colon@ (u16vector-index (lambda (k) (fx=? k #x3A)) seq)])
           (if (not colon@)
               (values (decode-raw-component->string seq who #f) #f)
               (values (decode-raw-component->string (u16vector-take seq colon@) who #f)
                       (decode-raw-component->string (u16vector-drop seq (fx+ colon@ 1)) who #f))))))

   (define split-userinfo
     (match-lambda*
       [("")
        (values #f #f)]
       [((? string? str))
        (let ([colon@ (string-index str (lambda (c) (char=? c #\:)))])
          (if (not colon@)
              (values (string-copy str) #f)
              (values (string-take str colon@)
                      (string-drop str (fx+ colon@ 1)))))]
       [(nonstr)
        (assertion-violationf 'split-userinfo "~s is not a string" nonstr)]))

  ;;;

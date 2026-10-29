;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;; SPDX-License-Identifier: MIT
;;
;; Guile port of the SRFI 275 path substrate.
(define-module (srfi :275 internal path)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module ((srfi srfi-1) #:select (take drop list=))
  #:use-module ((srfi srfi-13) #:select (string-null? string-every string-join))
  #:use-module (srfi srfi-4)
  #:use-module ((srfi srfi-14) #:select (char-set char-set-complement char-set-union char-set-contains?))
  #:use-module (rnrs records syntactic)
  #:use-module (rnrs base)
  #:use-module (rnrs arithmetic fixnums)
  #:use-module (rnrs conditions)
  #:use-module (rnrs exceptions)
  #:use-module (rnrs io ports)
  #:use-module (rnrs mutable-pairs)
  #:use-module ((srfi :275 internal utils) #:hide (char-set:full list-set!))
  #:export (path? path-raw-segments path-segments
            relative-path make-raw-relative-path empty-path
            absolute-path make-raw-absolute-path empty-absolute-path
            path-string? copy-path path=? path-length path-ref path-set! path-update
            relative-path? absolute-path? empty-path?
            rfc3987-path-abempty? rfc3987-path-absolute?
            rfc3987-path-rootless? rfc3987-path-noscheme?
            path->string string->path string->absolute-path string->relative-path
            list->absolute-path list->relative-path remove-dot-segments merge-paths
            build-path char-set:path string->raw-segment
            current-wd parent-wd current-wd? parent-wd? pseudo-absolute-path?
            empty-initial-segment?))

(define (assertion-violationf who template . args)
  (assertion-violation who (apply format #f template args)))
(define (errorf who template . args)
  (error who (apply format #f template args)))
(define list-take take)
(define (path-list-set! lst k x)
  (if (or (not (fixnum? k)) (fxnegative? k))
      (assertion-violation 'list-set! "invalid list index" k lst)
      (let loop ((rest lst) (i 0))
        (cond ((null? rest)
               (assertion-violation 'list-set! "invalid list index" k lst))
              ((fx=? i k) (set-car! rest x))
              (else (loop (cdr rest) (fx+ i 1)))))))

;; Guile's opaque R6RS records compare structurally under equal?, so the
;; Chez-specific record-type-equal-procedure registrations are unnecessary.
  (define-record-type path
    (fields (mutable raw-segments path-raw-segments set-path-raw-segments!))
    (sealed #f) (opaque #t))

  (define-record-type (relative-path make-raw-relative-path relative-path?)
    (parent path) (sealed #t) (opaque #t))

  (define-record-type (absolute-path make-raw-absolute-path absolute-path?)
    (parent path) (sealed #t) (opaque #t))

  (define (empty-path) (make-raw-relative-path '()))
  (define (empty-absolute-path) (make-raw-absolute-path '()))

  (define on-who?
    (case-lambda
     [(ex who)
      (and (who-condition? ex)
           (eq? (condition-who ex) who))]
     [(ex who . who*)
      (and (who-condition? ex)
           (memq (condition-who ex) (cons who who*)))]))

  (define (path-ref seq k)
    (guard
     (ex [(on-who? ex 'list-ref)
          (assertion-violationf 'path-ref "~s is not a valid index for ~s" k seq)]
         [else
          (raise-continuable ex)])
     (if (path? seq)
         (let ([raw-segment (list-ref (path-raw-segments seq) k)])
           (decode-raw-component->string raw-segment 'path-ref #f))
         (assertion-violationf 'path-ref
                               "~s is not a path" seq))))

  (define (%path-set! seq k x who)
    (guard
     (ex [(on-who? ex 'list-set!)
          (apply assertion-violationf
                 who
                 (condition-message ex)
                 (condition-irritants ex))]
         [else
          (raise-continuable ex)])
     (if (path? seq)
         (let ([raw-segment
                (string->raw-segment x #f 'path-set! seq k)])
           (path-list-set! (path-raw-segments seq)
                           k raw-segment))
         (assertion-violationf 'path-set!
                               "~s is not a path" seq))))

  (define (path-set! seq k x)
    (%path-set! seq k x 'path-set!))

  (define (path-update seq k x)
    (if (not (path? seq))
        (assertion-violationf 'path-update "~s is not a path" seq)
        (let ([tgt (copy-path seq)])
          (%path-set! tgt k x 'path-update)
          tgt)))

  ;; Raw segments are u16vectors: literal octets use 0..255, while percent-
  ;; encoded octets use 256..511. Copy each vector so persistent path-update
  ;; results cannot alias mutable segment storage in the original path.
  (define (copy-path path)
    (if (absolute-path? path)
        (make-raw-absolute-path (map u16vector-copy (path-raw-segments path)))
        (make-raw-relative-path (map u16vector-copy (path-raw-segments path)))))

  (define (path-length seq)
    (if (path? seq)
        (length (path-raw-segments seq))
        (assertion-violationf 'path-length "~s is not a path" seq)))

  (define (%path=? A B)
    (cond [(not (path? A))
           (assertion-violationf 'path=? "~s is not a path" A)]
          [(not (path? B))
           (assertion-violationf 'path=? "~s is not a path" B)]
          [else
           (and (eq? (absolute-path? A) (absolute-path? B))
                (list=
                 u16vector=
                 (path-raw-segments A)
                 (path-raw-segments B)))]))

  (define path=?
    (case-lambda
     [(path1 path2)
      (%path=? path1 path2)]
     [(path1 . path*)
      (let loop ([curr path1] [pool path*])
        (match pool
          ['() #t]
          [(next . rest)
           (and (%path=? curr next)
                (loop next rest))]))]))

  (define char-set:path
    (char-set-union char-set:uri char-set:ucschar))

  #|
     Like the Racket procedure, but we generalise the check from non-null to
     char-set:path, which is the union of char-set:uri and char-set:ucschar.
  |#

  (define (path-string? path)
    (or (path? path)
        (and (string? path)
             (string-every (lambda (char)
                             (char-set-contains? char-set:path char))
                           path))))

  #|
     Helper procedures for path shape
  |#

  (define (pseudo-absolute-path? path)
    (and (relative-path? path)
         (empty-initial-segment? (path-raw-segments path))))

  (define (empty-initial-segment? seq)
    (and (not (null? seq))
         (u16vector-empty? (car seq))))

  (define (empty-path? x)
    (and (relative-path? x)
         (null? (path-raw-segments x))))

  (define (rfc3987-path-abempty? x)
    (or (absolute-path? x)
        (empty-path? x)))

  (define (rfc3987-path-absolute? x)
    (and (absolute-path? x)
         (not (null? (path-raw-segments x)))
         (not (u16vector-empty?
               (car (path-raw-segments x))))))

  (define (rfc3987-path-rootless? x)
    (and (relative-path? x)
         (not (null? (path-raw-segments x)))
         (not (u16vector-empty?
               (car (path-raw-segments x))))))

  (define (rfc3987-path-noscheme? x)
    (and (relative-path? x)
         (not (null? (path-raw-segments x)))
         (not (u16vector-any
               (lambda (x) (fx=? x #x3A))
               (car (path-raw-segments x))))))

  #|
     String conversion
  |#
  (define (path->string path)
    (if (not (path? path))
        (assertion-violationf 'path->string "~s is not a path" path)
        (let ([segments
               (map (lambda (segment)
                      (decode-raw-component->string segment 'path->string #f))
                    (path-raw-segments path))])
          (cond
           [(relative-path? path)
            (string-join segments "/")]
           [(and (absolute-path? path)
                 (not (null? segments)))
            (string-join (cons "" segments) "/")]
           [else
            "/"]))))

  (define (path-segments path)
    (if (not (path? path))
        (assertion-violationf 'path-segments "~s is not a path" path)
        (map (lambda (segment)
               (decode-raw-component->string segment 'path-segments #f))
             (path-raw-segments path))))

  #|
     The four types of path are:

     a) path-abempty:    absolute path
     b) path-absolute:   absolute path with non-empty initial segment
     c) path-rootless:   relative path
     d) path-noscheme:   relative path with no colon in first segment

     We have an additional boolean flag in this low-level conversion
     procedure which says that we're in the path-noscheme production.
  |#
  (define string->raw-segment
    (match-lambda*
      [("" _ _ _ _)
       (u16vector)]
      [((? string? str) #f _ _ _)
       (let ([port (open-string-input-port str)])
         (let loop ([acc '()])
           (match (get-char port)
             [(? eof-object?)
              (reverse-list->u16vector acc)]
             [#\%  (loop (append (consume-escapes port 'string->uri-raw-path) acc))]
             [#\/  (loop (cons 303 acc))]
           [(? (lambda (char) (char-set-contains? char-set:reserved char)) char)
              (loop (cons (char->integer char) acc))]
             [(? (lambda (char) (char-set-contains? char-set:uri-unreserved char)) char)
              (loop (cons (char->integer char) acc))]
             [(? (lambda (char) (char-set-contains? char-set:ucschar char)) char)
              (loop (append (char->reverse-u8-list char) acc))]
             [char
              (loop (append (reverse-escape-utf8-octets char) acc))])))]
      [((? string? str) escape-these _ _ _)
       (let ([port (open-string-input-port str)])
         (let loop ([acc '()])
           (match (get-char port)
             [(? eof-object?)
              (reverse-list->u16vector acc)]
             [#\%  (loop (append (consume-escapes port 'string->uri-raw-path) acc))]
             [#\/  (loop (cons 303 acc))]
             [(? (lambda (char) (char-set-contains? escape-these char)) char)
              (loop (append (reverse-escape-utf8-octets char) acc))]
             [(? (lambda (char) (char-set-contains? char-set:reserved char)) char)
              (loop (cons (char->integer char) acc))]
             [(? (lambda (char) (char-set-contains? char-set:uri-unreserved char)) char)
              (loop (cons (char->integer char) acc))]
             [(? (lambda (char) (char-set-contains? char-set:ucschar char)) char)
              (loop (append (char->reverse-u8-list char) acc))]
             [char
              (loop (append (reverse-escape-utf8-octets char) acc))])))]
      [(nonstr _ who par idx)
       (assertion-violationf who
                             "~s, segment ~s of ~s, is not a string"
                             nonstr idx par)]))

  (define (consume-segments port acc who mkpath)
    (let loop ([rsegment '()] [acc acc])
      (match (get-char port)
        [(? eof-object?)
         (mkpath
          (reverse
           (cons (reverse-list->u16vector rsegment)
                 acc)))]
        [#\%
         (loop (append (consume-escapes port who) rsegment) acc)]
        [#\/
         (loop '() (cons (reverse-list->u16vector rsegment) acc))]
        [(? (lambda (char) (char-set-contains? char-set:reserved char)) char)
         (loop (cons (char->integer char) rsegment) acc)]
        [(? (lambda (char) (char-set-contains? char-set:uri-unreserved char)) char)
         (loop (cons (char->integer char) rsegment) acc)]
        [#\@ (loop (cons #x40 rsegment) acc)]
        [(? (lambda (char) (char-set-contains? char-set:ucschar char)) char)
         (loop (append (char->reverse-u8-list char) rsegment) acc)]
        [char
         (loop (append (reverse-escape-utf8-octets char) rsegment) acc)])))

  #|
     This variant throws an error if we land on a colon, since the
     path's first segment could otherwise be mistaken for scheme.
  |#
  (define (consume-segments/noscheme port who)
    (let loop ([rsegment '()] [acc '()])
      (match (get-char port)
        [(? eof-object?)
         (make-raw-relative-path
          (reverse
           (cons (reverse-list->u16vector rsegment)
                 acc)))]
        [#\%
         (loop (append (consume-escapes port who) rsegment) acc)]
        [#\/
         (consume-segments port
                           (cons (reverse-list->u16vector rsegment) acc)
                           who
                           make-raw-relative-path)]
        [#\:
         #| Check this after whether to escape, because if it's in the
            clear, then we need to prefix the path with dot+slash |#
         (loop (cons #x3A rsegment) (list (u16vector #x2E)))]
        [(? (lambda (char) (char-set-contains? char-set:reserved char)) char)
         (loop (cons (char->integer char) rsegment) acc)]
        [(? (lambda (char) (char-set-contains? char-set:uri-unreserved char)) char)
         (loop (cons (char->integer char) rsegment) acc)]
        [(? (lambda (char) (char-set-contains? char-set:ucschar char)) char)
         (loop (append (char->reverse-u8-list char) rsegment) acc)]
        [char
         (loop (append (reverse-escape-utf8-octets char) rsegment) acc)])))

  (define string->absolute-path
    (match-lambda*
      [(str)
       (string->absolute-path str #f)]
      [("" _)
       (error 'string->absolute-path "string was empty")]
      [((? string? str) nonempty-ini?)
       (let ([port (open-string-input-port str)])
         (match (get-char port)
           [#\/
            (if (and nonempty-ini? (equal? (lookahead-char port) #\/))
                (errorf 'string->absolute-path
                        "initial segment of ~s was empty"
                        str)
                (consume-segments port
                                  '()
                                  'string->absolute-path make-raw-absolute-path))]
           [char
            (errorf 'string->absolute-path
                    "~s did not start with slash"
                    char)]))]
      [(nonstr _ _)
       (assertion-violationf 'string->absolute-path "~a is not a string" nonstr)]))

  (define string->relative-path
    (match-lambda*
      [(str)
       (string->relative-path str #f)]
      [("" _)
       (error 'string->absolute-path "string was empty")]
      [((? string? str) #f)
       (let ([port (open-string-input-port str)])
         (match (lookahead-char port)
           [#\/
            (errorf 'string->relative-path
                    "~s started with a slash"
                    str)]
           [char
            (consume-segments port '()
                              'string->relative-path make-raw-relative-path)]))]
      [((? string? str) noscheme?)
       (let ([port (open-string-input-port str)])
         (match (lookahead-char port)
           [#\/
            (errorf 'string->relative-path
                    "~s started with a slash"
                    str)]
           [char
            (consume-segments/noscheme port 'string->relative-path)]))]
      [(nonstr _)
       (assertion-violationf 'string->relative-path "~a is not a string" nonstr)]))

  (define string->path
    (match-lambda*
      [("")
       (make-raw-relative-path '())]
      [((? string? str))
       (if (char=? (string-ref str 0) #\/)
           (string->absolute-path str)
           (string->relative-path str))]
      [(nonstr)
       (assertion-violationf 'string->path "~s is not a string" nonstr)]))

  (define %list->absolute-path
    (match-lambda*
      [('() _ _)
       (make-raw-absolute-path '())]
      [((? list? segments) nonempty-init? who)
       (cond
        [(not (string? (car segments)))
         (errorf who
                 "segment ~s at position ~s of ~s is not a string"
                 (car segments) 0 segments)]
        [(and nonempty-init?
              (string-null? (car segments)))
         (errorf who
                 "segment ~s at position ~s of ~s was empty"
                 (car segments) 0 segments)]
        [else
         (let loop ([pool segments] [k 0] [acc '()])
           (if (null? pool)
               (make-raw-absolute-path (reverse acc))
               (let ([segment
                      (string->raw-segment (car pool)
                                           #f who segments k)])
                 (loop (cdr pool) (fx+ k 1) (cons segment acc)))))])]
      [(nonlist _ who)
       (assertion-violationf who
                             "~s is not a proper list of string segments"
                             nonlist)]))

  (define list->absolute-path
    (case-lambda
     [(lst)
      (%list->absolute-path lst #f 'list->absolute-path)]
     [(lst nonempty-init?)
      (%list->absolute-path lst nonempty-init? 'list->absolute-path)]))

  (define %list->relative-path
    (match-lambda*
      [('() _ _)
       (make-raw-relative-path '())]
      [((? list? segments) noscheme? who)
       (define ini
         (string->raw-segment (car segments)
                              #f
                              who segments 0))
       (if (u16vector-empty? ini)
           (errorf who
                   "segment ~s at position ~s of ~s was empty"
                   (car segments) 0 segments)
           (let loop ([pool segments] [k 0] [acc '()])
             (if (null? pool)
                 (if (and noscheme? (u16vector-any (lambda (x) (fx=? x #x3A)) ini))
                     (make-raw-relative-path
                      (cons (u16vector #x2E) (reverse acc)))
                     (make-raw-relative-path
                      (reverse acc)))
                 (let ([segment
                        (string->raw-segment (car pool)
                                             #f who segments k)])
                   (loop (cdr pool) (fx+ k 1) (cons segment acc))))))]
      [(nonlist _ who)
       (assertion-violationf who
                             "~s is not a proper list of string segments"
                             nonlist)]))

  (define list->relative-path
    (case-lambda
     [(lst)
      (%list->relative-path lst #f 'list->relative-path)]
     [(lst noscheme?)
      (%list->relative-path lst noscheme? 'list->relative-path)]))

  #|
     No nice way to vary the character set in a variadic procedure like this,
     so the URI and IRI-specific versions of these are preferable, or the
     above `vector->*-path' procedures.

     Procedure input is similar to the following Racket procedure:

     https://docs.racket-lang.org/reference/Manipulating_Paths.html#%28def._%28%28quote._~23~25kernel%29._build-path%29%29

     However, the `base' initial argument, if it's a path, is merged using
     the above `merge-paths' procedure, so that it's similar to how relative
     references are resolved against a base.
  |#
  (define build-path
    (let ([shim-relative-path
           (lambda (segments)
             (make-raw-relative-path
              (path-raw-segments
               (%list->absolute-path segments
                                     #f 'build-path))))])
      (case-lambda
       [()
        (make-raw-relative-path '())]
       [(base . segments)
        (cond
         [(empty-path? base)
          (%list->relative-path segments #f 'build-path)]
         [(path? base)
          (merge-paths base (shim-relative-path segments) #f)]
         [(string? base)
          (%list->relative-path (cons base segments) #f 'build-path)]
         [else
          (assertion-violationf 'build-path
                                "initial segment ~s is not a path or string segment" base)])])))
  #|
     Dot segments removal
  |#
  (define (remove-dot-segments path)
    (if (not (path? path))
        (assertion-violationf 'remove-dot-segments
                              "~s is not a path" path)
        (let ([undotted
               (remove-dot-segments* (path-raw-segments path))])
          (match undotted
            ['()
             (if (relative-path? path)
                 (make-raw-relative-path '())
                 (make-raw-absolute-path '()))]
            [(head)
             (cond [(and (relative-path? path)
                         (u16vector-empty? head))
                    (make-raw-relative-path '())]
                   [(relative-path? path)
                    (make-raw-relative-path `(,head))]
                   [else
                    (make-raw-absolute-path `(,head))])]
            [(head . rest)
             (cond [(absolute-path? path)
                    (make-raw-absolute-path undotted)]
                   [(and (relative-path? path)
                         (u16vector-empty? head))
                    (make-raw-absolute-path rest)]
                   [else
                    (make-raw-relative-path undotted)])]))))

  (define (current-wd) (u16vector #x2E))
  (define (parent-wd) (u16vector #x2E #x2E))

  (define (current-wd? seq) (u16vector= seq (current-wd)))
  (define (parent-wd? seq) (u16vector= seq (parent-wd)))

  (define (remove-dot-segments* segments)
    (let loop ([segments segments]
               [trailing-slash? #f]
               [acc '()])
      (match segments
        ['()
         (if trailing-slash?
             (reverse (cons (u16vector) acc))
             (reverse acc))]
        [((? current-wd?) . rest)
         (loop rest #t acc)]
        [((? parent-wd?) . rest)
         (loop rest #t (if (pair? acc) (cdr acc) acc))]
        [(x . rest)
         (loop rest #f (cons x acc))])))

  (define (base-path-last seq)
    (if (and (absolute-path? seq)
             (null? (path-raw-segments seq)))
        #| An empty absolute path still has a leading slash, that is there
           really is a slash at position 0, unlike for relative paths: |#
        0
        (list-index-right u16vector?
                          (path-raw-segments seq))))

  (define merge-paths
    (case-lambda
     [(base-path ref-path)
      (merge-paths base-path ref-path #f)]
     [(base-path ref-path base-has-auth?)
      (cond
       [(not (path? ref-path))
        (assertion-violationf 'merge-paths
                              "~s is not a path" ref-path)]
       [(and base-has-auth? (empty-path? base-path))
        (make-raw-absolute-path (path-raw-segments (copy-path ref-path)))]
       [(empty-path? base-path)
        (copy-path ref-path)]
       [(not (path? base-path))
        (assertion-violationf 'merge-paths
                              "base ~s is not a path" base-path)]
       [else
        (let ([up-to (base-path-last base-path)])
          (cond [(not up-to)
                 (copy-path ref-path)]
                [(absolute-path? base-path)
                 (make-raw-absolute-path
                  (append (list-take (path-raw-segments base-path) up-to)
                          (path-raw-segments ref-path)))]
                [else
                 (make-raw-relative-path
                  (append (list-take (path-raw-segments base-path) up-to)
                          (path-raw-segments ref-path)))]))])]))

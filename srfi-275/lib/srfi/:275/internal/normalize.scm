;; SPDX-FileCopyrightText: 2026 D. Guthrie <dguthrie@posteo.net>
;;;
;;; SPDX-License-Identifier: MIT
(define-module (srfi :275 internal normalize)
  #:use-module (rnrs base)
  #:use-module (rnrs arithmetic fixnums)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs conditions)
  #:use-module (rnrs control)
  #:use-module (rnrs exceptions)
  #:use-module (rnrs unicode)
  #:use-module ((ice-9 match) #:select (match match-let match-lambda*))
  #:use-module (ice-9 format)
  #:use-module ((srfi srfi-1) #:select (last))
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-14)
  #:use-module (srfi :275 internal resource-identifier)
  #:use-module ((srfi :275 internal utils)
                #:select (char-set:iri-fragment char-set:iri-query
                          char-set:iri-segment
                          char-set:iri-unreserved char-set:iri-userinfo
                          char-set:reserved char-set:sub-delims
                          char-set:uri char-set:uri-fragment
                          char-set:uri-query char-set:uri-segment
                          char-set:uri-unreserved char-set:uri-userinfo
                          raise-overlong shifted-percent-encoding?
                          shift-percent-encoding u16vector-copy
                          u16vector-empty?
                          u16vector= unshift-percent-encoding))
  #:use-module (srfi :275 internal path)
  #:use-module (srfi :275 internal iri)
  #:use-module (srfi :275 internal uri)
  #:use-module ((srfi :275 internal iri) #:prefix iri:)
  #:use-module ((srfi :275 internal uri) #:prefix uri:)
  #:export (normalize-iri-case! normalize-iri-escape! normalize-iri-path! normalize-iri!
            normalize-iri-case normalize-iri-escape normalize-iri-path normalize-iri
            normalize-uri-case! normalize-uri-escape! normalize-uri-path! normalize-uri!
            normalize-uri-case normalize-uri-escape normalize-uri-path normalize-uri
            resolve-iri resolve-uri relativize-iri relativize-uri relativize-path
            uri->iri iri->uri copy-iri copy-uri))

(define (assertion-violationf who template . args)
  (assertion-violation who (apply format #f template args)))
(define (errorf who template . args)
  (error who (apply format #f template args)))
(define (void) (if #f #f))
(define copy-iri iri:copy-iri)
(define copy-uri uri:copy-uri)

(define raw-scheme resource-identifier-raw-scheme)
(define raw-user resource-identifier-raw-user)
(define raw-host resource-identifier-raw-host)
(define has-authority? resource-identifier-has-authority?)
(define raw-query resource-identifier-raw-query)
(define raw-fragment resource-identifier-raw-fragment)
(define set-raw-scheme! set-resource-identifier-raw-scheme!)
(define set-raw-user! set-resource-identifier-raw-user!)
(define set-raw-host! set-resource-identifier-raw-host!)
(define set-raw-query! set-resource-identifier-raw-query!)
(define set-raw-fragment! set-resource-identifier-raw-fragment!)
(define raw-user/ref relative-reference-raw-user)
(define raw-host/ref relative-reference-raw-host)
(define has-authority?/ref relative-reference-has-authority?)
(define raw-query/ref relative-reference-raw-query)
(define raw-fragment/ref relative-reference-raw-fragment)
(define set-raw-user!/ref set-relative-reference-raw-user!)
(define set-raw-host!/ref set-relative-reference-raw-host!)
(define set-raw-query!/ref set-relative-reference-raw-query!)
(define set-raw-fragment!/ref set-relative-reference-raw-fragment!)
  #|
     We can rely on this working for both URIs and IRIs because in UTF-8,
     ASCII is always represented as the codepoint in question.
  |#
  (define (normalize-ascii-case! seq)
    (let ([size (u16vector-length seq)])
      (let loop ([k 0])
        (unless (fx=? k size)
          (let ([octet (u16vector-ref seq k)])
            (when (fx<=? #x41 octet #x7A)
              (let ([down (char-downcase (integer->char octet))])
                (u16vector-set! seq k (char->integer down))))
            (loop (fx+ k 1)))))))

  (define %normalize-case!
    (let ([normalize-component!
           (lambda (seq)
             (when seq (normalize-ascii-case! seq)))])
      "Percent-encodings have a canonical upper-case form; scheme and host are case-insensitive."
      (lambda (res)
        (cond [(resource-identifier? res)
               (normalize-component! (raw-scheme res))
               (normalize-component! (raw-host res))]
              [(relative-reference? res)
               (normalize-component! (raw-host/ref res))]))))

  #|
     Escape (percent-encoding) normalization
  |#

  (define char-set:uri-host-clear (char-set-union char-set:sub-delims char-set:uri-unreserved))

  #|
     Need to add test vectors for the following, and I'm not sure it's quite
     right. We proceed as follows:
     a) If any character is reserved, don't touch it.
     b) Then, if the character is in the allowed range, maybe convert.
  |#
  (define (uri-initial-reencode! vec k cd allowed-range)
    (if (shifted-percent-encoding? cd)
        (let* ([octet (unshift-percent-encoding cd)]
               [pst (integer->char octet)])
          (cond [(char-set-contains? char-set:reserved pst)
                 (void)]
                [(char-set-contains? allowed-range pst)
                 (u16vector-set! vec k octet)]
                [else
                 (void)]))
        (let ([pst (integer->char cd)])
          (unless (char-set-contains? allowed-range pst)
            (u16vector-set! vec k (shift-percent-encoding cd))))))

  (define (reinterpret-uri-octets! seq allowed-range)
    (let ([size (u16vector-length seq)])
      (let loop ([k 0])
        (unless (fx=? k size)
          (let ([x (u16vector-ref seq k)])
            (uri-initial-reencode! seq k x allowed-range)
            (loop (fx+ k 1)))))))

  (define (%normalize-uri-escape! res)
    (define (normalize-component! seq allowed-range)
      (when seq (reinterpret-uri-octets! seq allowed-range)))
    (define (normalize-path! pth)
      (for-each
       (lambda (seg)
         (reinterpret-uri-octets! seg char-set:uri-segment))
       (path-raw-segments pth)))
   (cond
     [(non-relative-uri? res)
      (normalize-component! (raw-user res) char-set:uri-userinfo)
      (normalize-component! (raw-host res) char-set:uri-host-clear)
      (normalize-path! (resource-identifier-path res))
      (normalize-component! (raw-query res) char-set:uri-query)
      (normalize-component! (raw-fragment res) char-set:uri-fragment)]
     [(relative-uri? res)
      (normalize-component! (raw-user/ref res) char-set:uri-userinfo)
      (normalize-component! (raw-host/ref res) char-set:uri-host-clear)
      (normalize-path! (relative-reference-path res))
      (normalize-component! (raw-query/ref res) char-set:uri-query)
      (normalize-component! (raw-fragment/ref res) char-set:uri-fragment)]))

  (define iri-initial-reencode!
    (let* ([char-set:uri-disallowed; subset of U.S. ASCII chars disallowed in URIs
            (char-set-difference char-set:ascii char-set:uri)]
           [char-set:uri-reserved+disallowed
            (char-set-union (char-set #\%) char-set:reserved char-set:uri-disallowed)])
      (lambda (vec k cd)
        #| This procedure corresponds to step 2 of RFC 3987 section 3.2.
           (the steps after "... or any algorithm which produces the same result") |#
        (when (shifted-percent-encoding? cd)
          (let ([octet (unshift-percent-encoding cd)])
            (unless (char-set-contains? char-set:uri-reserved+disallowed
                                        (integer->char octet))
              (u16vector-set! vec k octet)))))))

  #| The following actually produces a list of octets, maybe smaller than the
     original u16vector encoding. |#
  (define (check-octet-sequence! seq allowed-range)
    (let ([size (u16vector-length seq)])
      (let recur ([k 0] [buffer '()])
        (if (fx=? k size)
            (begin
              (raise-overlong buffer 'normalize-iri-escape! k)
              (void))
            (match (u16vector-ref seq k)
              [(? shifted-percent-encoding? octet)
               (if (null? buffer)
                   (recur (fx+ k 1) '())
                   (begin
                     (raise-overlong buffer 'normalize-iri-escape! k)
                     (recur (fx+ k 1) '())))]
              [octet
               (recur (fx+ k 1) (cons octet buffer))])))))

  (define (reinterpret-iri-octets! seq allowed-range)
    #| This is essentially doing two things:
       a) We make everything an octet apart from escapes in the reserved (or otherwise disallowed)
          character set ranges
       b) If we find an invalid sequence then the entire sequence is preserved as percent
          encoding. |#
    (let ([size (u16vector-length seq)])
      (let loop ([k 0])
        (if (fx=? k size)
            (check-octet-sequence! seq allowed-range)
            (let ([x (u16vector-ref seq k)])
              (iri-initial-reencode! seq k x)
              (loop (fx+ k 1)))))))

  (define char-set:iri-host-clear (char-set-union char-set:uri-host-clear char-set:iri-unreserved))

  (define (%normalize-iri-escape! res)
    (define (normalize-component! seq allowed-range)
      (when seq (reinterpret-iri-octets! seq allowed-range)))
    (define (normalize-path! pth)
      (for-each
       (lambda (seg)
         (reinterpret-iri-octets! seg char-set:iri-segment))
       (path-raw-segments pth)))
    (cond
     [(non-relative-iri? res)
      (normalize-component! (raw-user res) char-set:iri-userinfo)
      (normalize-component! (raw-host res) char-set:iri-host-clear)
      (normalize-path! (resource-identifier-path res))
      (normalize-component! (raw-query res) char-set:iri-query)
      (normalize-component! (raw-fragment res) char-set:iri-fragment)]
     [(relative-iri? res)
      (normalize-component! (raw-user/ref res) char-set:iri-userinfo)
      (normalize-component! (raw-host/ref res) char-set:iri-host-clear)
      (normalize-path! (relative-reference-path res))
      (normalize-component! (raw-query/ref res) char-set:iri-query)
      (normalize-component! (raw-fragment/ref res) char-set:iri-fragment)]))

  (define (%normalize-path-segments! res)
    (define new-path
      (remove-dot-segments (resource-identifier-path res)))
    (cond [(path-settable? res new-path)
           #| Authority component is defined, and we have the expected
              `path-abempty' path structure; or authority component is
              undefined, and we have the expected relative path. |#
           (set-resource-identifier-path! res new-path)]
          [(absolute-path? new-path)
           #| We have a path which would be mistaken for `//', but we
              can't extract a non-absolute path in this case. The meaning
              can be preserved by adding ./ after the initial slash. |#
           (let ([prefixed-segments
                  (append `(,(u16vector #x2E) ,(u16vector))
                          (path-raw-segments new-path))])
             (set-resource-identifier-path!
              res
              (make-raw-relative-path prefixed-segments)))]
          [else
           (errorf '%normalize-path-segments!
                   "internal error eliciting path candidate ~s: host ~s unexpectedly set in ~s"
                   new-path
                   (iri-host res)
                   res)]))

  #|
     Conversion and equivalence
  |#
  (define (uri->iri res)
    (cond [(non-relative-uri? res)
           (let ([upgraded-structure (copy-resource-identifier make-raw-iri res)])
             (normalize-iri! upgraded-structure)
             upgraded-structure)]
          [(relative-uri? res)
           (let ([upgraded-structure (copy-relative-reference make-raw-relative-iri res)])
             (normalize-iri! upgraded-structure)
             upgraded-structure)]
          [else
           (assertion-violationf 'uri->iri "~s is not an URI" res)]))

  (define (iri->uri res)
    (cond [(non-relative-iri? res)
           (let ([downgraded-structure (copy-resource-identifier make-raw-uri res)])
             (normalize-uri! downgraded-structure)
             downgraded-structure)]
          [(relative-iri? res)
           (let ([downgraded-structure (copy-relative-reference make-raw-relative-uri res)])
             (normalize-uri! downgraded-structure)
             downgraded-structure)]
          [else
           (assertion-violationf 'iri->uri "~s is not an IRI" res)]))

  #|
     First, make no assumptions about the scheme in the reference, when it is
     identical to that of the base.

     The way the algorithm works is to proceed in checking all the way down
     to the path:
     1. If the scheme is defined for the reference IRI, use it verbatim.
     2. If the authority is defined for the reference IRI, use the base
        scheme + rest of the reference
     3. If reference's path is empty, then target path is base, then query
        is either the reference (if defined) or the base's.
     4. If reference's path has leading slash, use it. Otherwise, merge the
        paths. The query is the reference's.

     In all cases, the fragment is that of the reference's.

     This is necessarily in-place since we're transforming a relative
     reference into a non-relative resource identifier, calling `set!'
     or similar would be quite unexpected.
  |#
  (define (%resolve-reference base ref)
    (let* ([make-identifier (if (non-relative-iri? base)
                                make-raw-iri
                                make-raw-uri)]
           [make-resolved-reference
            (lambda args (apply make-identifier (cons #f args)))])
      (if (resource-identifier? ref)
        (let ([tgt (copy-resource-identifier make-identifier ref)])
          (%normalize-path-segments! tgt)
          tgt)
        (let ([tgt (copy-relative-reference make-resolved-reference ref)])
          (cond [(has-authority? tgt)
                 (set-raw-scheme! tgt (copy-raw-component (raw-scheme base)))
                 (%normalize-path-segments! tgt)
                 tgt]
                [else
                 (let ([base-path (resource-identifier-path base)]
                       [base-query (raw-query base)]
                       [tgt-path (resource-identifier-path tgt)]
                       [tgt-query (raw-query tgt)])
                   (set-raw-scheme! tgt (copy-raw-component (raw-scheme base)))
                   (set-raw-host! tgt (copy-raw-component (raw-host base)))
                   (set-raw-user! tgt (copy-raw-component (raw-user base)))
                   (set-resource-identifier-port! tgt (resource-identifier-port base))
                   (cond [(and (empty-path? tgt-path) (not tgt-query))
                          (set-resource-identifier-path! tgt (copy-path base-path))
                          (set-raw-query! tgt (copy-raw-component base-query))]
                         [(empty-path? tgt-path)
                          (set-resource-identifier-path! tgt (copy-path base-path))]
                         [(not (absolute-path? tgt-path))
                          ;; Set it to the merged path, then normalize that merged path:
                          (let ([concatenated
                                 (merge-paths (resource-identifier-path base)
                                              (resource-identifier-path tgt)
                                              (has-authority? base))])
                            (set-resource-identifier-path! tgt concatenated)
                            (%normalize-path-segments! tgt))]
                         [(absolute-path? tgt-path)
                          (%normalize-path-segments! tgt)])
                   tgt)])))))

  (define (resolve-iri base ref)
    (cond [(not (non-relative-iri? base))
           ;; https://www.rfc-editor.org/info/rfc3986/#section-5.1
           (errorf 'resolve-iri "base ~s is not a non-relative IRI" base)]
          [(not (iri? ref))
           (errorf 'resolve-iri "reference ~s is not an IRI or relative reference" ref)]
          [else
           (%resolve-reference base ref)]))

  (define (resolve-uri base ref)
    (cond [(not (non-relative-uri? base))
           ;; https://www.rfc-editor.org/info/rfc3986/#section-5.1
           (errorf 'resolve-uri "base ~s is not a non-relative URI" base)]
          [(not (uri? ref))
           (errorf 'resolve-uri "reference ~s is not an URI or relative reference" ref)]
          [else
           (%resolve-reference base ref)]))

  #|
     Elicit a relative reference between two URIs or IRIs.
     This corresponds to the Chicken uri-generic procedure `relative-from'
  |#
  (define (authority-equal? a b)
    (and (raw-component-equal? (raw-user a) (raw-user b))
         (raw-component-equal? (raw-host a) (raw-host b))
         (eqv? (resource-identifier-port a) (resource-identifier-port b))))

  (define finalise-extracted-path
    (match-lambda*
      [(#f #f #f (or (? rfc3987-path-absolute? old-path)
                     (? rfc3987-path-noscheme? old-path)
                     (? empty-path?    old-path)) copy-path)
       (copy-path old-path)]
      [(#f #f #f (? rfc3987-path-rootless? old-path) _)
       (make-raw-relative-path
        (append `(,(current-wd)) (path-raw-segments old-path)))]
      [(u h p old-path copy-path)
       (copy-path old-path)]))

  (define (%relativize base ident who maker copy)
    (cond [(not (u16vector= (raw-scheme ident)
                            (raw-scheme base)))
           (copy ident)]
          [(not (authority-equal? ident base))
           (let ([next-path (finalise-extracted-path
                             (raw-user ident) (raw-host ident) (resource-identifier-port ident)
                             (resource-identifier-path ident)
                             copy-path)])
             (maker (copy-raw-component (raw-user ident))
                    (copy-raw-component (raw-host ident) )
                    (resource-identifier-port ident)
                    next-path
                    (copy-raw-component (raw-query ident))
                    (copy-raw-component (raw-fragment ident))))]
          [(empty-path? (resource-identifier-path ident))
           (let ([next-path (finalise-extracted-path
                             (raw-user ident) (raw-host ident) (resource-identifier-port ident)
                             (resource-identifier-path ident)
                             copy-path)])
             (maker (copy-raw-component (raw-user ident))
                    (copy-raw-component (raw-host ident) )
                    (resource-identifier-port ident)
                    next-path
                    (copy-raw-component (raw-query ident))
                    (copy-raw-component (raw-fragment ident))))]
          [(not (path=? (resource-identifier-path ident)
                             (resource-identifier-path base)))
           (let ([next-path (finalise-extracted-path
                             #f #f #f
                             (relativize-path (remove-dot-segments (resource-identifier-path base))
                                              (remove-dot-segments (resource-identifier-path ident))
                                              base ident who)
                             identity)])
             (maker #f #f #f
                    next-path
                    (copy-raw-component (raw-query ident))
                    (copy-raw-component (raw-fragment ident))))]
          [(not (raw-component-equal? (raw-query ident)
                                      (raw-query base)))
           (maker #f #f #f
                  (empty-path)
                  (copy-raw-component (raw-query ident))
                  (copy-raw-component (raw-fragment ident)))]
          [(absolute-path? (resource-identifier-path ident))
           (match (path-raw-segments (resource-identifier-path ident))
             ['()
              (empty-absolute-path)]
             [(? list? segments)
              (let* ([final (last segments)]
                     [next-path (if (u16vector-empty? final)
                                    (make-raw-relative-path `(,(current-wd)))
                                    (make-raw-relative-path `(,(u16vector-copy final))))])
                ;; Is non-nil fragment here correct?
                (maker #f #f #f
                       next-path
                       #f
                       (copy-raw-component (raw-fragment ident))))])]
          [else #;relative-path?
           (errorf who "internal error: relative path case for non-base argument ~s is not implemented" ident)]))

  (define (relativize-iri base ident)
    (cond [(not (non-relative-iri? base))
           (assertion-violationf 'relativize-iri "base ~s is not a non-relative IRI" base)]
          [(not (non-relative-iri? ident))
           (assertion-violationf 'relativize-iri "non-base ~s is not a non-relative IRI" ident)]
          [else
           (%relativize base ident 'relativize-iri make-raw-relative-iri copy-iri)]))

  (define (relativize-uri base ident)
    (cond [(not (non-relative-uri? base))
           (assertion-violationf 'relativize-uri "base ~s is not a non-relative URI" base)]
          [(not (non-relative-uri? ident))
           (assertion-violationf 'relativize-uri "non-base ~s is not a non-relative URI" ident)]
          [else
           (%relativize base ident 'relativize-uri make-raw-relative-uri copy-uri)]))

  ;; based on Chicken uri-generic rel-path-from
  (define (relativize-path base-path path base-par par who)
    (cond [(not (rfc3987-path-abempty? base-path))
           (errorf who "normalized base path ~s of ~s is not RFC 3987 abempty" base-path base-par)]
          [(not (rfc3987-path-abempty? path))
           (errorf who "normalized path ~s of ~s is not RFC 3987 abempty" path par)]
          [(empty-path? base-path) (copy-path path)]
          [(empty-path? path)      (empty-path)]
          [else
           (let ([raw-extracted
                  (relativize-path/segments (path-raw-segments base-path)
                                            (path-raw-segments path))])
             (if (empty-initial-segment? raw-extracted)
                 (make-raw-absolute-path (cdr raw-extracted))
                 (make-raw-relative-path raw-extracted)))]))

  (define (dotted? segment)
    (or (parent-wd? segment) (current-wd? segment)))

  ;; based on Chicken uri-generic rel-path-from1
  (define (relativize-path/segments base-segments segments)
    (match-let [((base-final . base-init) (reverse base-segments))
                ((final . init)             (reverse segments))]
      (let ([relative-segments
             (relative-segments-from (reverse base-init)
                                     (reverse init))])
        (match relative-segments
          ['()
           (cond [(u16vector= final base-final)
                  '()]
                 [(u16vector-empty? final)
                  `(,(current-wd))]
                 [else
                  `(,(u16vector-copy final))])]
          [(_rsegs ... (? dotted?))
           (if (u16vector-empty? final)
               relative-segments
               (append relative-segments (list final)))]
          [_
           (append relative-segments (list final))]))))

  ;; based on Chicken uri-generic rel-segs-from
  (define relative-segments-from
    (match-lambda*
      [('() '())       '()]
      [('() init)      (diff-segments '()       init)]
      [(base-init '()) (diff-segments base-init '())]
      [((and (sb1 . rb1) base-init)
        (and (sa1 . ra1) init))
       (if (u16vector= sb1 sa1)
	   (relative-segments-from rb1 ra1)
	   (diff-segments base-init init))]))

  ;; based on Chicken uri-generic dif-segs-from
  (define diff-segments
    (match-lambda*
      [('() init) init]
      [(base-init init)
       (diff-segments (cdr base-init)
                      (cons (parent-wd) init))]))

  #|
     RFC 3986: Universal resource identifiers (IRIs)
  |#
  (define (normalize-uri-escape! res)
    (if (uri? res)
        (%normalize-uri-escape! res)
        (assertion-violationf 'normalize-uri-escape! "~s is not an URI or relative reference" res)))

  (define (normalize-uri-escape res)
    (if (uri? res)
        (let ([tgt (copy-uri res)])
          (%normalize-uri-escape! tgt)
          tgt)
        (assertion-violationf 'normalize-uri-escape "~s is not an URI or relative reference" res)))

  (define (normalize-uri-case! res)
    (if (uri? res)
        (%normalize-case! res)
        (assertion-violationf 'normalize-uri-case! "~s is not an URI or relative reference" res)))

  (define (normalize-uri-case res)
    (if (uri? res)
        (let ([tgt (copy-uri res)])
          (%normalize-case! tgt)
          tgt)
        (assertion-violationf 'normalize-uri-case "~s is not an URI or relative reference" res)))

  (define (normalize-uri-path! res)
    ;; collapse first two clauses into a `when' later:
    (cond [(non-relative-uri? res)
           (%normalize-path-segments! res)]
          [(relative-uri? res)
           (void)]
          [else
           (assertion-violationf 'normalize-uri-path! "~s is not an URI or relative reference" res)]))

  (define (normalize-uri-path res)
    ;; collapse first two clauses into a `when' later:
    (cond [(non-relative-uri? res)
           (let ([tgt (copy-uri res)])
             (%normalize-path-segments! tgt)
             tgt)]
          [(relative-uri? res)
           res]
          [else
           (assertion-violationf 'normalize-uri-path "~s is not an URI or relative reference" res)]))

  (define (normalize-uri! res)
    (cond [(non-relative-uri? res)
           (%normalize-uri-escape! res)
           (%normalize-case! res)
           (%normalize-path-segments! res)]
          [(relative-uri? res)
           (%normalize-uri-escape! res)
           (%normalize-case! res)]
          [else
           (assertion-violationf 'normalize-uri! "~s is not an URI or relative reference" res)]))

  (define (normalize-uri res)
    (cond [(non-relative-uri? res)
           (let ([tgt (copy-uri res)])
             (%normalize-uri-escape! tgt)
             (%normalize-case! tgt)
             (%normalize-path-segments! tgt)
             tgt)]
          [(relative-uri? res)
           (let ([tgt (copy-uri res)])
             (%normalize-uri-escape! tgt)
             (%normalize-case! tgt)
             tgt)]
          [else
           (assertion-violationf 'normalize-uri "~s is not an URI or relative reference" res)]))

  #|
     RFC 3987: Internationalised resource identifiers (IRIs)
  |#
  (define (normalize-iri-escape! res)
    (if (iri? res)
        (%normalize-iri-escape! res)
        (assertion-violationf 'normalize-iri-escape! "~s is not an IRI or relative reference" res)))

  (define (normalize-iri-escape res)
    (if (iri? res)
        (let ([tgt (copy-iri res)])
          (%normalize-iri-escape! tgt)
          tgt)
        (assertion-violationf 'normalize-iri-escape "~s is not an IRI or relative reference" res)))

  (define (normalize-iri-case! res)
    (if (iri? res)
        (%normalize-case! res)
        (assertion-violationf 'normalize-iri-case! "~s is not an IRI or relative reference" res)))

  (define (normalize-iri-case res)
    (if (iri? res)
        (let ([tgt (copy-iri res)])
          (%normalize-case! tgt)
          tgt)
        (assertion-violationf 'normalize-iri-case "~s is not an IRI or relative reference" res)))

  (define (normalize-iri-path! res)
    ;; collapse first two clauses into a `when' later:
    (cond [(non-relative-iri? res)
           (%normalize-path-segments! res)]
          [(relative-iri? res)
           (void)]
          [else
           (assertion-violationf 'normalize-iri-path! "~s is not an IRI or relative reference" res)]))

  (define (normalize-iri-path res)
    ;; collapse first two clauses into a `when' later:
    (cond [(non-relative-iri? res)
           (let ([tgt (copy-iri res)])
             (%normalize-path-segments! tgt)
             tgt)]
          [(relative-iri? res)
           res]
          [else
           (assertion-violationf 'normalize-iri-path "~s is not an IRI or relative reference" res)]))

  (define (normalize-iri! res)
    (cond [(non-relative-iri? res)
           (%normalize-iri-escape! res)
           (%normalize-case! res)
           (%normalize-path-segments! res)]
          [(relative-iri? res)
           (%normalize-iri-escape! res)
           (%normalize-case! res)]
          [else
           (assertion-violationf 'normalize-iri! "~s is not an IRI or relative reference" res)]))

  (define (normalize-iri res)
    (cond [(non-relative-iri? res)
           (let ([tgt (copy-iri res)])
             (%normalize-iri-escape! tgt)
             (%normalize-case! tgt)
             (%normalize-path-segments! tgt)
             tgt)]
          [(relative-iri? res)
           (let ([tgt (copy-iri res)])
             (%normalize-iri-escape! tgt)
             (%normalize-case! tgt)
             tgt)]
          [else
           (assertion-violationf 'normalize-iri "~s is not an IRI or relative reference" res)]))

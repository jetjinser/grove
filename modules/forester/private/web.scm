;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester private web)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi :275 iri)
  #:use-module (srfi :275 normalize)
  #:use-module (srfi :275 path)
  #:use-module (srfi :275 uri)
  #:use-module (srfi srfi-13)
  #:export (web-origin?
            web-origin-uri
            web-iri-uri
            web-idna-cache-identity
            external-url?
            web-base-url
            web-absolute-route-url
            web-xml-escape))

(define (external-url? value)
  (and (string? value)
       (or (string-prefix-ci? "https://" value)
           (string-prefix-ci? "http://" value)
           (string-prefix? "//" value))))

(define (parse-web-origin url)
  (and (string? url)
       (catch #t
         (lambda ()
           (let* ((iri (string->iri url))
                  (scheme (and iri (iri-scheme iri)))
                  (host (and iri (iri-host iri)))
                  (path (and iri (iri-path iri))))
             (and iri
                  (non-relative-iri? iri)
                  (or (string-ci=? scheme "http")
                      (string-ci=? scheme "https"))
                  (and (string? host) (not (string-null? host)))
                  (not (iri-user iri))
                  (or (not path)
                      (let ((path (path->string path)))
                        (or (string-null? path) (string=? path "/"))))
                  (not (iri-query iri))
                  (not (iri-fragment iri))
                  iri)))
         (lambda _ #f))))

(define (web-origin? url)
  (and (parse-web-origin url) #t))

(define (idna-to-ascii host)
  (let* ((port (open-pipe* OPEN_READ "idn2" "--quiet" "--tr46nt"
                           "--usestd3asciirules" "--" host))
         (result (read-line port))
         (status (close-pipe port)))
    (unless (and (status:exit-val status)
                 (= (status:exit-val status) 0)
                 (string? result)
                 (not (eof-object? result)))
      (error "Cannot convert HTTP IRI host to an IDNA domain" host))
    result))

(define (web-idna-cache-identity)
  "Return a stable cache component for the IDNA implementation in PATH.

The executable's version also identifies the linked libidn2 implementation
in the supported Guix package. If idn2 is unavailable, retain a sentinel so
ASCII-only rendering remains usable; Unicode host conversion will report its
own actionable failure when requested."
  (let ((port (false-if-exception
                (open-pipe* OPEN_READ "idn2" "--version"))))
    (if (not port)
        '(idn2 unavailable)
        (let* ((version (read-line port))
               (status (close-pipe port)))
          (if (and (status:exit-val status)
                   (= (status:exit-val status) 0)
                   (string? version)
                   (not (eof-object? version)))
              (list 'idn2 version)
              '(idn2 unavailable))))))

(define (web-iri-uri value)
  "Map an IRI to an ASCII URI for a network URL output boundary.

Unicode DNS hosts use IDNA; the remaining IRI components use SRFI-275's
generic UTF-8 percent-encoding mapping."
  (let* ((iri (and (string? value)
                   (catch #t
                     (lambda () (string->iri value))
                     (lambda _ #f))))
         (host (and iri (iri-host iri))))
    (unless iri
      (error "Expected a valid IRI to map to a URI" value))
    (let* ((ascii-host
             (and host
                  (not (string-every (lambda (character)
                                       (< (char->integer character) 128))
                                     host))
                  (idna-to-ascii host)))
           (uri (iri->uri (if ascii-host
                              (update-iri-host iri ascii-host)
                              iri)))
           (serialized (uri->string uri))
           (original-scheme-end (string-index value #\:))
           (serialized-scheme-end (string-index serialized #\:)))
      (if (and original-scheme-end serialized-scheme-end)
          ;; Preserve scheme spelling while applying the IRI-to-URI mapping.
          (string-append
            (substring value 0 (1+ original-scheme-end))
            (substring serialized (1+ serialized-scheme-end)))
          serialized))))

(define (web-origin-uri url)
  "Return an ASCII URI for the HTTP(S) origin IRI URL.

The host is converted with IDNA UTS #46 non-transitional processing; other
IRI components use SRFI-275's generic UTF-8 percent-encoding mapping."
  (let* ((iri (parse-web-origin url))
         (host (and iri (iri-host iri))))
    (unless iri
      (error "Expected an absolute HTTP(S) origin IRI" url))
    (if (string-every (lambda (character)
                        (< (char->integer character) 128))
                      host)
        url
        (web-iri-uri url))))

(define (web-base-url url)
  (let ((end (string-length url)))
    (if (and (> end 0) (char=? (string-ref url (- end 1)) #\/))
        (web-base-url (substring url 0 (- end 1)))
        url)))

(define (web-absolute-route-url base route)
  (unless (and (string? route) (string-prefix? "/" route))
    (error "Web route URL must begin with /" route))
  (string-append base route))

(define (web-xml-escape text)
  (string-concatenate
    (map (lambda (character)
           (case character
             ((#\&) "&amp;")
             ((#\<) "&lt;")
             ((#\>) "&gt;")
             ((#\") "&quot;")
             ((#\') "&apos;")
             (else (string character))))
         (string->list text))))

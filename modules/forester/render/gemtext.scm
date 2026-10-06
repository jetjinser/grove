;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render gemtext)
  #:use-module (grove build)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render text)
  #:use-module (forester render text routes)
  #:use-module (forester site)
  #:use-module (grove site)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:export (site->gemtext
            render-gemtext
            make-gemtext-renderer))

(define (default-gemtext-routes)
  (make-text-routes
    #:page-path (lambda (id) (string-append id ".gmi"))
    #:index-path "index.gmi"))

(define (line-text text)
  (string-join (string-tokenize text) " "))

(define (marked-line kind text)
  (string-append (string #\x1e) (string kind) text))

(define (link-line url label)
  (string-append
    (marked-line #\L
      (string-append "=> " url
        (if (string-null? label) "" (string-append " " label))))
    "\n"))

(define gemtext-uri-path-characters
  (string->char-set
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~!$&'()*+,;=:@/"))

(define (uri-scheme? value)
  (let ((colon (string-index value #\:)))
    (and colon (> colon 0)
         (ascii-alpha? (string-ref value 0))
         (every (lambda (character)
                  (or (ascii-alpha? character)
                      (and (char>=? character #\0) (char<=? character #\9))
                      (memv character '(#\+ #\. #\-))))
                (string->list (substring value 1 colon))))))

(define (ascii-alpha? character)
  (or (and (char>=? character #\a) (char<=? character #\z))
      (and (char>=? character #\A) (char<=? character #\Z))))

(define (split-uri-suffix value)
  (let ((query (string-index value #\?))
        (fragment (string-index value #\#)))
    (let ((end (cond ((and query fragment) (min query fragment))
                     (query query)
                     (fragment fragment)
                     (else (string-length value)))))
      (values (substring value 0 end) (substring value end)))))

(define (route-uri path)
  (uri-encode path #:unescaped-chars gemtext-uri-path-characters))

(define (page-url routes from-id to-id)
  (route-uri
    (text-relative-path
      (text-page-path routes from-id)
      (text-page-path routes to-id))))

(define (asset-url routes page source)
  (if (uri-scheme? source)
      source
      (call-with-values
        (lambda () (split-uri-suffix source))
        (lambda (path suffix)
          (let ((relative
                  (if (string-prefix? "/" path)
                      path
                      (text-relative-path
                        (text-page-path routes (site-page-id page)) path))))
            (string-append (route-uri relative) suffix))))))

(define (page-content-handlers forest routes page)
  (let ((references (outgoing-references forest (site-page-id page)))
        (pending-links '()))
    (define (queue-link url label)
      (set! pending-links (cons (link-line url label) pending-links)))
    (define (flush-links body)
      (let ((links (reverse pending-links)))
        (set! pending-links '())
        (string-append body
          (if (null? links) "" (string-append "\n" (string-concatenate links))))))
    `((tag . ,(lambda (_ body) ""))
      (taxon . ,(lambda (_ body) ""))
      (item . ,(lambda (_ body)
                 (flush-links
                   (string-append "\n" (marked-line #\M
                     (string-append "* " (line-text body))) "\n"))))
      (paragraph . ,(lambda (_ body)
                      (string-append (flush-links body) "\n")))
      (link . ,(lambda (_ body)
                 (when (null? references)
                   (error "Gemtext link rendering exceeded its index"
                          (site-page-id page)))
                 (let* ((reference (car references))
                        (target (find-page forest
                                           (reference-target-page-id reference)))
                        (url (page-url routes (site-page-id page)
                                       (reference-target-page-id reference)))
                        (label (line-text body))
                        (label (if (string-null? label)
                                   (site-page-title target) label)))
                   (set! references (cdr references))
                   (queue-link url label)
                   label)))
      (query-link . ,(lambda (element body)
                       (let* ((target-id (content-element-property element 'target))
                              (target (find-page forest target-id))
                              (url (page-url routes (site-page-id page) target-id))
                              (label (line-text body)))
                         (set! label (if (string-null? label)
                                         (site-page-title target) label))
                         (queue-link url label)
                         label)))
      (image . ,(lambda (element body)
                  (let* ((source (content-element-property element 'source))
                         (url (asset-url routes page source))
                         (label (line-text body))
                         (label (if (string-null? label) "[image]" label)))
                    (queue-link url label)
                    label)))
      (code-block . ,(lambda (element body)
                       (when (any (lambda (line) (string-prefix? "```" line))
                                  (string-split body #\newline))
                         (error "Gemtext code block in page ~a contains a fence line"
                                (site-page-id page)))
                       (string-append "\n" (marked-line #\F "```")
                                      (let ((language
                                              (content-element-property
                                                element 'language)))
                                        (if language
                                            (string-append " " (symbol->string language))
                                            ""))
                                      "\n" body
                                      (if (string-suffix? "\n" body) "" "\n")
                                      (marked-line #\F "```") "\n"))))))

(define (page-body forest routes page)
  (content->text (site-page-document page)
    #:handlers (page-content-handlers forest routes page)
    #:lazy-handlers
    (map (lambda (kind)
           (cons kind
             (lambda (element _render)
                 (let* ((id (tree-occurrence-target element))
                      (target (find-page forest id))
                      (url (page-url routes (site-page-id page) id)))
                 (string-append "\n" (link-line url (site-page-title target)))))))
         '(subtree transclude query-transclude))))

(define (title-line title)
  (string-append (marked-line #\H
                 (string-append "# " (line-text title))) "\n\n"))

(define (gemtext-syntax-line? line)
  (or (string-prefix? "=>" line)
      (string-prefix? "#" line)
      (string-prefix? "* " line)
      (string-prefix? ">" line)
      (string-prefix? "```" line)))

(define (finish-gemtext text)
  (let loop ((lines (string-split text #\newline))
             (preformatted? #f)
             (result '()))
    (if (null? lines)
        (string-join (reverse result) "\n")
        (let* ((line (car lines))
               (marked? (and (>= (string-length line) 2)
                             (char=? (string-ref line 0) #\x1e)))
               (kind (and marked? (string-ref line 1)))
               (body (if marked? (substring line 2) line))
               (fence? (and marked? (char=? kind #\F)))
               (next-preformatted? (if fence? (not preformatted?)
                                       preformatted?))
               (finished
                 (cond
                   ((and marked? (memv kind '(#\H #\L #\M #\F))) body)
                   (preformatted? body)
                   ((gemtext-syntax-line? body) (string-append " " body))
                   (else body))))
          (loop (cdr lines) next-preformatted? (cons finished result))))))

(define (index->gemtext forest routes)
  (finish-gemtext
   (string-append
    (title-line (site-title (site forest)))
    (string-concatenate
      (map (lambda (page)
             (string-append (marked-line #\L "=> ")
               (route-uri (text-relative-path (text-index-path routes)
                 (text-page-path routes (site-page-id page))))
               " " (line-text (site-page-title page)) "\n"))
           (pages forest))))))

(define (page->gemtext forest routes page)
  (let ((body (page-body forest routes page)))
    (finish-gemtext
      (string-append (title-line (site-page-title page)) body
                     (if (string-suffix? "\n" body) "" "\n")))))

(define (check-routes forest routes)
  (let ((paths (cons (text-index-path routes)
                     (map (lambda (page)
                            (text-page-path routes (site-page-id page)))
                          (pages forest)))))
    (unless (= (length paths) (length (delete-duplicates paths string=?)))
      (error "Gemtext routes produce duplicate output paths" paths))))

(define* (site->gemtext forest #:key (routes (default-gemtext-routes)))
  (unless (forest? forest)
    (error "Forester Gemtext renderer requires a forest" forest))
  (check-routes forest routes)
  (cons (cons "index" (index->gemtext forest routes))
        (map (lambda (page)
               (cons (site-page-id page) (page->gemtext forest routes page)))
             (pages forest))))

(define* (render-gemtext forest #:key (routes (default-gemtext-routes)))
  (unless (forest? forest)
    (error "Forester Gemtext renderer requires a forest" forest))
  (check-routes forest routes)
  (cons (make-artifact (text-index-path routes)
                       (index->gemtext forest routes)
                       (source-paths forest))
        (map (lambda (page)
               (make-artifact
                 (text-page-path routes (site-page-id page))
                 (page->gemtext forest routes page)
                 (page-source-paths forest page)))
             (pages forest))))

(define* (make-gemtext-renderer #:key (prefix #f)
                                (routes (default-gemtext-routes))
                                (cache? #t))
  (unless (text-routes? routes)
    (error "Gemtext renderer requires text routes" routes))
  (unless (or (not prefix) (artifact-path? prefix))
    (error "Gemtext output prefix must be a relative path or #f" prefix))
  (let ((render (lambda (forest)
                  (let ((artifacts (render-gemtext forest #:routes routes)))
                    (if prefix (prefix-artifacts prefix artifacts) artifacts)))))
    (make-cached-renderer render #:cache? cache?
      #:configuration
      (lambda (forest)
        (list 'gemtext prefix
              (map (lambda (page)
                     (cons (site-page-id page)
                           (text-page-path routes (site-page-id page))))
                   (pages forest))
              (text-index-path routes)
              (site-title (site forest)) (site-description (site forest)))))))

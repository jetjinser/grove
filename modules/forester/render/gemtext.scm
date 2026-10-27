;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render gemtext)
  #:use-module ((grove private ir) #:prefix ir:)
  #:use-module (grove build)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove private snapshot)
  #:use-module (grove private render text)
  #:use-module (grove render rules)
  #:use-module (grove render context)
  #:use-module (forester render text routes)
  #:use-module (forester backmatter)
  #:use-module (forester private render labels)
  #:use-module (forester private entity-uri)
  #:use-module (forester render context)
  #:use-module (grove i18n)
  #:use-module (forester i18n)
  #:use-module (forester site)
  #:use-module (grove site)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:export (site->gemtext
            render-gemtext
            make-gemtext-options
            gemtext-options?
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

(define (describes-gemtext page context)
  (let ((uris (or (assq-ref (site-page-metadata page) 'describes) '())))
    (if (null? uris)
        ""
        (string-append
          "\n" (title-line (chrome-message 'describes context))
          (string-concatenate
            (map (lambda (uri)
                   (let ((href (entity-uri-href uri)))
                     (if href (link-line href uri)
                         (string-append uri "\n"))))
                 uris))))))

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
      (blockquote . ,(lambda (_ body)
                       (string-append
                         (string-join
                           (map (lambda (line)
                                  (marked-line #\Q (string-append "> " line)))
                                (string-split (string-trim-right body #\newline)
                                              #\newline))
                           "\n")
                         "\n")))
      (link . ,(lambda (element body)
                 (when (null? references)
                   (error "Gemtext link rendering exceeded its index"
                          (site-page-id page)))
                 (let* ((reference (car references))
                        (target (require-page forest
                                           (reference-target-page-id reference)))
                        (url (page-url routes (site-page-id page)
                                       (reference-target-page-id reference)))
                        (fragment (ir:content-element-property/raw element 'fragment))
                        (url (if fragment (string-append url "#" fragment) url))
                        (label (line-text body))
                        (label (if (string-null? label)
                                   (site-page-title target) label)))
                   (set! references (cdr references))
                   (queue-link url label)
                   label)))
      (query-link . ,(lambda (element body)
                       (let* ((target-id (ir:content-element-property/raw element 'target))
                              (fragment (ir:content-element-property/raw element 'fragment))
                              (target (require-page forest target-id))
                              (url (page-url routes (site-page-id page) target-id))
                              (url (if fragment (string-append url "#" fragment) url))
                              (label (line-text body)))
                         (set! label (if (string-null? label)
                                         (site-page-title target) label))
                         (queue-link url label)
                         label)))
      (image . ,(lambda (element body)
                  (let* ((source (ir:content-element-property/raw element 'source))
                         (url (asset-url routes page source))
                         (label (line-text body))
                         (label (if (string-null? label) "[image]" label)))
                    (queue-link url label)
                    label)))
      (code . ,(lambda (_ body) body))
      (code-block . ,(lambda (element body)
                       (when (any (lambda (line) (string-prefix? "```" line))
                                  (string-split body #\newline))
                         (error "Gemtext code block in page ~a contains a fence line"
                                (site-page-id page)))
                       (string-append "\n" (marked-line #\F "```")
                                      (let ((language
                                              (ir:content-element-property/raw
                                                element 'language)))
                                        (if language
                                            (string-append " " (symbol->string language))
                                            ""))
                                      "\n" body
                                      (if (string-suffix? "\n" body) "" "\n")
                                      (marked-line #\F "```") "\n"))))))

(define (page-body forest routes page rules context)
  (content->text/internal (site-page-document page)
    #:internal-handlers (page-content-handlers forest routes page)
    #:rules rules
    #:context (render-context-with context
               #:forest forest
               #:routes routes
               #:page page)
    #:internal-lazy-handlers
    (map (lambda (kind)
           (cons kind
             (lambda (element _render)
                 (let* ((id (tree-occurrence-target element))
                      (target (require-page forest id))
                      (url (page-url routes (site-page-id page) id)))
                 (string-append "\n" (link-line url (site-page-title target)))))))
         '(subtree transclude query-transclude))))

(define (page-backmatter forest page routes sections labels reference-taxon-id)
  (define (section heading targets)
    (if (null? targets)
        ""
        (string-append
          (marked-line #\H (string-append "## " heading)) "\n"
          (string-concatenate
            (map (lambda (target)
                   (link-line
                     (page-url routes (site-page-id page) (site-page-id target))
                     (site-page-title target)))
                 targets))
          "\n")))
  (string-concatenate
    (map (lambda (name)
           (let ((heading (assq-ref labels name)))
             (case name
               ((context)
                (section heading
                         (context-pages forest (site-page-id page))))
               ((references)
                (section heading
                         (reference-pages forest (site-page-id page)
                                          reference-taxon-id)))
               ((backlinks)
                (section heading
                         (backlinks forest (site-page-id page))))
               ((related)
                (section heading
                         (related-pages forest (site-page-id page)
                                        reference-taxon-id))))))
         sections)))

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
                   ((and marked? (memv kind '(#\H #\L #\M #\Q #\F))) body)
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

(define (page->gemtext forest routes page backmatter backmatter-labels
                       reference-taxon-id rules context)
  (let ((body (page-body forest routes page rules context)))
    (finish-gemtext
      (string-append
        (title-line (site-page-title page))
        (link-line (route-uri
                     (text-relative-path
                       (text-page-path routes (site-page-id page))
                       (text-index-path routes)))
                   (chrome-message 'home context))
        "\n" body
        (describes-gemtext page context)
        (page-backmatter forest page routes backmatter backmatter-labels
                         reference-taxon-id)
        (if (string-suffix? "\n" body) "" "\n")))))

(define (check-routes forest routes)
  (let ((paths (cons (text-index-path routes)
                     (map (lambda (page)
                            (text-page-path routes (site-page-id page)))
                          (pages forest)))))
    (unless (= (length paths) (length (delete-duplicates paths string=?)))
      (error "Gemtext routes produce duplicate output paths" paths))))

(define-record-type <gemtext-options>
  (%make-gemtext-options prefix routes backmatter
                         reference-taxon-id locale messages rules)
  gemtext-options?
  (prefix gemtext-options-prefix)
  (routes gemtext-options-routes)
  (backmatter gemtext-options-backmatter)
  (reference-taxon-id gemtext-options-reference-taxon-id)
  (locale gemtext-options-locale)
  (messages gemtext-options-messages)
  (rules gemtext-options-rules))

(define (require-gemtext-options options who)
  (unless (gemtext-options? options)
    (error (string-append who " requires Gemtext options") options))
  options)

(define* (make-gemtext-options #:key
                               (prefix #f)
                               (routes (default-gemtext-routes))
                               (backmatter default-backmatter-sections)
                               (reference-taxon-id default-reference-taxon-id)
                               (locale #f)
                               (messages default-message-catalog)
                               (rules empty-render-rules))
  "Build a validated Gemtext-renderer configuration snapshot."
  (let* ((prefix (snapshot-data prefix))
         (backmatter (snapshot-data backmatter))
         (reference-taxon-id (snapshot-data reference-taxon-id))
         (locale (snapshot-data locale)))
  (unless (or (not prefix) (artifact-path? prefix))
    (error "Forester Gemtext prefix must be a relative artifact path or #f"
           prefix))
  (unless (text-routes? routes)
    (error "Forester Gemtext options require text routes" routes))
  (validate-backmatter-sections backmatter)
  (check-reference-taxon-id reference-taxon-id)
  (validate-chrome-options locale messages)
  (unless (render-rules? rules)
    (error "Forester Gemtext options require render rules" rules))
  (%make-gemtext-options prefix routes backmatter
                         reference-taxon-id locale messages rules)))

(define* (site->gemtext forest #:key (options (make-gemtext-options)))
  (let* ((_ (require-gemtext-options options "site->gemtext"))
         (routes (gemtext-options-routes options))
         (backmatter (gemtext-options-backmatter options))
         (reference-taxon-id (gemtext-options-reference-taxon-id options))
         (locale (gemtext-options-locale options))
         (messages (gemtext-options-messages options))
         (rules (compose-render-rules bilingual-text-rules
                                      (gemtext-options-rules options))))
  (validate-backmatter-sections backmatter)
  (check-reference-taxon-id reference-taxon-id)
  (unless (forest? forest)
    (error "Forester Gemtext renderer requires a forest" forest))
  (check-routes forest routes)
  (let* ((context
           (make-site-render-context forest
                                     #:routes routes
                                     #:locale locale
                                     #:messages messages))
         (backmatter-labels (localized-backmatter-labels context)))
    (cons (cons "index" (index->gemtext forest routes))
          (map (lambda (page)
                 (cons (site-page-id page)
                       (page->gemtext forest routes page backmatter
                                      backmatter-labels reference-taxon-id
                                      rules context)))
               (pages forest))))))

(define* (render-gemtext forest #:key (options (make-gemtext-options)))
  (let* ((_ (require-gemtext-options options "render-gemtext"))
         (prefix (gemtext-options-prefix options))
         (routes (gemtext-options-routes options))
         (backmatter (gemtext-options-backmatter options))
         (reference-taxon-id (gemtext-options-reference-taxon-id options))
         (locale (gemtext-options-locale options))
         (messages (gemtext-options-messages options))
         (rules (compose-render-rules bilingual-text-rules
                                      (gemtext-options-rules options))))
  (validate-backmatter-sections backmatter)
  (check-reference-taxon-id reference-taxon-id)
  (unless (forest? forest)
    (error "Forester Gemtext renderer requires a forest" forest))
  (check-routes forest routes)
  (let* ((context
           (make-site-render-context forest
                                     #:routes routes
                                     #:locale locale
                                     #:messages messages))
         (backmatter-labels (localized-backmatter-labels context)))
    (let ((artifacts
            (cons (make-artifact (text-index-path routes)
                                 (index->gemtext forest routes)
                                 (source-paths forest))
                  (map (lambda (page)
                         (make-artifact
                           (text-page-path routes (site-page-id page))
                           (page->gemtext forest routes page backmatter
                                          backmatter-labels reference-taxon-id
                                          rules context)
                           (page-source-paths forest page)))
                       (pages forest)))))
      (if prefix (prefix-artifacts prefix artifacts) artifacts)))))

(define* (make-gemtext-renderer #:key (options (make-gemtext-options))
                               (rules-id #f)
                               (cache? #t))
  (let* ((_ (require-gemtext-options options "make-gemtext-renderer"))
         (prefix (gemtext-options-prefix options))
         (routes (gemtext-options-routes options))
         (backmatter (gemtext-options-backmatter options))
         (reference-taxon-id (gemtext-options-reference-taxon-id options))
         (locale (gemtext-options-locale options))
         (messages (gemtext-options-messages options))
         (rules (gemtext-options-rules options))
         (effective-cache?
           (and cache?
                (or (eq? rules empty-render-rules)
                    (and rules-id #t)))))
  (validate-rules-id rules-id)
  (set! rules-id (snapshot-data rules-id))
  (unless (boolean? cache?)
    (error "Forester Gemtext renderer cache option must be a boolean" cache?))
  (validate-backmatter-sections backmatter)
  (check-reference-taxon-id reference-taxon-id)
  (unless (text-routes? routes)
    (error "Gemtext renderer requires text routes" routes))
  (unless (or (not prefix) (artifact-path? prefix))
    (error "Gemtext output prefix must be a relative path or #f" prefix))
  (let ((render (lambda (forest)
                  (render-gemtext forest #:options options))))
    (make-cached-renderer render #:cache? effective-cache?
      #:configuration
      (lambda (forest)
        (list 'gemtext prefix
              (map (lambda (page)
                     (cons (site-page-id page)
                           (text-page-path routes (site-page-id page))))
                   (pages forest))
              (text-index-path routes)
              backmatter reference-taxon-id rules-id
              (site-title (site forest)) (site-description (site forest))
              (resolve-chrome-locale locale (resolve-chrome-catalog messages))
              (message-catalog-default (resolve-chrome-catalog messages))
              (message-catalog-entries (resolve-chrome-catalog messages))))))))

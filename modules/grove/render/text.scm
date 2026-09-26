;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (grove render text)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:export (content->text
            site->text))

(define (children->text children)
  (string-concatenate (map content->text children)))

(define (element->text element)
  (let ((kind (content-element-kind element))
        (body (children->text (content-element-children element))))
    (case kind
      ((title paragraph) (string-append body "\n"))
      ((link)
       (string-append body " ["
                      (content-element-property element 'target)
                      ".txt]"))
      ((math)
       (let ((source-text
               (content-verbatim-string
                 (content-element-children element))))
         (if (content-element-property element 'display?)
             (string-append "$$\n" source-text "\n$$\n")
             (string-append "$" source-text "$"))))
      (else body))))

(define (content->text content)
  (cond
    ((string? content) content)
    ((content-text? content) (content-text-value content))
    ((content-layout? content) (content-layout-raw content))
    ((content-element? content) (element->text content))
    ((content-document? content)
     (children->text (content-document-children content)))
    (else (error "not a content node" content))))

(define (heading title)
  (string-append title "\n" (make-string (string-length title) #\=) "\n"))

(define (page-reference page)
  (string-append (site-page-title page)
                 " (" (site-page-id page) ".txt)"))

(define (index->text site)
  (string-append
    (heading (site-title site))
    "\n"
    (string-concatenate
      (map (lambda (page)
             (string-append "- " (page-reference page) "\n"))
           (site-pages site)))))

(define (page->text site page)
  (let ((incoming (site-backlinks site (site-page-id page)))
        (body (string-trim-both
                (content->text (site-page-document page))
                (char-set #\newline))))
    (string-append
      (heading (site-page-title page))
      "\nHome: index.txt\n\n"
      body
      (if (null? incoming)
          "\n"
          (string-append
            "\n\nLinked from:\n"
            (string-concatenate
              (map (lambda (source)
                     (string-append "- " (page-reference source) "\n"))
                   incoming)))))))

(define (site->text site)
  (unless (site? site)
    (error "text renderer requires a Grove site" site))
  (when (member "index" (map site-page-id (site-pages site)))
    (error "text index path conflicts with site page" "index"))
  (cons (cons "index" (index->text site))
        (map (lambda (page)
               (cons (site-page-id page) (page->text site page)))
             (site-pages site))))

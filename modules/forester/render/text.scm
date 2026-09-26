;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (forester render text)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render text)
  #:use-module (grove site)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:export (forest->text
            render-forest-text))

(define link-handlers
  `((link . ,(lambda (element body)
               (string-append body " ["
                              (content-element-property element 'target)
                              ".txt]")))))

(define (heading title)
  (string-append title "\n" (make-string (string-length title) #\=) "\n"))

(define (page-reference page)
  (string-append (site-page-title page)
                 " (" (site-page-id page) ".txt)"))

(define (index->text forest)
  (let ((site (forest-site forest)))
    (string-append
      (heading (site-title site))
      "\n"
      (string-concatenate
        (map (lambda (page)
               (string-append "- " (page-reference page) "\n"))
             (site-pages site))))))

(define (page->text forest page)
  (let ((incoming (forest-backlinks forest (site-page-id page)))
        (body (string-trim-both
                (content->text (site-page-document page)
                               #:handlers link-handlers)
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

(define (forest->text forest)
  (unless (forest? forest)
    (error "Forester text renderer requires a forest" forest))
  (let ((site (forest-site forest)))
    (when (member "index" (map site-page-id (site-pages site)))
      (error "text index path conflicts with forest page" "index"))
    (cons (cons "index" (index->text forest))
          (map (lambda (page)
                 (cons (site-page-id page) (page->text forest page)))
               (site-pages site)))))

(define (render-forest-text forest)
  (map (lambda (page)
         (make-artifact (string-append (car page) ".txt") (cdr page)))
       (forest->text forest)))

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester render-golden)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-64)
  #:use-module (forester render graph)
  #:use-module (forester render gemtext)
  #:use-module (forester render html)
  #:use-module (forester render rss)
  #:use-module (forester render site)
  #:use-module (forester render sitemap)
  #:use-module (forester render text)
  #:use-module (forester site)
  #:use-module (grove site)
  #:use-module (grove output)
  #:use-module (grove render html routes)
  #:use-module (tests unit forester fixtures))

(define page
  (make-site-page
    "root" "Root"
    (evaluate-document "@paragraph{Hello, grove.}")
    #:metadata '((summary . "Root summary")
                 (published . "2026-01-02"))))

(define (forest)
  (make-forest
    (make-site (list page) #:title "Test" #:home "root")))

(define* (html-output forest #:key (site-url #f) (locale #f))
  (assoc-ref
    (site->html forest
                #:options
                (make-html-options
                  #:routes (make-html-routes #:home-page "root")
                  #:mathjax-src #f
                  #:settings (and site-url
                                  (make-site-settings #:site-url site-url
                                                      #:default-locale locale))
                  #:locale locale))
    "root"))

(define* (text-output forest #:key (locale #f))
  (assoc-ref (site->text forest
                         #:options (make-text-options #:locale locale))
             "root"))

(define* (gemtext-output forest #:key (locale #f))
  (assoc-ref (site->gemtext forest
                            #:options (make-gemtext-options #:locale locale))
             "root"))

(define (rss-output forest)
  (artifact-content
    (car (render-rss forest
                     #:settings (make-site-settings
                                  #:site-url "https://example.org"
                                  #:default-locale "en")
                     #:routes (make-html-routes #:home-page "root")))))

(define (sitemap-output forest)
  (artifact-content
    (car (render-sitemap forest
                         #:settings (make-site-settings
                                      #:site-url "https://example.org"
                                      #:default-locale "en")
                         #:routes (make-html-routes #:home-page "root")))))

(define (graph-output forest)
  (call-with-output-string
    (lambda (port) (write (site->graph forest) port))))

(define (golden name actual)
  (let ((path (string-append "tests/golden/" name)))
    (if (getenv "UPDATE_GOLDEN")
        (begin
          (unless (file-exists? "tests/golden")
            (mkdir "tests/golden"))
          (call-with-output-file path
            (lambda (port) (display actual port))))
        (test-equal name
          (call-with-input-file path get-string-all)
          actual))))

(test-begin "forester render golden")

(golden "root.html" (html-output (forest)))
(golden "root.txt" (text-output (forest)))
(golden "root.gmi" (gemtext-output (forest)))
(golden "feed.xml" (rss-output (forest)))
(golden "sitemap.xml" (sitemap-output (forest)))
(golden "graph.scm" (graph-output (forest)))

(let* ((a #f)
       (b #f)
       (thread-a
         (call-with-new-thread
           (lambda ()
             (set! a (html-output (forest)
                                  #:site-url "https://a.example"
                                  #:locale "en")))))
       (thread-b
         (call-with-new-thread
           (lambda ()
             (set! b (html-output (forest)
                                  #:site-url "https://b.example"
                                  #:locale "zh-Hans"))))))
  (join-thread thread-a)
  (join-thread thread-b)
  (test-assert "parallel HTML render keeps the first context"
    (and (string-contains a "https://a.example")
         (string-contains a "lang=\"en\"")
         (not (string-contains a "https://b.example"))))
  (test-assert "parallel HTML render keeps the second context"
    (and (string-contains b "https://b.example")
         (string-contains b "lang=\"zh-Hans\"")
         (not (string-contains b "https://a.example")))))

(test-assert "text renderer reads the render context locale"
  (and (string-contains (text-output (forest) #:locale "zh-Hans") "首页")
       (not (string-contains (text-output (forest) #:locale "en") "首页"))))

(test-assert "gemtext renderer reads the render context locale"
  (and (string-contains (gemtext-output (forest) #:locale "zh-Hans") "首页")
       (not (string-contains (gemtext-output (forest) #:locale "en") "首页"))))

(test-end "forester render golden")

;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester browse)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (grove ir)
  #:use-module (grove site)
  #:use-module (forester site)
  #:use-module (forester browse)
  #:use-module (forester i18n)
  #:use-module (tests unit forester fixtures))

(define (browse-output forest text)
  (let ((output (open-output-string)))
    (browse-forest forest #:input (open-input-string text) #:output output)
    (get-output-string output)))

(define (count-substring text needle)
  (let loop ((start 0) (count 0))
    (let ((index (string-contains text needle start)))
      (if index
          (loop (+ index (string-length needle)) (+ count 1))
          count))))

(define forest
  (make-forest
    (make-site (list algebra limits) #:title "Test" #:home "algebra")))

(test-begin "forester terminal browser")

(let ((text (browse-output forest "q\n")))
  (test-assert "browser starts at the site home page"
    (string-contains text "Algebra & limits"))
  (test-assert "inline links are numbered and navigable"
    (string-contains text "[1] limits"))
  (test-assert "image alternative text is shown"
    (string-contains text "An algebra diagram."))
  (test-assert "image resource path is shown"
    (string-contains text "images/algebra.svg")))

(let ((text (browse-output forest "1\nb\nq\n")))
  (test-assert "a number follows a page link"
    (string-contains text "Limits"))
  (test-assert "back returns to the previous page"
    (>= (count-substring text "Algebra & limits") 2)))

(let* ((custom
         (make-site-page "custom" "Custom"
           (make-content-document
             (list (make-content-element
                     'unknown-widget
                     (list (make-content-text "visible payload")))))))
       (custom-forest
         (make-forest (make-site (list custom) #:home "custom")))
       (text (browse-output custom-forest "q\n")))
  (test-assert "unknown elements are diagnosed"
    (string-contains text "unsupported elements"))
  (test-assert "unknown element children are not silently dropped"
    (string-contains text "visible payload")))

(let* ((host
         (make-site-page "host" "Host"
           (make-content-document
             (list (make-content-element
                     'transclude '()
                     #:properties '((target . "limits"))
                     #:flow-kind 'block)))))
       (expand-forest
         (make-forest (make-site (list host algebra limits) #:home "host"))))
  (test-assert "transclusions can expand inline"
    (string-contains (browse-output expand-forest "x1\nq\n")
                     "the algebra note")))

(let* ((browser (make-forest-browser #:start "limits"))
       (output (open-output-string)))
  (browser forest #:input (open-input-string "q\n") #:output output)
  (test-assert "make-forest-browser honors an explicit start page"
    (string-contains (get-output-string output) "Limits")))

(let* ((page
         (make-site-page "bilingual" "Bilingual"
           (make-content-document
             (list (bilingual (lang "en" "One.") (lang "zh-CN" "一。"))))))
       (forest (make-forest (make-site (list page) #:home "bilingual"))))
  (test-assert "the browser marks bilingual lanes"
    (let ((text (browse-output forest "q\n")))
      (and (string-contains text "[en] One.")
           (string-contains text "[zh-Hans] 一。")))))

(test-end "forester terminal browser")

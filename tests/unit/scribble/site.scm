;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit scribble site)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (system base language)
  #:use-module (language scribble spec)
  #:use-module (language scribble site))

(define environment (default-environment scribble))

(define (evaluate-document source)
  (eval ((language-reader scribble)
          (open-input-string source)
          environment)
        environment))

(define algebra
  (make-site-page
    "algebra"
    "Algebra & limits"
    (evaluate-document
      "@paragraph{The @link[\"limits\"]{limits} are useful. @math{x^2+y^2=z^2}}\n@display-math{a^2+b^2=c^2}")))

(define limits
  (make-site-page
    "limits"
    "Limits"
    (evaluate-document
      "@paragraph{See @link[\"algebra\"]{the algebra note}.}")))

(test-begin "scribble site")

(let* ((rendered (site->html (list algebra limits)))
       (algebra-html (assoc-ref rendered "algebra"))
       (limits-html (assoc-ref rendered "limits")))
  (test-assert "at-exp link resolves to a page"
    (string-contains algebra-html "href=\"/limits.html\">limits</a>"))
  (test-assert "links produce reverse links"
    (and (string-contains algebra-html "class=\"backlinks\"")
         (string-contains algebra-html "href=\"/limits.html\">Limits</a>")))
  (test-assert "reverse links point back to their source"
    (string-contains limits-html "href=\"/algebra.html\">Algebra &amp; limits</a>"))
  (test-assert "inline math keeps TeX delimiters"
    (string-contains algebra-html "\\(x^2+y^2=z^2\\)"))
  (test-assert "display math keeps TeX delimiters"
    (string-contains algebra-html "\\[a^2+b^2=c^2\\]"))
  (test-assert "HTML loads MathJax"
    (string-contains algebra-html "mathjax@3.2.2/es5/tex-chtml.js"))
  (test-assert "MathJax script has an explicit closing tag"
    (string-contains algebra-html "</script>"))
  (test-assert "HTML text is escaped"
    (string-contains algebra-html "Algebra &amp; limits")))

(test-error "unresolved links fail site rendering" #t
  (site->html
    (list (make-site-page "broken" "Broken"
            (evaluate-document "@paragraph{@link[\"missing\"]{nowhere}}")))))

(test-error "duplicate page ids fail site rendering" #t
  (site->html
    (list (make-site-page "same" "First" (evaluate-document "first"))
          (make-site-page "same" "Second" (evaluate-document "second")))))

(test-end "scribble site")

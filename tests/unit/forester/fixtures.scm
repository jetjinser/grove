;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester fixtures)
  #:use-module (system base language)
  #:use-module (language scribble spec)
  #:use-module (forester render html)
  #:use-module (forester site)
  #:use-module (grove ir)
  #:use-module (grove render html routes)
  #:use-module (grove site)
  #:export (evaluate-document
            algebra
            limits
            render-test-html))

(define environment (default-environment scribble))
(eval '(use-modules (forester commands)
                     (forester relations)
                     (forester site)
                     (grove site)
                     (grove relation dsl)
                     ((grove math notation) #:prefix notation:))
      environment)

(define* (evaluate-document source #:optional (filename #f))
  (let ((port (open-input-string source)))
    (when filename (set-port-filename! port filename))
    (scribble-evaluate-document port environment)))

(define algebra
  (make-site-page
    "algebra"
    "Algebra & limits"
    (evaluate-document
      "@paragraph{The @link[\"limits\"]{limits} are useful. @tex-math{x^2+y^2=z^2}}\n@tex-display-math{a^2+b^2=c^2}\n@paragraph{@image[\"images/algebra.svg\"]{An algebra diagram.}}")
    #:metadata '((summary . "Algebra notes and mathematical ideas."))))

(define limits
  (make-site-page
    "limits"
    "Limits"
    (evaluate-document
      "@paragraph{See @link[\"algebra\"]{the algebra note}.}")))

(define (render-test-html forest . options)
  (let ((routes
          (make-html-routes
            #:home-page (site-page-id (car (pages forest))))))
    (site->html forest
                #:options (apply make-html-options #:routes routes options))))

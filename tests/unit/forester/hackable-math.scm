;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests unit forester hackable-math)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (system base language)
  #:use-module (language scribble spec)
  #:use-module (forester commands)
  #:use-module (forester site)
  #:use-module (grove build)
  #:use-module (grove private renderer)
  #:use-module (grove ir)
  #:use-module (grove render plain-text)
  #:use-module (grove output)
  #:use-module (grove site)
  #:use-module (grove render rules)
  #:use-module (examples hackable-math extension)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64))

(define environment (default-environment scribble))
(eval '(use-modules (forester commands)
                    (examples hackable-math extension))
      environment)

(define (evaluate source)
  (scribble-evaluate-document (open-input-string source) environment))

(define (find-kind value kind)
  (define (search children)
    (and (pair? children)
         (or (find-kind (car children) kind)
             (search (cdr children)))))
  (cond
    ((content-element? value)
     (if (eq? (content-element-kind value) kind)
         value
         (search (content-element-children value))))
    ((content-document? value)
     (search (content-document-children value)))
    (else #f)))

;; Unit tests exercise the adapter's command lines with deterministic stand-ins.
;; Real Typst/TeX/Racket installations belong in the integration environment,
;; not as ambient prerequisites for `blue check`.
(define compiler-test-root
  (string-append (or (getenv "TMPDIR") "/tmp")
                 "/grove-math-tools-" (number->string (getpid))))
(define compiler-test-bin (string-append compiler-test-root "/bin"))
(define old-path (getenv "PATH"))

(define (write-tool name body)
  (let ((path (string-append compiler-test-bin "/" name)))
    (call-with-output-file path (lambda (port) (display body port)))
    (chmod path #o755)))

(mkdir compiler-test-root)
(mkdir compiler-test-bin)
(write-tool "typst"
  "#!/bin/sh\n[ \"$1\" = compile ] || exit 2\nprintf '%s\\n' '<svg>typst</svg>'\n")
(write-tool "latex"
  (string-append
    "#!/bin/sh\nout=\nwhile [ $# -gt 0 ]; do\n"
    "  if [ \"$1\" = -output-directory ]; then shift; out=$1; fi\n"
    "  shift\ndone\n"
    "[ -n \"$out\" ] || exit 2\n: > \"$out/main.dvi\"\n"))
(write-tool "dvisvgm"
  "#!/bin/sh\n[ -f \"$4\" ] || exit 2\nprintf '%s\\n' '<svg>latex</svg>'\n")
(write-tool "racket"
  "#!/bin/sh\nprintf '%s\\n' '<m:math xmlns:m=\"http://www.w3.org/1998/Math/MathML\"><m:mi>x</m:mi></m:math>'\n")
(setenv "PATH"
        (string-append compiler-test-bin ":" (or old-path "")))

(test-begin "hackable source math")

(let* ((source-root (dirname (getenv "GROVE_SRCDIR")))
       (sample (call-with-input-file
                 (string-append source-root
                   "/examples/hackable-math/math.plant")
                 get-string-all))
       (document (evaluate sample)))
  (test-equal "the example plant contains both SVG source languages"
    '(typst latex)
    (let loop ((nodes (let find ((node document))
                        (cond
                          ((content-element? node)
                           (append (if (eq? (content-element-kind node)
                                            'compiled-math)
                                       (list node)
                                       '())
                                   (append-map find
                                     (content-element-children node))))
                          ((content-document? node)
                           (append-map find
                             (content-document-children node)))
                          (else '())))))
      (if (null? nodes)
          '()
          (cons (content-element-property (car nodes) 'language)
                (loop (cdr nodes))))))
  (test-assert "the example plant contains a SMathML source node"
    (find-kind document 'smathml)))

(let* ((document
        (evaluate
          (string-append
            "@typst-math[#:alt \"square\"]{"
            "#let f(x) = x^2\n$ integral f(x) dx $}")))
       (element (find-kind document 'compiled-math)))
  (test-equal "Typst in a plant becomes a typed content node"
    'compiled-math (content-element-kind element))
  (test-equal "Typst nodes retain their language for project compilers"
    'typst (content-element-property element 'language))
  (test-assert "Typst output paths are derived from language and source"
    (let ((path (content-element-property element 'asset-path)))
      (and (string-prefix? "assets/math/typst-" path)
           (string-suffix? ".svg" path))))
  (test-equal "Typst source survives document reading"
    "#let f(x) = x^2\n$ integral f(x) dx $"
    (content->plain-text element)))

(test-assert "project Typst adapter compiles inline source to SVG"
  (string-contains
    (typst-source->svg
      (string-append
        "#set page(width: auto, height: auto, margin: 4pt)\n"
        "$ integral x^2 dif x $"))
    "<svg"))

(test-assert "project LaTeX adapter compiles inline source to SVG"
  (string-contains
    (latex-source->svg "\\[\\int x^2 \\, dx\\]")
    "<svg"))

(let* ((document
        (evaluate
          (string-append
            "@typst-math{$ x^2 $}"
            "@typst-math{$ x^2 $}")))
       (page (make-site-page "math" "Math" document))
       (forest (make-forest (make-site (list page))))
       (compiler-calls 0)
       (renderer
         (make-math-asset-renderer
           #:compiler (lambda (_language _source)
                        (set! compiler-calls (+ compiler-calls 1))
                        "<svg/>")))
       (artifacts ((cached-renderer-procedure renderer) forest)))
  (test-equal "repeated identical source shares one generated asset"
    1 (length artifacts))
  (test-equal "repeated identical source compiles only once"
    1 compiler-calls)
  (test-assert "generated SVG artifact uses the source-derived path"
    (string-prefix? "assets/math/typst-"
      (artifact-path (car artifacts)))))

(let* ((document
        (evaluate
          (string-append
            "@latex-math{"
            "\\begin{aligned}x^2 + y^2 &= z^2\\end{aligned}}")))
       (element (find-kind document 'compiled-math)))
  (test-equal "LaTeX source is distinct from Typst source"
    'latex (content-element-property element 'language))
  (test-equal "LaTeX braces and commands survive document reading"
    "\\begin{aligned}x^2 + y^2 &= z^2\\end{aligned}"
    (content->plain-text element)))

(let* ((document
        (evaluate
          "@smathml{(display (Xml (Math (Mi \"x\"))))}"))
       (element (find-kind document 'smathml))
       (rules
         (make-smathml-html-rules
           #:compile (lambda (source)
                       (test-equal "SMathML compiler receives authored Racket"
                         "(display (Xml (Math (Mi \"x\"))))" source)
                       (smathml-source->xml source))))
       (rule (render-rules-ref rules 'smathml))
       (html (rule #f element (content-element-children element)
                   (lambda () 'fallback))))
  (test-equal "SMathML source has its own document node"
    'smathml (content-element-kind element))
  (test-equal "SMathML XML is bridged to Grove's HTML SXML"
    '(m:math (@ (xmlns:m "http://www.w3.org/1998/Math/MathML"))
      (m:mi "x"))
    html))

(test-error "Typst nodes reject empty source" #t
  (typst-math ""))
(test-error "math asset renderers reject a compiler with the wrong arity" #t
  (make-math-asset-renderer #:compiler (lambda (_source) "<svg/>")))
(test-error "SMathML HTML rules reject a compiler with the wrong arity" #t
  (make-smathml-html-rules #:compile (lambda () "<math/>")))
(test-assert "math asset compilers may accept optional trailing arguments"
  (make-math-asset-renderer
    #:compiler (lambda* (language source #:optional (mode #f)) "<svg/>")))
(test-assert "SMathML compilers may accept rest arguments"
  (make-smathml-html-rules
    #:compile (lambda (source . options) "<math/>")))

(test-end "hackable source math")
(if old-path
    (setenv "PATH" old-path)
    (unsetenv "PATH"))
(system* "rm" "-rf" compiler-test-root)

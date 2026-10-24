;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (examples hackable-math extension)
  #:use-module (forester site)
  #:use-module (grove build)
  #:use-module (grove ir)
  #:use-module (grove output)
  #:use-module (grove render context)
  #:use-module (grove render html routes)
  #:use-module (grove render rules)
  #:use-module (grove site)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (language scribble command)
  #:use-module (sxml simple)
  #:export (typst-math
            latex-math
            smathml
            smathml-source->xml
            make-math-asset-renderer
            make-smathml-html-rules
            math-html-rules))

(define (source-string source who)
  (let ((source (content-verbatim-string (command-body source))))
    (unless (and (string? source) (not (string-null? source)))
      (error who "requires non-empty source" source))
    source))

(define (make-source-math language source asset-path alt)
  (unless (artifact-path? asset-path)
    (error "compiled math requires a relative asset path" asset-path))
  (unless (or (not alt) (string? alt))
    (error "compiled math alternative text must be a string or #f" alt))
  (make-content-element 'compiled-math (list source)
    #:properties `((asset-path . ,asset-path)
                   (alt . ,(or alt source))
                   (language . ,language))))

(define*-public (typst-math #:key asset-path (alt #f) . source)
  "Put Typst source in the document and compile it to an SVG asset."
  (make-source-math 'typst (source-string source "@typst-math")
                    asset-path alt))

(define*-public (latex-math #:key asset-path (alt #f) . source)
  "Put LaTeX source in the document and compile it to an SVG asset."
  (make-source-math 'latex (source-string source "@latex-math")
                    asset-path alt))

(define*-public (smathml #:key (display? #f) . source)
  "Put Racket/SMathML source in the document for a project HTML rule."
  (unless (boolean? display?)
    (error "@smathml #:display? must be a boolean" display?))
  (make-content-element 'smathml
    (list (source-string source "@smathml"))
    #:properties `((display? . ,display?))
    #:flow-kind (if display? 'block 'inline)))

(define (with-temporary-directory prefix thunk)
  (let ((directory (mkdtemp
                     (string-append (or (getenv "TMPDIR") "/tmp")
                                    "/" prefix "-XXXXXX"))))
    (dynamic-wind
      (lambda () #t)
      (lambda () (thunk directory))
      (lambda ()
        (for-each
          (lambda (entry)
            (unless (member entry '("." ".."))
              (let ((path (string-append directory "/" entry)))
                (when (file-exists? path) (delete-file path)))))
          (scandir directory))
        (when (file-exists? directory) (rmdir directory))))))

(define (run-captured program . arguments)
  (let* ((pipe (apply open-pipe* OPEN_READ program arguments))
         (output (get-string-all pipe))
         (status (close-pipe pipe)))
    (unless (zero? (status:exit-val status))
      (error "external math compiler failed" program status))
    output))

(define-public (typst-source->svg source)
  "Compile Typst SOURCE from the project to an SVG string."
  (unless (string? source)
    (error "Typst source must be a string" source))
  (with-temporary-directory
    "grove-typst"
    (lambda (directory)
      (let ((input (string-append directory "/main.typ")))
        (call-with-output-file input
          (lambda (port) (display source port)))
        (run-captured "typst" "compile" "--format" "svg" input "-")))))

(define-public (latex-source->svg source)
  "Compile a LaTeX math fragment SOURCE to SVG using LaTeX and dvisvgm."
  (unless (string? source)
    (error "LaTeX source must be a string" source))
  (with-temporary-directory
    "grove-latex"
    (lambda (directory)
      (let ((input (string-append directory "/main.tex"))
            (dvi (string-append directory "/main.dvi")))
        (call-with-output-file input
          (lambda (port)
            (display "\\documentclass{article}\n\\usepackage{amsmath}\n"
                     port)
            (display "\\pagestyle{empty}\n\\begin{document}\n" port)
            (display source port)
            (display "\n\\end{document}\n" port)))
        (run-captured "latex" "-interaction=nonstopmode" "-halt-on-error"
                      "-output-directory" directory input)
        (run-captured "dvisvgm" "--no-fonts" "--bbox=min" "--stdout" dvi)))))

(define-public (smathml-source->xml source)
  "Run Racket/SMathML source and return the MathML XML it writes to stdout.

SOURCE is trusted project code. It runs with SMathML's mathml and xml modules
available; project code can replace this adapter for other Racket modules or
output conventions."
  (unless (string? source)
    (error "SMathML source must be a string" source))
  (with-temporary-directory
    "grove-smathml"
    (lambda (directory)
      (let ((script (string-append directory "/render.rkt")))
        (call-with-output-file script
          (lambda (port)
            (display "#lang racket\n" port)
            (display "(require SMathML/mathml SMathML/xml)\n" port)
            (display source port)
            (newline port)))
        (let* ((pipe (open-pipe* OPEN_READ "racket" script))
               (xml (get-string-all pipe))
               (status (close-pipe pipe)))
          (unless (zero? (status:exit-val status))
            (error "Racket/SMathML compiler failed" status))
          xml)))))

(define (math-elements content)
  (cond
    ((content-document? content)
     (append-map math-elements (content-document-children content)))
    ((content-element? content)
     (if (eq? (content-element-kind content) 'compiled-math)
         (list content)
         (append-map math-elements (content-element-children content))))
    (else '())))

(define (site-math forest)
  (append-map
    (lambda (page)
      (map (lambda (element) (cons page element))
           (math-elements (site-page-document page))))
    (pages forest)))

(define (math-configuration compiler-id forest)
  (list 'compiled-math compiler-id
        (map (lambda (entry)
               (let ((page (car entry)) (element (cdr entry)))
                 (list (site-page-id page)
                       (content-element-property element 'asset-path)
                       (content-element-property element 'language)
                       (content->plain-text element))))
             (site-math forest))))

(define* (make-math-asset-renderer #:key compiler compiler-id
                                      (dependencies '()))
  "Return a cached renderer that turns compiled-math nodes into SVG artifacts.

COMPILER maps a language symbol and source string to SVG text. COMPILER-ID should include
the tool version and options that affect the output. DEPENDENCIES lists files
read by the compiler, such as macro packages or font configuration."
  (unless (procedure? compiler)
    (error "math asset renderer requires a compiler procedure" compiler))
  (unless (and (string? compiler-id) (not (string-null? compiler-id)))
    (error "math asset renderer requires a stable compiler ID" compiler-id))
  (unless (and (list? dependencies)
               (every (lambda (path)
                        (and (string? path) (not (string-null? path))))
                      dependencies))
    (error "math asset renderer dependencies must be a list of paths"
           dependencies))
  (make-cached-renderer
    (lambda (forest)
      (map
        (lambda (entry)
          (let* ((page (car entry))
                 (element (cdr entry))
                 (path (content-element-property element 'asset-path)))
            (unless (artifact-path? path)
              (error "compiled math node has an invalid asset path" path))
            (let* ((svg (compiler (content-element-property element 'language)
                                  (content->plain-text element)))
                   (sources (delete-duplicates
                              (if (site-page-source page)
                                  (cons (site-page-source page) dependencies)
                                  dependencies)
                              string=?)))
              (unless (string? svg)
                (error "math compiler must return SVG text" svg))
              (make-artifact path svg (and (pair? sources) sources)))))
        (site-math forest)))
    #:configuration (lambda (forest) (math-configuration compiler-id forest))
    #:inputs (lambda () dependencies)))

(define* (make-smathml-html-rules #:key compile)
  "Interpret @smathml nodes using COMPILE, which maps Racket source to XML.

The XML is parsed as SXML and handed to Grove's HTML serializer. Keep the
Racket/SMathML invocation and tool version in the project so this bridge
remains replaceable."
  (define mathml-uri "http://www.w3.org/1998/Math/MathML")
  (define (proper-attributes attributes)
    (let loop ((remaining attributes) (result '()))
      (if (and (pair? remaining) (pair? (car remaining)))
          (loop (cdr remaining) (cons (car remaining) result))
          (reverse result))))
  (define (declare-mathml-namespace node)
    (let* ((tag (car node))
           (qualified? (and (symbol? tag)
                            (string-prefix? "m:" (symbol->string tag))))
           (body (cdr node))
           (attributes (and (pair? body)
                            (pair? (car body))
                            (eq? (caar body) '@)
                            (proper-attributes (cdar body))))
           (children (if (and (pair? body)
                              (pair? (car body))
                              (eq? (caar body) '@))
                         (cdr body)
                         body)))
      (cons tag
            (cons `(@ (,(if qualified? 'xmlns:m 'xmlns) ,mathml-uri)
                    ,@(or attributes '()))
                  children))))
  (unless (procedure? compile)
    (error "SMathML HTML rules require a compiler procedure" compile))
  (make-render-rules
    (list
      (cons 'smathml
        (lambda (_context element _children next)
          (let* ((xml (compile (content->plain-text element)))
                 (tree (and (string? xml)
                            (xml->sxml
                              xml
                              #:namespaces `((m . ,mathml-uri)))))
                 (nodes (and tree (cdr tree))))
            (unless (pair? nodes)
              (error "SMathML compiler must return an XML fragment" xml))
            (if (content-element-property element 'display?)
                `(div (@ (class "smathml-display"))
                      ,@(map declare-mathml-namespace nodes))
                (if (null? (cdr nodes))
                    (declare-mathml-namespace (car nodes))
                    (map declare-mathml-namespace nodes)))))))))

(define math-html-rules
  (make-render-rules
    (list
      (cons 'compiled-math
        (lambda (context element _children next)
          (let ((routes (render-context-ref context 'routes))
                (path (content-element-property element 'asset-path))
                (alt (content-element-property element 'alt)))
            (if (and routes (artifact-path? path))
                `(img (@ (src ,(html-asset-url routes path))
                         (alt ,(or alt "Math formula"))))
                (next))))))))

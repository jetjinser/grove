;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Development environment mirroring the nix devShell in nix/flake.nix:
;;;
;;;   guix shell -m manifest.scm
;;;   blue check
;;;
;;; blue, faster-minikanren, and grove-highlights are not in Guix proper
;;; and are built from the same pinned revisions as nix/pkgs.  guile-ts,
;;; tree-sitter-scheme, and everything else come from the archive; the
;;; load-path and extension search paths come from Guile's own
;;; search-path declarations, so no manual GUILE_* exports are needed.

(use-modules (gnu packages)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages license)
             (gnu packages racket)
             (gnu packages texinfo)
             (gnu packages tree-sitter)
             (guix build-system gnu)
             (guix build-system guile)
             (guix build-system trivial)
             (guix gexp)
             (guix git-download)
             (guix search-paths)
             (guix utils)
             ((guix licenses) #:prefix license:)
             (guix packages))

(define guile (specification->package "guile"))
(define libunistring (specification->package "libunistring"))

;; SMathML is a Racket collection, rather than a standalone executable.
;; Keep its upstream revision fixed and expose the collection directory to
;; Racket through Guix's normal search-path activation.
(define racket-smathml
  (package
    (name "racket-smathml")
    (version "0-unstable-2026-09-04")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alpha-beta-eta/SMathML")
             (commit "20dfc39ddf60688870e54cfeaa6215297ea21d0c")))
       (file-name "racket-smathml-0-unstable-2026-09-04")
       (sha256
        (base32 "1kyxyjh33hg1jrhamyf04i6w2s8bingn6fx3dydfk4lbd7v0j56v"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (delete 'check)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((target (string-append (assoc-ref outputs "out")
                                           "/share/racket/collects/SMathML")))
                (mkdir-p target)
                (copy-recursively "." target)))))))
    (home-page "https://github.com/alpha-beta-eta/SMathML")
    (synopsis "Embedded Racket DSL for HTML and MathML")
    (description
     "SMathML is a Racket collection for constructing and transforming
HTML, MathML, and related web structures.  Grove exposes it in the
development environment without depending on it at runtime.")
    (license license:expat)))

;; Setting PLTCOLLECTS replaces Racket's usual collection roots.  Add the
;; VM's standard collections as another Guix-managed path so both the normal
;; Racket library and SMathML remain loadable in a pure dev shell.
(define racket-vm-collections
  (let* ((racket-package (specification->package "racket-minimal"))
         (vm (cadr (assoc "racket-vm-cs"
                          (package-inputs racket-package)))))
    (package
      (name "racket-vm-collections")
      (version (package-version vm))
      (source #f)
      (build-system trivial-build-system)
      (arguments
       (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (let ((target (string-append #$output
                                         "/share/racket/default-collects"))
                  (collections
                   (string-append #$(this-package-input "racket-vm-cs")
                                  "/opt/racket-vm/collects")))
              (mkdir-p (dirname target))
              (symlink collections target)))))
      (inputs (list vm))
      (native-search-paths
       (list (search-path-specification
              (variable "PLTCOLLECTS")
              (files '("share/racket/collects"
                       "share/racket/default-collects")))))
      (home-page "https://racket-lang.org/")
      (synopsis "Racket VM collection search path")
      (description
       "Expose the standard Racket VM collection directory through Guix's
PLTCOLLECTS search path, alongside additional Racket collections.")
      (license license:asl2.0))))

;; Guix's glibc-locales package installs locale data under lib/locale, but
;; does not expose GUIX_LOCPATH as a search path.  A pure shell therefore
;; needs a package that both brings in those data and declares their runtime
;; lookup path.
(define grove-glibc-locales
  (let ((locales (specification->package "glibc-locales")))
    (package
      (name "grove-glibc-locales")
      (version (package-version locales))
      (source #f)
      (build-system trivial-build-system)
      (arguments
       (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (let ((target (string-append #$output "/lib/locale"))
                  (data (string-append #$(this-package-input "locales")
                                       "/lib/locale")))
              (mkdir-p (dirname target))
              (symlink data target)))))
      (inputs (list (list "locales" locales)))
      (native-search-paths
       (list (search-path-specification
              (variable "GUIX_LOCPATH")
              (files '("lib/locale")))))
      (home-page "https://www.gnu.org/software/libc/")
      (synopsis "Expose Guix locale data to pure shells")
      (description
       "Expose the locale data supplied by glibc-locales through
GUIX_LOCPATH so locale-aware programs can use it in pure Guix shells.")
      (license license:lgpl2.1+))))

(define blue
  (package
    (name "blue")
    (version "0-unstable-2026-09-26")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/lapislazuli/blue")
             (commit "cbc0a0a65089b2cce2a8f572c97a6546054da833")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z0kq5mqqblq2dffsgy4fprcxrqm1m7kvxvpwkxcr5xhdgm2h584"))
       (patches
        (list (local-file "nix/pkgs/blue/blue-coverage.patch")
              (local-file "nix/pkgs/blue/blue-coverage-compile-tests.patch")
              (local-file "nix/pkgs/blue/blue-sandman-shutdown.patch")
              (local-file "nix/pkgs/blue/blue-coverage-api.patch")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (mkdir "build")
              (with-directory-excursion "build"
                (invoke "../bootstrap")
                (invoke "./pre-inst-env" "blue" "configure"
                        (string-append "--prefix=" #$output))
                (invoke "./pre-inst-env" "blue" "build"))))
          (replace 'install
            (lambda _
              (with-directory-excursion "build"
                (invoke "./pre-inst-env" "blue" "install")))))))
    (native-inputs (list guile texinfo))
    (home-page "https://codeberg.org/lapislazuli/blue")
    (synopsis "Generic build system crafted entirely in Guile")
    (description
     "Blue is a generic build system written in Guile.  Grove uses it to
drive its module builds and test suites from a blueprint file.")
    (license license:lgpl3+)))

(define faster-minikanren
  (package
    (name "guile-faster-minikanren")
    (version "0-unstable-2026-07-11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/michaelballantyne/faster-minikanren")
             (commit "a25c70209800ab781983ace5c6840784bb32b4da")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fpdbiybw97raz3ak855i8fcbwbzjimg3pwgixcs5960gfi5r2b8"))
       ;; mk-guile imports (rnrs) without a duplicates declaration, so
       ;; loading it warns twice per process about overriding core `map'
       ;; and `error'.  The rnrs bindings are meant to shadow the core
       ;; ones; declare it.
       (patches
        (list (local-file "nix/pkgs/faster-minikanren-duplicates.patch")))))
    (build-system guile-build-system)
    (arguments
     (list
      ;;     Only the seven Guile module files are installed; the test
      ;;     scripts and the Racket/Chez flavors stay out.  find-files
      ;;     matches base names only, so this is a whitelist.
      #:scheme-file-regexp
      "^(mk-guile|mk-vicare|mk|matche|full-interp|numbers|simple-interp)\\.scm$"
      ;;     The interpreters must compile while mk-guile is loaded,
      ;;     which guild cannot express; a phase below handles them.
      ;;     Unlike scheme-file-regexp this matches the whole relative
      ;;     path, so it stays end-anchored only.
      #:not-compiled-file-regexp
      "(full-interp|numbers|simple-interp)\\.scm$"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'stage-module-sources
            (lambda _
              ;;     The modules live at the source root under their own
              ;;     namespace prefix, which mk-guile's include-from-path
              ;;     expects on the load path.  Move (not copy) them so
              ;;     only the namespaced copies are installed.
              (mkdir-p "faster-minikanren")
              (for-each
               (lambda (file)
                 (rename-file file (string-append "faster-minikanren/" file)))
               '("mk-guile.scm" "mk-vicare.scm" "mk.scm" "matche.scm"
                 "full-interp.scm" "numbers.scm" "simple-interp.scm"))))
          (add-after 'build 'compile-interpreter-modules
            (lambda* (#:key outputs #:allow-other-keys)
              ;;     guile is on PATH (set-paths), which is also where
              ;;     the version probe falls back when given no path.
              (let* ((effective (target-guile-effective-version))
                     (out (assoc-ref outputs "out"))
                     (site (string-append out "/share/guile/site/"
                                          effective))
                     (go (string-append out "/lib/guile/" effective
                                        "/site-ccache")))
                (for-each
                 (lambda (module)
                   (invoke "guile" "--no-auto-compile"
                           "-L" site "-C" go "-c"
                           (string-append
                            "(use-modules (system base compile)"
                            " (faster-minikanren mk-guile))"
                            "(compile-file \"faster-minikanren/" module
                            ".scm\" #:output-file \"" go
                            "/faster-minikanren/" module
                            ".go\" #:env (current-module))")))
                 '("full-interp" "numbers" "simple-interp")))))
          (add-after 'compile-interpreter-modules 'check
            (lambda* (#:key outputs tests? #:allow-other-keys)
              (when tests?
                (let* ((effective (target-guile-effective-version))
                       (out (assoc-ref outputs "out"))
                       (site (string-append out "/share/guile/site/"
                                            effective))
                       (go (string-append out "/lib/guile/" effective
                                          "/site-ccache")))
                  (invoke "guile" "--no-auto-compile"
                          "-L" site "-C" go
                          "test-all-guile.scm"))))))))
    (native-inputs (list guile))
    (home-page "https://github.com/michaelballantyne/faster-minikanren")
    (synopsis
     "Fast miniKanren with disequality and absento for Guile")
    (description
     "A fast implementation of miniKanren with disequality and absento,
compatible with Racket, Chez, and Guile.  Grove uses its Guile flavor
(faster-minikanren mk-guile) to evaluate relation queries.")
    (license license:expat)))

(define grove-highlights
  ;;     The Scheme grammar and its highlight queries, wired into the
  ;;     (grove highlight languages) registry Grove reads at runtime.
  (let ((scheme-grammar tree-sitter-scheme))
    (package
      (name "grove-highlights")
      (version (package-version scheme-grammar))
      (source #f)
      (build-system trivial-build-system)
      (arguments
       (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (define grammar #$(this-package-input "tree-sitter-scheme"))
            (define grammar-source
              #$(package-source scheme-grammar))
            (define ts #$(this-package-input "guile-ts"))
            (define guile #$(this-package-native-input "guile"))
            (define out #$output)
            ;;     The manifest's Guile and the build Guile are both 3.0.
            (define effective #$(effective-version))
            (define site (string-append out "/share/guile/site/" effective))
            (for-each mkdir-p
                      (list (string-append out "/lib/tree-sitter")
                            (string-append out "/share/grove/queries/scheme")
                            (string-append out "/share/doc/grove-highlights")
                            (string-append site "/grove/highlight")))
            (symlink (string-append grammar "/lib/tree-sitter/"
                                    "libtree-sitter-scheme.so")
                     (string-append out "/lib/tree-sitter/scheme.so"))
            (copy-file (string-append grammar-source
                                      "/queries/highlights.scm")
                       (string-append out "/share/grove/queries/scheme/"
                                      "highlights.scm"))
            (copy-file (string-append grammar-source "/LICENSE")
                       (string-append out "/share/doc/grove-highlights/"
                                      "scheme-LICENSE"))
            (call-with-output-file
                (string-append site "/grove/highlight/languages.scm")
              (lambda (port)
                (format port ";;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
(define-module (grove highlight languages) #:export (languages))
(define languages
  '((scheme ~s \"tree_sitter_scheme\"
            ~s)))
"
                       (string-append out "/lib/tree-sitter/scheme.so")
                       (string-append out "/share/grove/queries/scheme/"
                                      "highlights.scm"))))
            ;;     Exercise the real Grove predicate compiler and the
            ;;     packaged parser, as the nix build does.
            (mkdir-p "test-modules/grove")
            (copy-file #$(local-file "modules/grove/highlight.scm")
                       "test-modules/grove/highlight.scm")
            (call-with-output-file "validate-highlights.scm"
              (lambda (port)
                (display "
(use-modules (grove highlight)
             (grove highlight languages)
             (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1))
(for-each
 (match-lambda
   ((name library entry query)
    (let* ((highlighter (make-highlighter library entry
                          (call-with-input-file query
                            (lambda (port)
                              (set-port-encoding! port \"UTF-8\")
                              (get-string-all port)))))
           (source \"(define 平方 2) ; 中文\")
           (pieces (highlight-code highlighter source)))
      (unless (and (string=? source
                             (apply string-append (map cdr pieces)))
                   (member (cons \"keyword\" \"define\") pieces))
        (error \"packaged grammar/query validation failed\" name)))))
 languages)
" port)))
            (invoke (string-append guile "/bin/guile") "--no-auto-compile"
                    "-L" "test-modules" "-L" site
                    "-L" (string-append ts "/share/guile/site/" effective)
                    "-C" (string-append ts "/lib/guile/" effective
                                        "/site-ccache")
                    "validate-highlights.scm"))))
      (native-inputs (list guile))
      (inputs (list scheme-grammar guile-ts))
      (home-page "https://github.com/6cdh/tree-sitter-scheme")
      (synopsis "Scheme grammar and highlight queries for Grove")
      (description
       "Packages the tree-sitter Scheme grammar with its highlight query
file and generates the (grove highlight languages) registry module that
Grove's highlighter consumes.")
      (license (list license:gpl3+ license:expat)))))

(packages->manifest
 (append
  (list blue faster-minikanren grove-highlights guile libunistring
        grove-glibc-locales
        racket-smathml racket-vm-collections)
  (map specification->package
       (list ;;     /bin/sh and env for subprocess-spawning code under a
             ;;     containerized shell.
             "bash"
             "coreutils"
             "guile-ssh"
             "guile-gcrypt"
             "guile-quickcheck"
             "guile-ts"
             ;; Optional tools for project-side integration experiments.
             "graphviz"
             "typst"
             "racket-minimal"
             "texlive-scheme-basic"
             "texlive-dvisvgm"
             "reuse"))))

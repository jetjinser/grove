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
             (gnu packages texinfo)
             (gnu packages tree-sitter)
             (guix build-system gnu)
             (guix build-system trivial)
             (guix gexp)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages))

(define guile (specification->package "guile"))

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
              (local-file "nix/pkgs/blue/blue-coverage-compile-tests.patch")))))
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
        (base32 "0fpdbiybw97raz3ak855i8fcbwbzjimg3pwgixcs5960gfi5r2b8"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((guix build gnu-build-system)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (let ((sources '("mk-guile.scm" "mk-vicare.scm" "mk.scm"
                               "matche.scm" "full-interp.scm" "numbers.scm"
                               "simple-interp.scm")))
                (mkdir-p "build/source/faster-minikanren")
                (mkdir-p "build/site-ccache/faster-minikanren")
                (for-each
                 (lambda (file)
                   (copy-file file
                              (string-append "build/source/faster-minikanren/"
                                             file)))
                 sources)
                (with-directory-excursion "build"
                  ;;     mk-guile compiles with plain guild.
                  (invoke "guild" "compile" "-L" "source"
                          "-o" "site-ccache/faster-minikanren/mk-guile.go"
                          "source/faster-minikanren/mk-guile.scm")
                  ;;     The interpreters must compile while mk-guile is
                  ;;     loaded, which guild cannot express.
                  (for-each
                   (lambda (module)
                     (invoke "guile" "--no-auto-compile"
                             "-L" "source" "-C" "site-ccache" "-c"
                             (format #f (string-append
                                         "(use-modules (system base compile)"
                                         " (faster-minikanren mk-guile))"
                                         "(compile-file \"source/faster-minikanren/~a.scm\""
                                         " #:output-file \"site-ccache/faster-minikanren/~a.go\""
                                         " #:env (current-module))")
                             module module)))
                   '("full-interp" "numbers" "simple-interp"))))))
          (replace 'check
            (lambda _
              (invoke "guile" "--no-auto-compile"
                      "-L" "build/source" "-C" "build/site-ccache"
                      "test-all-guile.scm")))
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (use-modules (ice-9 popen)
                           (ice-9 rdelim)
                           (srfi srfi-13))
              (let* ((effective
                      (string-trim-both
                       (read-line
                        (open-input-pipe
                         "guile -c '(display (effective-version))'"))))
                     (site (string-append #$output "/share/guile/site/"
                                          effective))
                     (ccache (string-append #$output "/lib/guile/"
                                            effective "/site-ccache")))
                (copy-recursively "build/source/faster-minikanren"
                                  (string-append site "/faster-minikanren"))
                (copy-recursively "build/site-ccache/faster-minikanren"
                                  (string-append ccache "/faster-minikanren"))))))))
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
  (list blue faster-minikanren grove-highlights guile)
  (map specification->package
       (list "guile-ssh"
             "guile-gcrypt"
             "guile-quickcheck"
             "guile-ts"
             "reuse"))))

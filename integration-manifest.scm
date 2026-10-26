;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Optional tools for running Grove's external-tool integration experiments.
;;; The LaTeX adapter needs the LaTeX engine/kernel, amsmath, and dvisvgm;
;;; the full basic scheme also installs unrelated TeX collections.
;;; Keep this separate from manifest.scm so ordinary builds and checks do not
;;; pull in TeX Live or the Racket toolchain.
;;;
;;;   guix shell -m manifest.scm -m integration-manifest.scm
;;;   blue check tests/unit/forester/hackable-math.scm

(use-modules (gnu packages)
             (guix build-system gnu)
             (guix build-system trivial)
             (guix gexp)
             (guix git-download)
             (guix search-paths)
             ((guix licenses) #:prefix license:)
             (guix packages))

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
HTML, MathML, and related web structures.  Grove exposes it in an optional
development environment without depending on it at runtime.")
    (license license:expat)))

;; Setting PLTCOLLECTS replaces Racket's usual collection roots.  Add the
;; VM's standard collections as another Guix-managed path so both the normal
;; Racket library and SMathML remain loadable in a pure shell.
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

(packages->manifest
 (append
  (list racket-smathml racket-vm-collections)
  (map specification->package
       '("graphviz"
         "typst"
         "racket-minimal"
         "texlive-latex-bin"
         "texlive-amsmath"
         "texlive-dvisvgm"))))

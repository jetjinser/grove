;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; The Grove package, loadable from manifest.scm.  blue drives the
;;; build: it configures, compiles the module and object trees, and
;;; installs the CLI wrapper with the dependency paths it sees at build
;;; time, so every dependency must be visible in the build environment.

(define* (make-grove blue guile faster-minikanren grove-highlights
                     guile-gcrypt guile-ssh guile-ts coreutils
                     #:key
                     (version "0-unstable-2026-10-10")
                     (source
                      (local-file (dirname (current-filename))
                                  "grove-checkout"
                                  #:recursive? #t
                                  #:select? (lambda (file stat)
                                              (not (member
                                                    (basename file)
                                                    '(".git" "build"
                                                      ".blue-store"
                                                      "result")))))))
  (package
    (name "grove")
    (version version)
    (source source)
    (build-system gnu-build-system)
    (arguments
     (list
      ;;     blue check runs in the development environment, where guile
      ;;     is on PATH and locales are pinned.
      #:tests? #f
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-13))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'set-load-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((effective #$(version-major+minor (package-version guile)))
                     (gcrypt #$(this-package-input "guile-gcrypt"))
                       (minikanren #$(this-package-input "guile-faster-minikanren"))
                       (ts #$(this-package-input "guile-ts"))
                       (highlights #$(this-package-input "grove-highlights"))
                       (ssh #$(this-package-input "guile-ssh"))
                       (site (lambda (out)
                               (string-append out "/share/guile/site/" effective)))
                       (ccache (lambda (out)
                                 (string-append out "/lib/guile/" effective
                                                "/site-ccache")))
                       (outs (list gcrypt minikanren ts highlights ssh)))
                  (setenv "GUILE_LOAD_PATH"
                          (string-join (map site outs) ":"))
                  (setenv "GUILE_LOAD_COMPILED_PATH"
                          (string-join (map ccache outs) ":"))
                  (setenv "GUILE_EXTENSIONS_PATH"
                          (string-append ssh "/lib/guile/" effective
                                         "/extensions")))))
          (replace 'build
            (lambda _
              ;;     blue 的编译器在运行期禁用自动编译，首次构建会与自己
              ;;     的 stage1 模块图竞争。先自己跑三轮 guild compile：
              ;;     失败容忍、逐轮逼近不动点，产物目录置于 compiled-path
              ;;     最先，blue 的 stage1 即可解析全部模块。guild 的依赖
              ;;     自动编译需要可写 HOME 且显式开启。
              (setenv "HOME" (or (getenv "TMPDIR") (getcwd)))
              (setenv "GUILE_AUTO_COMPILE" "1")
              (let ((prebuilt (string-append (getcwd) "/prebuilt")))
                  (setenv "GUILE_LOAD_COMPILED_PATH"
                          (string-append prebuilt ":"
                                         (getenv "GUILE_LOAD_COMPILED_PATH")))
                  (for-each
                   (lambda (_)
                     (for-each
                      (lambda (file)
                        (let* ((relative
                                (string-drop file
                                             (string-length "modules/")))
                               (go (string-append prebuilt "/"
                                                  (string-drop-right
                                                   relative 4)
                                                   ".go")))
                          (mkdir-p (dirname go))
                          (false-if-exception
                           (invoke "guild" "compile" "-L" "modules"
                                   "-o" go file))))
                      (find-files "modules" "\\.scm$")))
                   (iota 3)))
                (invoke "blue" "configure"
                        (string-append "prefix=" #$output)
                        (string-append "exec-prefix=" #$output)
                        (string-append "bindir=" #$output "/bin")
                        (string-append
                         "guile-module-dir=" #$output
                         "/share/guile/site/" #$(version-major+minor
                                                 (package-version guile)))
                        (string-append
                         "guile-object-dir=" #$output
                         "/lib/guile/" #$(version-major+minor
                                          (package-version guile))
                         "/site-ccache"))
                (invoke "blue" "build")))
            (replace 'install
              (lambda _
                (invoke "blue" "install")
                (wrap-program (string-append #$output "/bin/grove")
                  (list "PATH" "prefix"
                        (list (string-append #$(this-package-input "guile")
                                             "/bin:"
                                             #$(this-package-input "coreutils")
                                             "/bin")))))))))
      (native-inputs (list blue guile))
      (inputs
       (append
        (list guile faster-minikanren grove-highlights)
        (map specification->package
             (list "coreutils" "guile-gcrypt" "guile-ssh" "guile-ts"))))
      (synopsis "Static site generator for executable, forester-style notes")
      (description
       "Grove renders scribble-style trees into a connected site: analysis
builds a relation layer over pages, transclusions, and anchors, and
renderers turn it into HTML, text, RSS, and sitemaps.  Projects extend
it through hooks and miniKanren queries over those relations.")
      (home-page #f)
      (license license:gpl3+)))

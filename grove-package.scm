;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Factory for Grove's standalone Guix package, used by both the local
;;; package entry point and the (grove packages) channel module. Blue builds
;;; the modules and installs a CLI wrapper with the dependency paths it sees.

(define* (make-grove blue guile faster-minikanren grove-highlights libunistring
                     guile-gcrypt guile-ssh guile-ts coreutils guile-srfi-275
                     libidn2
                     #:key
                     (source-root (dirname (current-filename)))
                     (version "0-unstable-2026-11-01")
                     (source
                      ;;     过滤必须看相对根部的路径：basename 一刀切
                      ;;     会把 modules/grove/build/ 连同根部的构建
                      ;;     产物目录一起删掉，蓝图的模块树就缺角了。
                      (let ((root source-root))
                        (local-file root "grove-checkout"
                                    #:recursive? #t
                                    #:select?
                                    (lambda (file stat)
                                      (or (string=? file root)
                                          (let ((relative
                                                 (cond
                                                  ((string-prefix? root file)
                                                   (string-trim-both
                                                    (string-drop file
                                                                 (string-length
                                                                  root))
                                                    #\/))
                                                  (else file))))
                                            (not
                                             (or (member relative
                                                         '("" ".git"
                                                           ".direnv"
                                                           ".blue-store"
                                                           "result"))
                                                 (string-prefix?
                                                  ".git/" relative)
                                                 (string-prefix?
                                                  ".direnv/" relative)
                                                 (string-prefix?
                                                  ".blue-store/" relative)
                                                 (string-prefix?
                                                  "result/" relative)
                                                 ;;     build/：根部与
                                                 ;;     examples 的产物目
                                                 ;;     录不要；
                                                 ;;     modules/grove/build
                                                 ;;     是模块树，要留。
                                                 (and (or (string=? relative
                                                                    "build")
                                                          (string-suffix?
                                                           "/build"
                                                           relative))
                                                      (not (string-prefix?
                                                            "modules/"
                                                            relative))))))))))))
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
              (let* ((effective #$(version-major+minor
                                   (package-version guile)))
                     (blue #$(this-package-native-input "blue"))
                     (gcrypt #$(this-package-input "guile-gcrypt"))
                     (minikanren
                      #$(this-package-input "guile-faster-minikanren"))
                     (ts #$(this-package-input "guile-ts"))
                     (highlights #$(this-package-input "grove-highlights"))
                     (srfi275 #$(this-package-input "guile-srfi-275"))
                     (ssh #$(this-package-input "guile-ssh"))
                     (site (lambda (out)
                             (string-append out "/share/guile/site/"
                                            effective)))
                     (ccache (lambda (out)
                               (string-append out "/lib/guile/" effective
                                              "/site-ccache")))
                     (outs (list blue gcrypt minikanren ts highlights ssh
                                 srfi275)))
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
              ;;     的 stage1 模块图竞争。先自己跑 guild compile 把全部
              ;;     模块预热到 prebuilt/：编译一个模块时它的依赖经
              ;;     auto-compile 落进可写的 HOME 缓存，因此跳过已有产物
              ;;     的后续轮次只需补前一轮的失败者；最后一轮不再容忍
              ;;     失败，并断言 prebuilt 齐全——"no code for module"
              ;;     这类被 false-if-exception 吞掉的真实错误必须在这里
              ;;     现形，而不是留给 blue 的 stage1 去报含混的错。
              (setenv "HOME" (or (getenv "TMPDIR") (getcwd)))
              (setenv "GUILE_AUTO_COMPILE" "1")
              (let* ((prebuilt (string-append (getcwd) "/prebuilt"))
                     (sources (find-files "modules" "\\.scm$"))
                     (go-path
                      (lambda (file)
                        (string-append
                         prebuilt "/"
                         (string-drop-right
                          (string-drop file (string-length "modules/")) 4)
                         ".go"))))
                (setenv "GUILE_LOAD_COMPILED_PATH"
                        (string-append prebuilt ":"
                                       (getenv "GUILE_LOAD_COMPILED_PATH")))
                (for-each
                 (lambda (round)
                   (for-each
                    (lambda (file)
                      (let ((go (go-path file)))
                        (unless (file-exists? go)
                          (mkdir-p (dirname go))
                          (if (= round 2)
                              (invoke "guild" "compile" "-L" "modules"
                                      "-o" go file)
                              (false-if-exception
                               (invoke "guild" "compile" "-L" "modules"
                                       "-o" go file))))))
                    sources))
                 (iota 3))
                (for-each
                 (lambda (file)
                   (unless (file-exists? (go-path file))
                     (error "guild compile failed for module" file)))
                 sources)
                (invoke "blue" "configure"
                        (string-append "prefix=" #$output)
                        (string-append "exec-prefix=" #$output)
                        (string-append "bindir=" #$output "/bin")
                        (string-append
                         "guile-module-dir=" #$output
                         "/share/guile/site/"
                         #$(version-major+minor (package-version guile)))
                        (string-append
                         "guile-object-dir=" #$output
                         "/lib/guile/"
                         #$(version-major+minor (package-version guile))
                         "/site-ccache"))
                (invoke "blue" "build"))))
          (replace 'install
            (lambda _
              (invoke "blue" "install")
              (wrap-program (string-append #$output "/bin/grove")
                (list "PATH" ":" 'prefix
                      (list (string-append #$(this-package-input "guile")
                                           "/bin")
                            (string-append
                             #$(this-package-input "coreutils")
                             "/bin")
                            (string-append
                             #$(this-package-input "libidn2")
                             "/bin")))))))))
    (native-inputs (list blue guile))
    (inputs
     (append
      (list guile faster-minikanren grove-highlights libunistring
            libidn2)
      (map specification->package
           (list "coreutils" "guile-gcrypt" "guile-ssh" "guile-ts"))))
    ;; Project files and extensions can import SRFI 275 directly, and Grove's
    ;; own Forester IRI support uses it at runtime.
    (propagated-inputs (list guile-srfi-275))
    (synopsis "Static site generator for executable, forester-style notes")
    (description
       "Grove renders Scribble-style trees into connected sites. Analysis
builds relations over pages, transclusions, and anchors; renderers turn the
model into HTML, text, feeds, and other artifacts. Projects compose build
stages and renderers, and can query those relations with miniKanren.")
    (home-page #f)
    (license license:gpl3+)))

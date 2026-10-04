# SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
# SPDX-License-Identifier: GPL-3.0-or-later

{
  lib,
  runCommand,
  tree-sitter-grammars,
  guile,
  guile-ts,
}:

let
  scheme = tree-sitter-grammars.tree-sitter-scheme;
in
runCommand "grove-highlights-${scheme.version}"
  {
    nativeBuildInputs = [ guile ];
    meta = {
      description = "Scheme grammar and matching highlight queries for Grove";
      license = lib.toList scheme.meta.license ++ [ lib.licenses.gpl3Plus ];
      homepage = "https://github.com/6cdh/tree-sitter-scheme";
      platforms = lib.platforms.unix;
    };
  }
  ''
    mkdir -p "$out/lib/tree-sitter" "$out/share/grove/queries/scheme" \
      "$out/${guile.siteDir}/grove/highlight" "$out/share/doc/grove-highlights"
    ln -s ${scheme}/parser "$out/lib/tree-sitter/scheme.so"
    cp ${scheme}/queries/highlights.scm "$out/share/grove/queries/scheme/"
    cp ${scheme.src}/LICENSE "$out/share/doc/grove-highlights/scheme-LICENSE"
    cat > "$out/${guile.siteDir}/grove/highlight/languages.scm" <<EOF_MODULE
    ;;; SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
    ;;; SPDX-License-Identifier: GPL-3.0-or-later
    (define-module (grove highlight languages) #:export (languages))
    (define languages
      '((scheme "$out/lib/tree-sitter/scheme.so" "tree_sitter_scheme"
                "$out/share/grove/queries/scheme/highlights.scm")))
    EOF_MODULE

    # Exercise the actual Grove predicate compiler as well as parser loading.
    mkdir -p test-modules/grove
    cp ${../../modules/grove/highlight.scm} test-modules/grove/highlight.scm
    export GUILE_LOAD_PATH=${guile-ts}/${guile.siteDir}
    export GUILE_LOAD_COMPILED_PATH=${guile-ts}/${guile.siteCcacheDir}
    export GUILE_AUTO_COMPILE=0
    guile -L test-modules -L "$out/${guile.siteDir}" -c '
      (use-modules (grove highlight) (grove highlight languages)
                   (ice-9 match) (ice-9 textual-ports) (srfi srfi-1))
      (for-each
        (match-lambda
          ((name library entry query)
           (let* ((h (make-highlighter library entry
                       (call-with-input-file query
                         (lambda (port)
                           (set-port-encoding! port "UTF-8")
                           (get-string-all port)))))
                  (source "(define 平方 2) ; 中文")
                  (pieces (highlight-code h source)))
             (unless (and (string=? source (apply string-append (map cdr pieces)))
                          (member (cons "keyword" "define") pieces))
               (error "packaged grammar/query validation failed" name))))) languages)'
  ''

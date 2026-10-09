# SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{
  lib,
  stdenv,
  makeWrapper,
  blue,
  guile,
  coreutils,
  guile-gcrypt,
  faster-minikanren,
  guile-ts,
  grove-highlights,
  guile-ssh,
  src,
}:

let
  guile-modules = [
    guile-gcrypt
    faster-minikanren
    guile-ts
    grove-highlights
    guile-ssh
  ];
in
stdenv.mkDerivation {
  pname = "grove";
  version = "0-unstable-2026-10-10";
  inherit src;

  strictDeps = true;

  nativeBuildInputs = [
    blue
    guile
    makeWrapper
  ];
  buildInputs = [ guile ] ++ guile-modules;

  #     The blue-installed wrapper bakes the load and extension paths it
  #     sees at build time, so the dependency sites must be visible here;
  #     grove-highlights' registry also points at its own tree-sitter
  #     library, carried by its package output.
  GUILE_LOAD_PATH = lib.makeSearchPath guile.siteDir guile-modules;
  GUILE_COMPILED_PATH = lib.makeSearchPath guile.siteCcacheDir guile-modules;
  GUILE_EXTENSIONS_PATH = "${guile-ssh}/lib/guile/${lib.versions.majorMinor guile.version}/extensions";

  configurePhase = ''
    runHook preConfigure

    blue configure prefix=$out exec-prefix=$out bindir=$out/bin \
      guile-module-dir=$out/${guile.siteDir} \
      guile-object-dir=$out/${guile.siteCcacheDir}

    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild

    #     blue's stage1 compiler runs with auto-compilation disabled, so
    #     a fresh tree races its own module graph. Compile everything
    #     ourselves first: repeated passes reach a fixpoint as compiled
    #     dependencies accumulate, and the result goes first on the
    #     compiled path for blue's stage1.  HOME points at a writable
    #     directory so guild's dependency auto-compilation can cache.
    export HOME=$PWD/guile-home
    mkdir -p "$HOME"
    export GUILE_COMPILED_PATH=$PWD/prebuilt''${GUILE_COMPILED_PATH:+:$GUILE_COMPILED_PATH}
    for pass in 1 2 3; do
      for f in $(find modules -name '*.scm' | sort); do
        go="prebuilt/''${f#modules/}"
        go="''${go%.scm}.go"
        mkdir -p "$(dirname "$go")"
        guild compile -L modules -o "$go" "$f" || true
      done
    done

    blue build

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    blue install

    wrapProgram $out/bin/grove \
      --prefix PATH : ${lib.makeBinPath [ guile coreutils ]} \
      --set-default GUILE_AUTO_COMPILE 0

    runHook postInstall
  '';

  meta = {
    description = "Static site generator for executable, forester-style notes";
    longDescription = ''
      Grove renders scribble-style trees into a connected site: analysis
      builds a relation layer over pages, transclusions, and anchors, and
      renderers turn it into HTML, text, RSS, and sitemaps. Projects extend
      it through hooks and miniKanren queries over those relations.
    '';
    license = lib.licenses.gpl3Plus;
    mainProgram = "grove";
    platforms = lib.platforms.all;
  };
}

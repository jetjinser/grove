# SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{
  lib,
  stdenv,
  fetchFromGitHub,
  guile,
  nix-update-script,
}:

let
  guileSources = [
    "mk-guile.scm"
    "mk-vicare.scm"
    "mk.scm"
    "matche.scm"
    "full-interp.scm"
    "numbers.scm"
    "simple-interp.scm"
  ];
  guileSourceArgs = lib.escapeShellArgs guileSources;
in
stdenv.mkDerivation {
  pname = "faster-minikanren";
  version = "0-unstable-2026-07-11";
  __structuredAttrs = true;
  strictDeps = true;

  src = fetchFromGitHub {
    owner = "michaelballantyne";
    repo = "faster-minikanren";
    rev = "a25c70209800ab781983ace5c6840784bb32b4da";
    hash = "sha256-aIlconvApKJZj4/f8WqUf/HFHIqloKnGV/kkvnxc7To=";
  };

  # mk-guile imports (rnrs) without a duplicates declaration, so loading
  # it warns twice per process about overriding core `map' and `error'.
  # The rnrs bindings are meant to shadow the core ones; declare it.
  patches = [ ./faster-minikanren-duplicates.patch ];

  nativeBuildInputs = [ guile ];
  doCheck = true;

  buildPhase = ''
    runHook preBuild

    mkdir -p build/source/faster-minikanren
    cp ${guileSourceArgs} build/source/faster-minikanren/
    mkdir -p build/site-ccache/faster-minikanren
    guild compile \
      -L build/source \
      -o build/site-ccache/faster-minikanren/mk-guile.go \
      build/source/faster-minikanren/mk-guile.scm
    for module in full-interp numbers simple-interp; do
      source="build/source/faster-minikanren/$module.scm"
      output="build/site-ccache/faster-minikanren/$module.go"
      GUILE_AUTO_COMPILE=0 guile \
        -L build/source \
        -C build/site-ccache \
        -c "(use-modules (system base compile) (faster-minikanren mk-guile)) (compile-file \"$source\" #:output-file \"$output\" #:env (current-module))"
    done

    runHook postBuild
  '';

  checkPhase = ''
    runHook preCheck

    GUILE_AUTO_COMPILE=0 guile \
      -L build/source \
      -C build/site-ccache \
      test-all-guile.scm

    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall

    moduleDir="$out/${guile.siteDir}/faster-minikanren"
    ccacheDir="$out/${guile.siteCcacheDir}/faster-minikanren"
    mkdir -p "$moduleDir" "$ccacheDir"
    cp ${guileSourceArgs} "$moduleDir/"
    cp build/site-ccache/faster-minikanren/*.go "$ccacheDir/"

    runHook postInstall
  '';

  passthru.updateScript = nix-update-script { };

  meta = {
    description = "A fast implementation of miniKanren with disequality and absento, compatible with Racket, Chez, and Guile";
    homepage = "https://github.com/michaelballantyne/faster-minikanren";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ jinser ];
    mainProgram = "faster-minikanren";
    platforms = lib.platforms.all;
  };
}

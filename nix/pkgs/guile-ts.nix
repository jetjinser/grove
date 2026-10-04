# SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{
  lib,
  stdenv,
  fetchFromCodeberg,
  fetchFromGitHub,
  autoreconfHook,
  pkg-config,
  guile,
  texinfo,
}:

let
  # guile-ts still uses APIs removed in Tree-sitter 0.26. Only its C library
  # is needed here; keep the compatible dependency local to this package.
  tree-sitter = stdenv.mkDerivation {
    pname = "tree-sitter-library";
    version = "0.25.10";
    src = fetchFromGitHub {
      owner = "tree-sitter";
      repo = "tree-sitter";
      tag = "v0.25.10";
      hash = "sha256-aHszbvLCLqCwAS4F4UmM3wbSb81QuG9FM7BDHTu1ZvM=";
    };
    dontConfigure = true;
    makeFlags = [ "PREFIX=$(out)" ];
    meta.license = lib.licenses.mit;
  };
in
stdenv.mkDerivation {
  pname = "guile-ts";
  version = "0.3.0-unstable-2025-06-28";

  src = fetchFromCodeberg {
    owner = "Z572";
    repo = "guile-ts";
    rev = "767fb70d31fab223652f44497402a0acda4b9662";
    hash = "sha256-pWMKt/dGB0D6T2AjCHVrfeSuRk7jVXJY4Zt9/p5DuIc=";
  };

  strictDeps = true;
  nativeBuildInputs = [
    autoreconfHook
    pkg-config
    guile
    texinfo
  ];
  buildInputs = [
    guile
    tree-sitter
  ];

  doCheck = true;
  patches = [ ../patches/guile-ts-fix-sibling-tests.patch ];
  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    env -u LTDL_LIBRARY_PATH GUILE_AUTO_COMPILE=0 \
      guile -L "$out/${guile.siteDir}" -C "$out/${guile.siteCcacheDir}" \
      -c '(use-modules (ts) (oop goops)) (unless (is-a? (make <ts-parser>) <ts-parser>) (exit 1))'
    runHook postInstallCheck
  '';

  postPatch = ''
    # Retain the uninstalled library lookup used by upstream's build and tests,
    # then use an absolute store path when loading the installed modules.
    sed -i -E \
      's@\(load-extension "libguile_ts" "([^"]+)"\)@(or (false-if-exception (load-extension "libguile_ts" "\1")) (load-extension "${placeholder "out"}/lib/libguile_ts" "\1"))@g' \
      ts/*.scm
  '';

  meta = {
    description = "Guile bindings for Tree-sitter parsing and queries";
    homepage = "https://codeberg.org/Z572/guile-ts";
    license = lib.licenses.gpl3Plus;
    maintainers = with lib.maintainers; [ jinser ];
    platforms = lib.platforms.unix;
  };
}

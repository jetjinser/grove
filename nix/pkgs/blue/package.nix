# SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{
  lib,
  stdenv,
  fetchFromCodeberg,
  guile,
  texinfo,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "blue";
  version = "0-unstable-2026-09-26";

  src = fetchFromCodeberg {
    owner = "lapislazuli";
    repo = "blue";
    rev = "cbc0a0a65089b2cce2a8f572c97a6546054da833";
    hash = "sha256-BBUo6muwl8z65Hf3PU8NFefO8nXEP+1cE5gujGvBE3w=";
  };

  strictDeps = true;

  patches = [
    ./blue-coverage.patch
    ./blue-coverage-compile-tests.patch
    ./blue-sandman-shutdown.patch
  ];

  buildInputs = [ guile ];
  nativeBuildInputs = [
    guile
    texinfo
  ];

  buildPhase = ''
    runHook preBuild

    mkdir build

    pushd build
    ../bootstrap
    ./pre-inst-env blue configure --prefix=$out
    ./pre-inst-env blue build
    popd

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    pushd build
    ./pre-inst-env blue install
    popd

    runHook postInstall
  '';

  meta = {
    description = "A generic build-system crafted entirely in Guile";
    homepage = "https://codeberg.org/lapislazuli/blue";
    license = lib.licenses.lgpl3Plus;
    maintainers = with lib.maintainers; [ jinser ];
    mainProgram = "blue";
    platforms = lib.platforms.all;
  };
})

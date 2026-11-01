# SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{
  lib,
  stdenv,
  guile,
}:

stdenv.mkDerivation {
  pname = "guile-srfi-275";
  version = "0-unstable-2026-10-30";

  src = ../../srfi-275;

  strictDeps = true;

  nativeBuildInputs = [ guile ];
  buildInputs = [ guile ];

  dontConfigure = true;

  doCheck = true;
  checkPhase = ''
    runHook preCheck
    make check GUILE=${lib.getExe guile}
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    make install GUILE=${lib.getExe guile} PREFIX=$out DESTDIR=
    runHook postInstall
  '';

  meta = {
    description = "Guile implementation of SRFI 275";
    longDescription = ''
      A standalone Guile implementation of SRFI 275: URIs, IRIs, and basic
      paths.
    '';
    homepage = "https://srfi.schemers.org/srfi-275/";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ jinser ];
    platforms = lib.platforms.unix;
  };
}

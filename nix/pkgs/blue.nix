{
  lib,
  stdenv,
  fetchFromCodeberg,
  guile,
  texinfo,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "blue";
  version = "0-unstable-2026-05-28";

  src = fetchFromCodeberg {
    owner = "lapislazuli";
    repo = "blue";
    rev = "40ae4f5f1ec2536a0ede54636701688b8569b9bf";
    hash = "sha256-BsYoERsoj1ZepQi13ORRioq4iCLA3U7Wc9Srf9gKmSY=";
  };

  strictDeps = true;

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

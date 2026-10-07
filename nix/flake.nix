# SPDX-FileCopyrightText: 2026 Jinser Kafka <aimer@purejs.icu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

{
  description = "Grove SSG";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.devshell.flakeModule
      ];

      perSystem =
        {
          self',
          pkgs,
          lib,
          ...
        }:
        {
          packages = lib.packagesFromDirectoryRecursive {
            inherit (pkgs) callPackage;
            directory = ./pkgs;
          } // {
            grove-highlights = pkgs.callPackage ./pkgs/grove-highlights.nix {
              guile-ts = self'.packages.guile-ts;
            };
          };
          devshells.default = {
            packages = with pkgs; [
              guile
              (guile-ssh.overrideAttrs (old: {
                version = "1.0.0";
                src = fetchFromGitHub {
                  owner = "artyom-poptsov";
                  repo = "guile-ssh";
                  rev = "03cfa463a275803c64ee84168c3d126a65f65796";
                  hash = "sha256-gaCFQxn69KENqdET/331Bnznl9uNwgUO2xuIE4Zpxgo=";
                };
                nativeBuildInputs = old.nativeBuildInputs
                  ++ [ autoreconfHook pkg-config ];
              }))
              guile-gcrypt
              guile-quickcheck
              self'.packages.blue
              self'.packages.faster-minikanren
              self'.packages.guile-ts
              self'.packages.grove-highlights

              reuse
            ];
            env = [
              {
                name = "GUILE_LOAD_PATH";
                prefix = "$DEVSHELL_DIR/${pkgs.guile.siteDir}";
              }
              {
                name = "GUILE_LOAD_COMPILED_PATH";
                prefix = "$DEVSHELL_DIR/${pkgs.guile.siteCcacheDir}";
              }
              {
                name = "GUILE_EXTENSIONS_PATH";
                prefix = "$DEVSHELL_DIR/lib/guile/${builtins.baseNameOf pkgs.guile.siteDir}/extensions";
              }
            ];
          };
        };

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
    };
}

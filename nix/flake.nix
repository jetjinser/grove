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
          };
          devshells.default = {
            packages = with pkgs; [
              racket

              guile
              guile-gcrypt
              guile-quickcheck
              self'.packages.blue
              self'.packages.faster-minikanren

              reuse
            ];
            env = [
              {
                name = "GUILE_LOAD_PATH";
                prefix = "$DEVSHELL_DIR/${pkgs.guile.siteDir}";
              }
              {
                name = "GUILE_LOAD_PATH";
                prefix = "${pkgs.guile-gcrypt}/share/guile/site/3.0";
              }
              {
                name = "GUILE_LOAD_COMPILED_PATH";
                prefix = "$DEVSHELL_DIR/${pkgs.guile.siteCcacheDir}";
              }
              {
                name = "GUILE_LOAD_COMPILED_PATH";
                prefix = "${pkgs.guile-gcrypt}/lib/guile/3.0/site-ccache";
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

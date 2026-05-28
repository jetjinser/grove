{
  description = "Grove SSG";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

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
        { self', pkgs, ... }:
        {
          packages = {
            blue = pkgs.callPackage ./pkgs/blue.nix { };
          };
          devshells.default = {
            packages = with pkgs; [
              guile
              guile-quickcheck
              self'.packages.blue
            ];
            env = [
              {
                name = "GUILE_LOAD_PATH";
                prefix = "$DEVSHELL_DIR/${pkgs.guile.siteDir}";
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

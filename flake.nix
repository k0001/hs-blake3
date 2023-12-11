{
  description = "Haskell 'blake3' library";

  inputs = {
    flakety.url = "github:k0001/flakety";
    nixpkgs.follows = "flakety/nixpkgs";
    flake-parts.follows = "flakety/flake-parts";
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = inputs.nixpkgs.lib.composeManyExtensions [
        inputs.flakety.overlays.default
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = prev.lib.composeExtensions
              (prev.haskell.packageOverrides or (_: _: { }))
              (hself: hsuper: { blake3 = hself.callPackage ./blake3 { }; });
          };
        })
      ];
      systems = [ "x86_64-linux" ];
      perSystem = { config, system, pkgs, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.self.overlays.default ];
        };
        packages = {
          default = pkgs.releaseTools.aggregate {
            name = "every output from this flake";
            constituents = [
              config.devShells.ghc96
              config.packages.blake3__ghc96
              config.packages.blake3__ghc96.doc
              config.packages.blake3__ghc96__sdist
              config.packages.blake3__ghc96__sdistDoc

              config.devShells.ghc98
              config.packages.blake3__ghc98
              config.packages.blake3__ghc98.doc
              config.packages.blake3__ghc98__sdist
              config.packages.blake3__ghc98__sdistDoc
            ];
          };

          blake3__ghc96 = pkgs.haskell.packages.ghc96.blake3;
          blake3__ghc96__sdist =
            pkgs.haskell.packages.ghc96.cabalSdist { src = ./blake3; };
          blake3__ghc96__sdistDoc =
            pkgs.haskell.lib.documentationTarball config.packages.blake3__ghc96;

          blake3__ghc98 = pkgs.haskell.packages.ghc98.blake3;
          blake3__ghc98__sdist =
            pkgs.haskell.packages.ghc98.cabalSdist { src = ./blake3; };
          blake3__ghc98__sdistDoc =
            pkgs.haskell.lib.documentationTarball config.packages.blake3__ghc98;
        };
        devShells = let
          mkShellFor = ghc:
            ghc.shellFor {
              packages = p: [ p.blake3 ];
              withHoogle = false;
              nativeBuildInputs =
                [ pkgs.cabal-install pkgs.cabal2nix pkgs.ghcid ];
            };
        in {
          default = config.devShells.ghc98;
          ghc96 = mkShellFor pkgs.haskell.packages.ghc96;
          ghc98 = mkShellFor pkgs.haskell.packages.ghc98;
        };
      };
    };
}

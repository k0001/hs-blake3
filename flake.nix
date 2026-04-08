{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/46db2e09e1d3f113a13c0d7b81e2f221c63b8ce9";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    ram.flake = false;
    ram.url = "github:jappeace/ram/fb16285c2fc303894575757786cf8f87d183b88f";
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } (
      { withSystem, ... }:
      let
        # mapListToAttrs f [a b] = {a = f a; b = f b;}
        mapListToAttrs =
          f: xs:
          builtins.listToAttrs (
            builtins.map (x: {
              name = x;
              value = f x;
            }) xs
          );
        ghcVersions = [
          #"ghc967"
          #"ghc984"
          #"ghc9102"
          "ghc9122"
          #"ghc9141"
        ];
      in
      {
        systems = nixpkgs.lib.systems.flakeExposed;
        imports = [
          inputs.haskell-flake.flakeModule
        ];
        flake.haskellFlakeProjectModules = mapListToAttrs (
          ghc:
          (
            { pkgs, lib, ... }:
            withSystem pkgs.system (
              { config, ... }: config.haskellProjects.${ghc}.defaults.projectModules.output
            )
          )
        ) ghcVersions;
        perSystem =
          {
            self',
            pkgs,
            config,
            ...
          }:
          {
            haskellProjects = mapListToAttrs (ghc: {
              basePackages = pkgs.haskell.packages.${ghc};
              settings = {
                blake3 = {
                  check = true;
                  haddock = true;
                  libraryProfiling = true;
                };
              };
              packages = {
                ram.source = inputs.ram;
              };
              autoWire = [
                "packages"
                "checks"
                "devShells"
              ];
              devShell = {
                tools = hp: { inherit (pkgs) cabal2nix; };
              };
            }) ghcVersions;
            packages.default = pkgs.releaseTools.aggregate {
              name = "every output from this flake";
              constituents = builtins.concatMap (ghc: [
                config.devShells.${ghc}
                config.packages."${ghc}-blake3"
                config.packages."${ghc}-blake3".doc
              ]) ghcVersions;
            };
            packages.doc = self'.packages.ghc9122-blake3.doc;
            devShells.default = self'.devShells.ghc9122;
          };
      }
    );
}

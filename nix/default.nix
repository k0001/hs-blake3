let
  sources = import ./sources.nix;

  ghc-overrides = pkgs: self: super:
    let hs = pkgs.haskell.lib;
    in {
      blake3 = super.callPackage ../blake3/pkg.nix { };
      by = super.callPackage "${sources.by}/by/pkg.nix" { };

      _shell = super.shellFor {
        withHoogle = false;
        packages = p: [ p.blake3 ];
      };
    };

  pkgs-overlay = self: super: {
    _here = {
      ghc865 = super.haskell.packages.ghc865.override {
        overrides = ghc-overrides self;
      };
      ghc883 = super.haskell.packages.ghc883.override {
        overrides = ghc-overrides self;
      };
      ghc8101 = super.haskell.packages.ghc8101.override {
        overrides = ghc-overrides self;
      };
    };
  };

  pkgs = import sources.nixpkgs { overlays = [ pkgs-overlay ]; };

in pkgs

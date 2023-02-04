let pkgs = import ./nix;
in rec {
  ghc865 = pkgs._here.ghc865.blake3;
  ghc883 = pkgs._here.ghc883.blake3;
  ghc8101 = pkgs._here.ghc8101.blake3;
  ghc925 = pkgs._here.ghc925.blake3;
}

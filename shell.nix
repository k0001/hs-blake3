let pkgs = import ./nix;
in rec {
  ghc865 = pkgs._here.ghc865._shell;
  ghc883 = pkgs._here.ghc883._shell;
  ghc8101 = pkgs._here.ghc8101._shell;
  ghc925 = pkgs._here.ghc925._shell;
}

{ mkDerivation, base, stdenv, nix-gitignore, memory, tasty, tasty-hunit }:
mkDerivation {
  pname = "blake3";
  version = "0.3";
  src = nix-gitignore.gitignoreSourcePure ../.gitignore ./.;
  libraryHaskellDepends = [ base memory ];
  testHaskellDepends = [ base memory tasty tasty-hunit ];
  homepage = "https://github.com/k0001/blake3";
  description = "BLAKE3 hashing algorithm";
  license = stdenv.lib.licenses.asl20;
}

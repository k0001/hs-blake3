{ mkDerivation, base, by, stdenv, tasty, tasty-hunit }:
mkDerivation {
  pname = "blake3";
  version = "0.3";
  src = ./.;
  libraryHaskellDepends = [ base by ];
  testHaskellDepends = [ base by tasty tasty-hunit ];
  homepage = "https://github.com/k0001/hs-blake3";
  description = "BLAKE3 hashing algorithm";
  license = stdenv.lib.licenses.asl20;
}

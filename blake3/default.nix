{ mkDerivation, base, lib, memory, tasty, tasty-hunit }:
mkDerivation {
  pname = "blake3";
  version = "0.3.1";
  src = ./.;
  libraryHaskellDepends = [ base memory ];
  testHaskellDepends = [ base memory tasty tasty-hunit ];
  homepage = "https://github.com/k0001/hs-blake3";
  description = "BLAKE3 hashing algorithm";
  license = lib.licenses.asl20;
}

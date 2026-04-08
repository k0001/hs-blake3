{ mkDerivation, base, lib, ram, tasty, tasty-hunit }:
mkDerivation {
  pname = "blake3";
  version = "0.3.1";
  src = ./.;
  libraryHaskellDepends = [ base ram ];
  testHaskellDepends = [ base ram tasty tasty-hunit ];
  homepage = "https://github.com/k0001/hs-blake3";
  description = "BLAKE3 hashing algorithm";
  license = lib.licensesSpdx."Apache-2.0";
}

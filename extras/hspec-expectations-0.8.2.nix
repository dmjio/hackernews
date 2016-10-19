{ mkDerivation, base, call-stack, HUnit, nanospec, stdenv }:
mkDerivation {
  pname = "hspec-expectations";
  version = "0.8.2";
  sha256 = "1vxl9zazbaapijr6zmcj72j9wf7ka1pirrjbwddwwddg3zm0g5l1";
  libraryHaskellDepends = [ base call-stack HUnit ];
  testHaskellDepends = [ base call-stack HUnit nanospec ];
  doCheck = false;
  homepage = "https://github.com/hspec/hspec-expectations#readme";
  description = "Catchy combinators for HUnit";
  license = stdenv.lib.licenses.mit;
}

{ mkDerivation, base, call-stack, directory, hspec-core
, hspec-discover, hspec-expectations, hspec-meta, HUnit, QuickCheck
, stdenv, stringbuilder, transformers
}:
mkDerivation {
  pname = "hspec";
  version = "2.3.2";
  sha256 = "1d1g0cgm56yjzq5xd186w7kz5548dyp938cs59f99k45snfgclp8";
  libraryHaskellDepends = [
    base call-stack hspec-core hspec-discover hspec-expectations HUnit
    QuickCheck transformers
  ];
  testHaskellDepends = [
    base call-stack directory hspec-core hspec-discover
    hspec-expectations hspec-meta HUnit QuickCheck stringbuilder
    transformers
  ];
  doCheck = false;
  homepage = "http://hspec.github.io/";
  description = "A Testing Framework for Haskell";
  license = stdenv.lib.licenses.mit;
}

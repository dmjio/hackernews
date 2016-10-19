{ mkDerivation, ansi-terminal, async, base, call-stack, deepseq
, hspec-expectations, hspec-meta, HUnit, process, QuickCheck
, quickcheck-io, random, setenv, silently, stdenv, tf-random, time
, transformers
}:
mkDerivation {
  pname = "hspec-core";
  version = "2.3.2";
  sha256 = "1fa16mldzr4fjz8h7x1afrp8k8ngjh79wwxi6wlffkas8w3msv8w";
  libraryHaskellDepends = [
    ansi-terminal async base call-stack deepseq hspec-expectations
    HUnit QuickCheck quickcheck-io random setenv tf-random time
    transformers
  ];
  testHaskellDepends = [
    ansi-terminal async base call-stack deepseq hspec-expectations
    hspec-meta HUnit process QuickCheck quickcheck-io random setenv
    silently tf-random time transformers
  ];
  doCheck = false;
  homepage = "http://hspec.github.io/";
  description = "A Testing Framework for Haskell";
  license = stdenv.lib.licenses.mit;
}

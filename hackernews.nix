{ mkDerivation, aeson, base, hspec, http-client, http-client-tls
, servant, servant-client, stdenv, text, transformers, compiler, QuickCheck
, quickcheck-instances, nixpkgs, http-types, string-conversions, hspec-core
}:
let
 phantomjs  = nixpkgs.nodePackags.phantomjs;
 ghcjs-base = nixpkgs.haskell.packages.ghcjs.ghcjs-base;
 ghc-deps = [
    aeson base http-client servant servant-client text
    transformers http-client-tls http-types string-conversions
    quickcheck-instances QuickCheck
   ];
 ghcjs-deps = [ hspec-core QuickCheck
                ghcjs-base aeson base text
                transformers hspec servant quickcheck-instances
                string-conversions ];
 ghcjs-testdeps = [ phantomjs ] ++ ghcjs-deps;
 ghc-testdeps   = [ base hspec http-client-tls transformers
                    quickcheck-instances
                  ];
 testDeps =
   if compiler == "ghcjs"
     then ghcjs-testdeps
     else ghc-testdeps;
 exeDeps =
   if compiler == "ghcjs"
     then [ base ghcjs-base ]
     else [ base http-client-tls http-client ];
 libDeps =
   if compiler == "ghcjs"
     then ghcjs-deps
     else ghc-deps;
in mkDerivation {
  pname = "hackernews";
  version = "1.0.0.0";
  src = ./.;
  isExecutable = true;
  isLibrary = true;
  libraryHaskellDepends = libDeps;
  executableHaskellDepends = exeDeps;
  testHaskellDepends = testDeps;
  description = "API for Hacker News";
  license = stdenv.lib.licenses.mit;
}

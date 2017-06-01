{ mkDerivation, aeson, base, http-client, servant
, servant-client, http-client-tls, hspec-core, hspec
, stdenv, text, transformers, compiler, QuickCheck, semigroups
, quickcheck-instances, pkgs, http-types, string-conversions
}:
let
 isGhcjs = compiler == "ghcjs" || compiler == "ghcjsHEAD";
 phantomjs  = pkgs.nodePackags.phantomjs;
 ghcjs-base = pkgs.haskell.packages.ghcjs.ghcjs-base;
 ghc-deps = [
    aeson base http-client servant servant-client text
    transformers http-client-tls http-types string-conversions
    quickcheck-instances QuickCheck
   ];
 ghcjs-deps = [ hspec-core QuickCheck semigroups
                ghcjs-base aeson base text
                transformers hspec servant quickcheck-instances
                string-conversions ];
 ghcjs-testdeps = [ phantomjs ] ++ ghcjs-deps;
 ghc-testdeps   = [ base hspec http-client-tls transformers
                    quickcheck-instances
                  ];
 testDeps =
   if isGhcjs
     then ghcjs-testdeps
     else ghc-testdeps;
 exeDeps =
   if isGhcjs
     then [ base ghcjs-base ]
     else [ base http-client-tls http-client ];
 libDeps =
   if isGhcjs
     then ghcjs-deps
     else ghc-deps;
in mkDerivation {
  pname = "hackernews";
  version = "1.2.0.0";
  src = ./.;
  isExecutable = true;
  isLibrary = true;
  jailbreak = isGhcjs;
  libraryHaskellDepends = libDeps;
  executableHaskellDepends = exeDeps;
  testHaskellDepends = testDeps;
  description = "API for Hacker News";
  license = stdenv.lib.licenses.mit;
}

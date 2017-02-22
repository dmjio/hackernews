{ mkDerivation, aeson, base, http-client_0_5_3_2
, stdenv, text, transformers, compiler, QuickCheck, semigroups
, quickcheck-instances, nixpkgs, http-types, string-conversions
}:
let
 callHP = nixpkgs.haskell.packages."${compiler}".callPackage;
 hspec-discover = callHP ./extras/hspec-discover-2.3.2.nix { };
 hspec-expectations = callHP ./extras/hspec-expectations-0.8.2.nix { };
 hspec-core = callHP ./extras/hspec-core-2.3.2.nix { inherit hspec-expectations; };
 http-client = http-client_0_5_3_2;
 hspec = callHP ./extras/hspec-2.3.2.nix { inherit hspec-core hspec-expectations hspec-discover; };
 http-api-data = callHP ./extras/http-api-data-0.3.1.nix { };
 http-client-tls = callHP ./extras/http-client-tls-0.3.3.nix { inherit http-client; };
 servant = callHP ./extras/servant-0.9.0.1.nix { inherit http-api-data; };
 servant-client = callHP ./extras/servant-client-0.9.0.1.nix { inherit http-api-data servant http-client-tls http-client; };
 phantomjs  = nixpkgs.nodePackags.phantomjs;
 ghcjs-base = nixpkgs.haskell.packages.ghcjs.ghcjs-base;
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
 isGhcjs = compiler == "ghcjs" || compiler == "ghcjsHEAD";
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
  version = "1.1.1.0";
  src = ./.;
  isExecutable = true;
  isLibrary = true;
  libraryHaskellDepends = libDeps;
  executableHaskellDepends = exeDeps;
  testHaskellDepends = testDeps;
  description = "API for Hacker News";
  license = stdenv.lib.licenses.mit;
}

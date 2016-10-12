{ mkDerivation, aeson, base, hspec, http-client, http-client-tls
, servant, servant-client, stdenv, text, transformers, compiler
, quickcheck-instances, basic-sop, servant-quickcheck, nixpkgs, generics-sop
, http-types, string-conversions
}:
let
 ghcjs-base = nixpkgs.haskell.packages.ghcjs.ghcjs-base;
 lib = import "${nixpkgs.path}/pkgs/development/haskell-modules/lib.nix" { pkgs = nixpkgs; };
 basic-sop' = lib.dontHaddock basic-sop;
 ghc-deps = [
    aeson base http-client servant servant-client text
    transformers http-client-tls http-types string-conversions
   ];
 ghcjs-deps = [ ghcjs-base aeson base text transformers hspec ];
 ghc-testdeps = [ base hspec http-client-tls transformers
        quickcheck-instances
        servant-quickcheck generics-sop
        basic-sop' ];
 testDeps =
   if compiler == "ghcjs"
     then ghcjs-deps
     else ghc-testdeps;
 exeDeps =
   if compiler == "ghcjs"
     then [ base ghcjs-base ]
     else [ http-client-tls http-client ];
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

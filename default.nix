{ compiler ? "ghc802" }:
let
  config = { allowBroken = true; };
  pkgs = import <nixpkgs> { inherit config; };
in with pkgs.haskell.lib;
rec {
   hackernews = dontCheck (pkgs.haskell.packages.${compiler}.callPackage ./hackernews.nix { inherit compiler pkgs; });
   release = sdistTarball hackernews;
}

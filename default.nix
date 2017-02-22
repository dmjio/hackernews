{ compiler ? "ghc801" }:
let
  config = { allowBroken = true; };
  pkgs = import <nixpkgs> { inherit config; };
in
{
   hackernews = pkgs.haskell.packages.${compiler}.callPackage ./hackernews.nix { inherit compiler pkgs; };
}

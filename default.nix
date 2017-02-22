{ pkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
{
   hackernews = pkgs.haskell.packages.${compiler}.callPackage ./hackernews.nix { inherit compiler pkgs; };
}

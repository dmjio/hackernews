{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
    nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./hackernews.nix
      { inherit compiler nixpkgs; }

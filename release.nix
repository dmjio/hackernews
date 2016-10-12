{  }:
let
  pkgs = import <nixpkgs> {};
  hnpkgs = {
     hnGhcjs = pkgs.haskell.packages.ghcjs.callPackage ./default.nix { compiler = "ghcjs"; };
     hnGhc801 = pkgs.haskell.packages.ghc801.callPackage ./default.nix { compiler = "ghc801"; };
    };
in hnpkgs

{  }:
let
  pkgs = import <nixpkgs> {};
in {
     pkg = pkgs.haskell.packages.ghc801.callPackage ./../default.nix { compiler = "ghc801"; };
   }

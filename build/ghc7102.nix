{  }:
let
  pkgs = import <nixpkgs> {};
in {
     pkg = pkgs.haskell.packages.ghc7102.callPackage ./../default.nix { compiler = "ghc7102"; };
   }

{  }:
let
  pkgs = import <nixpkgs> {};
in {
     pkg = pkgs.haskell.packages.ghc763.callPackage ./../default.nix { compiler = "ghc763"; };
   }

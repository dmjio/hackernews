{  }:
let
  pkgs = import <nixpkgs> {};
in {
     pkg = pkgs.haskell.packages.ghc784.callPackage ./../default.nix { compiler = "ghc784"; };
   }

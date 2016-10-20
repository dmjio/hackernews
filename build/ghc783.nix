{  }:
let
  pkgs = import <nixpkgs> {};
in {
     pkg = pkgs.haskell.packages.ghc783.callPackage ./../default.nix { compiler = "ghc783"; };
   }

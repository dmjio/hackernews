{  }:
let
  pkgs = import <nixpkgs> {};
in {
     pkg = pkgs.haskell.packages.ghc7103.callPackage ./../default.nix { compiler = "ghc7103"; };
   }

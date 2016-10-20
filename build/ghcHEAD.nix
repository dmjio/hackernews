{  }:
let
  pkgs = import <nixpkgs> {};
in {
     pkg = pkgs.haskell.packages.ghcHEAD.callPackage ./../default.nix { compiler = "ghcHEAD"; };
   }

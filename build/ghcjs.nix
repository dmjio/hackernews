{  }:
let
  pkgs = import <nixpkgs> {};
in {
     pkg = pkgs.haskell.packages.ghcjs.callPackage ./../default.nix { compiler = "ghcjs"; };
   }

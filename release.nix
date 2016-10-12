{  }:
let
  pkgs = import <nixpkgs> {};
  hnpkgs = {
     hnGhcjs = pkgs.haskell.packages.ghcjs.callPackage ./default.nix { compiler = "ghcjs"; };
     hnGhc801 = pkgs.haskell.packages.ghc801.callPackage ./default.nix { compiler = "ghc801"; };
     hnGhc7103 = pkgs.haskell.packages.ghc7103.callPackage ./default.nix { compiler = "ghc7103"; };
     hnGhcLTS70 = pkgs.haskell.packages.lts-7_0.callPackage ./default.nix { compiler = "lts-7_0"; };
    };
in hnpkgs

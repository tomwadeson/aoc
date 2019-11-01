{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;

  ghcide = 
    (import (builtins.fetchTarball "https://github.com/hercules-ci/ghcide-nix/tarball/master") {})
      .ghcide-ghc865;
in
pkgs.stdenv.mkDerivation {
  name = "Haskell";
  buildInputs = with pkgs; [
    ghc
    cabal-install
    ghcide
    hlint
    ormolu
  ];
  shellHook = ''
    export NIX_GHC="${pkgs.ghc}/bin/ghc"
    export NIX_GHCPKG="${pkgs.ghc}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${pkgs.ghc}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
}

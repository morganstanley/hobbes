{ pkgs ? import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) {} }:

pkgs.stdenv.mkDerivation {
  name = "hobbes";
  src = ./.;

  nativeBuildInputs = with pkgs; [
    cmake
    gcc5
    llvm_6
    ncurses
    python27
    readline
    zlib
  ];

  doCheck = true;
  checkTarget = "test";
}

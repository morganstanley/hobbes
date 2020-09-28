{ pkgs ? import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/cc1ae9f21b9e0ce998e706a3de1bad0b5259f22d.tar.gz) {} }:

pkgs.stdenv.mkDerivation {
  name = "hobbes";
  src = ./.;

  nativeBuildInputs = with pkgs; [
    ccls
    cmake
    ninja
    meson
    gcc8
    llvm_6
    ncurses
    python27
    readline
    zlib
  ];

  doCheck = false;
  checkTarget = "test";
}

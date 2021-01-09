{ pkgs ? import (fetchTarball { url="https://github.com/NixOs/nixpkgs/archive/a3ab47ec9067b5f9fccda506fc8641484c3d8e73.tar.gz";
                                sha256="1f1q89ka73ksvl5hgbrf624x0niwm2rg3dzxiscir0bji6zvjy15";
                              }) {} }:
with pkgs;
let
  commonBuildInputs = [
    ccls
    cmake
    ninja
    meson
    ncurses
    python27
    readline
    zlib
  ];
  src = ./.;
  doCheck = true;
  checkTarget = "test";
  llvmSupportVersion = [ 6 8 9 10 ];
  defaultGCC = gcc10;
  hobbesDrv = version: gccv: stdenv.mkDerivation {
    pname = "hobbes-llvm";
    version = builtins.toString version;
    inherit src doCheck checkTarget;
    nativeBuildInputs = commonBuildInputs ++ [ gccv (builtins.getAttr ("llvm_" + (builtins.toString version)) pkgs) ];
  };
in
  builtins.listToAttrs (builtins.map (v: { name = "hobbes-llvm-" + (toString v); value = hobbesDrv v defaultGCC; }) llvmSupportVersion) // 
  {
    hobbes = stdenv.mkDerivation {
      name = "hobbes";
      inherit src doCheck checkTarget;
      nativeBuildInputs = commonBuildInputs ++ [ gcc8 llvm_6 ];
    };
  }

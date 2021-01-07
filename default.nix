{ pkgs ? import (fetchTarball { url="https://github.com/NixOs/nixpkgs/archive/a3ab47ec9067b5f9fccda506fc8641484c3d8e73.tar.gz";
                                sha256="1f1q89ka73ksvl5hgbrf624x0niwm2rg3dzxiscir0bji6zvjy15";
                              }) {},
}:
let
  commonBuildInputs = with pkgs; [
    ncurses
    python27
    readline
    zlib
  ];
  commonNativeInputs = with pkgs; [
    ccls
    cmake
    ninja
    meson
    python27
  ];
  src = ./.;
  doCheck = true;
  checkTarget = "test";
  llvmSupportVersions = [ 6 8 9 10 11 ];
  hobbesDrv = with pkgs;
    makeOverridable ({ version,
                       pks ? pkgs,
                       stdenv ? pks.stdenv,
                       gcc ? pks.gcc10
                     }: stdenv.mkDerivation {
                       pname = "hobbes-llvm-" + (builtins.toString version);
                       version = "unstable";
                       inherit
                         src
                         doCheck
                         checkTarget;
                       nativeBuildInputs = commonNativeInputs ++ [ gcc ];
                       buildInputs = commonBuildInputs ++ [ (builtins.getAttr ("llvm_" + (builtins.toString version)) pkgs) ];
                       postPatch = ''
                         substituteInPlace CMakeLists.txt \
                         --replace "\''${CMAKE_SOURCE_DIR}" "${src}"
                       '';
                     });
in with pkgs; recurseIntoAttrs (rec {
  hobbesPackages = recurseIntoAttrs (builtins.listToAttrs (builtins.map (version: {
    name = "llvm-" + (toString version);
    value = recurseIntoAttrs {
      hobbes = callPackage hobbesDrv {
        inherit
          version;
      };
    };
  }) llvmSupportVersions));
  hobbes = callPackage hobbesPackages.llvm-6.hobbes.override { gcc = gcc8; };
})

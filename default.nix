{ nixpkgs ? import (fetchTarball { url="https://github.com/NixOs/nixpkgs/archive/a3ab47ec9067b5f9fccda506fc8641484c3d8e73.tar.gz";
                                   sha256="1f1q89ka73ksvl5hgbrf624x0niwm2rg3dzxiscir0bji6zvjy15";
                                 }),
}:
let
  overlays = [
    (import ./nix/overlays.nix {
      version = "unstable";
      src = ./.;
      llvmVersions = [ 6 8 9 10 11 ];
      gccConstraints = [
        { gccVersion = 6; llvmVersions = [ 6 8 9 ]; }
        { gccVersion = 8; llvmVersions = [ 6 8 9 10 11 ]; }
        { gccVersion = 9; llvmVersions = [ 6 8 9 10 11 ]; }
        { gccVersion = 10; llvmVersions = [ 6 8 9 10 11 ]; }
      ];
    })
  ];
  
  pkgs = nixpkgs {
    inherit overlays;
  };
in with pkgs; recurseIntoAttrs {
  inherit hobbesPackages;
  inherit (hobbesPackages.gcc-8.llvm-6) hobbes;
}

{
  description = "A language and an embedded JIT compiler";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" ] (system:
      let
        overlays = [
          (import ./nix/overlays.nix {
            inherit system;
            version = "${nixpkgs.lib.substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
            src = self;
            llvmVersions = [ 16 18 20 21 ];
            gccConstraints = [
              { gccVersion = 13; llvmVersions = [ 16 18 20 21 ]; }
              { gccVersion = 14; llvmVersions = [ 16 18 20 21 ]; }
              { gccVersion = 15; llvmVersions = [ 16 18 20 21 ]; }
            ];
          })
        ];

        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
        rec {
          packages = flake-utils.lib.flattenTree (pkgs.recurseIntoAttrs {
            inherit (pkgs) hobbesPackages;
          });
          defaultPackage = packages."hobbesPackages/clang-18/hobbes";
        });
}

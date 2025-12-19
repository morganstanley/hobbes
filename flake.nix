{
  description = "A language and an embedded JIT compiler";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [
          (import ./nix/overlays.nix {
            inherit system;
            version = "${nixpkgs.lib.substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
            src = self;
            llvmVersions = [ 8 9 10 11 12 16 ];
            gccConstraints = [
              { gccVersion = 8; llvmVersions = [ 8 9 10 11 12 16 ]; }
              { gccVersion = 9; llvmVersions = [ 8 9 10 11 12 16 ]; }
              { gccVersion = 10; llvmVersions = [ 8 9 10 11 12 16 ]; }
              { gccVersion = 12; llvmVersions = [ 8 9 10 11 12 16 ]; }
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
          defaultPackage = packages."hobbesPackages/clang-8/hobbes";
        });
}

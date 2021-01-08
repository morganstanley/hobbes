{
  description = "A language and an embedded JIT compiler";
  
  inputs.flake-utils.url = "github:numtide/flake-utils";
  
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        version = "${nixpkgs.lib.substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
        overlays = [
          (final: prev:
            with final;
            let
              src = self;
              nativeBuildInputs = [ cmake ninja ];
              buildInputs = [ ncurses readline zlib ];
              doCheck = true;
              doTarget = "test";
              meta = with stdenv.lib; {
                description = "A language and an embedded JIT compiler";
                longDescription = ''
                  Hobbes is a language, embedded compiler, and runtime for efficient
                  dynamic expression evalution, data storage and analysis.
                '';
                license = licenses.asl20;
                maintainers = with maintainers; [ kthielen thmzlt smunix ];
              };
            in {
              hobbes-llvm-x = stdenv.mkDerivation {
                name = "hobbes-llvm-x-${version}";
                inherit src nativeBuildInputs doCheck doTarget;
                buildInputs = buildInputs;
              };
              hobbes-llvm-6 = stdenv.mkDerivation {
                name = "hobbes-llvm-6-${version}";
                inherit src nativeBuildInputs doCheck doTarget;
                buildInputs = buildInputs ++ [ llvm_6 ];
              };
              hobbes-llvm-8 = stdenv.mkDerivation {
                name = "hobbes-llvm-8-${version}";
                inherit src nativeBuildInputs doCheck doTarget;
                buildInputs = buildInputs ++ [ llvm_8 ];
              };
            })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
        rec {
          packages = flake-utils.lib.flattenTree {
            inherit (pkgs) hobbes-llvm-x;
            inherit (pkgs) hobbes-llvm-6;
            inherit (pkgs) hobbes-llvm-8;
          };
          defaultPackage = packages.hobbes-llvm-6;
        });
}

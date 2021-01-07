{
  description = "Hobbes Compiler";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      version = "${nixpkgs.lib.substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
      overlays = [
        (final: prev: {
          hobbes-master =
            with final;
            stdenv.mkDerivation {
              name = "hobbes-master-${version}";
              src = ./.;
              nativeBuildInputs = [ cmake ninja ];
              buildInputs = [ llvm_6 ncurses readline zlib ];
              doCheck = false;
              doTarget = "test";
            };
          hobbes-compat =
            with final;
            stdenv.mkDerivation {
              name = "hobbes-compat-${version}";
              src = fetchFromGitHub { owner="smunix";
                                      repo="hobbes";
                                      rev="046ad1f631c90b8d70b3f14675dbe702f6343434";
                                      sha256="0zhd0v2gjj67r69qwvn6fbjhmnbnaikshdvq53q9vqvfipi42lqz";
                                    };
              nativeBuildInputs = [ cmake ninja ];
              buildInputs = [ llvm_6 ncurses readline zlib ];
              doCheck = false;
              doTarget = "test";
            };
        })
      ];
    in
      {
        packages.x86_64-linux = import nixpkgs {
          inherit system overlays;
        };
        defaultPackage.x86_64-linux = (import nixpkgs {
          inherit system overlays;
        }).hobbes-master;
    };
}

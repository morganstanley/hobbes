{
  description = "A language and an embedded JIT compiler";
  
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        LLVMs = [ 6 8 9 10 ];
        version = "${nixpkgs.lib.substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
        overlays = [
          (final: prev:
            with final;
            let
              src = self;
              nativeBuildInputs = [ cmake ninja ];
              buildInputs = [ ncurses readline zlib python27 readline zlib ];
              doCheck = true;
              doTarget = "test";
              dontStrip = true;
              separateDebugInfo = true;
              meta = with stdenv.lib; {
                description = "A language and an embedded JIT compiler";
                longDescription = ''
                  Hobbes is a language, embedded compiler, and runtime for efficient
                  dynamic expression evalution, data storage and analysis.
                '';
                license = licenses.asl20;
                maintainers = with maintainers; [ kthielen thmzlt smunix ];
              };
              withLLVM = v : stdenv.mkDerivation {
                pname = "hobbes-llvm-" + (toString v);
                inherit version src nativeBuildInputs meta doCheck doTarget dontStrip;
                buildInputs = buildInputs ++ [ (builtins.getAttr ("llvm_" + (toString v)) final) ];
              };
            in
              builtins.listToAttrs (
                builtins.map
                  (v: { name="hobbes-llvm-" + toString v;
                        value = withLLVM v;
                      })
                  LLVMs))
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
        rec {
          packages = flake-utils.lib.flattenTree
            (builtins.listToAttrs (
              builtins.map (v: rec {
                name="hobbes-llvm-" + toString v;
                value=(builtins.getAttr name pkgs);
              })
                LLVMs)
            );
          defaultPackage = packages.hobbes-llvm-6;
        });
}

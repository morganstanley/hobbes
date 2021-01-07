{
  description = "A language and an embedded JIT compiler";
  
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        # LLVM supported versions
        llvmSupportedVersions = [ 6 8 9 10 11 ];
        
        debug = false;
        
        version = "${nixpkgs.lib.substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
        
        overlays = [
          (final: prev:
            with final;
            let
              dbg = if debug == true
                    then enableDebugging
                    else (x: x);
              
              src = self;
              
              nativeBuildInputs = [ cmake ninja python27 ];
              
              buildInputs = [ ncurses readline zlib python27 ];
              
              doCheck = true;
              
              doTarget = "test";
              
              dontStrip = debug;
              
              separateDebugInfo = debug;
              
              meta = with stdenv.lib; {
                description = "A language and an embedded JIT compiler";
                longDescription = ''
                  Hobbes is a language, embedded compiler, and runtime for efficient
                  dynamic expression evalution, data storage and analysis.
                '';
                license = licenses.asl20;
                maintainers = with maintainers; [ kthielen thmzlt smunix ];
              };
              
              withLLVM = makeOverridable ({ v, stdenv ? final.stdenv, gcc ? final.gcc } : stdenv.mkDerivation {
                pname = "hobbes-llvm-" + (toString v);
                inherit version src meta doCheck doTarget dontStrip;
                nativeBuildInputs = nativeBuildInputs ++ [ gcc ];
                buildInputs = buildInputs ++ [ (builtins.getAttr ("llvm_" + (toString v)) final) ];
                postPatch = ''
                  substituteInPlace CMakeLists.txt \
                     --replace "\''${CMAKE_SOURCE_DIR}" "${src}"
                '';
              });
            in { hobbesPackages = recurseIntoAttrs (builtins.listToAttrs (
                   builtins.map
                     (v: { name="llvm-" + toString v;
                           value = recurseIntoAttrs ({ hobbes = dbg (callPackage withLLVM { inherit v; gcc = gcc10; }); });
                         })
                     llvmSupportedVersions));
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
          defaultPackage = packages."hobbesPackages/llvm-6/hobbes";
        });
}

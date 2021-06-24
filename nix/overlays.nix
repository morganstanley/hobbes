{ src,
  version,
  llvmVersions,
  gccConstraints,
  system,
  debug ? true,
}: final: prev:
with final;
let
  dbg = if debug == true
        then enableDebugging
        else (x: x);
  
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

  darwinOnly = v : if system == "x86_64-darwin" then v else {};

  linuxOnly = v : if system == "x86_64-linux" then v else {};
  
  when = c: m: if c then m else {};
  
  withGCC = { gccVersion ? 10 }:
    let gccPkgs = { gccVersion }: builtins.getAttr ("gcc" + (toString gccVersion) + "Stdenv") final;
        llvmPkgs = { llvmVersion }: builtins.getAttr ("llvmPackages_" + (toString llvmVersion)) final;                    
    in makeOverridable ({ llvmVersion, stdenv ? (gccPkgs { inherit gccVersion; }) } : stdenv.mkDerivation {
      pname = "hobbes-gcc-" + toString gccVersion + "-llvm-" + toString llvmVersion;
      inherit version src meta doCheck doTarget dontStrip;
      nativeBuildInputs = nativeBuildInputs;
      buildInputs = buildInputs ++ [ (llvmPkgs { inherit llvmVersion; }).llvm ];
      postPatch = ''
                  substituteInPlace CMakeLists.txt \
                     --replace "\''${CMAKE_SOURCE_DIR}" "${src}"
                '';
    });
  
  withCLANG =
    let llvmPkgs = { llvmVersion }: builtins.getAttr ("llvmPackages_" + (toString llvmVersion)) final;
    in makeOverridable ({ llvmVersion, stdenv ? (llvmPkgs { inherit llvmVersion; }).stdenv } : stdenv.mkDerivation {
      pname = "hobbes-clang-" + (toString llvmVersion);
      inherit version src meta doCheck doTarget dontStrip;
      nativeBuildInputs = nativeBuildInputs;
      buildInputs = buildInputs ++ [ (llvmPkgs { inherit llvmVersion; }).llvm ];
      postPatch = ''
                  substituteInPlace CMakeLists.txt \
                     --replace "\''${CMAKE_SOURCE_DIR}" "${src}"
                '';
    });

in { hobbesPackages = when stdenv.isLinux (recurseIntoAttrs (builtins.listToAttrs (builtins.map (gccConstraint: {
       name = "gcc-" + toString gccConstraint.gccVersion;
       value = recurseIntoAttrs (builtins.listToAttrs (builtins.map (llvmVersion: {
         name = "llvm-" + toString llvmVersion;
         value = recurseIntoAttrs ({ hobbes = dbg (callPackage (withGCC { inherit (gccConstraint) gccVersion; }) { inherit llvmVersion; }); });
       }) gccConstraint.llvmVersions));
     }) gccConstraints)))
     // recurseIntoAttrs (builtins.listToAttrs (
       builtins.map
         (llvmVersion: { name="clang-" + toString llvmVersion;
                         value = recurseIntoAttrs ({ hobbes = dbg (callPackage withCLANG { inherit llvmVersion; }); });
                       })
         llvmVersions));
   }

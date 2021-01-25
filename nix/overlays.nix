{ src, version, llvmVersions, gccConstraints, system, debug ? false, }:
final: prev:
with final;
let
  dbg = if debug == true then enableDebugging else (x: x);

  nativeBuildInputs = [ cmake ninja python27 ];

  buildInputs = [ ncurses readline zlib python27 ];

  cmakeFlags = [ "-GNinja" ];

  dontUseNinjaBuild = true;

  dontUseNinjaInstall = true;

  dontUseNinjaCheck = true;

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

  when = c: m: if c then m else { };

  withGCC = { gccVersion ? 10 }:
    let
      gccPkgs = { gccVersion }:
        builtins.getAttr ("gcc" + (toString gccVersion) + "Stdenv") final;
      llvmPkgs = { llvmVersion }:
        builtins.getAttr ("llvmPackages_" + (toString llvmVersion)) final;
    in makeOverridable
    ({ llvmVersion, stdenv ? (gccPkgs { inherit gccVersion; }) }:
      stdenv.mkDerivation {
        pname = "hobbes-gcc-" + toString gccVersion + "-llvm-"
          + toString llvmVersion;
        inherit version src meta doCheck doTarget dontStrip cmakeFlags
          dontUseNinjaBuild dontUseNinjaInstall dontUseNinjaCheck;
        nativeBuildInputs = nativeBuildInputs;
        buildInputs = buildInputs
          ++ [ (llvmPkgs { inherit llvmVersion; }).llvm ];
        postPatch = ''
          substituteInPlace CMakeLists.txt \
             --replace "\''${CMAKE_SOURCE_DIR}" "${src}"
        '';
        buildPhase = ''
          ${shake}/bin/shake --color --report --timings -j -f build.ninja
        '';
      });

  withCLANG = let
    llvmPkgs = { llvmVersion }:
      builtins.getAttr ("llvmPackages_" + (toString llvmVersion)) final;
  in makeOverridable
  ({ llvmVersion, stdenv ? (llvmPkgs { inherit llvmVersion; }).stdenv }:
    stdenv.mkDerivation {
      pname = "hobbes-clang-" + (toString llvmVersion);
      inherit version src meta doCheck doTarget dontStrip cmakeFlags
        dontUseNinjaBuild dontUseNinjaInstall dontUseNinjaCheck;
      nativeBuildInputs = nativeBuildInputs;
      buildInputs = buildInputs ++ [ (llvmPkgs { inherit llvmVersion; }).llvm ];
      postPatch = ''
        substituteInPlace CMakeLists.txt \
           --replace "\''${CMAKE_SOURCE_DIR}" "${src}"
      '';
      buildPhase = ''
        ${shake}/bin/shake --color --report --timings -j -f build.ninja
      '';
      checkPhase = ''
        echo $(pwd)
        ls $(echo $(pwd))
        ./hobbes-test
      '';
      installPhase = ''
        make install
      '';
    });
  
  hobbesbuild = with haskellPackages; when stdenv.isDarwin (callCabal2nix "hobbesbuild" "${src}/shake" { inherit shake shake-language-c; });

  build = makeOverridable ({pkg, zlib, ncurses, readline, llvm ? llvm_8, clang ? clang_8, pkgconfig }: stdenv.mkDerivation {
    pname = "build";
    inherit version;
    src = "${pkg.src}";
    propagatedBuildInputs = [ hobbesbuild cabal-install xcbuild zlib ncurses readline llvm clang pkgconfig ];
    buildPhase = ''
    echo $(pwd)
    ls .
    ${hobbesbuild}/bin/hobbesbuild
    '';
    installPhase = ''
      mkdir -p "$out"
    '';
  });
in rec {
  hobbesPackages = when stdenv.isLinux (recurseIntoAttrs (builtins.listToAttrs
    (builtins.map (gccConstraint: {
      name = "gcc-" + toString gccConstraint.gccVersion;
      value = recurseIntoAttrs (builtins.listToAttrs (builtins.map
        (llvmVersion: {
          name = "llvm-" + toString llvmVersion;
          value = recurseIntoAttrs ({
            hobbes = dbg
              (callPackage (withGCC { inherit (gccConstraint) gccVersion; }) {
                inherit llvmVersion;
              });
          });
        }) gccConstraint.llvmVersions));
    }) gccConstraints))) // recurseIntoAttrs (builtins.listToAttrs (builtins.map
      (llvmVersion: {
        name = "clang-" + toString llvmVersion;
        value = recurseIntoAttrs ({
          hobbes = dbg (callPackage withCLANG { inherit llvmVersion; });
        });
      }) llvmVersions)) // recurseIntoAttrs {
        inherit
          zlib
          ncurses
          readline
          llvm_8
          clang_8
        ;
        inherit (haskellPackages)
          shake
          shake-language-c
        ;
        # build = with haskellPackages; when stdenv.isDarwin (callCabal2nix "hobbesbuild" "${src}/shake" { inherit shake shake-language-c; });
        build = callPackage build { pkg = hobbesPackages.clang-8.hobbes; inherit zlib ncurses readline pkgconfig; };
      };
}

# ConanCenter recipe (draft)

A staging copy of a recipe for [ConanCenter](https://github.com/conan-io/conan-center-index), laid out as it is there (`recipes/hobbes/`).  It was submitted as [conan-io/conan-center-index#31555](https://github.com/conan-io/conan-center-index/pull/31555), which has not been reviewed yet.  It packages the hobbes 1.0.0 release archive; the recipe at the root of this repository (`conanfile.py`) packages a checkout.

```
$ cd packaging/conan-center/recipes/hobbes/all
$ conan create . --version=1.0.0 --build="missing:~llvm-core/*"
```

(with zlib pinned to 1.3.1 in the profile, as "Conan" in the top-level README explains.)

## How it differs from the recipe at the root

| | root `conanfile.py` | this recipe | why |
|---|---|---|---|
| Sources | `exports_sources` from the checkout | `get()` of the release archive, with its sha256 from the release's `SHA256SUMS` | ConanCenter builds from published sources |
| Version | `set_version()` falls back to the commit | none; the version comes from `config.yml` | ConanCenter recipes must not set a version |
| LLVM | `llvm-core/[>=8 <23]` | `llvm-core/19.1.7` | version ranges are allowed for a short list of packages only (zlib is one, LLVM is not) |
| `with_system_llvm`, `with_system_readline` | options | removed | only ConanCenter recipes may be required; using the machine's libraries is for "exceptional cases" and separate system-package recipes |
| LLVM version check in `validate()` | checks the major version | removed | the version is pinned |
| CMake | whatever is installed | `tool_requires("cmake/[>=3.19 <4]")` | ConanCenter assumes CMake 3.15; hobbes needs 3.19 |
| Layout | `cmake_layout(self)` | `cmake_layout(self, src_folder="src")` | ConanCenter convention |
| `url` | this repository | conan-center-index | ConanCenter convention; `homepage` is this repository |
| `lib/hobbes-llvm.json` | LLVM's libraries, directories, system libraries and defines | LLVM's libraries only | the rest was for an LLVM that is not a Conan package |

`test_package/` is the same but for its minimum CMake version (3.15) and a `tool_requires("cmake/[>=3.20 <4]")`: LLVM's CMake files call `cmake_path()`, and ConanCenter's Linux image has CMake 3.15.

## What a ConanCenter reviewer may still object to

* **`lib/hobbes-llvm.json`.**  `package()` asks `llvm-config` which LLVM libraries hobbes links and saves the answer in the package; `package_info()` reads it to name the `llvm-core` components a user links.  Recipes there normally list components in the recipe.  The alternative is to require all of `llvm-core::llvm-core`, which also links LLVM's line editor and the terminal library under it.
* **`-L` flags in `generate()`.**  hobbes's `CMakeLists.txt` links what `llvm-config` names (`-lLLVMCore ... -lz3`), so the recipe adds the library directories of its dependencies to the link line.  The cleaner fix is in hobbes: link LLVM's CMake targets.
* **No `cmake.install()`.**  hobbes's `CMakeLists.txt` installs both libraries and all of the build's programs; the recipe copies the one library, the headers, `hi` and `hog` itself.
* **`readline/*:with_library=curses` in `default_options`.**  Setting another package's option is discouraged, and it means readline is always built from source (ConanCenter has no prebuilt package of that combination).
* **No `shared` option.**  hobbes builds static libraries only; `package_type` says so.
* **Inclusion criteria.**  ConanCenter weighs a project's release history and outside use.  1.0.0 is hobbes's first tagged release, and its guidance is to wait a few days after a release before adding it.

## Tested

All runs used the command at the top of this file with `--build="hobbes/*"` added, so hobbes is built from the release archive even when a package of it is cached:

```
$ conan create . --version=1.0.0 --build="missing:~llvm-core/*" --build="hobbes/*"
```

and the default profile from `conan profile detect` (gcc 13, `compiler.cppstd=gnu17`, `libstdc++11`, Release) with this appended:

```
[replace_requires]
zlib/*: zlib/1.3.1
```

The only prebuilt `llvm-core/19.1.7` on ConanCenter for this configuration was built against zlib 1.3.  ConanCenter's `libxml2/2.15.4` binary was rebuilt against zlib 1.4, so with the pin it is built from source.

**x86-64 Linux, Conan's CMake (2026-11-24).**  A cloud container on x86-64 hardware (Intel Xeon, 4 cores), Ubuntu 24.04.5, gcc 13.3.0, Conan 2.34.0 (installed with `pipx`), no `[platform_tool_requires]`.  `conan create` exited 0.

* Downloaded from ConanCenter: `llvm-core/19.1.7`, `zlib/1.3.1`, `cmake/3.31.12`, `cmake/4.5.0` (libxml2's `cmake/[>=3.18]`), `z3/4.13.0`, `zstd/1.5.6`, `ncurses/6.4`, `editline/3.1`, `termcap/1.3.1`, `libiconv/1.17`.
* Built: `libxml2/2.15.4`, `readline/8.2`, `hobbes/1.0.0`.  hobbes's sources came from `https://github.com/morganstanley/hobbes/releases/download/v1.0.0/hobbes-1.0.0.tar.gz`, and it was configured and built with the `cmake/3.31.12` that the recipe's `tool_requires("cmake/[>=3.19 <4]")` brought.
* `test_package` built and ran, printing `hobbes says the squares of 1..10 sum to 385`.

No change to the recipe was needed.

**x86-64 Linux under emulation, the machine's CMake.**  An earlier run in an x86-64 Ubuntu 24.04 container emulated on an Apple Silicon Mac, gcc 13.3.0, Conan 2.34.0, the same command and zlib pin.  Conan's CMake binary segfaulted under emulation, so Ubuntu's CMake 3.28.3 was given through `[platform_tool_requires]` (`cmake/3.28.3`).  It downloaded `llvm-core/19.1.7` and `zlib/1.3.1`, built `libxml2`, `readline` and `hobbes/1.0.0`, and the test program ran.

**ConanCenter's Linux image (2026-11-24).**  `conanio/gcc11-ubuntu16.04` under podman on x86-64: gcc 11.1.0, glibc 2.23, CMake 3.15.7, Conan 2.32.0, `compiler.cppstd=17`.  hobbes built and packaged, but `test_package` failed to configure with the image's CMake (`Unknown CMake command "cmake_path"`, from `llvm-core`'s CMake files); with the `tool_requires` above it built and ran.  Without the zlib pin the run stops at "Missing prebuilt package for 'llvm-core/19.1.7'": every `llvm-core` binary on ConanCenter was built against zlib 1.3.

**macOS, Apple Silicon (2026-11-24).**  apple-clang 21, Conan 2.34.0, `--build=missing`.  ConanCenter has no `llvm-core` binary for this compiler, so LLVM and its dependencies were built from source, then hobbes; `test_package` ran.

**Not tried:** AArch64 Linux, clang on Linux, `build_type=Debug`, `fPIC=False`, and ConanCenter's own CI.

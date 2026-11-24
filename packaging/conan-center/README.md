# ConanCenter recipe (draft)

A staging copy of a recipe for [ConanCenter](https://github.com/conan-io/conan-center-index), laid out as it would be there (`recipes/hobbes/`).  It has not been submitted.  It packages the hobbes 1.0.0 release archive; the recipe at the root of this repository (`conanfile.py`) packages a checkout.

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

`test_package/` is the same but for its minimum CMake version (3.15).

## What a ConanCenter reviewer may still object to

* **`lib/hobbes-llvm.json`.**  `package()` asks `llvm-config` which LLVM libraries hobbes links and saves the answer in the package; `package_info()` reads it to name the `llvm-core` components a user links.  Recipes there normally list components in the recipe.  The alternative is to require all of `llvm-core::llvm-core`, which also links LLVM's line editor and the terminal library under it.
* **`-L` flags in `generate()`.**  hobbes's `CMakeLists.txt` links what `llvm-config` names (`-lLLVMCore ... -lz3`), so the recipe adds the library directories of its dependencies to the link line.  The cleaner fix is in hobbes: link LLVM's CMake targets.
* **No `cmake.install()`.**  hobbes's `CMakeLists.txt` installs both libraries and all of the build's programs; the recipe copies the one library, the headers, `hi` and `hog` itself.
* **`readline/*:with_library=curses` in `default_options`.**  Setting another package's option is discouraged, and it means readline is always built from source (ConanCenter has no prebuilt package of that combination).
* **No `shared` option.**  hobbes builds static libraries only; `package_type` says so.
* **Inclusion criteria.**  ConanCenter weighs a project's release history and outside use.  1.0.0 is hobbes's first tagged release, and its guidance is to wait a few days after a release before adding it.

## Tested

See the pull request or commit that added this directory.

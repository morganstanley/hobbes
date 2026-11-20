import io
import json
import os
import re

from conan import ConanFile
from conan.errors import ConanException, ConanInvalidConfiguration
from conan.tools.build import check_min_cppstd
from conan.tools.cmake import CMake, CMakeDeps, CMakeToolchain, cmake_layout
from conan.tools.files import copy, load, save
from conan.tools.scm import Git, Version

required_conan_version = ">=2.0"


class HobbesConan(ConanFile):
    name = "hobbes"
    description = "A language and an embedded JIT compiler for dynamic expression evaluation, data storage and analysis"
    license = "Apache-2.0"
    url = "https://github.com/morganstanley/hobbes"
    homepage = "https://github.com/morganstanley/hobbes"
    topics = ("jit", "compiler", "llvm", "language", "structured-logging")
    package_type = "static-library"

    settings = "os", "arch", "compiler", "build_type"
    options = {
        "fPIC": [True, False],
        # Build against an LLVM that is installed on the machine, not one that
        # is a Conan package: for a toolchain that has its own LLVM, or an LLVM
        # version that no package exists for. CMake finds it the way it does in
        # any hobbes build (set LLVM_DIR to the directory of its LLVMConfig.cmake
        # if it isn't found), and it needs LLVM's static libraries.
        "with_system_llvm": [True, False],
        # Likewise for the readline (and the curses under it) that hi's prompt
        # uses: the machine's, not a Conan package's.
        "with_system_readline": [True, False],
    }
    default_options = {
        "fPIC": True,
        "with_system_llvm": False,
        "with_system_readline": False,
        # one terminal library in hi, not two: readline's default here is
        # termcap, which defines the same symbols as the ncurses it also needs
        "readline/*:with_library": "curses",
    }

    # the LLVM major versions hobbes builds with (include/hobbes/util/llvm.H)
    _llvm_majors = (8, 9, 10, 11, 12, 16, 18, 19, 20, 21, 22)

    exports_sources = (
        "CMakeLists.txt",
        "cmake/*",
        "include/*",
        "lib/*",
        "bin/*",
        # (CMakeLists.txt declares the test suite whether or not it is built)
        "test/*",
        "LICENSE.md",
        "NOTICE.md",
    )

    def set_version(self):
        # hobbes has no tagged releases yet: name a version with --version, or
        # get one that says which commit it was built from
        if not self.version:
            try:
                self.version = "0.0.0-git." + Git(self, self.recipe_folder).get_commit()[:12]
            except Exception:
                self.version = "0.0.0-dev"

    def layout(self):
        cmake_layout(self)

    def requirements(self):
        if not self.options.with_system_llvm:
            # The newest LLVM that hobbes supports and a remote has: 19 from
            # ConanCenter. To build with another, pin it in a profile or a
            # lockfile ("[replace_requires] llvm-core/*: llvm-core/16.0.6").
            #
            # hobbes's headers include LLVM's (hobbes/util/llvm.H), so its users compile against them too
            self.requires("llvm-core/[>=8 <23]", transitive_headers=True)
        self.requires("zlib/[>=1.2.11 <2]")
        # For hi's prompt only (and with it, the ncurses that readline draws
        # the terminal with). It is linked into hi and not into the library,
        # so a user of the package neither sees nor links it.
        if not self.options.with_system_readline:
            self.requires("readline/8.2", visible=False)

    def validate(self):
        check_min_cppstd(self, 17)
        if self.settings.os not in ("Linux", "Macos"):
            raise ConanInvalidConfiguration("hobbes builds on Linux and macOS only")
        if self.settings.arch not in ("x86_64", "armv8"):
            raise ConanInvalidConfiguration("hobbes generates code for x86-64 and AArch64 only")
        if not self.options.with_system_llvm:
            llvm = self.dependencies["llvm-core"]
            if Version(llvm.ref.version).major not in self._llvm_majors:
                raise ConanInvalidConfiguration(
                    f"hobbes doesn't build with LLVM {llvm.ref.version}; it supports major versions "
                    + ", ".join(str(m) for m in self._llvm_majors))
            if llvm.options.shared:
                raise ConanInvalidConfiguration("hobbes links LLVM statically (llvm-core:shared=False)")

    def generate(self):
        deps = CMakeDeps(self)
        deps.generate()

        tc = CMakeToolchain(self)
        # hobbes asks llvm-config which libraries to link and gets them by name
        # (-lLLVMCore ... -lz3 -lxml2 ...), so the linker has to be told where
        # the packages that hold them are
        libdirs = []
        for dep in self.dependencies.host.values():
            libdirs.extend(dep.cpp_info.aggregated_components().libdirs)
        link_dirs = ["-L" + d for d in libdirs]
        tc.extra_exelinkflags.extend(link_dirs)
        tc.extra_sharedlinkflags.extend(link_dirs)
        # find_package(Readline) is answered by the package's own CMake config
        # here, not by cmake/FindReadline.cmake, and that doesn't set the
        # variables CMakeLists.txt reads; set them, to the package's target
        # (with the machine's readline, cmake/FindReadline.cmake finds it as usual)
        if not self.options.with_system_readline:
            readline = self.dependencies["readline"].cpp_info.aggregated_components()
            tc.cache_variables["Readline_INCLUDE_DIRS"] = ";".join(readline.includedirs)
            tc.cache_variables["Readline_LIBRARIES"] = "readline::readline"
        tc.generate()

    def build(self):
        cmake = CMake(self)
        cmake.configure()
        # not the default target, which is also the test suite
        cmake.build(target="hobbes-pic" if self.options.fPIC else "hobbes")
        cmake.build(target="hi")
        cmake.build(target="hog")

    def _llvm_config(self, *args):
        """Ask the llvm-config that CMakeLists.txt found (and linked hobbes by) a question."""
        cache = load(self, os.path.join(self.build_folder, "CMakeCache.txt"))
        found = re.search(r"^llvm-config:FILEPATH=(.+)$", cache, re.MULTILINE)
        if not found or found.group(1).endswith("-NOTFOUND"):
            raise ConanException("can't tell which llvm-config hobbes was built with")
        out = io.StringIO()
        self.run('"%s" %s' % (found.group(1), " ".join(args)), stdout=out)
        return out.getvalue().split()

    def _llvm_usage(self):
        """What a program that links this package needs from LLVM.

        That is the libraries of the components hobbes uses, not all of LLVM
        (which would bring in, for one, the line editor and the terminal
        library under it). llvm-config knows which those are, so it is asked
        now -- the same llvm-config, for the same components, as
        CMakeLists.txt -- and the answer is kept with the package. For an
        LLVM installed on the system it also has to say where LLVM is, which
        a Conan package of LLVM says for itself.
        """
        major = int(self._llvm_config("--version")[0].split(".")[0])
        components = ["ipo", "aarch64" if self.settings.arch == "armv8" else "x86", "mcjit"]
        if major >= 11:
            components.append("orcjit")
        libs = self._llvm_config("--libs", "--link-static", *components)
        system_libs = self._llvm_config("--system-libs", "--link-static", *components)
        return {
            "version": ".".join(self._llvm_config("--version")),
            "includedirs": self._llvm_config("--includedir"),
            "libdirs": self._llvm_config("--libdir"),
            "libs": [l[2:] for l in libs if l.startswith("-l")],
            # (less the ones CMakeLists.txt leaves out, for the reasons it gives)
            "system_libs": [l[2:] for l in system_libs
                            if l.startswith("-l") and "::" not in l and l[2:] not in ("xml2", "z")],
            "defines": [f[2:] for f in self._llvm_config("--cppflags") if f.startswith("-D")],
        }

    def package(self):
        copy(self, "LICENSE.md", self.source_folder, os.path.join(self.package_folder, "licenses"))
        copy(self, "NOTICE.md", self.source_folder, os.path.join(self.package_folder, "licenses"))
        copy(self, "*.H", os.path.join(self.source_folder, "include"), os.path.join(self.package_folder, "include"))
        lib = "libhobbes-pic.a" if self.options.fPIC else "libhobbes.a"
        copy(self, lib, self.build_folder, os.path.join(self.package_folder, "lib"), keep_path=False)
        for tool in ("hi", "hog"):
            copy(self, tool, self.build_folder, os.path.join(self.package_folder, "bin"), keep_path=False)
        save(self, os.path.join(self.package_folder, "lib", "hobbes-llvm.json"),
             json.dumps(self._llvm_usage(), indent=2))

    def package_info(self):
        self.cpp_info.set_property("cmake_file_name", "hobbes")
        self.cpp_info.set_property("cmake_target_name", "hobbes::hobbes")
        self.cpp_info.libs = ["hobbes-pic" if self.options.fPIC else "hobbes"]
        if self.settings.os == "Linux":
            self.cpp_info.system_libs = ["pthread", "dl", "rt"]
        llvm = json.loads(load(self, os.path.join(self.package_folder, "lib", "hobbes-llvm.json")))
        if not self.options.with_system_llvm:
            # (each LLVM library is a component of the llvm-core package)
            self.cpp_info.requires = ["zlib::zlib"] + ["llvm-core::" + lib for lib in llvm["libs"]]
        else:
            self.cpp_info.requires = ["zlib::zlib"]
            self.cpp_info.includedirs.extend(llvm["includedirs"])
            self.cpp_info.libdirs.extend(llvm["libdirs"])
            self.cpp_info.libs.extend(llvm["libs"])
            self.cpp_info.system_libs.extend(llvm["system_libs"])
            self.cpp_info.defines.extend(llvm["defines"])

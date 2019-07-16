cmake_minimum_required(VERSION 3.4.1)
project(hobbes)

enable_language(CXX)

set (CMAKE_CXX_STANDARD 11)

find_package(ZLIB REQUIRED)
include_directories(${ZLIB_INCLUDE_DIRS})

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "LLVM_INCLUDE_DIRS=${LLVM_INCLUDE_DIRS}")
message(STATUS "LLVM_DEFINITIONS=${LLVM_DEFINITIONS}")

message(STATUS "SYSTEM_PROCESSOR ${CMAKE_SYSTEM_PROCESSOR}")
if (CMAKE_SYSTEM_PROCESSOR MATCHES "(x86)|(X86)|(amd64)|(AMD64)")
  set(llvm_libs_processor
    x86
    x86codegen
    x86asmparser
    x86disassembler
    x86asmprinter
    x86desc
    x86info
    x86utils
  )
elseif (CMAKE_SYSTEM_PROCESSOR MATCHES "arm")
  set(llvm_libs_processor
    armcodegen
    armasmparser
    armdisassembler
    armasmprinter
    armdesc
    arminfo
  )
endif()

function(with_llvm_config trget)
  if (${LLVM_PACKAGE_VERSION} VERSION_LESS "3.6")
    set(llvm_jit_lib jit)
  else()
    set(llvm_jit_lib mcjit)
    set(llvm_passes_lib passes)
  endif()

  if (${LLVM_PACKAGE_VERSION} VERSION_LESS "3.8")
    set(llvm_extra_libs ipa)
  else()
  endif()

  if (${LLVM_PACKAGE_VERSION} VERSION_LESS "5.0.0")
  else()
    set(llvm_extra_libs orcjit)
  endif()

  set(llvm_libs
    analysis
    asmparser
    asmprinter
    bitreader
    core
    codegen
    executionengine
    instcombine
    ${llvm_extra_libs}
    ipo
    ${llvm_jit_lib}
    mcparser
    object
    ${llvm_passes_lib}
    runtimedyld
    scalaropts
    support
    target
    transformutils
    ${llvm_libs_processor}
  )
  set(local_libs ${ARGN})
  llvm_map_components_to_libnames(local_llvm_libs ${llvm_libs})
  message(STATUS "target : ${trget}")
  message(STATUS "target_libs : ${local_libs}")
  message(STATUS "llvm_libs : ${local_llvm_libs}")

  get_target_property(t ${trget} TYPE)
  if ("x${t}" STREQUAL "xSTATIC_LIBRARY")
    target_link_libraries(${trget} INTERFACE ${local_libs} ${local_llvm_libs})
  elseif("x${t}" STREQUAL "xSHARED_LIBRARY" OR "x${t}" STREQUAL "xMODULE_LIBRARY")
    target_link_libraries(${trget} PRIVATE ${local_libs} ${local_llvm_libs})
  else()
    target_link_libraries(${trget} ${local_libs} ${local_llvm_libs})
  endif()
endfunction(with_llvm_config)

if(UNIX AND NOT APPLE)
  set(LINUX TRUE)
endif()

if(APPLE)
  set(sys_libs pthread dl)
  set(cxx_flags "-Wreorder")
  include_directories(/usr/local/opt/readline/include)
  link_directories(/usr/local/opt/readline/lib)
endif()
if(LINUX)
  set(sys_libs pthread dl rt)
  set(cxx_flags "-Werror=old-style-cast -Werror -Wall -Wextra -Winit-self -Wreturn-type -Wunused-variable -Wsign-compare -Warray-bounds -Wunknown-pragmas -Wuninitialized -Wstrict-aliasing -Wunused-value -Wunused-label -Wswitch -Wcast-align -Wctor-dtor-privacy -Wmissing-noreturn -Wunused-parameter -Wreorder")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${cxx_flags}")

if (MSVC)
  message(STATUS "MSVC compiler isn't supported yet")
elseif ((CMAKE_C_COMPILER_ID MATCHES "GNU" OR CMAKE_C_COMPILER_ID MATCHES "Clang") AND NOT APPLE)
  if (NOT WIN32)
      # assume Linux
  endif ()
  message(STATUS "Compiling on GNU or Clang platform")
  # set(SANITIZER_FLAGS "-fsanitize=address")
  set(CMAKE_CXX_FLAGS "-frtti -O2 -fomit-frame-pointer -Wall -Wno-sign-compare -Winline -Wno-delete-non-virtual-dtor -std=c++11 -fexceptions")
  set(CMAKE_CXX_FLAGS_DEBUG "${SANITIZER_FLAGS} -fno-omit-frame-pointer -frtti -O0 -g3 -Wall -Wno-sign-compare -Wno-delete-non-virtual-dtor -std=c++11 -fexceptions")
  set(CMAKE_CXX_FLAGS_RELEASE "-frtti -DNDEBUG -O2 -fomit-frame-pointer -Wall -Wno-sign-compare -Winline -Wno-delete-non-virtual-dtor -std=c++11 -fexceptions")
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${SANITIZER_FLAGS} -DNDEBUG -frtti -O2 -g3 -Wall -Wno-sign-compare -Wno-delete-non-virtual-dtor -std=c++11 -fexceptions")
elseif (APPLE)
  message(STATUS "Compiling on APPLE platform")
  set(CMAKE_CXX_FLAGS "-O3 -DNDEBUG -Wall -frtti -Wno-sign-compare -Wno-delete-non-virtual-dtor -fexceptions")
  set(CMAKE_CXX_FLAGS_DEBUG "-frtti -O0 -g3 -Wall -Wno-sign-compare -Wno-delete-non-virtual-dtor -fexceptions")
  set(CMAKE_CXX_FLAGS_RELEASE "-frtti -DNDEBUG -O2 -Wall -Wno-sign-compare -Wno-delete-non-virtual-dtor -fexceptions")
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-frtti -DNDEBUG -O2 -g3 -Wall -Wno-sign-compare -Wno-delete-non-virtual-dtor -fexceptions")
else()
  message(FATAL_ERROR "Unsupported platform")
endif ()

message(STATUS "CMAKE_CXX_FLAGS=${CMAKE_CXX_FLAGS}")
message(STATUS "CMAKE_CXX_FLAGS_DEBUG=${CMAKE_CXX_FLAGS_DEBUG}")
message(STATUS "CMAKE_CXX_FLAGS_RELEASE=${CMAKE_CXX_FLAGS_RELEASE}")
message(STATUS "CMAKE_CXX_FLAGS_RELWITHDEBINFO=${CMAKE_CXX_FLAGS_RELWITHDEBINFO}")

file(GLOB_RECURSE lib_headers lib/*.H)
file(GLOB_RECURSE lib_source lib/*.C)
set(lib_files ${lib_headers} ${lib_source})
include_directories(include)

add_library(hobbes STATIC ${lib_files})
with_llvm_config(hobbes ${ZLIB_LIBRARIES})

add_library(hobbes-pic STATIC ${lib_files})
with_llvm_config(hobbes-pic ${ZLIB_LIBRARIES})
set_property(TARGET hobbes-pic PROPERTY POSITION_INDEPENDENT_CODE TRUE)

file(GLOB hi_files bin/hi/*.C)
add_executable(hi ${hi_files})

with_llvm_config(hi hobbes ncurses readline ${sys_libs})

file(GLOB_RECURSE hog_files bin/hog/*.C)
add_executable(hog ${hog_files})
with_llvm_config(hog hobbes ncurses ${sys_libs})

enable_testing()
file(GLOB test_files test/*.C)
add_executable(hobbes-test ${test_files})
with_llvm_config(hobbes-test hobbes ncurses ${sys_libs})
add_test(hobbes-test hobbes-test)
include(FindPythonInterp)
set_property(TARGET hobbes-test PROPERTY COMPILE_FLAGS "-DPYTHON_EXECUTABLE=\"${PYTHON_EXECUTABLE}\" -DSCRIPT_DIR=\"${CMAKE_SOURCE_DIR}/scripts/\"")

install(TARGETS hobbes hobbes-pic DESTINATION "lib")
install(TARGETS hi hog DESTINATION "bin")
install(DIRECTORY "include/hobbes" DESTINATION "include")
install(DIRECTORY "scripts" DESTINATION "scripts")


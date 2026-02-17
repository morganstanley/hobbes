cmake_minimum_required(VERSION 3.5)
project(hobbes)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

find_package(ZLIB REQUIRED)
include_directories(${ZLIB_INCLUDE_DIRS})

find_package(Readline REQUIRED)
include_directories(${Readline_INCLUDE_DIRS})

set(CURSES_NEED_NCURSES TRUE)
find_package(Curses REQUIRED)
include_directories(${CURSES_INCLUDE_DIRS})

find_package(LLVM 12 CONFIG QUIET)
find_package(LLVM REQUIRED CONFIG)
include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

set(CMAKE_CXX_STANDARD 17)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Using LLVM binary dir: ${LLVM_TOOLS_BINARY_DIR}")

if (${LLVM_PACKAGE_VERSION} VERSION_LESS "3.6")
  set(jit_lib jit)
elseif(${LLVM_PACKAGE_VERSION} VERSION_LESS "11.0")
  set(jit_lib mcjit)
else()
  set(jit_lib mcjit orcjit)
endif()
IF(CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
  set(llvm_arch_lib x86)
ELSEIF(CMAKE_SYSTEM_PROCESSOR STREQUAL "aarch64" OR CMAKE_SYSTEM_PROCESSOR STREQUAL "arm64")
  set(llvm_arch_lib aarch64)
ENDIF()

find_program(llvm-config llvm-config PATHS ${LLVM_TOOLS_BINARY_DIR} NO_DEFAULT_PATH)
find_program(llvm-config llvm-config)
message(STATUS "Using LLVM config: ${llvm-config}")
if (${LLVM_PACKAGE_VERSION} VERSION_LESS "4.0")
  execute_process(COMMAND ${llvm-config} --libs ipo ${llvm_arch_lib} ${jit_lib}
                  OUTPUT_VARIABLE llvm_libs
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${llvm-config} --ldflags
                  OUTPUT_VARIABLE llvm_ldflags
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${llvm-config} --system-libs
                  OUTPUT_VARIABLE sys_libs
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
else()
  execute_process(COMMAND ${llvm-config} --libs ipo ${llvm_arch_lib} ${jit_lib} --link-static
                  OUTPUT_VARIABLE llvm_libs
                  OUTPUT_STRIP_TRAILING_WHITESPACE
                  COMMAND_ERROR_IS_FATAL ANY)
  execute_process(COMMAND ${llvm-config} --ldflags
                  OUTPUT_VARIABLE llvm_ldflags
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${llvm-config} --system-libs
                  OUTPUT_VARIABLE sys_libs
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()

find_package(zstd QUIET)
if(zstd_FOUND)
  set(sys_libs ${sys_libs} zstd::libzstd_shared)
else()
  find_library(ZSTD_LIBRARY NAMES zstd)
  if(ZSTD_LIBRARY)
    set(sys_libs ${sys_libs} ${ZSTD_LIBRARY})
  endif()
endif()

message(STATUS "llvm libs: ${llvm_libs}")
message(STATUS "llvm ldflags: ${llvm_ldflags}")
message(STATUS "llvm syslibs: ${sys_libs}")

file(GLOB_RECURSE lib_headers lib/*.H)
file(GLOB_RECURSE lib_source lib/*.C)
set(lib_files ${lib_headers} ${lib_source})
include_directories(include)

file(GLOB test_files test/*.C)
file(GLOB hi_files bin/hi/*.C)
file(GLOB_RECURSE hog_files bin/hog/*.C)

if(UNIX AND NOT APPLE)
  set(LINUX TRUE)
endif()

if(APPLE)
  set(sys_libs ${sys_libs} pthread dl)
  set(cxx_flags "-Wreorder")
endif()
if(LINUX)
  set(sys_libs ${sys_libs} pthread dl rt)
  set(cxx_flags "-Werror=old-style-cast -Werror -Wall -Wextra -Winit-self -Wreturn-type -Wunused-variable -Wsign-compare -Warray-bounds -Wunknown-pragmas -Wuninitialized -Wstrict-aliasing -Wunused-value -Wunused-label -Wswitch -Wcast-align -Wctor-dtor-privacy -Wmissing-noreturn -Wunused-parameter -Wreorder -Wno-deprecated-declarations -Wno-dangling-reference -Wno-mismatched-new-delete")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${cxx_flags}")
set(CMAKE_CXX_FLAGS_DEBUG "-g")
set(CMAKE_CXX_FLAGS_RELEASE "-O2")

option(USE_ASAN_UBSAN "Use address,undefined sanitizers" OFF)
if(USE_ASAN_AND_UBSAN)
  # _FORTIFY_SOURCE doesn't play well with asan
  # https://github.com/google/sanitizers/issues/247
  add_definitions(-U_FORTIFY_SOURCE)
  add_compile_options("-fno-omit-frame-pointer;-fsanitize=address,undefined;-fno-optimize-sibling-calls;-fno-sanitize-recover=all")
  add_link_options("-fsanitize=address,undefined;-fno-optimize-sibling-calls;-fno-sanitize-recover=all")
  add_compile_options("$<$<OR:$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_ID:AppleClang>>:-fsanitize=local-bounds,float-divide-by-zero,nullability,signed-integer-overflow,shift,integer-divide-by-zero>")
  add_link_options("$<$<OR:$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_ID:AppleClang>>:-fsanitize=local-bounds,float-divide-by-zero,nullability,signed-integer-overflow,shift,integer-divide-by-zero>")
  add_compile_options("$<$<CXX_COMPILER_ID:GNU>:--param=max-vartrack-size=60000000>")
  message(STATUS "address and undefined sanitizers enabled")
endif()

add_library(hobbes STATIC ${lib_files})
target_link_libraries(hobbes PUBLIC ${llvm_ldflags} ${llvm_libs} ${ZLIB_LIBRARIES} ${CURSES_LIBRARIES} ${sys_libs})
add_library(hobbes-pic STATIC ${lib_files})
target_link_libraries(hobbes-pic PUBLIC ${llvm_ldflags} ${llvm_libs} ${ZLIB_LIBRARIES} ${CURSES_LIBRARIES} ${sys_libs})
set_property(TARGET hobbes-pic PROPERTY POSITION_INDEPENDENT_CODE TRUE)

add_executable(hi ${hi_files})
target_link_libraries(hi PUBLIC "-rdynamic" PRIVATE hobbes ${Readline_LIBRARIES})
add_executable(hog ${hog_files})
target_link_libraries(hog PRIVATE hobbes)

enable_testing()
add_executable(mock-proc test/mocks/proc.C)
add_executable(hobbes-test ${test_files})
target_link_libraries(hobbes-test PRIVATE hobbes)
add_test(hobbes-test hobbes-test)
add_test(NAME rosetta-examples COMMAND bash ${CMAKE_SOURCE_DIR}/examples/rosetta/run_tests.sh $<TARGET_FILE:hi>)
find_package(Python COMPONENTS Interpreter)
message("Python_EXECUTABLE " ${Python_EXECUTABLE})
set_property(TARGET hobbes-test PROPERTY COMPILE_FLAGS "-DPYTHON_EXECUTABLE=\"${Python_EXECUTABLE}\" -DSCRIPT_DIR=\"${CMAKE_SOURCE_DIR}/scripts/\"")

install(TARGETS hobbes hobbes-pic DESTINATION "lib")
install(TARGETS hi hog hobbes-test DESTINATION "bin")
install(DIRECTORY "include/hobbes" DESTINATION "include")
install(DIRECTORY "scripts" DESTINATION "scripts")


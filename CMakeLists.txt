cmake_minimum_required(VERSION 3.4.1)
project(hobbes)
set (CMAKE_CXX_STANDARD 11)

find_package(ZLIB REQUIRED)
include_directories(${ZLIB_INCLUDE_DIRS})

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

if (${LLVM_PACKAGE_VERSION} VERSION_LESS "3.6")
  set(jit_lib jit)
else()
  set(jit_lib mcjit)
endif()

find_program(llvm-config llvm-config PATHS ${LLVM_TOOLS_BINARY_DIR})
if (${LLVM_PACKAGE_VERSION} VERSION_LESS "4.0")
  execute_process(COMMAND ${llvm-config} --libs x86 ipo ${jit_lib}
                  OUTPUT_VARIABLE llvm_libs
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${llvm-config} --ldflags
                  OUTPUT_VARIABLE llvm_ldflags
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${llvm-config} --system-libs
                  OUTPUT_VARIABLE sys_libs
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
else()
  execute_process(COMMAND ${llvm-config} --libs x86 ipo ${jit_lib} --link-static
                  OUTPUT_VARIABLE llvm_libs
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${llvm-config} --ldflags
                  OUTPUT_VARIABLE llvm_ldflags
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND ${llvm-config} --system-libs --ignore-libllvm
                  OUTPUT_VARIABLE sys_libs
                  OUTPUT_STRIP_TRAILING_WHITESPACE)
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
  include_directories(/usr/local/opt/readline/include)
  link_directories(/usr/local/opt/readline/lib)
endif()
if(LINUX)
  set(sys_libs ${sys_libs} pthread dl rt)
  set(cxx_flags "-Werror=old-style-cast -Werror -Wall -Wextra -Winit-self -Wreturn-type -Wunused-variable -Wsign-compare -Warray-bounds -Wunknown-pragmas -Wuninitialized -Wstrict-aliasing -Wunused-value -Wunused-label -Wswitch -Wcast-align -Wctor-dtor-privacy -Wmissing-noreturn -Wunused-parameter -Wreorder")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${cxx_flags}")
set(CMAKE_CXX_FLAGS_DEBUG "-g")
set(CMAKE_CXX_FLAGS_RELEASE "-O2")

add_library(hobbes STATIC ${lib_files})
target_link_libraries(hobbes PUBLIC ${llvm_ldflags} ${llvm_libs} ${ZLIB_LIBRARIES} ncurses ${sys_libs})
add_library(hobbes-pic STATIC ${lib_files})
target_link_libraries(hobbes-pic PUBLIC ${llvm_ldflags} ${llvm_libs} ${ZLIB_LIBRARIES} ncurses ${sys_libs})
set_property(TARGET hobbes-pic PROPERTY POSITION_INDEPENDENT_CODE TRUE)

add_executable(hi ${hi_files})
target_link_libraries(hi PRIVATE hobbes readline)
add_executable(hog ${hog_files})
target_link_libraries(hog PRIVATE hobbes)

enable_testing()
add_executable(hobbes-test ${test_files})
target_link_libraries(hobbes-test PRIVATE hobbes)
add_test(hobbes-test hobbes-test)
include(FindPythonInterp)
set_property(TARGET hobbes-test PROPERTY COMPILE_FLAGS "-DPYTHON_EXECUTABLE=\"${PYTHON_EXECUTABLE}\" -DSCRIPT_DIR=\"${CMAKE_SOURCE_DIR}/scripts/\"")

install(TARGETS hobbes hobbes-pic DESTINATION "lib")
install(TARGETS hi hog DESTINATION "bin")
install(DIRECTORY "include/hobbes" DESTINATION "include")
install(DIRECTORY "scripts" DESTINATION "scripts")


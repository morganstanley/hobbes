cmake_minimum_required(VERSION 3.4.1)
project(hobbes)
set (CMAKE_CXX_STANDARD 11)

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
# we seem to need to pull in several components on the mac, probably doesn't work correctly for all LLVM versions
# maybe this should be an OS/LLVM switch?
llvm_map_components_to_libnames(llvm_libs analysis asmparser asmprinter bitreader bitwriter codegen core engine executionengine instcombine jit lto mc mcanalysis mcjit native nativecodegen scalaropts selectiondag tablegen target transformutils vectorize x86 x86asmparser x86asmprinter x86codegen x86desc x86disassembler x86info x86utils)

enable_testing()

file(GLOB_RECURSE lib_headers lib/*.H)
file(GLOB_RECURSE lib_source lib/*.C)
set(lib_files ${lib_headers} ${lib_source})
include_directories(lib)

add_library(hobbes STATIC ${lib_files})
target_link_libraries(hobbes ${llvm_libs})
add_library(hobbes-pic STATIC ${lib_files})
target_link_libraries(hobbes-pic ${llvm_libs})
set_property(TARGET hobbes-pic PROPERTY POSITION_INDEPENDENT_CODE TRUE)

file(GLOB test_files test/*.C)
file(GLOB hi_files bin/hi/*.C)
file(GLOB_RECURSE hog_files bin/hog/*.C)

add_executable(hobbes-test ${test_files})
target_link_libraries(hobbes-test hobbes pthread dl ncurses)
add_test(hobbes-test hobbes-test)

add_executable(hi ${hi_files})
target_link_libraries(hi hobbes pthread dl ncurses readline)
add_executable(hog ${hog_files})
target_link_libraries(hog hobbes pthread dl ncurses)

install(TARGETS hobbes hobbes-pic DESTINATION "lib")
install(TARGETS hi hog DESTINATION "bin")
install(FILES ${lib_headers} DESTINATION "include")

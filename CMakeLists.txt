cmake_minimum_required(VERSION 3.4.1)
project(hobbes)
set (CMAKE_CXX_STANDARD 11)

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
# 'x86' module should have covered 'x86asmparser' and 'x86codegen' already, but using 'x86' alone didn't work
llvm_map_components_to_libnames(llvm_libs x86asmparser x86codegen x86 mcjit passes)

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
target_link_libraries(hobbes-test hobbes pthread dl rt ncurses tinfo)
add_test(hobbes-test hobbes-test)

add_executable(hi ${hi_files})
target_link_libraries(hi hobbes pthread dl rt ncurses tinfo readline)
add_executable(hog ${hog_files})
target_link_libraries(hog hobbes pthread dl rt ncurses tinfo)

install(TARGETS hobbes hobbes-pic DESTINATION "lib")
install(TARGETS hi hog DESTINATION "bin")
install(FILES ${lib_headers} DESTINATION "include")

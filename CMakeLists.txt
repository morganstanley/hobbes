cmake_minimum_required(VERSION 3.4.1)
project(hobbes)
set (CMAKE_CXX_STANDARD 11)

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})
# not sure how to select just the necessary components across all LLVM versions
# for now, pull in everything and hope that the linker will sort it out
# maybe this should be an OS/LLVM switch?
llvm_map_components_to_libnames(llvm_libs all)

file(GLOB_RECURSE lib_headers lib/*.H)
file(GLOB_RECURSE lib_source lib/*.C)
set(lib_files ${lib_headers} ${lib_source})
include_directories(include)

add_library(hobbes STATIC ${lib_files})
target_link_libraries(hobbes ${llvm_libs})
add_library(hobbes-pic STATIC ${lib_files})
target_link_libraries(hobbes-pic ${llvm_libs})
set_property(TARGET hobbes-pic PROPERTY POSITION_INDEPENDENT_CODE TRUE)

file(GLOB test_files test/*.C)
file(GLOB hi_files bin/hi/*.C)
file(GLOB_RECURSE hog_files bin/hog/*.C)

if(UNIX AND NOT APPLE)
   set(LINUX TRUE)
 endif()

if(APPLE)
  set(sys_libs pthread dl)
endif()
if(LINUX)
  set(sys_libs pthread dl rt)
endif()

enable_testing()
add_executable(hobbes-test ${test_files})
target_link_libraries(hobbes-test hobbes ncurses ${sys_libs})
add_test(hobbes-test hobbes-test)

add_executable(hi ${hi_files})
target_link_libraries(hi hobbes ncurses readline ${sys_libs})
add_executable(hog ${hog_files})
target_link_libraries(hog hobbes ncurses ${sys_libs})

install(TARGETS hobbes hobbes-pic DESTINATION "lib")
install(TARGETS hi hog DESTINATION "bin")
install(DIRECTORY "include/hobbes" DESTINATION "include")

# Build on OSX El Capitan 10.11.6 before get_clocktime() was defined:
OPTION (OSX_EL_CAPITAN "Build for OSX prior to 10.12 Sierra" OFF)
if(APPLE)
 if (OSX_EL_CAPITAN)
   ADD_DEFINITIONS("-DOSX_BEFORE_SIERRA")
 endif()
endif()


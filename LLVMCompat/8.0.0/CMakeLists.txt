file(GLOB_RECURSE lib_headers lib/*.H)
file(GLOB_RECURSE lib_source lib/*.C)
set(lib_files ${lib_headers} ${lib_source})
include_directories(include)

add_library(llvmCompat STATIC ${lib_files})
with_llvm_config(llvmCompat ${ZLIB_LIBRARIES})


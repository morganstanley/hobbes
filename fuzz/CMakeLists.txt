# libFuzzer harnesses for the surfaces that consume untrusted bytes:
# the binary type decoder, the fregion file reader, and the parser.
#
# With a Clang that ships libFuzzer (upstream/Homebrew Clang; AppleClang does
# not), each harness builds as a real fuzzer. Otherwise it builds as a
# standalone runner that replays files given on the command line, so crash
# reproducers can be checked with any compiler. Combine with
# -DUSE_ASAN_AND_UBSAN=ON to catch silent out-of-bounds reads.

if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  set(fuzz_driver "")
  set(fuzz_flags -fsanitize=fuzzer)
  message(STATUS "fuzzers: building with libFuzzer (${CMAKE_CXX_COMPILER_ID})")
else()
  set(fuzz_driver standalone_main.C)
  set(fuzz_flags "")
  message(STATUS "fuzzers: ${CMAKE_CXX_COMPILER_ID} has no libFuzzer; building standalone reproducer runners")
endif()

function(add_fuzzer name source)
  add_executable(${name} ${source} ${fuzz_driver})
  target_link_libraries(${name} PRIVATE hobbes)
  if(fuzz_flags)
    target_compile_options(${name} PRIVATE ${fuzz_flags})
    target_link_options(${name} PRIVATE ${fuzz_flags})
  endif()
endfunction()

add_fuzzer(fuzz-type-decode fuzz_type_decode.C)
add_fuzzer(fuzz-fregion-reader fuzz_fregion_reader.C)
add_fuzzer(fuzz-parse-expr fuzz_parse_expr.C)

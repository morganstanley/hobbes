# libFuzzer harnesses for the surfaces that consume untrusted bytes:
# the binary type decoder, the fregion file reader, and the parser.
#
# With a Clang that ships libFuzzer (upstream/Homebrew Clang; AppleClang does
# not), each harness builds as a real fuzzer. Otherwise it builds as a
# standalone runner that replays files given on the command line, so crash
# reproducers can be checked with any compiler. Combine with
# -DUSE_ASAN_AND_UBSAN=ON to catch silent out-of-bounds reads.

# A fuzzing service supplies its own engine to link against rather than
# libFuzzer via -fsanitize=fuzzer: OSS-Fuzz passes $LIB_FUZZING_ENGINE, which
# selects between libFuzzer, AFL++, honggfuzz and centipede and may be either a
# library path or a flag. It is handed to the linker as-is. Instrumentation is
# unaffected -- that comes from -fsanitize=fuzzer-no-link, which BUILD_FUZZERS
# applies to the whole build.
set(FUZZING_ENGINE_LIB "" CACHE STRING
    "Fuzzing engine to link harnesses against (e.g. OSS-Fuzz's LIB_FUZZING_ENGINE); libFuzzer via -fsanitize=fuzzer when empty")

if(FUZZING_ENGINE_LIB)
  set(fuzz_driver "")
  set(fuzz_flags "")
  message(STATUS "fuzzers: linking against externally supplied engine ${FUZZING_ENGINE_LIB}")
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
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
  if(FUZZING_ENGINE_LIB)
    # as a link library rather than a link option: the engine is often a static
    # archive, which only resolves symbols in objects that precede it
    target_link_libraries(${name} PRIVATE ${FUZZING_ENGINE_LIB})
  endif()
endfunction()

add_fuzzer(fuzz-type-decode fuzz_type_decode.C)
add_fuzzer(fuzz-fregion-reader fuzz_fregion_reader.C)
add_fuzzer(fuzz-parse-expr fuzz_parse_expr.C)

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

# Clang is the usual source of libFuzzer, but having Clang is not the same as
# having it: a distribution can package the sanitizer runtimes separately, and
# then -fsanitize=fuzzer compiles and fails to link. Ask the toolchain instead
# of assuming, so that turning BUILD_FUZZERS on cannot break a build that only
# wants the corpus replayed -- the standalone runner replays it just as well.
include(CheckCXXSourceCompiles)
if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  # append rather than replace, and put back whatever was there: a toolchain
  # file may have set flags the probe needs to compile at all (a sysroot, a
  # stdlib), and the rest of the configuration needs them after it
  set(saved_required_flags "${CMAKE_REQUIRED_FLAGS}")
  set(CMAKE_REQUIRED_FLAGS "${CMAKE_REQUIRED_FLAGS} -fsanitize=fuzzer")
  check_cxx_source_compiles("
    #include <cstddef>
    #include <cstdint>
    extern \"C\" int LLVMFuzzerTestOneInput(const uint8_t*, size_t) { return 0; }
  " HAVE_LIBFUZZER)
  set(CMAKE_REQUIRED_FLAGS "${saved_required_flags}")
  unset(saved_required_flags)
endif()

if(FUZZING_ENGINE_LIB)
  set(fuzz_driver "")
  set(fuzz_flags "")
  message(STATUS "fuzzers: linking against externally supplied engine ${FUZZING_ENGINE_LIB}")
elseif(HAVE_LIBFUZZER)
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

# Replay each harness's checked-in corpus as part of `ctest`.
#
# The corpus is where reproducers land when a fuzzing service reports a crash,
# and until now nothing in a normal build ran them: the harnesses are only
# built when BUILD_FUZZERS is on, and the seeds reached a fuzzer only through
# the zips oss-fuzz-build.sh hands to OSS-Fuzz and ClusterFuzzLite. Replaying
# them here costs seconds -- the whole parse-expr corpus is about fifteen --
# and turns them into a regression test that runs under whatever sanitizers
# the build was configured with, on every compiler in the matrix.
function(add_corpus_replay_test harness corpus)
  set(dir "${CMAKE_CURRENT_SOURCE_DIR}/corpus/${corpus}")
  if(NOT IS_DIRECTORY "${dir}")
    return()
  endif()

  add_test(
    NAME ${harness}-corpus
    COMMAND ${CMAKE_COMMAND}
            -DHARNESS=$<TARGET_FILE:${harness}>
            -DCORPUS=${dir}
            -P ${CMAKE_CURRENT_SOURCE_DIR}/replay-corpus.cmake)

  # Hobbes reclaims evaluation memory by resetting an arena rather than
  # freeing objects one at a time, so a leak check at exit reports
  # transaction-scoped data as lost by design -- which is why the .options
  # files shipped to OSS-Fuzz carry detect_leaks=0. ctest does not read those,
  # so say it here. LSAN_OPTIONS rather than ASAN_OPTIONS: it is read second
  # for leak settings, and so does not discard the rest of a build's ASan
  # configuration.
  set_tests_properties(${harness}-corpus PROPERTIES
    ENVIRONMENT "LSAN_OPTIONS=detect_leaks=0")
endfunction()

add_corpus_replay_test(fuzz-type-decode type-decode)
add_corpus_replay_test(fuzz-fregion-reader fregion-reader)
add_corpus_replay_test(fuzz-parse-expr parse-expr)

# Replay every input in a corpus directory through a fuzz harness, once each.
#
# Driven as a ctest command rather than expanded into add_test() so that the
# corpus is read when the test runs and not when cmake last configured it: a
# reproducer committed alongside a fix is then replayed by the next `ctest`,
# with nothing to remember to reconfigure.
#
# Both shapes of harness take inputs as arguments and fail by exiting non-zero
# -- libFuzzer runs each named file once, and the standalone runner built for
# compilers without libFuzzer does the same -- so this needs no fuzzing engine
# and says nothing about coverage. It is a regression check: every input that
# once crashed hobbes, replayed under whatever sanitizers the build enables.
#
# Expects HARNESS (the built executable) and CORPUS (a directory of inputs).

if(NOT DEFINED HARNESS OR NOT DEFINED CORPUS)
  message(FATAL_ERROR "replay-corpus: both HARNESS and CORPUS must be defined")
endif()

file(GLOB entries "${CORPUS}/*")

set(inputs "")
foreach(entry IN LISTS entries)
  if(NOT IS_DIRECTORY "${entry}")
    list(APPEND inputs "${entry}")
  endif()
endforeach()

if(NOT inputs)
  message(STATUS "replay-corpus: no inputs in ${CORPUS}, nothing to replay")
  return()
endif()

list(LENGTH inputs count)
message(STATUS "replay-corpus: replaying ${count} input(s) from ${CORPUS}")

execute_process(COMMAND "${HARNESS}" ${inputs} RESULT_VARIABLE status)

# a signal is reported as a string ("Segmentation fault"), an exit code as a
# number, and either means the harness did not survive its own corpus
if(NOT status STREQUAL "0")
  message(FATAL_ERROR "replay-corpus: ${HARNESS} did not survive ${CORPUS} (${status})")
endif()

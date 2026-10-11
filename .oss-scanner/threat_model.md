# Threat model

The threat model is `doc/en/security.rst`, with a summary in `SECURITY.md`.
Read both before starting; what they put in and out of scope applies here.

In short: Hobbes source is trusted. Once compiled, Hobbes code can do anything
C can do in the host process, by design, so nothing that follows from running
(or type-checking) Hobbes source is a finding. What must be safe on arbitrary
bytes is everything read *before* that trust applies: source text being lexed
and parsed, binary type descriptions (`hobbes::decode`), RPC handshake and
framing, `hog -s` transaction streams, `hi -w` requests before evaluation,
and the structural validation of structured data files and `hog` queue
segments.

## How to exercise it

The image has one build in `/src/build`: `-O2 -g`, AddressSanitizer and
UndefinedBehaviorSanitizer, asserts on, fuzz harnesses linked with libFuzzer.

- `fuzz/README.md` lists the harnesses and the surface each covers: binary
  type descriptions, structured data files, source text, and `hog -s`
  transaction streams. They are built as `/src/build/fuzz/fuzz-<name>` with
  seed corpora in `fuzz/corpus/<name>/`. Pass a file to replay one input. Use
  `-detect_leaks=0`.
- RPC handshake and framing, and `hi -w` request parsing and path mapping, have
  no harness. Exercise them through the tools below.
- `/src/build/hobbes-test` is the unit test suite (`--tests <Group>` runs the
  group in `test/<Group>.C`).
- `/src/build/hi` and `/src/build/hog` are the tools; `hi -w <port>`,
  `hi -p <port>` and `hog -s <port>` can be driven over localhost.
- If a stack overflow does not reproduce, retry with `ulimit -s 4096`.

## How you rate severity

- **Critical**: memory corruption reachable from a network listener before the
  peer is trusted, with a credible path to code execution; `hi -w` serving
  files outside its document roots.
- **High**: any other out-of-bounds write, use-after-free or double free from
  an untrusted surface; an out-of-bounds read over the network that discloses
  memory.
- **Medium**: other out-of-bounds reads; crashes, allocations sized by an
  unverified length, or non-terminating loops from an untrusted surface; one
  connection stalling a listener for everyone.
- **Low**: the same, reachable only from a structured data file or queue
  segment (trusted writer) with no memory corruption.
- Throwing an exception on malformed input is intended, not a finding.
- LeakSanitizer reports are expected: evaluation memory is arena-scoped and
  released in bulk.

## Reports and patches

What makes a report quick to act on here, roughly in order of how much time
each saves:

1. **A file that reproduces it, and the one command that replays it.** For a
   harness, that is `/src/build/fuzz/fuzz-<name> <file>`; for `hi`, `hog` or a
   listener, the exact argv and the bytes sent. Name a harness reproducer
   `<source>_<id>_<what>.bin`, as `fuzz/corpus/` already does
   (`ossfuzz_554287846_determinization_steps.bin`), and say which corpus
   directory it belongs in.
2. **The sanitizer report as emitted**, with the frames. Which sanitizer fired
   and on which line matters as much as the backtrace: an alignment report
   reads very differently from a heap overflow.
3. **Which surface it came in through**, in the terms used above -- source
   text, a binary type description, an RPC frame, a `hog -s` transaction, an
   `hi -w` request, a structured data file, a queue segment. That decides the
   severity bucket and often the right layer for the fix.
4. **Where the untrusted value is first trusted.** A length read from the wire
   and used to size an allocation should be bounded at the read, not at the
   allocation, and a patch at the wrong layer tends to leave the other callers
   exposed. Say which call sites share the path.
5. **Whether it is reachable with a default-built embedder**, or needs an
   option that is off by default (`enableFileWrites`, `enableRemoteInvocation`
   and the rest in `include/hobbes/eval/cc.H` are all off unless the embedder
   turns them on).

A proposed patch is welcome and is read as a starting point, not something to
be merged as sent. One that fits the tree:

- Adds a regression test in the existing group for the code it touches
  (`test/<Group>.C`, run with `hobbes-test --tests <Group>`) -- a hardening
  test goes with what it exercises, not in a group of its own.
- Adds the reproducer to `fuzz/corpus/<name>/` when a harness reaches the bug,
  so the replay in CI keeps it fixed.
- Rejects malformed input by throwing. Exceptions are the error path here;
  `abort()`, `exit()` and writing to stderr are not.
- Compiles clean under `-Werror` across the matrix in
  `.github/workflows/build.yml` (gcc 13-15, clang 16-21). Two traps worth
  knowing: a variable used only by an `assert` becomes unused where `NDEBUG`
  is set, and `-Wold-style-cast` is an error.
- Says what it costs on the normal path if it adds a check to a hot one --
  `region::malloc` and the type memo are on every allocation and every
  inference.

If the fix is not obvious, a report that stops at 1-4 is more useful than a
patch that guesses; say what you considered and why you left it.

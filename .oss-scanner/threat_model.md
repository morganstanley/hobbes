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

- `fuzz/README.md` maps each untrusted surface to a harness, built as
  `/src/build/fuzz/fuzz-<name>` with seed corpora in `fuzz/corpus/<name>/`.
  Pass a file to replay one input. Use `-detect_leaks=0`.
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

Fixes land on `main`. A patch should add a regression test in
`test/<Group>.C`, and, when a fuzz harness reaches the bug, the reproducer in
that harness's `fuzz/corpus/` directory.

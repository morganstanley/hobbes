# AGENTS.md

## Project Overview

Hobbes is a compiled, strongly-typed language with an embedded JIT compiler and runtime for efficient dynamic expression evaluation, data storage, and analysis. It is designed for high-performance integration with C/C++ applications. Originally developed at Morgan Stanley, licensed under Apache 2.0.

## Build

Requires CMake 3.4+, LLVM 12-16, a C++17 compiler (GCC or Clang), and system libraries (zlib, readline, ncurses, zstd).

```bash
cmake --build /path/to/build -j$(nproc)
```

The build produces:
- `libhobbes.a` / `libhobbes-pic.a` — static libraries
- `hi` — interactive REPL shell
- `hog` — structured data recording utility
- `hobbes-test` — test runner

Set `LLVM_DIR` if CMake cannot find LLVM's `LLVMConfig.cmake`.

## Testing

Custom framework in `test/test.H`. Tests use the `TEST(Group, Name)` macro and assertions like `EXPECT_EQ`, `EXPECT_TRUE`, `EXPECT_ALMOST_EQ`, `EXPECT_EXCEPTION`.

```bash
# Run all tests
./hobbes-test

# Run a specific test group
./hobbes-test --tests Storage
```

There are 18 test groups: Arrays, Compiler, Convert, Definitions, Existentials, MC, Matching, Net, Objects, PREPL, Prelude, Python, Recursives, Spawn, Storage, Structs, TypeInf, Variants.

## Architecture

### Directory Layout

```
include/hobbes/       Public C++ API headers
  eval/               Compiler and JIT infrastructure
  lang/               Type system and language core
  db/                 Structured data handling
  ipc/                Inter-process communication
  parse/              LALR(1) parser infrastructure
  read/               Expression/module parsing
  events/             Event handling
  mc/                 Machine code generation (x86-64 only)
  util/               Utilities (strings, memory regions, arrays)
lib/hobbes/           Implementation sources (.C files)
  boot/               Bootstrap and prelude definitions
  db/                 Storage implementation
  eval/               JIT/compilation implementation
  ipc/                Network and RPC
  lang/               Core language and type system
    preds/            Type class constraint system
    pat/              Pattern matching
  parse/              Parser implementation
  read/               Lexer/parser generation
  events/             Event signal infrastructure
  util/               Utility implementation
bin/
  hi/                 Interactive REPL source
  hog/                Data recording utility source
test/                 Test suite (one .C file per group)
scripts/              Python utilities (fregion.py reader)
cmake/                CMake find-modules
doc/                  Documentation
```

### Key Subsystems

**Compiler (`eval/`)**: `cc.H` is the main compiler class. `jitcc.H` handles LLVM JIT. Expression compilation produces native code via LLVM MCJIT or ORC JIT.

**Type System (`lang/`)**: Hindley-Milner type inference with type classes, functional dependencies, and qualified types. Core types in `expr.H` and `type.C`. Constraint resolution in `preds/`.

**Storage (`db/`, `storage.H`, `fregion.H`)**: Memory-mapped file regions with page-based allocation. `fregion.H` defines the on-disk format — file header, page table, and environment bindings. `storage.H` provides ring-buffer shared memory with QoS and transaction support.

**Machine Code (`mc/`)**: `encode.H` is an x86-64 instruction encoder. `regalloc.H` does register allocation (depends on `encode.H`). `liveness.H` is architecture-independent liveness analysis. The entire MC encoding/regalloc path is x86-64 only.

**Networking (`ipc/`)**: RPC protocol via `prepl.C` and `net.C`. Supports remote expression evaluation.

**Parsing (`parse/`, `read/`)**: LALR(1) parser generator. `hexpr.parse.C` and `hexpr.lex.C` define the Hobbes expression grammar.

## Platform Notes

### macOS / Apple Silicon (ARM64)

- Page size is 16KB (vs 4KB on x86-64). The fregion format supports this.
- MC module tests (`test/MC.C`) are guarded with `#if defined(__x86_64__)` since the encoder emits x86 opcodes. The `BlockLivenessEqualsBasicLiveness` test is architecture-independent and runs on all platforms.
- Net tests pass but the process may terminate with `libc++abi: terminating` during cleanup due to detached thread lifecycle — this is cosmetic, not a test failure.

## Conventions

### File Extensions

- Headers: `.H` (e.g., `expr.H`, `storage.H`)
- Implementation: `.C` (e.g., `expr.C`, `cc.C`)

### Naming

- Types/classes: `PascalCase` (e.g., `ExprPtr`, `MonoTypePtr`)
- Functions/methods: `camelCase` (e.g., `compileFn`, `typeEnv`)
- Type aliases use `using` declarations

### Macros

- `DEFINE_STRUCT(Name, fields...)` — reflect C++ structs for Hobbes
- `DEFINE_STORAGE_GROUP(Name, ...)` — configure storage groups
- `HSTORE(group, field, value)` — record data
- `DEFINE_NET_CLIENT(Name, ...)` — define RPC client interfaces
- `TEST(Group, Name)` — define a test case

### Contribution Requirements

See `CONTRIBUTING.md`. Key points:
- Developer Certificate of Origin (DCO) required for first contribution
- Commits must include `"Covered by <dco>"` line
- Update README for API/behavior changes
- Add or update tests for all changes
- Squashed commits preferred
- One reviewer sign-off required to merge

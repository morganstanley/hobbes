# Security Policy

## Reporting a Vulnerability

If you believe you have found a security issue or vulnerability in Hobbes, we
encourage you to let us know right away. Please report it through Morgan
Stanley's coordinated vulnerability disclosure process:

https://www.morganstanley.com/vulnerability-disclosure

Please do not report security vulnerabilities through public GitHub issues.

## Scope: what is (and is not) a vulnerability in Hobbes

Hobbes is an embedded compiler and JIT. It runs inside a host process, with
that process's privileges, and by design it is **not a sandbox**. Some
behaviors that would be vulnerabilities in other software are intentional
design choices here. The full threat model is documented in
[doc/en/security.rst](doc/en/security.rst); in brief:

**In scope** (please report):

* Memory unsafety (out-of-bounds read/write, use-after-free, unchecked
  allocation sizes) reachable from *malformed data*: structured data files
  read by `hobbes::fregion::reader` / `hi` / `hog`, binary type descriptions
  decoded by `hobbes::decode` (`hobbes/lang/type.H`), or network payloads
  processed before a peer would be trusted.
* Crashes or memory corruption in the lexer/parser triggered by malformed
  *source text*, as distinct from the behavior of successfully compiled code.
* Vulnerabilities in the build, release, or CI pipeline of this repository.

**Out of scope** (by design, not vulnerabilities):

* Hobbes code having full access to the host process. Hobbes source code is
  trusted code, equivalent to C++ compiled into the process. There is no
  sandboxed runtime, no array bounds checking in generated code, and direct
  memory access is a feature.
* Remote code execution over the RPC/networking layer by a connected peer.
  Hobbes RPC exists to let peers define and invoke native code remotely; it
  must only be exposed on trusted internal networks (see the threat model).
* Denial of service through legitimately compiled Hobbes code (e.g. a
  nonterminating expression).

## Supported Versions

Security fixes are applied to the `main` branch. There are currently no
maintained release branches; consumers are expected to track `main`.

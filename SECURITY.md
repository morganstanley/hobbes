# Security Policy

## Reporting a Vulnerability

If you believe you have found a security issue or vulnerability in Hobbes, we
encourage you to let us know right away. Please report it through Morgan
Stanley's responsible disclosure program:

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
  allocation sizes), unbounded allocation, or non-terminating loops reachable
  from *malformed data*:
  * structured data files read by `hobbes::fregion::reader` / `hi` / `hog`,
    including the stored lengths, offsets and series links a reader follows
    after opening a file, and the shared-memory queue segments `hog` reads
    from local producers;
  * binary type descriptions decoded by `hobbes::decode`
    (`hobbes/lang/type.H`);
  * network input read before a peer would be trusted: the RPC handshake and
    framing, transaction streams received by `hog -s`, and HTTP requests to
    `hi -w` before any expression in them is evaluated (including which files
    a request path can reach).
* Crashes, memory corruption, or unbounded work in the lexer/parser triggered
  by malformed *source text*, as distinct from type-checking or running it.
* One connection or producer being able to crash or stall a listener for
  everyone else before it is trusted.
* Vulnerabilities in the build, release, or CI pipeline of this repository.

**Out of scope** (by design, not vulnerabilities):

* Hobbes code having full access to the host process. Hobbes source code is
  trusted code, equivalent to C++ compiled into the process. There is no
  sandboxed runtime, no array bounds checking in generated code, and direct
  memory access is a feature.
* Side effects of *type-checking* Hobbes source. Some type-class constraints
  (`Connect`, `Invoke`, `LoadFile`, `Ls`, `Process`) consult files, the
  network or other programs while an expression's type is inferred, so
  type-checking source carries the same trust as running it.
* Remote code execution over the RPC/networking layer by a connected peer.
  Hobbes RPC exists to let peers define and invoke native code remotely; it
  must only be exposed on trusted internal networks (see the threat model).
* Getting around `option Safe`. It is a name-based guard rail against
  accidents, not a sandbox; code compiled under it is still trusted.
* Denial of service through legitimately compiled Hobbes code (e.g. a
  nonterminating expression).

Type-checking side effects and ways around `option Safe` are still worth
hearing about as hardening changes: open an ordinary GitHub issue or pull
request for those.

## Supported Versions

Security fixes are applied to the `main` branch. There are currently no
maintained release branches; consumers are expected to track `main`.

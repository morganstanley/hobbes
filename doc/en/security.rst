.. _hobbes_security:

Security Model
**************

This page describes the trust boundaries Hobbes assumes, what that means for
applications embedding it, and which classes of behavior are considered
security defects (please report those — see ``SECURITY.md`` in the repository
root) versus intentional design.

Hobbes is an embedded compiler and JIT for high-performance integration with
C/C++ applications. It runs *inside* a host process, with the privileges of
that process. It is not a sandbox, and the security boundary is always owned
by the embedding application.

Trust boundaries
================

Hobbes source code is trusted code
----------------------------------

Hobbes scripts and expressions are equivalent to C++ compiled into your
process. Generated code has direct access to memory, arrays are not
bounds-checked at runtime, and there are no runtime safety features or
resource limits. Evaluating an expression means running native code with the
full privileges of the host process.

**Implication:** never compile and evaluate Hobbes source text from an
untrusted party. Treat Hobbes source the way you treat C++ source: something
you review and deploy, not something you accept as input.

Type-checking is not a safe way to inspect untrusted source
-----------------------------------------------------------

Deciding what type an expression has is part of compiling it, and some
type-class constraints are resolved by consulting the outside world. Resolving
them happens during inference — for a ``:t`` in the REPL, a ``typeof``, a net
REPL ``prepare``, or an application that only validates a user's expression
without ever running it — before there is any decision to evaluate anything.
The constraints that currently do this are:

* ``Connect "host:port" c`` opens a network (or Unix-domain) connection to
  learn the peer's type environment, and ``Invoke`` sends an expression over
  such a connection to be evaluated there. Both are refused unless the
  embedding application allows them with ``cc::enableRemoteConnections`` and
  ``cc::enableRemoteInvocation``, either for an exact set of ``host:port``
  strings or for any target. The two are independent: being willing to
  connect to a peer does not imply trusting it to run code.
* ``LoadFile "path" f`` opens a structured data file to learn its type. For an
  *output* file — which creates and truncates ``path`` — this is refused
  unless the embedding application allows it with ``cc::enableFileWrites``,
  again either for an exact set of paths or for any path. Opening an input
  file to read its type is not gated.
* ``Ls "pattern" x`` expands a filesystem glob into the type;
* ``Process "cmd" p`` starts a program, and is refused unless the embedding
  application allows that exact command with ``cc::enableProcessSpawning``.

**Implication:** type-checking untrusted Hobbes source carries the same trust
assumption as evaluating it. Only the step before it — reading source text,
below — is held to the "safe on arbitrary bytes" standard. Constraint
resolution that performs I/O is a design property of these classes, not a
defect; the opt-in gates above narrow the ones that reach outside the process
to write, connect, or execute, but nothing turns type-checking into a safe way
to inspect source you do not trust. Narrowing the rest (``Ls``, and reading an
input file) further is welcome as a hardening change through the normal issue
tracker.

Reading source text must be safe
--------------------------------

The parser sits on the near side of the boundary: *reading* source text
(lexing and parsing, before anything is type-checked, compiled or evaluated)
must be safe on arbitrary bytes. A crash or memory error in the lexer/parser
on malformed input is a defect.

Safe on arbitrary bytes includes not being asked for unbounded work, and
because nearly everything that handles an expression walks it recursively,
nesting depth is stack depth. Where input can ask for more than that, the
read (or compile) fails with an error at a fixed limit rather than running on:

.. list-table::
   :header-rows: 1
   :widths: 40 60

   * - Limit
     - Default
   * - Expression nesting, checked on every expression, definition, instance
       member and quoted expression the parser returns
     - 1,000 levels (``maxExprNestingDepth``)
   * - Terms in a regex literal
     - 1,000
   * - Regex DFA construction
     - 10,000 states (``cc::regexMaxDFAStates``); the expression form of a
       DFA and its transitions have their own budgets
       (``cc::regexMaxExprDFASize``, ``cc::regexMaxExprDFATransitions``)
   * - Pattern-match tables
     - 1,000,000 cells, 10,000 levels of decision depth
   * - Type-class instance resolution
     - refused once 256 nested steps each ask about a larger type than the
       step before, or one step asks about a type 32× the size of the
       original request

The ``cc`` setters raise the regex budgets where a legitimate program needs
more. These limits are on the reading and compiling side only; they do not
make evaluating an expression cheap, or type-checking it safe (above).

Networking and RPC assume a trusted network
-------------------------------------------

The networking layer (``hobbes/ipc``, ``hobbes::net``, and ``hi -p``) lets a
connected peer define and invoke expressions remotely — that is its purpose.
A peer that can connect can execute native code in the server process.

There is no built-in authentication, authorization, or transport encryption.

**Implication:** only expose Hobbes RPC endpoints on trusted internal
networks. If the transport crosses anything less trusted, the embedding
application must provide the controls (network segmentation, firewalls,
authenticated tunnels such as TLS or SSH).

Protocol handling on the near side of the boundary must still be robust.
Bytes read before a peer is trusted — the handshake and version word, message
framing and lengths, and the binary type descriptions decoded by
``hobbes::decode`` (``hobbes/lang/type.H``) — must be rejected cleanly: no
memory unsafety, no allocation sized by an unverified length, and no way for
one connection to crash or stall the process for everyone else. The same
standard applies to the other listeners Hobbes ships:

* ``hog``'s network collector (``hog -s <port>``) accepts a stream of
  transactions from any producer that connects and decodes it with the
  ``HStoreRead`` instances in ``bin/hog/boot/read.hob``. Nothing
  authenticates a producer, so every length, count and statement id it sends
  is untrusted input.
* ``hi -w`` parses HTTP requests and serves files before evaluating
  anything. Request parsing, and mapping a request path to a file under the
  server's document roots, are on the near side of the boundary; the
  expressions a query carries are not.

Defects in any of these are in scope for security reports.

``option Safe`` is a guard rail, not a sandbox
----------------------------------------------

``option Safe`` (on by default in ``hi``, see ``bin/hi/evaluator.H``) refuses
to compile expressions that name certain primitives — unchecked casts, raw
array construction, and ``hi``'s process and filesystem helpers such as
``pexec`` and ``readfile`` — and definitions built from them. It is a
deny-list over names, applied before type-checking. It exists to keep
well-meaning code away from the sharpest tools, and it narrows what an
expression reaching ``hi -p`` or ``hi -w`` can do by accident.

It is **not** a security boundary. The language gives many routes to raw
memory, files and the network that are not on the list, and modules can
adjust the list with ``{-# SAFE name #-}``/``{-# UNSAFE name #-}`` pragmas.
Code that can be compiled with ``Safe`` on must still be trusted. A way to
reach a primitive that ``Safe`` meant to deny is worth reporting as a
hardening issue, but it is not a vulnerability in its own right.

``hi -o no-Safe`` turns it off for a local session. Don't combine that with
``-p`` or ``-w``.

Structured data files assume a trusted writer
---------------------------------------------

Structured data (fregion) files — as produced by ``hog`` and read by ``hi``
and the ``hobbes::fregion::reader`` API (declared in ``hobbes/fregion.H``) —
are memory-mapped into the reading process.
The reader validates file structure (malformed images are rejected with an
error rather than trusted), but these files are designed as a
high-performance shared medium between cooperating processes, not as an
interchange format for data from arbitrary sources.

``hog``'s local transport is the same kind of shared medium: a producer and
``hog`` share a memory-mapped queue segment (``hobbes/storage.H``), and
``hog`` reads the producer's statement metadata and transactions out of it.

**Implications:**

* Prefer reading structured data files written by processes you trust, and
  use filesystem permissions to control who can write them: a writer shares a
  memory mapping with every reader. The same holds for who may connect to a
  ``hog`` group's local socket.
* Even so, the reader's structural validation must be safe on arbitrary
  bytes. That covers everything a reader takes from the file or segment to
  decide where to read next: file headers, page metadata, environment
  records, the lengths and offsets of stored arrays, strings and file
  references, the links between the batches of a stored series, and a queue
  segment's header, indices and per-page byte counts. An out-of-bounds read
  or write, an unbounded allocation, or a loop that does not terminate,
  triggered by a corrupt or crafted file or segment, is a defect — report it.

Summary table
=============

.. list-table::
   :header-rows: 1
   :widths: 45 55

   * - Input
     - Trust assumption
   * - Hobbes source, compiled and evaluated
     - Trusted — equivalent to native code
   * - Hobbes source, type-checked only
     - Trusted — constraint resolution reads files and globs the filesystem;
       spawning, connecting, remote invocation and file creation are opt-in
   * - Hobbes source, lexed/parsed only
     - Untrusted — the parser must be safe and bounded
   * - Expressions under ``option Safe``
     - Trusted — ``Safe`` is a guard rail, not a boundary
   * - RPC peers (post-handshake semantics)
     - Trusted — peers execute code by design
   * - RPC wire bytes (handshake, framing, type descriptions)
     - Untrusted — the decoder must be safe
   * - ``hog -s`` transaction streams
     - Untrusted — decoding must be safe
   * - ``hi -w`` HTTP requests (before evaluation)
     - Untrusted — parsing and path mapping must be safe
   * - Structured data files and ``hog`` queue segments
     - Trusted writers — the reader must still reject malformed images
       safely

How Hobbes is tested against these inputs
=========================================

* **Fuzzing.** The untrusted-input surfaces have libFuzzer harnesses in
  ``fuzz/`` (type decoding, structured data file images, and source text).
  They run continuously on OSS-Fuzz and on every pull request through
  ClusterFuzzLite, and CI replays their corpora as tests in its
  AddressSanitizer and UndefinedBehaviorSanitizer build (see
  ``fuzz/README.md``).
* **Static analysis.** CodeQL scans ``main`` on every push and on a
  schedule, and OpenSSF Scorecard scores the repository's supply-chain
  posture.
* **Pinned CI.** Workflow actions are pinned to commit SHAs and updated by
  Dependabot; workflows run with read-only tokens unless a job needs more.

Guidance for embedding applications
===================================

* Run processes embedding Hobbes with the least privilege they need; assume
  any Hobbes code they evaluate — or type-check — can do anything the
  process can do.
* Keep RPC endpoints, ``hi -p``/``hi -w`` and ``hog -s`` on trusted network
  segments; wrap them in authenticated, encrypted transports if they must
  cross anything else.
* Don't rely on ``option Safe`` to contain code you don't trust.
* Only call ``cc::enableProcessSpawning``, ``cc::enableRemoteConnections``,
  ``cc::enableRemoteInvocation`` or ``cc::enableFileWrites`` on a compiler
  that never type-checks untrusted input: an allowed command is spawned, and
  an allowed target connected to, invoked on, or truncated, as soon as
  matching text is type-checked. Prefer the allowlist form over the
  allow-anything form where the application knows its targets.
* This is what decides the default in ``hi``: a plain ``hi`` session or
  script is source the user chose to run, so it enables connections,
  invocation and file writes, the same way running a ``python`` or ``perl``
  script is expected to reach the disk and the network. ``hi -p`` and
  ``hi -w`` do not: that compiler type-checks expressions arriving from
  whoever can reach the port, so those constraints stay refused there.

  The two are all-or-nothing per process, because the startup scripts and
  the served expressions share one compiler. A script that opens an output
  file or a connection therefore cannot be loaded by a ``hi`` that also
  serves ``-p`` or ``-w``; run the serving instance from a script that does
  not need them, or embed Hobbes and allowlist the exact targets. (Note that
  ``invoke`` additionally needs ``-o no-Safe`` in any ``hi``, gates aside:
  the code it generates names ``unsafeCast``, which ``option Safe`` denies.)
* Restrict write access to structured data files, and access to ``hog``'s
  local sockets, to the processes that are supposed to produce them.
* Know the memory model before pointing analysis tooling at an embedding
  process: evaluation memory is transaction-scoped and reclaimed by resetting
  an arena rather than by destructors, so per-allocation leak checkers report
  it as lost by design (see :ref:`hobbes_memory_model`).
* Keep up with ``main``: security fixes land there (see ``SECURITY.md``).

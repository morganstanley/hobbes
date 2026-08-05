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

The parser, however, sits on the near side of this boundary: *reading* source
text (lexing and parsing, before anything is compiled or evaluated) must be
safe on arbitrary bytes. A crash or memory error in the lexer/parser on
malformed input is a defect.

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

Protocol handling on the near side of the boundary must still be robust:
malformed bytes on the wire — in particular the binary type descriptions
decoded by ``ty::decode`` before any expression is accepted — must be
rejected cleanly, never cause memory unsafety. Defects here are in scope for
security reports.

Structured data files assume a trusted writer
---------------------------------------------

Structured data (fregion) files — as produced by ``hog`` and read by ``hi``
and the ``fregion.H`` reader — are memory-mapped into the reading process.
The reader validates file structure (malformed images are rejected with an
error rather than trusted), but these files are designed as a
high-performance shared medium between cooperating processes, not as an
interchange format for data from arbitrary sources.

**Implications:**

* Prefer reading structured data files written by processes you trust, and
  use filesystem permissions to control who can write them: a writer shares a
  memory mapping with every reader.
* Even so, the reader's structural validation (file headers, page metadata,
  environment records, stored lengths and offsets) must be safe on arbitrary
  bytes. An out-of-bounds read or write triggered by a corrupt or crafted
  file is a defect — report it.

Summary table
=============

===============================================  ==========================================
Input                                            Trust assumption
===============================================  ==========================================
Hobbes source (compiled and evaluated)           Trusted — equivalent to native code
Hobbes source (lexed/parsed only)                Untrusted — parser must be safe
RPC peers (post-handshake semantics)             Trusted — peers execute code by design
RPC wire bytes (framing, type descriptions)      Untrusted — decoder must be safe
Structured data files (fregion / hog logs)       Trusted writers — reader must still
                                                 reject malformed images safely
===============================================  ==========================================

Guidance for embedding applications
===================================

* Run processes embedding Hobbes with the least privilege they need; assume
  any Hobbes code they evaluate can do anything the process can do.
* Keep RPC endpoints on trusted network segments; wrap them in authenticated,
  encrypted transports if they must cross anything else.
* Restrict write access to structured data files to the processes that are
  supposed to produce them.
* Keep up with ``main``: security fixes land there (see ``SECURITY.md``).

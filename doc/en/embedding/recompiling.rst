.. _hobbes_recompiling:

Recompiling Off the Critical Path
*********************************

An application that embeds Hobbes usually has one thread that must never
pause: the one handling orders, ticks, or requests. The same application
usually has logic that changes during the day — a limit table edited from a
GUI, a rule set reloaded from a file — and each change means a compile. A
large match table takes seconds to compile even on a release build (see
:ref:`hobbes_recompiling_budget`), and a compile that runs on the handling
thread is a pause of exactly that length.

Nothing in Hobbes needs to change to avoid this; the compile has to happen on
another thread, and the handling thread has to switch between compiled
functions rather than compile them. This page covers the properties of
``hobbes::cc`` that shape such a design, two patterns that satisfy them, and
the numbers to budget with.

What a ``hobbes::cc`` is
========================

Four facts about the compiler object drive everything below.

**It is state, not a value.** A ``cc`` accumulates everything done to it:
each ``bind``, ``define``, module compiled into it with ``hobbes::compile``,
type alias, class instance, and every option set on it
(``buildColumnwiseMatches``, ``ignoreUnreachableMatches``, and the rest). It has no copy constructor, and
there is no way to clone one — the state includes a JIT session holding
machine code. Two ``cc`` objects are identical only if they were built by
the same sequence of operations.

**A compiled function is a pointer that lives as long as its ``cc``.** What
``compileFn`` returns is plain machine code. Calling it does not touch the
``cc`` and takes no lock, so it is safe to call from any thread, as fast as
the equivalent C++ — and it is a dangling pointer the moment the ``cc`` that
produced it is destroyed. Whoever holds the pointer must keep the ``cc``
alive.

**Definitions are permanent.** ``cc::define`` throws rather than redefine a
name that already has a definition, and there is no way to unload machine
code (``cc::releaseMachineCode`` exists, but what it forwards to currently
does nothing). A ``cc`` only grows. Replacing logic therefore means compiling a
*new* function — under a new name, or anonymously via ``compileFn`` — and
switching to it, never editing the old one in place.

**Compiling holds one process-wide lock.** The ``cc`` operations that touch
the compiler or the JIT — ``compileFn``, ``define``, ``bind``, ``readExpr``,
``search``, ``readModule`` and their relatives — each take
``hobbes::hlock``, a single recursive mutex shared by every ``cc`` in the
process, for the duration of the call. (Plain option setters such as
``buildColumnwiseMatches`` do not.) ``compileFn`` takes it once to parse
and again for the compile proper, which is where the seconds go, so for
practical purposes the lock is held for the length of the compile. Separate
``cc`` objects on separate threads are supported (the
``Compiler/ccInManyThreads`` test exercises exactly that), but their
compiles serialise, and a locking ``cc`` method called on another thread
while a compile is running waits for the compile to finish. On the handling
thread this is the whole hazard in miniature: a "cheap" lazy ``compileFn``
or ``bind`` that happens to land during a background compile stalls for the
remaining length of it.

Together these give the rules for the handling thread:

* it calls compiled functions and does nothing else with Hobbes — no
  ``cc`` method of any kind while the application is live;
* it switches between functions by swapping a pointer, between two events,
  never mid-call;
* it never runs on a ``cc`` that another thread is compiling into, and no
  ``cc`` it might still be calling into is destroyed or compiled into until
  it has said it has moved off.

The shape of the solution
=========================

Whichever pattern below is used, the source of truth is not a ``cc`` but a
**description of the current logic** that the application owns: a map from
name to source text (plus options), kept in the order the names were first
introduced. Every ``cc`` the application ever builds is built from that
description by one bootstrap function, and nothing else is allowed to call
``bind`` or ``define`` — that single rule is what makes any two ``cc``
objects faithful copies of each other.

.. code-block:: c++

  struct Logic {
    // names in first-definition order, so that a later edit to an early
    // definition keeps its place ahead of whatever depends on it
    std::vector<std::string>           order;
    std::map<std::string, std::string> source;
  };

  std::unique_ptr<hobbes::cc> makeCC(const Logic& logic) {
    auto c = std::make_unique<hobbes::cc>();
    c->buildColumnwiseMatches(true);                             // every option, every time
    c->bind("lookupAccount", &lookupAccount);                    // every binding, every time
    hobbes::compile(c.get(), c->readModuleFile("trading.hob"));  // every module, every time
    for (const auto& n : logic.order) {
      c->define(n, logic.source.at(n));
    }
    return c;
  }

A change arriving from the GUI is then: update ``Logic``, produce a new
compiled function from it on a **compiler thread**, hand the function to
the handling thread as an event on its own queue, and let the handling
thread swap it in between two events. Keep the ``Logic`` description even
if it is only used to rebuild after a failure; it is also the audit trail of
what the humans changed and the state to restore on restart.

While the compile is in flight the handling thread keeps using the previous
function. When the update is driven by a person at a GUI this is the right
behaviour — echo back "compiling" and then "live" — and it avoids the only
alternative, which is stalling. (If a change must take effect at the
instant it arrives, the bridge is an interpreted matcher installed
immediately and replaced by the compiled one when it is ready; that costs
match speed for the duration of the compile rather than stalling.)

Pattern one: rebuild from the description
=========================================

The simplest correct design. Each change builds a whole new ``cc`` from
``Logic`` on the compiler thread, the handling thread swaps to its function,
and the previous ``cc`` is destroyed — on a reaper thread, since tearing
down a JIT session with a large module in it is itself not free.

.. code-block:: c++

  struct Matcher {
    uint64_t                    version;
    MatchFn                     fn;      // what the handler calls
    std::shared_ptr<hobbes::cc> owner;   // keeps fn valid
  };

  // compiler thread
  Matcher build(uint64_t version, const Logic& logic) {
    std::shared_ptr<hobbes::cc> c = makeCC(logic);
    MatchFn f = c->compileFn<MatchFnSig>(...);
    f(sampleInput);                       // first call off the hot path
    return {version, f, c};
  }

  // handling thread, on receiving Matcher m from its queue
  void onMatcherReady(Matcher m) {
    if (m.version != latestVersion) return;   // superseded, drop it
    reaper.dispose(std::move(current.owner)); // old cc dies elsewhere
    current = std::move(m);
  }

Its properties:

* **Stable names.** Because every ``cc`` is built fresh, an edited
  definition keeps its name and everything that depends on it is re-resolved
  against the new body. This is the only pattern that works when Hobbes
  code refers to the changing logic by name.
* **Validation is free.** A bad edit fails inside ``makeCC`` or
  ``compileFn`` on the compiler thread; report the error, keep the current
  ``cc``, nothing to roll back.
* **Cost is proportional to the whole description, not the change.** A
  fresh ``cc`` recompiles the prelude and every table, not just the edited
  one. Keeping a *spare* ``cc`` — bootstrapped with the prelude ahead of
  time and refilled after each use — hides the prelude; it does not hide
  the other tables.
* **Destroying the old ``cc`` reclaims little.** See
  :ref:`hobbes_recompiling_budget`; do not choose this pattern for its
  memory behaviour.

Pattern two: two compilers, alternating
=======================================

Two ``cc`` objects built by ``makeCC`` at startup, one *hot* (the handling
thread calls into it) and one *cold*. A change is compiled into the cold
side as a new anonymous function; the handling thread swaps to it, at which
point the sides exchange roles; the next change is compiled into the other
side.

.. code-block:: c++

  hobbes::cc* side[2];
  int hot = 0;

  // compiler thread
  Matcher build(uint64_t version, const std::string& tableSource) {
    hobbes::cc* cold = side[1 - hot];
    MatchFn f = cold->compileFn<MatchFnSig>(..., tableSource);
    f(sampleInput);
    return {version, f, nullptr};          // both ccs live forever; nothing to own
  }

  // handling thread
  void onMatcherReady(Matcher m) {
    if (m.version != latestVersion) return;
    current = std::move(m);
    compiler.post(Swapped{m.version});     // "I am off the old side"
  }

  // compiler thread, on Swapped: hot = 1 - hot, and only now may the
  // next build touch what used to be the hot side

The ``Swapped`` acknowledgement is the one piece of protocol that is not
optional. After the swap the previously hot ``cc`` is the one the handling
thread was just calling into; the compiler thread must not compile into it
until the handler has said it has moved, and "I posted the swap event a
while ago" is not the same as that. Gate the next build on the
acknowledgement, not on time.

There is no need to replay the change onto the other side. Each compiled
table is self-contained, so the cold side does not have to hold the previous
version of anything, only the prelude; each side ends up holding every other
version. This halves the compiler-thread work and the memory growth relative
to compiling every change into both sides — and it is why the pattern
requires that changing logic is a *leaf*: the application holds the
function pointer, and no Hobbes definition refers to it by name. If other
definitions do depend on it, ``define``'s refusal to redefine means they
cannot be updated in place, and pattern one is the answer.

Its properties:

* **Cost is proportional to the change.** One table compile per edit,
  whatever else is defined.
* **Validation is still free.** A failed compile on the cold side leaves
  the hot side untouched.
* **Keep pattern one as the fallback.** If a cold-side build fails for a
  reason that is not the edit itself (memory, most likely), that side is no
  longer trustworthy; rebuild it with ``makeCC`` from the description.

Choosing
--------

* Changing logic is a leaf and there is more than a table or two → pattern
  two for the change, ``Logic`` plus ``makeCC`` for startup and recovery.
* A single table, or logic that other Hobbes definitions reference → pattern
  one, with a spare ``cc`` so an edit costs only its compiles.

In both cases build the ``Logic`` description first; it is the invariant
part, and switching between the patterns later changes nothing the handler
or the GUI can see.

Checking two compilers agree
============================

The bootstrap-only rule is the guarantee, but it is worth a mechanical check
at startup and after every recovery. Both ``cc`` objects expose their
environment:

.. code-block:: c++

  auto a = side[0]->typeEnv()->typeEnvTable();   // std::map<std::string, PolyTypePtr>
  auto b = side[1]->typeEnv()->typeEnvTable();
  // same key set, and hobbes::show(a[n]) == hobbes::show(b[n]) for each n

This catches a binding or definition present on one side and missing on the
other. It does not catch two definitions of the same name and type with
different bodies — only the bootstrap rule does — so for logic whose
decisions matter, run a corpus of real inputs through the function from
each side and compare results. Since the two sides serve consecutive
requests, a divergence would appear as inconsistent decisions across a
swap.

.. _hobbes_recompiling_budget:

What to budget
==============

Measured on a release build (clang 18, LLVM 18, x86-64 Linux), compiling
five successive versions of a generated match table into one long-lived
``cc``, resident size read after ``malloc_trim`` so it reflects what is
retained rather than what the allocator is holding:

====================================  ========  ====================  ==================================
Table                                 Compile   Retained per version  Freed when the ``cc`` is destroyed
====================================  ========  ====================  ==================================
70 rows × 12 columns, row-pivot       ~17 s     ~150 MB               ~20%
70 × 12, ``buildColumnwiseMatches``   ~0.7 s    ~24 MB                ~10%
400 × 12, ``buildColumnwiseMatches``  ~5 s      ~150 MB               ~12%
====================================  ========  ====================  ==================================

Three things follow.

* **Every compile of a large table pins on the order of 100 MB for the
  life of the process**, in either pattern. Destroying the ``cc`` returns
  only a small fraction of it, so building a fresh ``cc`` per change is not
  a way to avoid the growth. Multiply by the number of edits per day and by
  the number of days between restarts, and watch resident size in
  production rather than assuming.
* **``buildColumnwiseMatches`` is the largest lever available.** For the
  same 70-row table it is a 25× faster compile and 6× less memory; a
  400-row table compiles columnwise in the time a 70-row one takes
  row-pivot. Set it in ``makeCC`` for any ``cc`` that will compile a wide
  table.
* **Compile time varies with the table, not only its size.** Of the five
  70×12 tables above, one took twice as long as the others. Report the
  measured time back to whoever made the change rather than promising a
  fixed one.

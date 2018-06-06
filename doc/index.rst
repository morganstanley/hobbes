Hobbes
======

Hobbes is a domain-targeting programming language and execution environment built and maintained at Morgan Stanley. By design , it fullfils four major design goals:
   * Dynamic, in-process rewriting of processing rules for domain objects (trades, orders, executions)
   * Out-of-band processing of structured logs produced by order managers and surrounding processes
   * Rock-solid, ultra-low-latency execution

Hobbes was developed at Morgan Stanley to manage the runtime of low-latency processes such as equities trading engines, which generally cannot be restarted during the trading day.

The target user base is the development and production management teams responsible for building and maintaining these processes in production. As such, Hobbes is *obsessively* pragmatic: the vast majority of language design choices are aimed at fullfilling these needs.

Hobbes is battle-tested through hundreds of thousands of hours of processing time and many hundreds of millions of trading decisions, and has never been responsible for an outage of any kind.

Perhaps most surprisingly, Hobbes is a variant of the lazy, pure-functional programming language *Haskell*

.. note:: Why a custom language?
   It's a great question. The domain (dynamic, non-developer-driven changes to execution behaviour of processes which can't be restarted and which come with a *tight* latency budget) is specialised enough to quickly exhaust existing options.

   Hosting a python environment would be user-friendly but not fast enough.

   Dynamically-compiled C++ would be fast but ugly, brittle and complex.

   Marshalling runtime execution to another process, perhaps one which enjoyed more natural dynamic reimplementation of natural language, would quickly blow latency into the long grass.

Digging into the domain
=======================

Taking a deeper look into our domain might help clear things up.

An order manager takes instruction in the form "Buy 100 stocks of IBM if the price is below 144.2 USD" This is called a *limit order*, because the action is contingent on some property of the stock (i.e. its price).

The order manager ("OM") will hold this order and watch the price of IBM at different exchanges, and once the price drops below 144.2 USD it'll go ahead and buy the stock. An alternative is a *market order*, where the stock is bought regardless of the price.



There's some added complexity, though. One issue is that trades are visible to other players in the market, and if we start to execute a large order we'll affect the price of the stock to our detriment. So a number of trading strategies exist to constrain the effects of an order on the market - this is the basis of *algo trading*.

Further, a client may make further conditions on the execution of an order: they may want to split the order across a number of trading venues (half to NYSE, half to LSE). Depending on the client, you may wish to offer them credit. There may be long-standing *master* agreements between parties that form the basis of a clients trading decisions, that the OM must take into account when deciding when, where, and how to place orders.



This complexity mounts very quickly, and must be managed dynamically (in both senses: *quickly* and *at runtime*!).

However, the go-to tools developers use to manage complex decision trees full of runtime information (type hierarchies and virtual function calls; long if-else chains or switches) are wildly inappropriate in the hot path of a low-latency OM.



Two things are important:
   * The ability to construct and compile a decision tree for a given stock
   * The ability to quickly recompile it when given new constraints

Both the dynamic portion (operands to function calls and control-flow statements; i.e., the stock price) *and* the static portion (the operations themselves) must be changed.

In particular, in-process, compiled logic allows the processor to maximise efficiency by filling an instruction and data pipeline, thus enjoying the benefits of mechanical sympathy. The output of Hobbes is put in a critical trading path where it sees a very large proportion of all daily US equities trades.

Why not Haskell?
================

Despite some similarities, the Hobbes programming language is emphatically *not* a Haskell clone. There are some major differences:

   * Hobbes is not a pure functional programming language
   * Execution of Hobbes code is *strict*, not lazy. Data is evaluated when it's defined, not when it's used.
   * There is no implementation of the Haskell *Boot* libraries - the core standard library that exists by contract in a Haskell implementation.
   * There's no implementation of Haskell "Core" - the typechecked, mostly-unchanging barebones language that all Haskell extensions ultimately compile down to.


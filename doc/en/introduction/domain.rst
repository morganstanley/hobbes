.. _domain:

Digging into the domain
***********************

Taking a deeper look into our domain might help clear things up.

Hobbes has been designed from the ground up to help devops staff manage the in-process configuration of extremely low latency order managers.

An Order Manager ("OM") takes instructions in the form "Buy 100 stocks of *Example co* if the price is below 144.2 USD". This is called a *limit order*, because the action is contingent on some property of the stock (in this case, its price).

The Order Manager will hold this order and watch the price at which *Example co* is being sold at different exchanges, and once the price drops below 144.2 it'll go ahead and issue an instruction to buy the stock. Some time later the exchange will respond with the result: either the purchase succeeded and the order filled, or else the details of the failure.

The alternative to a *limit order* is a *market order*, where the stock is bought regardless of the current price.

Complex Business
================

There's some added complexity, though. One issue is that trades are visible to other players in the market, and if we execute a large order all at once, we'll affect the price of the stock to our detriment. A number of trading strategies exist to constrain the effects of an order on the market - this is the basis of *algo trading*.

Further, a client may make further conditions on the execution of an order: they may want to split the order across a number of trading venues (half to NYSE, half to LSE). Depending on the client, you may wish to offer them credit. There may be long-standing *master agreements* between parties that form the basis of a clients trading decisions, that the OM must take into account when deciding when, where, and how to place orders.

This complexity mounts very quickly, and must be managed dynamically -- in both senses: *quickly* and *at runtime*!

However, the go-to tools which developers use to manage complex decision trees full of runtime information (type hierarchies with virtual function calls; long if-else chains or switches) are wildly inappropriate in the hot path of a low-latency OM.

Two things are important:

   * The ability to construct and compile a decision tree for a given stock
   * The ability to quickly change the behaviour when given new constraints

Both the dynamic portion (the operands to function calls and control flow statements; e.g. the stock's price) *and* the static portion (the operations themselves) must be changeable.

In particular: in-process, compiled logic allows the processor to maximise efficiency by filling an instruction and data pipeline, thus enjoying the benefits of mechanical sympathy. The output of the Hobbes compiler lives in a critical trading path where it sees a very large proportion of daily US equities trades.

.. note:: Why a custom language?

  It's a great question. The domain (dynamic, non-developer-driven changes to execution behaviour of processes which can't be restarted, and which come with a *tight* latency budget) is specialised enough to quickly exhaust most existing solutions:

   * Hosting a python environment would be user-friendly but not fast enough.
   * Dynamically-compiled C++ would be fast but ugly, brittle, and complex.
   * Marshalling runtime execution decisions to another process, perhaps one which enjoyed more natural dynamic mapping to natural language, would quickly blow latency away.

  In addition, a custom *type system* allows for tight bindings with existing data in hosting applications, along with low-latency custom serialisation for binary logging.

Why not Haskell?
================

Despite some similarities, the Hobbes programming language is emphatically *not* a Haskell clone. There are some major differences:

   * Hobbes is not a pure functional language
   * Execution of Hobbes code is *strict*, not lazy. Data dependencies are evaluated when they're defined, not when they're used.
   * There's no implementation of the Haskell *boot* libraries - the core standard library that exists by contract in a Haskell implementation.
   * There's no implementation of Haskell *core* - the typechecked, mostly-unchanging barebones language that all Haskell language extensions ultimately compile down to.
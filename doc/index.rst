Hobbes
------

Hobbes is a domain-targeting programming language and execution environment built and maintained at Morgan Stanley. By design , it fullfils four major design goals:
   * Dynamic, in-process rewriting of processing rules for domain objects (trades, orders, executions)
   * Out-of-band processing of structured logs produced by order managers and surrounding processes
   * Rock-solid, ultra-low-latency execution

Hobbes was developed at Morgan Stanley to manage the runtime of low-latency processes such as equities trading engines, which generally cannot be restarted during the trading day.

The target user base is the development and production management teams responsible for building and maintaining these processes in production. As such, Hobbes is *obsessively* pragmatic: the vast majority of language design choices are aimed at fullfilling these needs.

Hobbes is battle-tested through hundreds of thousands of hours of processing time and many hundreds of millions of trading decisions, and has never been responsible for an outage of any kind.

Perhaps most surprisingly, Hobbes is a variant of the lazy, pure-functional programming language *Haskell*

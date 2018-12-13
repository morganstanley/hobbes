Hobbes
******

Hobbes is a domain-targeting programming language and execution environment built and maintained at Morgan Stanley.

By design, it fulfills three major goals:

   * Dynamic, in-process rewriting of processing rules for domain objects (trades, orders, executions)
   * Persistence and out-of-band processing of structured logs for order managers and surrounding processes
   * Rock solid, ultra-low latency execution

Hobbes was developed to manage the runtime of low-latency processes such as equities trading engines, which generally cannot be restarted during the working day.

The target user base is the development and production management teams responsible for building and maintaining these processes in production. As such, Hobbes is *obsessively* pragmatic: the vast majority of design choices are aimed at fulfilling these needs.

Perhaps most surprisingly, Hobbes is a variant of the pure-functional programming language *Haskell*. The following is an example of some Hobbes code from a production system:

.. code-block:: haskell

  nil :: () -> (^x.(()+(a*x)))
  nil _ = roll(|0=()|)

  cons :: (a, ^x.(()+(a*x))) -> (^x.(()+(a*x)))
  cons x xs = roll(|1=(x,xs)|)

Read on to discover more about the Hobbes language - its design and purpose, and how you can use it in your systems!

Contribution
============

A step by step guide on how to contribute to Hobbes can be found
:ref:`here <contributing>`.

License
=======

License information `here <https://github.com/Morgan-Stanley/hobbes/blob/master/LICENSE.md>`_.

.. toctree::
  :hidden:
  :caption: Introduction

  introduction/components
  introduction/domain

.. toctree::
  :hidden:
  :caption: Language

  language/types
  language/controlflow
  language/polymorphism

.. toctree::
  :hidden:
  :caption: Embedding
  
  embedding/compiler
  embedding/REPL_extensions

.. toctree::
  :hidden:
  :caption: Logging

  logging/HLOG
  logging/hog
  logging/hi
  logging/datatypes

.. toctree::
  :hidden:
  :caption: Networking

  networking/hi
  networking/cpp
  networking/datatypes

.. toctree::
  :hidden:
  :maxdepth: 2
  :caption: Appendix

  appendix/stdlib
  appendix/match

.. toctree::
  :hidden:
  :maxdepth: 2
  :caption: Examples
  
  examples/simplerepl
  examples/raindrop_logger

.. toctree::
  :hidden:
  :maxdepth: 2
  :caption: About
  
  about


Hi extensions
*************

``hi`` is a rich and expansive REPL, with a number of tools which allow us to inspect generated code.

Commandline options
===================

-s
  Silent mode - don't display the prelude or version information

-p nnnn
  Listen on port *nnnn*

Type information
================

Hobbes is a strong, statically-typed language, meaning every expression has a type which is known at compile time. The Hobbes compiler makes this type information available to the REPL Hi.

``:t`` shows us the Hobbes type of the defined function:

:: 

  > add = (\a b.a+b)
  > :t add
  Add a b c => (a * b) -> c

In this case, Hi shows us that we've implicitly defined ``add`` as an implementation of the typeclass ``Add``. 

Digging a little further, we can use ``:c`` to see the implementation of the ``Add`` typeclass, which is defined as any type which has an ``+`` member:

::

  > :c Add
  class Add | #0 #1 -> #2 where
    + :: (#0 * #1) -> #2

We can see all implemetations of a typeclass with ``:i``:

:: 
  
  > :i Add

  instance Add char char char where
    + = cadd::(char * char) -> char::(char * char) -> char
  instance Add byte byte byte where
    + = badd::(byte * byte) -> byte::(byte * byte) -> byte
  instance Add short short short where
    + = sadd::(short * short) -> short::(short * short) -> short
  ...



Timing information
==================

Some timing data can be retrieved for the complation and evaluation phases of a Hobbes expression:

::
  
  > :e add(1, 3)
  average over 1000 runs: 43ns
  minimum runtime: 42ns
  maximum runtime: 131ns

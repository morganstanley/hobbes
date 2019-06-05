.. _hi:

hi, the Hobbes interpreter
**************************

In the ``bin`` directory of your Hobbes build, you'll find the Hobbes interactive interpreter, "hi". You can use it to execute much of the code you see on these pages by just typing the line and hitting 'enter'. It's an example of a REPL, or "Read, Eval, Print Loop" - because those are the key steps that allow it to execute the code you enter.

When used interactively, Hi will show you the results of the execution of a line immediately:

::
  
  hobbes/bin $ hi

  hi : an interactive shell for hobbes
    type ':h' for help on commands

  > true
  true
  > 1 + 2.3
  3.3

Throughout this documentation we'll show the Hi prompt ('>') to indicate text you can type into Hi. You don't need to type the prompt too.

.. _hi_load_files:

hi can read files
=================

If you pass the name of a text file to the Hobbes interpreter hi, it'll evaluate the code in the file and make symbol names available to you in the hi session:

:: 

  bin/hobbes $ cat match.hob
  foo = match 3 with
    | 1 -> "hello"
    | 2 -> "hobbes"
    | _ -> "oops"

  bin/hobbes $ hi match.hob
  hi : an interactive shell for hobbes
    type ':h' for help on commands

  loaded module 'match.hob'
  > foo
  "oops"

This makes it much easier to write more complex, multi-line expressions of the kind you'll see in later sections. Some of these examples are therefore shown without the hi prompt.

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

Commandline options
===================

-s
  Silent mode - don't display the prelude or version information

-p nnnn
  Listen on port *nnnn*
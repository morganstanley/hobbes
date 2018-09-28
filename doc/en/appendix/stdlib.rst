.. _standard_library:

The Standard Library
***************************

As well as the compiler infrastructure, Hobbes comes packed with a rich Standard Library of functions and types that make it easy to get started.

They're all available in the default namespace, and we've used quite a few of them already throughout the documentation.

In this section, we'll draw your attention to some of the most common and useful elements of the Hobbes Standard Library.

All the members here are written in plain, vanilla Hobbes - so digging into the source on GitHub is a great way to learn the language!

Simple Arithmetic
=================

These Type Classes, and their instances (what we'd call a 'realisation' of the type class for a concrete type) allow Hobbes to handle arithmetic in a polymorphic manner:

::
  
  class Add a b c | a b -> c where
    (+) :: (a,b) -> c

  instance Add int int int where
    (+) = iadd

  instance Add long long long where
    (+) = ladd

  ...

Because the ``Add`` Type Class (and the rest of the family: ``Subtract``, ``Multiply``, and ``Divide``) is available in the global namespace, I can use its instances implicitly - i.e., the ``+`` operator is defined for all the basic types:

:: 

  > 1 + 2
  3
  
.. note:: **built in?**
  
  It's important to not that the ``+`` operator isn't "built in" to Hobbes as operators might be in other programming languages. The resolution of ``+`` works because the compiler has recognised that there is a typeclass ``Add`` which provides an operator called ``+`` that can act on two ints.

Indeed, we can support ``+`` elsewhere by instantiating the Type Class for our own types:

::
  
  type counter = { count: int}

  counterAdd = (\x y. { count = iadd(x.count,y.count)})

  instance Add counter counter counter where
    (+) = counterAdd

.. note:: **type definitions in ``hi``**

  Just like in Haskell, the hi REPL doesn't support type definitions. So in this case, we've included the above Hobbes code in a file called ``counter.hob`` and instructed the ``hi`` REPL to read it at startup. The members defined in ``counter.hob`` can then be used directly.

::

  $ ./hi counter.hob
  loaded module 's.hob'

  > {count = 22} + {count = 33}
  55
  >

Maybe
=====

``Maybe`` is deeply idiomatic in functional programming, and is starting to seep into more popular languages too.

Effectively, it's a clean handling of the case where a function may legitimately not be able to provide a value.

.. note:: **Maybe**
  
  Consider a function ``getUser([char]) -> User``, which gets user information from some external source, when given the users name.

  What would you expect the function to return if a user with that name was not found? It's an entirely reasonable situation and not really *exceptional* at all.

  Indeed, in some languages you'd expect the function to throw a UserNotFoundException, which you'd have to catch and deal with outside the regular flow of control.

  In Functional Languages, we'd change the signature of the function to return a ``Maybe`` type.

The ``maybe`` is really a sum type that looks like this:

:: 
  
  (()+a)

That is, it can either have a value, or it can have no value. You instantiate ``maybe`` with the two constructors ``just`` and ``nothing``:

::

  > maybenums = [just(1), just(2), nothing, just(3)]
  maybenums = [1, 2, , 3]
  > dropNulls(maybenums)
  [1, 2, 3]

Maybe has a large number of utility functions that deal with either case - i.e. where there is a value and where there is no value.

::

  > map((\x.x+2), maybenums)
  [3, 4, , 5]

  fromMaybe(0, maybenums[1])
  > 2

  fromMaybe(0, maybenums[2])
  > 0

In the above example we're extracting the value from the ``maybe`` if one exists, or else we're providing a sensible default (in this case, the integer ``0``).

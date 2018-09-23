Control Flow
************

The Hobbes language itself is quite small. We've already seen the definition of a function type using the function-literal, or *lambda* syntax, in :ref:`here <polymorphism>`.

Further function definitions, as well as types, type classes, and their instances, must be written in a Hobbes file in order to be defined.

.. note:: Hi can read files
  
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

  This makes it much easier to write more complex, multi-line expressions of the kind we'll see in this section. Some of these examples are therefore shown without the hi prompt.

The control flow rules for Hobbes are similar to Haskell:

If/else
=======

::

  if a < 1 then 2 else 3

.. note:: Note that 'if' is an expression, meaning that it resolves to a value:
  
  ::

    > b = if true then 12 else 13
    > b
    12

.. _hobbes_pattern_matching:

Pattern matching
================

.. note:: **Comments**

  Single-line comments in Hobbes begin with "//"

Hobbes has rich support for pattern matching - in some sense, this is where it shines.

::

  match 3 with 
    | 1 -> "hello" 
    | 2 -> "hobbes"  
    | _ -> "oops!"

    todo: match on aggregate types. unapply/deconstruct syntax?

Comprehensions
==============

Similar to comprehensions in Python, these allow us to describe the algorithm used to create a sequence of data.

.. warning:: Remember, Hobbes code is executed eagerly, meaning the comprehension will be evaluated in full when it is declared. Stay away from infinite sequences!

::

  [show(x) | x <- [0..20], x % 3 == 0]

This can be read as "for each x in 0 to 20, where x is divisible by 3, show x".

The comprehension is split into a mapping function, a generator expression, and a filter. The mapping function is applied to the results of the generator function where the filter holds true.

The comprehension syntax is an expression, and can therefore be used anywhere a range of elements is expected. For example, the Hobbes standard library contains the following code:

::

  productWith :: ((a, b) -> c, [a], [b]) -> [c]
  productWith f xs ys = concat([[f(x,y) | y <- ys] | x <- xs])

This describes a function ``productWith``, which combines the cross product of elements from two lists with a function:

::

  > productWith((\x y.x+y),[1,2,3],[4,5,6])
  [5, 6, 7, 6, 7, 8, 7, 8, 9]

If we were to write out this ``productWith`` function in a less functional style, it might look like this:

::

  for(x in 1, 2, 3)
    for(y in 4, 5, 6)
      yield (x, y)


Local variables
===============

In order to avoid polluting the Hobbes global namespace, we can declare variables as *local* to the current expression:

::
  
  > let x = 9 in x * x
  81
  > x
  stdin:1,1-1: Undefined variable: 'x' ...

In this case the name ``x`` is only in scope in the following expression. This allows us to re-use names without having to deal with ``x1``, ``x2``, etc.

Let expressions can allow more than one local variable to be declared:

::
  
  > let x=1; y=10 in x + y
  11

Indeed, let expressions are very powerful. In the following example we're first declaring, and then decomposing a tuple before the expression itself is evaluated:

::

  > let f = (\x.(x,x)); (x, y) = f(20) in x + y
  40

Notice how we're even able to reuse the name ``x`` across both the function declaration and the resultant tuple deconstruction. ``Let`` expressions are evaluated in declaration order, before the execution primary expression.

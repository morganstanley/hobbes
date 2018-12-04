.. _typesystem:

The Type System
***************

Like many functional-style programming languages, the power of Hobbes lies in its rich type system, so that's where we'll start. There are primitive types, arrays, record types, tuples, and variants.

.. hint:: 

  **Hi, the Hobbes REPL**

  In the ``bin`` directory of your Hobbes build, you'll find the Hobbes interactive interpreter, "Hi". You can use it to execute any of the code you see on these pages by just typing the line and hitting 'enter'. It's an example of a REPL, or "Read, Eval, Print Loop".

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


Primitive Types
===============

The Hobbes primitive types are arranged in memory in a manner which allows for free marshalling when Hobbes is executing in a C++ process. Each of the primitive types has a simple literal syntax allowing for the easy initialisation of a value:

Unit
  A simple null-type with only one value

  ::

    > ()

Bool
  Either ``true`` or ``false``

  ::

    > true
    true

Char
  A single character of text

  ::

    > 's'
    s

Byte
  A single byte (``0``-``255``)

  ::

    > 0Xad
    0Xad

Short
  A two-byte number

  ::

    > 3S
    3S

Int
  A four-byte number

  ::

    > 4
    4

Long
  An eight-byte number

  :: 

    > 5L
    5

Float
  A single-precision (4 byte) floating-point number

  ::

    > 3.0f
    3f

  .. note:: float literals must be a decimal number followed by the character 'f'.

Double
  A double-precision (8-byte) floating-point number

  ::

    > 3.0
    3

Arrays
======

Just like in C++, Hobbes arrays are zero-indexed, contiguous values in memory, with special syntax for ``char`` and ``byte`` arrays. Hobbes supports bounds-checking to prevent a common class of bug by maintaining the array length alongside the data in memory.

::

  > [0, 1, 1, 2, 3]
  [0, 1, 1, 2, 3]

  > "Hello, Hobbes!"
  "Hello, Hobbes!"

  > 0xdeadbeef
  0xdeadbeef

.. note:: **Strings**

  In Hobbes, a String is simply an array of ``char``. 

.. warning:: **0x versus 0X**

  It's important the note the subtle difference between the literal syntax for ``byte`` and for ``byte`` *arrays* - the case of the 'X' is very important!

  Uppercase for ``byte``, lowercase for ``byte`` array.

Array functions
---------------

A number of functions are overloaded for array types:

::
  > [0, 1, 2] ++ [3, 4, 5]
  [0, 1, 2, 3, 4, 5]

  > size([0,1,2])
  3

You can index into an array using square brackets:

::
  > nums = [6, 2, 4, 6, 5, 9, 8, 5, 6, 3]
  > nums[3]
  6


In addition, the open and closed slice syntax is available:

::

  > nums[3:6]
  [6, 5, 9]
  > nums[2:]
  [4, 6, 5, 9, 8, 5, 6, 3]

You can read ``[2:]`` as "the second index, until the end". The converse works, too:

::

  > nums[:2]
  [3, 6, 5, 8, 9, 5, 6, 4]

This is "the end until the second index". Indexes from the end of the array can be counted with a *unary negate*:

::

  > nums[2:-3]
  [4, 6, 5, 9, 8]

And of course you do that in both positions in the slice:

::

  > nums[-2:-4]
  [5, 8]

.. warning:: **Array indexes**

  Array indexes in Hobbes aren't bounds checked, so whilst you can *slice* from the end of an array, you can't use the same syntax to *index*:

  ::

    > nums[-3]
    51627831

  In addition, attempts to slice off the end of an array will act as though you were slicing from the beginning or the end, respectively:

  ::

    > nums[-2014:305]
    [6, 2, 4, 6, 5, 9, 8, 5, 6, 3]

Array sequences
---------------

A *sequence expression* can be used to initialise an array of ints. The syntax is simple:

::
  
  > [1..4]
  [1, 2, 3, 4]
  > :t [1..4]
  [int]

We can take this further and generate *infinite* sequences by leaving the upper bound open:

::

  > [0..]
  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]...

Infinite sequences are useful structures for performing work many times over without mutating a loop variable. Special care has been taken to ensure their evaluation isn't eager, however - as your program might never stop! For more information about the *type* of an infinite sequence, take a look at the infinite squences section in :ref:`polymorphism <polymorphism>`

Records
=======

Records are a common way to keep closely-associated pieces of data together in functional progamming, and they're often referred to as an *and* type: a hostport is a host *and* a port - and that's it. No behaviour, and its identity is simply the two elements.

Record types are similar in spirit to C++ structs, with ad-hoc declaration and initialisation, plus type inference:

::

  > {name="Sam", age=23, job="writer"}

Records are examples of structural types, meaning that in Hobbes, even though they are both examples of different anonymous ad-hoc types, the two are *equivalent*:

::

  > {name="Sam", job="Writer"} == {job="Writer", name="Sam"}
  true

.. note:: **Equivalence vs Equality**
  Although it's true to say that, in Hobbes, the two record instances above are *equivalent*, they're not *equal*, and so the following equality test would fail to compile:
  
  ::
  
    > {name="Sam", job="Writer"} === {job="Writer", name="Sam"}
    stdin:1,28-30: Cannot unify types: { name:[char], job:[char] } != { job:[char], name:[char] }

  This is because the equivalence relationship is determined not by any special logic in the Hobbes compiler, but by the equivalency type class ``Equiv``. This class contains the implementation of ``==`` and thus decides how to unpack the record instances and compare them.
  
  A type class is a way of describing expected behaviour on a type. In the Hi REPL, I can unpack the ``Equiv`` typeclass with ``:c``:

  ::

    > :c Equiv
    class Equiv where
      == :: (#0 * #1) -> bool

  For more information about typeclasses in Hobbes, see :ref:`Type Classes <type_classes>`.

Tuples
======

Like records but with no field names, tuples are used to keep commonly-associated data together. The canonical example is the host/port pair:

::

  > endpoint = ("lndev1", 3923)
  > endpoint
  ("lndev1", 3923)

.. note:: **Assignment**
  
  Notice here that we've assigned the tuple to the name *endpoint*. This name now exists in the global context. For local scoping, see of values, see :ref:`Local scoping <let_expressions>`

.. note:: **Pretty-printing**
  
  Hobbes has good support for printing the primitive and scalar types: char arrays are printed as strings, the literal syntax is displayed when printing to *standard out*, etc.

  When we deal with arrays of records or tuples, Hobbes gives us a convenient table notation:

  ::

    > [{First=1, Second="two"},{First=3, Second="Four"},{First=5, Second="Six"}]

    First Second
    ----- ------
        1    two
        3   Four
        5    Six 


Variants
========

The variant is the richest way to declare a type in the Hobbes type system, because it gives us the opportunity to declare a value which can be one of a number of named cases. If the Record type is an *and*, the Variant is an *or*.

This allows us to model enum-like structures with associated data. In the following example, we're declaring a type called ``status`` which can model the success or failure of a service call. In the case of a failure, we'll be given an error code which we'll want to react to. However, in the successful case, there's nothing more to do:

::

  type status = |
    Succeeded,
    Failed: int
  |

  > status = |Succeeded| :: status
  |Succeeded|

.. warning:: **type declarations in hi**
  
  Hi doesn't currently support some Hobbes expressions, including type declarations. You can write your types in a file and have them loaded into a hi session by following the instructions in :ref:`Hi can load files <hi_load_files>`.

.. note:: **Type Annotations**
    
  Sometimes Hobbes requires us to specify the type of a value. In the case above, we want to be careful about the instantiation of the ``|Succeeded|`` type: we need to be clear that we're instantiating a subtype of ``status``, rather than a naked record type with just one subtype which happens to be called 'Succeeded'. Hi can show us the inferred type of a value with ``:t``:

  ::

    > :t |Succeeded|
    |Succeeded=()|::a=>a
      
    > :t |Succeeded| :: status
    |Succeeded, Failed:int|

  The ``::`` allows us to specify the type of the variable using what's called a *type annotation*. More information about types and type annotations is available in :ref:`Polymorphism in Hobbes <polymorphism>`.

As we'll see in :ref:`pattern matching <hobbes_pattern_matching>`, Hobbes has rich language support for building logic based on variant types.

Sum types
=========

Just as the tuple type can be thought of as simply a record using numbered placement instead of names, the sum is a variant without names: a true union.

::

  > |1="hello"| :: (int+[char])
  |1="hello"|
  > |0=3| :: (int+[char])
  |0=3|

In this case we're using the index (0 or 1) to specify the actual variant type we're using - int or char array. An instance of the first type must hold an int, and an instance of the second type must hold a char array - in this case, a String.

Recursive type definitions
==========================

With a small adjustment, the sum type can be used to model both cases in our list:

::

  ^x.(()+([char]*x))

In this type expression we use the caret to give a name to the type which can be used recursively throughout the expression. In this case the list type, ``x``, is declared as a sum type of an empty list, or a string and a list.

We can easily construct one using Hobbes's constructor syntax:

::

  > cons(1, cons(2, cons(3, nil())))
  1:2:3:[]

Whilst this construction syntax might look unwieldy, the generation of such structures is commonly algorhithmic, and (as discussed earlier), the payoff is in Hobbes's rich matching syntax.

Many structures in Hobbes can be defined recursively because, as we'll see, recursion is a deeply powerful element of functional programming.
The Hobbes Type System
**********************

The Hobbes Type System is functional in spirit. There are primitive types, arrays, record types, tuples, and variants.

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
  Either true or false

  ::

    > true
    true

Char
  A single character of text

  ::

    > 's'
    s

Byte
  A single byte (0-255)

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

Just like in C++, Hobbes arrays are contiguous values in memory, with special syntax for char and byte arrays. Hobbes supports bounds-checking to prevent a common class of bug by maintaining the array length alongside the data in memory.

::

  > [0, 1, 1, 2, 3]
  [0, 1, 1, 2, 3]

  > "Hello, Hobbes!"
  "Hello, Hobbes!"

  > 0xdeadbeef
  0xdeadbeef

.. warning:: 0x versus 0X

  It's important the note the subtle difference between the literal syntax for bytes and for byte *arrays* - the case of the 'X' is very important!

Records
=======

Records are a common way to keep closely-associated pieces of data together in functional progamming, and they're often referred to as an "AND" type: a hostport is a host AND a port - and that's it. No behaviour, and its identity is simple the two elements.

Record types are similar to structs, with ad-hoc initialisation and type inference:

::

  > {name="Sam", age=23, job="writer"}

Records are examples of structural types, meaning that in Hobbes, even though they are both examples of different anonymous ad-hoc types, the two are equivalent:

::

  > {name="Sam", job="Writer"} == {job="Writer", name="Sam"}
  true

.. note:: **Equivalence vs Equality**
  Although it's true to say that, in Hobbes, the two record instances above are *equivalent*, they're not *equal*, and so the following test would fail to compile:
  
  ::
  
    > {name="Sam", job="Writer"} === {job="Writer", name="Sam"}
    stdin:1,28-30: Cannot unify types: { name:[char], job:[char] } != { job:[char], name:[char] }

  This is because the equivalence relationship is determined not by any special logic in the Hobbes compiler, but by the equivalency type class ``Equiv``. This class contains the implementation of `==` and thus decides how to unpack the record instances and compare them. In the Hi REPL, I can unpack the ``Equiv`` typeclass with ``:c``:

  ::

    > :c Equiv
    class Equiv where
      == :: (#0 * #1) -> bool


Tuples
======

Like records but with no field names, tuples are used to keep commonly-associated data together. The canonical example is the host/port pair:

::

  > endpoint = ("lndev1", 3923)
  > endpoint
  ("lndev1", 3923)

.. note:: **Pretty-printing**
  
  Hobbes has good support for printing the primitive and scalar types: char arrays are printed as strings, the literal syntax is displayed when printing to STDOUT, etc.

  When we deal with arrays of records or tuples, Hobbes gives us a convenient table notation:

  ::

    > [{First=1, Second="two"},{First=3, Second="Four"},{First=5, Second="Six"}]

    First Second
    ----- ------
        1    two
        3   Four
        5    Six 

Polymorphism
============

The hobbes approach to polymorphism is delivered through Type Classes, a way of declaring a piece of behaviour that a type can support.

For example, all the numeric types support addition, and so I can declare a function using Hobbes's anonymous function syntax:

::

  > add = (\u v x y z.u+v+x+y+z)

This can be read as "a function which takes arguments u, v, x, y, and z, and adds them all together". The backslash starts the function (or "lambda", because if you squint your eyes it looks a bit like the lowercase Greek letter "λ"), and the period separates the argument list from the function definition. 

The types of the variables are left out, yet Hobbes will quite happily figure out types from the context in which they're used. In this case, we can say that the type of those variables is "something which supports addition". Therefore, if we call 'add' with instances of numeric types, we'll get the answer we're looking for:

::

  > add(0X01, 2, 3S, 4L, 5.0)
  15

.. note :: **Type inference in Hi**

  Hi has a number of advanced features, one of which is that it can show you the inferred type of an expression you've typed. We can use Hi to show us the inferred type of a smaller, simpler variant of our anonymous function:

  ::

    :t (\y z.y+z)
    Add a b c => (a * b) -> c

  Hi has inferred the type for our three variables (two parameters and one return value) to be the Type Class 'Add', and is showing the type of the function is one that takes a and b ("a * b") and returns c.

  We've already seen an implementation of the equivalence typeclass ``equiv``. Others include Multiply (applied to types which have a ``*``) and Print (for types which can be printed).

Type annotations
================

We've seen that Hobbes is able to infer the type of a variable:

::

  > :t 3.2
  double

We can also declare a polymorphic function and shown that Hobbes can infer the type of its parameters. This is powerful for two reasons:

  #. We don't have to write as much code
  #. We can share behaviour across data types as long as they share the ability to perform certain actions. That is, our algorithms become more generic.

::

  > :t (\y.y+1)
  Add a int b => (a) -> b

.. note:: **Type class redux**

  ``Add a int b => (a) -> b``

  Is the Hobbes type name for the lambda function we declared in the REPL. There are two parts, separated by the ``=>``. It's easiest to take them backwards: The second part is the actual type of the variable, which is ``(a) -> b``. This can be read as "a function that takes a value of type ``a`` and returns a value of type ``b``".
  
  The first part is for type *restrictions*: things the compiler knows about ``a`` and ``b`` that limit what data variable of those types can represent. In this case, Hobbes is simply telling us that ``a`` must implements the ``Add`` type class (i.e. it overloads the `+` operator), and that ``b`` is the result of adding an ``a`` to and ``int``.  
  
It's important to note that at runtime, all Hobbes functions are *monomorphic* - all the type parameters are resolved to the actual runtime type of the variable and a specific function is output for evaluation. 

Of course, in some situations we might want to declare a function with a specific type. In that case, we can use a *type annotation* to declare a variable's type. This can give us extra type safety in cases where we don't want generic behaviour:

::

  > i = 3:: int
  > j = 3.2:: int
  stdin:1,5-7: Cannot unify types: double != int
  1 j = 3.2:: int 

We've specified that ``j`` is an int and the compiler has prevented us from assigning a double value to j.

Similarly, there's no silent upcasting:

::

  > k = 3:: float
  stdin:1,5-5: Cannot unify types: int != float
  1 k = 3:: float  

Type annotations allow us to specify types for functions which would otherwise be generic, using the Hobbes type notations for functions that we saw before. In this case we don't need to specify type restrictions, we can just declare the value to be a function type from concrete type to concrete type:

::

  > add1i = (\a.a+1)::int->int
  > add1i(3)
  4
  > add1i(3.4)
  stdin:1,1-6: Cannot unify types: int != double
  1 add1i(3.4)

And because of the consistency of types:

::
  
  > add1f = (\a.a+1)::int->float
  stdin:1,9-28: Unsatisfiable predicate: Add int int float
  1 add1f = (\a.a+1)::int->float

...because an ``int`` plus 1 is another ``int``, *not* a ``float``.

Type constraints
================

We can take this one step further:

::

  > :t \x.x.Name

Remember that, in our lambda syntax, this can be read as "A function which takes x and returns x.Name" - i.e. the only thing we know about the type of x is that it has a member called Name. Hi will then give names to those two as-yet unnamed types: it calls them 'a' and 'b':

::

  a.Name::b => (a) -> b

Here, the double colon is a *type annotation*, meaning that the type of "a.Name" is b. Once those types are resolved, we're left with a function from type a to type b ("(a) -> b").

Variants
========

A variant is an interesting part of any functional type system, and gives us the chance to talk about two important things in programming: data modelling and recursion.

Data modelling
--------------

In Object Oriented programming, we model our problem domain as a series of interconnected objects - each of which is a container for some state and behaviour, and which has an identity. In Java, for example, state is encapsulated as a series of private data members which are made available through getter and setter methods.

Functional programming is much more data-oriented. Data is organised as ad-hoc instances of records or sums, with separate functions that drive data around a process. 

As such, some of the design tools that we see in object-oriented programming find less favour in functional programming. It's rare to see inheritance used to model data, for example, and much more common to move the decision as to what logic to implement for a given runtime type into a function.

Recursion
---------

One feature that sees more use in functional programming than in OO is *recursion*. Most functional languages, Hobbes included, allow the programmer to elegantly and accurately model both types and algorithms in a recursive manner.

The canonical example is the List. In a functional type system, a List is a *recursive* type: it's defined as a head and a tail, where the head is a value and the tail is a List. In the final case, the tail is the empty list, ``[]``.

In functions which deal with Lists there are then two cases to consider: the regular case where you can process the head and then recurse over the tail, or the base case where you simply process the head.

Variants
--------

The variant is therefore the richest way to declare a type in the Hobbes type system, because it gives us the opportunity to declare a value which can be one of a number of named cases. If the Record type is an AND, the Variant is an OR:

::

  > a = |one|::|one,two,three|
  > :t a
  |one, two, three|
  > a
  |one|

We've declared the variable named ``a`` to have the value ``|one|`` with the annotated type ``|one,two,three|`` - that is, the union of three possible values ``one``, ``two``, and ``three``.

We can say that instances of that type can have data associated with them:

::
  
  > |one="hello"|::|one:[char], two, three|
  |one="hello"|
  > |two|::|one:[char], two, three|
  |two|

This allows us to model enum-like structures with associated data:

::

  type status = |
    Succeeded,
    Failed: int
  |

 > status = |succeeded|::status
 |succeeded|

As we'll see :ref:`later <hobbes_pattern_matching>`, Hobbes has rich language support for building logic based on variant types.

Sum types
---------

Just as the tuple type can be thought of as simply a record using numbered placement instead of names, the sum is a variant without names: a true union.

::

  > |1="hello"|::(int+[char])
  |1="hello"|
  > |0=3|::(int+[char])
  |0=3|

In this case we're using the index (0 or 1) to specify the actual variant type we're using - int or char array. An instance of the first type must hold an int, and an instance of the second type must hold a char array - in this case, a String.

Recursive structures
--------------------

With a small adjustment, the variant type can be used to model both cases in our list:

::

  ^x.(()+([char]*x))

In this type expression we use the caret to give a name to the type which can be used recursively throughout the expression. In this case the list type, ``x``, is declared as a sum type of an empty list, or a string and a list.

We can easily construct one using Hobbes's constructor syntax:

::

  > cons(1, cons(2, cons(3, nil())))
  1:2:3:[]

Whilst this construction syntax might look unwieldy, the generation of such structures is commonly algorhithmic, and (as discussed earlier), the payoff is in Hobbe's rich matching syntax.
.. _polymorphism:

Polymorphism
************

.. _type_classes:

Type Classes
============

The hobbes approach to polymorphism is delivered through Type Classes, a way of externally declaring a piece of behaviour that a type can support. Type classes are a rich and powerful way of adding bits of functionality to existing types.

For example, all the numeric types support addition, and so I can declare a function using Hobbes's anonymous function syntax:

::

  > add = (\u v x y z.u+v+x+y+z)

This can be read as "a function which takes arguments u, v, x, y, and z, and adds them all together". The backslash starts the function (or "lambda", because if you squint your eyes it looks a bit like the lowercase Greek letter "λ"), and the period separates the argument list from the function definition. 

The types of the variables are left out, yet Hobbes will quite happily figure out types from the context in which they're used. In this case, we can say that the type of those values is "something which supports addition". Therefore, if we call 'add' with instances of numeric types, we'll get the answer we're looking for:

::

  > add(0X01, 2, 3S, 4L, 5.0)
  15

.. note:: **Type inference in Hi**

  Hi has a number of advanced features, one of which is that it can show you the inferred type of an expression you've typed. We can use Hi to show us the inferred type of a smaller, simpler variant of our anonymous function:

  ::

    :t (\y z.y+z)
    Add a b c => (a * b) -> c

  Hi has inferred the type for our three values (two parameters and one return value) to be the Type Class 'Add', and is showing the type of the function is one that takes a and b ("a * b") and returns c.

  There are two parts, separated by the ``=>``. It's easiest to take them backwards: The second part is the actual type of the value, which is ``(a * b) -> c``. This can be read as "a function that takes an instance of ``a``, and an instance of ``b``, and returns an instance of ``c``.
  
  The *first* part is for type *restrictions*: things the compiler knows about ``a``, ``b`` and ``c`` that limit what data instances of those types can represent. In this case, Hobbes is simply telling us that they must implement the ``Add`` type class (i.e. they overload the `+` operator).

  Hobbes has simply inferred this about those types from the context in which they're used. This is in stark contrast to languates where types are restricted on what interfaces they implement. 

Many other type classes are available in the :ref:`Hobbes standard library <standard_library>`. We've already seen an implementation of the equivalence typeclass ``equiv``. Others include ``multiply`` (applied to types which have a ``*``) and ``print`` (for types whose values can be printed).

.. _type_annotations:

Type annotations
================

We've seen that ``hi`` is able to infer the type of a value:

::

  > :t 3.2
  double

We can also declare a polymorphic function and shown that Hobbes can find restrictions on the type of its parameters. This is powerful for two reasons:

  #. We don't have to write as much code
  #. We can share behaviour across data types as long as they share the ability to perform certain actions. That is, our algorithms become more generic.

::

  > :t (\y.y+1)
  Add a int b => (a) -> b
  
It's important to note that at runtime, all Hobbes functions are *monomorphic* - all the type parameters are resolved to the actual runtime type of the value and a specific function is output for evaluation. 

Of course, in some situations we might want to declare a function or value with a specific type. In that case, we can use a *type annotation* to declare a value's type. This can give us extra type safety in cases where we don't want generic behaviour:

::

  > i = 3 :: int
  > j = 3.2 :: int
  stdin:1,5-7: Cannot unify types: double != int
  1 j = 3.2 :: int 

We've specified that ``j`` is an int and the compiler has prevented us from assigning a double value to ``j``.

Similarly, there's no silent upcasting:

::

  > k = 3:: float
  stdin:1,5-5: Cannot unify types: int != float
  1 k = 3:: float  

Type annotations allow us to specify types for functions which would otherwise be generic, using the type notations for functions that we saw before. In this case we don't need to specify type restrictions, we can just declare the value to be a function type from concrete type to concrete type:

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
  a.Name::b => (a) -> b

Remember that, in our lambda syntax, this can be read as "A function which takes x and returns x.Name" - i.e. the only thing we know about the type of x is that it has a member called Name. Hi will then give names to those two as-yet unnamed types: it calls them 'a' and 'b'.

Note that Hobbes has inferred the type restriction on b: It's whatever type the value of "a.Name" is. This function will work for *any* type that has a member called ``Name``, which can be of any type!
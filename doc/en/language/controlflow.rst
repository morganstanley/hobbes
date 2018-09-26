Control Flow
************

The Hobbes language itself is quite small. We've already seen the definition of a function type using the function-literal, or *lambda* syntax, in :ref:`Types <polymorphism>`.

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

Functions
=========

We've seen the lambda syntax for in-line function definition, but in Hobbes functions are first declared by their type, and then implemented for specific values.

::
  
  addOne :: int -> int
  addOne s = s + 1

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

Match expressions are perhaps the most powerful element of Hobbes syntax, because they let you perform actions based on the value or even type of other expressions.

In the simplest form, they’re a bit like a switch statement:

::

  match 3 with 
  | 1 -> show("hello")
  | 2 -> show("hobbes")
  | _ -> show("oops!")

.. note::
  In Hobbes match expressions, the underscore serves two purposes.
  
  Firstly, it's a wildcard. In the above code the underscore is the default case that will be executed if all the above cases have failed to match.

  Secondly, it's an instruction not to bind a name to the matched element. We'll see more about binding below!

When used with more than one value they can be used to match against any element in the set. However, the Hobbes compiler is smart, and will complain if you haven't provided a case for all the potential options. In the next example, we'll be caught red-handed with a so-called "inexhaustive match":

::

  match 1 2 with
  | 1 _ -> show("first!")
  | 1 2 -> show("second!")

.. warning::
  Inexhaustive patterns in match expression

  We can always resolve this by adding a default case, or by providing cases for all the possible options - although this might mean writing a lot of code!

  ::

    match 1 2 with
    | 1 _ -> show("first!")
    | 1 2 -> show("second!")
    | _ _ -> show("default!")

In the above example, it's important to note that the matching works top-down, meaning that the first valid case will be evaluated:

::

  match 1 2 with
  | _ 2 -> show("matched!")
  | 1 2 -> show("didn't match!")
  | _ _ -> show("didn't even get here!")

The way to read the first case is "*any value* followed by the integer ``2``". Even though the second match is more specific (i.e., both elements match the values), it's the first case that's matched.

Also, note that because we're matching against two values, we have to use two underscores in the final case. If we fail to do that, Hobbes will tellus "row #3 has 1 columns, but should have 2".

.. note::

  Just as ``if`` expressions can be written on one line, we can save space (and be more idiomatic) in our Hobbes code in the same way. The above match can be re-written as

  ::

    match 1 2 with | _ 2 -> show("matched!") | 1 2 -> show("didn't match!") | _ _ -> show("didn't even get here!")

  The Hobbes standard library is full of code like this, and Hobbes developers quickly get used to writing code this terse. You can decide what works best for you!

Matching and binding
--------------------

As well as matching on values, we can also bind values to names within a match case. In the following example, we're matching on the first element of the tuple and binding to the second:

::

  match 'a' 123 with
  | 'a' fst -> show(fst)
  | 'b' snd -> show(snd)
  | _ _ -> show("default")

In each case, we're simply matching on the (char) value of the first element. If that matches, we bind the second element to a value. In the first case (which ultimately is matched), the name we give the value is ``fst``, but there's nothing special about that; we could have called it anything. The name ``fst`` is then lexically scoped to the match expression following the arrow - it's not available in other cases, or outside the match. 

.. note::
  To some programmers, this “match and bind” behaviour seems strange, and it’s another good example of the terse vs powerful dynamic often found in functional programming.

Tuples
------

Hobbes also lets us match against the values of tuple elements, leading to another common idiom. The ease with which we can match and bind using the match syntax with tuples means that ad-hoc tuples are often created simply to limit pollution of the global namespace with values which could be scoped more appropriately. Consider the below case:

::

  match env getHostPort(env) with
  | "dev" (host, port) -> connect(host, port)
  | "qa" (host, port) -> connectqa(host, port, qadb)
  | "prod" (host, _) -> connectkrb(host)
  | _ _ -> ...

In this case we're creating a tuple simply for the purposes of immediately mathching against its values and unpacking it.

Here again the underscore is used as a wildcard - in this case you can read it to mean "there is a value here but I don't care what it is, and I don't want to use it".

This matching-and-binding logic can be generalised to arrays, too:

::

  match [("sam", 2013), ("james", 2012), ("stephen", 2010)] with
  | [_ (n, 2012), _] -> show(n)
  | _ -> show("none")

And, because of the way character arrays are matched, even to regular expressions:

::

  match hostname with
  | '.*qa$' -> show("qa")
  | _ -> show("prod")

Guard matching
--------------

We can also match based on ranges of values, using a so-called "guard":

::

  match 1 with
  | x where x < 10 -> show("small!")
  | _ -> show("large!")

.. note::

  The rules for match expressions are simple: every case in the expression must be reachable (i.e., no previous row can have matched against all the possible values for this row) and the match table must be exhaustive (i.e. all possible cases must be matched against).

  These rules combined explain why you so commonly see wildcard matches at the end of a match expression - the wildcard catches any cases that haven't previously been matched; and putting it at the end it prevents further cases from being unreachable.

  Remember, the rule is *first possible match*, not *most specific match*!

Matching on Variants
--------------------

Just like with tuples we can match on - and unpack - sum and variant types. Recall our status type from earlier:

::
  
  type status = | Succeess, Failure: int|

We can write a matching function which classifies values of this type and acts accordingly:

::
  
  classify :: status -> [char]
  classify s = match s with
  | |Success| -> "finished"
  | |Failure=x| -> "failed with error" ++ show(x)

Similarly to the complex match expressions above, we can match on values as well, to provide special functionality for specific cases:

::

  classify :: status -> [char]
  classify s = match s with
  | |Success| -> "Succeeded"
  | |Failure=404| -> "Not Found"
  | |Failure=err| -> "Error: " ++ show(err)

Match expressions
-----------------

In the previous examples, we've been calling the unit ``show`` function in our match cases. But in Hobbes, just like with ``if``, ``match`` is an expression - that means it’s results can be assigned directly to a name:

::

  hostport = match env with | "prod" -> "lnprd" | "qa" -> "euqa" | _ -> "ln123dev"

And just like with ``if``, the types of the expressions in each branch must also match. Failing to ensure the cases are of the same type will result in an error:

::

  > match 1 with | 0 -> "hello" | _ -> 1
  Cannot unify types: [char] != int

Sugar
-----

The Hobbes compiler uses match expressions "under the covers" in a number of different situations. For example, the ``matches`` keyword can be used to perform all the unpacking and pattern-matching that a single-case match statement can:

::

  (1, 2) matches (1, 2)

is re-written by the Hobbes compiler to

::
  
  match (1, 2) with
  | (1, 2) -> true
  | _ -> false

Similarly, these two are equivalent in Hobbes:

::

  "sam" matches '..m'
  match "sam" with
  | '..m' -> true
  | _ -> false

This process of conversion to another program structure is commonly called "desugaring", because the nicer, lighter-weight style is known as "syntactic sugar". Sweet!

Match performance
-----------------

Because matching on values is such a fundemental and commonly-used part of Hobbes, much work has been done to ensure that the evaluation of match expressions is highly performant.

  todo: more on performance?

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

Let allows us to unpack tuple values in a convenient format:

::

  > hostport = ("lndev1", 234)
  > let (h, p) = hostport in show(p)
  234

.. note::

  This form of ``let`` is actually converted into a simple ``match`` for us by the Hobbes compiler:
  
  ::

    match hostport with
    | (h, p) -> show(p)

  Note that this "de-sugaring" will only take place if the compiler can determine that the match can never fail.

Finally, let's wrap all that up with a match and a for comprehension:

::

  > let start=1; end=4 in match [i | i <- [start..end], i % 2 == 0] with | [2, 4] -> "evens" | _ -> "odds"
  evens
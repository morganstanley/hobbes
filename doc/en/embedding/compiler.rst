.. _hobbes_hosting:

The Embedded Compiler
****************************

Hobbes is a simple language with a rich type system, whose driving aim is to allow efficient reimplementation of functional logic at runtime.

As such, it comes packaged with a highly efficient compiler and type marshalling system.

Example: Hi, the Hobbes REPL
============================

We've already seen one place where the Hobbes compiler is used. In its simplest form, the Hobbes REPL is a loop which takes Hobbes code, runs it line-by-line through the compiler, interprets the results and prints the output:

.. code-block:: c++

  int main() {
    hobbes::cc c;

    while (std::cin) {
      std::cout << "> " << std::flush;
      std::string line;
      std::getline(std::cin, line);

      try {
        c.compileFn<void()>("print(" + line + ")")();
      } catch (std::exception& ex) {
        std::cout << "*** " << ex.what();
      }

      std::cout << std::endl;
      hobbes::resetMemoryPool();
    }
  }

That call to ``hobbes::resetMemoryPool()`` at the bottom of the loop is doing
more than tidying up, and it deserves its own section.

.. _hobbes_memory_model:

The memory model: transactions, not destructors
===============================================

Hobbes evaluation runs inside what is best thought of as a *transaction*.
Memory allocated while evaluating — by Hobbes code itself, and by the
``hobbes::make``/``makeString``/``makeArray`` helpers described below — comes
from a thread-local memory region (an arena of pages with a bump pointer, see
``hobbes/util/region.H``). Ending the transaction with
``hobbes::resetMemoryPool()`` does not free those objects one at a time: it
resets the region's heap pointer, and later transactions allocate straight
over the old data. No destructors run, then or ever.

This is a deliberate trade, made for the same reason Hobbes exists at all:
processes that cannot pause during the working day. A reset costs the same
whether the transaction allocated a kilobyte or a gigabyte, the pages are
reused so steady-state evaluation never touches the system allocator, and
evaluated code pays nothing for ownership bookkeeping it does not need.

It also has consequences an embedding application must respect:

* **Copy out what you keep.** Anything produced by an evaluation that should
  outlive the transaction must be copied into memory you own before the
  reset; a pointer held across ``resetMemoryPool()`` silently becomes a
  pointer into whatever the next transaction writes there.
* **No destructors means no RAII.** C++ types whose destructors matter
  (owning pointers, file handles, locks) do not belong inside
  region-allocated data. Nothing will ever run them.
* **Per-allocation audit tools will misreport this design.** Tools built on
  the model that every allocation is individually owned and freed —
  LeakSanitizer above all — will report region-backed and
  transaction-scoped data as leaked, at process exit especially. This is the
  audit's model failing to fit, not memory being lost: leak detection is
  disabled as a matter of policy for the fuzz targets (see
  ``fuzz/README.md``), and heap growth *across* transactions in a long-lived
  process, the thing actually worth watching, is caught by resident-size
  monitoring rather than by exit-time leak checks.

Binding Functions
=================

The Hobbes compiler allows us to *bind* a C++ function; to make it available by name in the Hobbes environment:

.. code-block:: C++

  int addTwo(int i){
    return 2 + i;
  }

  ...
  c.bind("addTwo", &addTwo);
  ...

.. note:: **Function Pointers**

  A key aspect of functional programming is that functions are just special kinds of *data*, and that whilst there may be operations which can be performed on data, the application which can be performed on a function is that of *parameter application*. Once a function has had all its parameters applied the code is executed and the return value given back to the callsite.

  This has a two major implications: we can assign functions to values (as we've already seen), and we can write *higher-order functions*: those which take another function as a parameter, or return a function. In C++, the type of a function is denoted entirely by its signature, the abstract format for which is the function pointer:

  .. code-block:: c++

    int sum(int a, int b){
      return a + b;
    }

    int (*twoIntOperation)(int, int) = sum;

  We declare ``sum`` as the concrete example of a function which takes two ints and returns an int, and assign it to the variable ``twoIntOperation``. The type of ``twoIntOperation`` is exactly that: a function which takes two ints and returns one.

  The syntax is a little hairy. In Hobbes, the function type syntax is a lot clearer, as we'll see very soon.

Once I've bound ``addTwo`` to the Hobbes compiler ``c`` it becomes available in the REPL environment. By invoking ``addTwo``, I can see that Hobbes knows the type signature and knows how to invoke the function:

::

  > addTwo
  *** stdin:1,1-13: Failed to compile expression due to unresolved type constraint:
    Print (int) -> int
  > addTwo(3)
  5
  > 

Hobbes has a rich binding environment, with bi-directional support for marshalling common C++ types (as we've already seen, with int), as well as C++ structs, and the hobbes types such as record and variant.

Simple scalar types
=====================

As we've already seen, the Tuple is an important and common basic type in Functional Programming, used to keep elements of data together in a small lexical scope. Over time many of these functional types have 'leaked' into C++, where we now have ``std::tuple`` and the "two-tuple" special case, ``std::pair``.

Hobbes is aware of the parametric ``std::pair`` and ``std::tuple`` types and composes them appropriately:

.. code-block:: c++

  typedef std::pair<int, const hobbes::array<char>*> Writer;

  Writer* getWriter(){
    return hobbes::make<Writer>(34, hobbes::makeString("Sam"));
  }

  ...
  c.bind("getWriter", &getWriter);
  ...

Then, in the Hobbes REPL:

::

  > getWriter()
  (34, "Sam")

  
.. note::

  ``hobbes::make`` and its cousins ``hobbes::makeString`` and ``hobbes::makeArray`` (up next!) allow Hobbes to allocate memory itself from the thread-local memory region described in :ref:`hobbes_memory_model` — released by resetting the region, not by destructors. They have the added advantage of outputting hobbes-native types which, amongst other things, are able to pretty-print themselves.

Structs
=======

There's usually a point at which our pairs or tuples grow in importance in our domain, and we want to give names to the members. In C++ we might use a struct for this purpose. Hobbes allows us to expose our C++ structs with the ``DEFINE_STRUCT`` macro:

.. code-block:: c++

  DEFINE_STRUCT(Writer,
   (size_t, age),
   (const hobbes::array<char>*, name)
  );

  hobbes::array<Writer>* getWriters(){
    auto writers = hobbes::makeArray<Writer>(2);

    writers->data[0].age = 21;
    writers->data[0].name = hobbes::makeString("John");

    writers->data[1].age = 22;
    writers->data[1].name = hobbes::makeString("Paul");

    return writers;
  }

  ...
  c.bind("getWriters", $getWriters);
  ...

...and in the REPL:

::

  > getWriters
  age   name
  ___ ______
  21    John
  22    Paul

.. note::

  Hobbes has been able to determine appropriate column names from the struct definition, just as it does with our hobbes-native record type!

Variant
=======

Slightly more complex, our 'OR' type, the variant:

.. code-block:: c++

  typedef hobbes::variant<int, const hobbes::array<char>*> CountOrMessage;

  CountOrMessage* classify(int i){
    if(i<22){
      return hobbes::make<CountOrMessage>(i);
    }else{
      return hobbes::make<CountOrMessage(hobbes::makeString("haha"));
    }
  }

  ...
  c.bind("classify", $classify);
  ...

In our example we define our variant type in C++, and then create an instance depending on the value of some function parameter. Then in the Hobbes REPL we are able to call the bound function ``classify`` and deal with the result in a functional manner:

::

  > classify(12)
  |0=12|
  > classify(42)
  |1="haha!"|
  
Completing the round trip
=========================

In Hobbes we can expose higher-order functions simply. This allows us to expose Hobbes functionality in C++, completing the round trip!

As a Haskell-like language, the syntax is elegant. In the following example we declare a function ``binaryIntFn`` which takes two ints and returns an int. By compiling this function and binding it to the runtime Hobbes environment, we're able to "plug in" behaviour based on elements of the runtime environment:

.. code-block:: c++

  int binaryIntFn(int (*pf)(int, int), int x){
    return pf(x, x);
  }

::

  >  binaryIntFn(\x y.x+y, 3)
  6
  > binaryIntFn(\x y.x*y, 4)
  16

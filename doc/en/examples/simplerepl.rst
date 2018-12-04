.. _repl_example:

A Simple REPL
*************

In the Embedding Hobbes chapter we built a simple Hobbes interpreter with two-way function bindings.

This allowed us to call C++ functions from within the Hobbes environment, and also, via the function pointer, to execute Hobbes code in the containing C++ code. 

Here's the full code listing. Afterwards we'll dig into all the parts one by one:

.. code-block:: c++

  #include <iostream>
  #include <stdexcept>
  #include <hobbes/hobbes.H>

  int addTwo(int i){ return 2 + i; }

  DEFINE_STRUCT(Writer,
    (size_t, age),
    (const hobbes::array<char>*, name)
  );

  typedef std::pair<int, const hobbes::array<char>*> Writer;

  Writer* getWriter(){
    return hobbes::make<Writer1>(34, hobbes::makeString("Sam"));
  }

  hobbes::array<Writer>* getWriters(){
    auto writers = hobbes::makeArray<Writer>(4);

    writers->data[0].age = 21;
    writers->data[0].name = hobbes::makeString("John");

    writers->data[1].age = 22;
    writers->data[1].name = hobbes::makeString("Paul");

    writers->data[2].age = 22;
    writers->data[2].name = hobbes::makeString("George");

    writers->data[3].age = 42;
    writers->data[3].name = hobbes::makeString("Sam");

    return writers;
  }

  typedef hobbes::variant<int, const hobbes::array<char>*> CountOrMessage;

  CountOrMessage* classify(int i){
    if(i < 22){
      return hobbes::make<CountOrMessage>(i);
    }else{
      return hobbes::make<CountOrMessage>(hobbes::makeString("haha!"));
    }
  }

  int binaryIntFn(int (*pf)(int, int), int x){
    return pf(x, x);
  }

  int main() {
    hobbes::cc c;

    c.bind("addTwo", &addTwo);
    c.bind("getWriters", &getWriters);
    c.bind("getWriter", &getWriter);
    c.bind("classify", &classify);
    c.bind("binaryIntFn", &binaryIntFn);

    while (std::cin) {
      std::cout << "> " << std::flush;

      std::string line;
      std::getline(std::cin, line);
      if (line == ":q") break;

      try {
        c.compileFn<void()>("print(" + line + ")")();
      } catch (std::exception& ex) {
        std::cout << "*** " << ex.what();
      }

      std::cout << std::endl;
      hobbes::resetMemoryPool();
    }

    return 0;
  }

Sections
========

Prelude
-------

::
  
  #include <iostream>
  #include <stdexcept>
  #include <hobbes/hobbes.H>

First comes the include statements for the c++ pre-processor. The Hobbes header file is available in the `GitHub repo 
<http://github.com/Morgan-Stanley/hobbes/>`_ under the *include* directory.

A simple function
-----------------

::
  
  int addTwo(int i){ return 2 + i; }

This simple c++ funciton takes an int and adds 2 to it, returning the result. It's used later, as an example of binding a c++ funciton in to the hobbes environment.

Binding custom datatypes
------------------------

::
  
  DEFINE_STRUCT(Writer,
    (size_t, age),
    (const hobbes::array<char>*, name)
  );

  typedef std::pair<int, const hobbes::array<char>*> Writer1;

  Writer* getWriter(){
    return hobbes::make<Writer>(34, hobbes::makeString("Sam"));
  }

Here, we use the ``DEFINE_STRUCT`` macro to make Hobbes aware of a custom datatype, and use the ``make*`` functions for our custom type and for ``std::string``. This makes Hobbes responsible for allocating space for this data, and allows it to deallocate the memory when it's finished.

Arrays
------

::

  hobbes::array<Writer>* getWriters(){
    auto writers = hobbes::makeArray<Writer>(4);

    writers->data[0].age = 21;
    writers->data[0].name = hobbes::makeString("John");

    writers->data[1].age = 22;
    writers->data[1].name = hobbes::makeString("Paul");

    writers->data[2].age = 22;
    writers->data[2].name = hobbes::makeString("George");

    writers->data[3].age = 42;
    writers->data[3].name = hobbes::makeString("Sam");

    return writers;
  }

We're extending the example to include ``makeArray``, showing how Hobbes can interact with collections.

Variants
--------

::

  typedef hobbes::variant<int, const hobbes::array<char>*> CountOrMessage;

Firstly, we use a c++ *typedef** to give a nice name to a Hobbes variant

::

  CountOrMessage* classify(int i){
    if(i < 22){
      return hobbes::make<CountOrMessage>(i);
    }else{
      return hobbes::make<CountOrMessage>(hobbes::makeString("haha!"));
    }
  }

Hobbes creates factory methods for each subclass in the variant, and so, depending on some external factor, we're able to initialise either a *count* or a *message*.

Function pointers
-----------------

::
  
  int binaryIntFn(int (*pf)(int, int), int x){
    return pf(x, x);
  }

The basic mechanism by which work is abstracted, and how we can externalise behaviour from Hobbes - allowing us to interact with Hobbes functionality from outside the environment. In this case we expect ``binaryIntFn`` to be called with two items - firstly, a function which takes two ``int``s and returns an ``int``, and secondly an ``int``.

The result of the application of the function with the second argument twice is then returned to Hobbes as an ``int``.

main()
======

Finally, the ``main`` method brings it all together:

::

  int main() {
    hobbes::cc c;

    c.bind("addTwo", &addTwo);
    c.bind("getWriters", &getWriters);
    c.bind("getWriter", &getWriter);
    c.bind("classify", &classify);
    c.bind("binaryIntFn", &binaryIntFn);

    while (std::cin) {
      std::cout << "> " << std::flush;

      std::string line;
      std::getline(std::cin, line);
      if (line == ":q") break;

      try {
        c.compileFn<void()>("print(" + line + ")")();
      } catch (std::exception& ex) {
        std::cout << "*** " << ex.what();
      }

      std::cout << std::endl;
      hobbes::resetMemoryPool();
    }

    return 0;
  }

#. We initialise an instance of ``hobbes::cc`` on the stack.
#. We bind five functions to it by their address.
#. Then, in a loop, we print a prompt, accept a string from STDIN, and attempt to compile and execute the input string.
#. Any failures are caught and reported
#. Finally, any unreferenced data left after the REPL loop is collected to avoid memory leaks.


C++ Bindings
************

In addition to the native support Hobbes has for networking, there's a useful C++ API that you can use to send messages to Hobbes processes.

DEFINE_NET_CLIENT
=================

::

  #include <iostream>
  #include <hobbes/net.H>
  
  DEFINE_NET_CLIENT(
    Connection,                       // Profile name, which we'll use later
    (mul, int(int,int), "\\x y.x*y")  // Functionality to evaluate remotely
  );
  
Similarly to the Logging code, we first define the Connection semantics that we want to use. This macro creates a connection class for us which has a constructor and a data member for the ``mul`` function, along with all the type negotiotiation and connectiona management logic.

Using the connection
====================

::

  int main(int, char**) {
    try {
      Connection c("localhost:8080");
  
      std::cout << "c.mul(1,2) = " << c.mul(1,2) << std::endl;
  
    } catch (std::exception& e) {
      std::cout << "*** " << e.what() << std::endl;
    }
    return 0;
  }

Here we're instantiating the synthesised ``Connection`` class, invoking its ``mul`` member (remotely!) and printing out the result.

Running the code
================

Again, spin up an instance of *hi* listening on port 8080. You won't see any output from here.

::

  $ hi -s -p 8080

Next, simply run your c++ driver program!

::

  $ ./test
  c.mul(1,2) = 2
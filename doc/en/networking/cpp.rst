.. _hobbes_networking_cpp:

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
      Connection c("localhost:8080",
                   hobbes::net::NetREPLSharedSecret("example-secret"));
  
      std::cout << "c.mul(1,2) = " << c.mul(1,2) << std::endl;
  
    } catch (std::exception& e) {
      std::cout << "*** " << e.what() << std::endl;
    }
    return 0;
  }

Here we're instantiating the synthesised ``Connection`` class, invoking its ``mul`` member (remotely!) and printing out the result.

Authenticating
==============

``NetREPLSharedSecret`` is the built-in way to authenticate to a listener that requires it, and what ``hi -p`` expects. An application that embeds Hobbes can use a handshake of its own instead -- Kerberos/GSSAPI, a signed token -- by installing a ``hobbes::NetREPLAuthenticator`` on the listener and passing the matching ``hobbes::net::NetREPLClientAuthenticator`` to the connection class in place of the secret:

::

  // server (an application embedding hobbes)
  hobbes::installNetREPL(8080, &compiler, std::make_shared<MyAuthenticator>());

  // client
  MyClientAuthenticator auth;
  Connection c("localhost:8080", auth);

Both are small interfaces over an exchange of opaque tokens; Hobbes moves the tokens and never interprets them, and neither side touches the socket. The client side lives in the header-only ``hobbes/netauth.H`` (included by ``hobbes/net.H``), so a client still needs no Hobbes library. ``doc/en/netrepl-custom-authentication.md`` in the source tree describes the interfaces and the wire format, and sketches a GSSAPI authenticator.

Either way this authenticates the peer when the connection opens. It does not encrypt the connection: for remote access keep the listener on loopback and use a same-host TLS proxy or a tunnel.

Running the code
================

Again, spin up an instance of *hi* listening on port 8080. You won't see any output from here.

::

  $ printf '%s\n' 'example-secret' > netrepl.secret
  $ chmod 600 netrepl.secret
  $ hi -s -p 8080 --netrepl-secret-file netrepl.secret

Next, simply run your c++ driver program!

::

  $ ./test
  c.mul(1,2) = 2

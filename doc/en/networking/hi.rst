.. _hobbes_networking_hi:

Networking with hi
******************

Hobbes supports native, typesafe client-server network programming in a very similar manner to its support for structured logfiles. Connection information alongside the types of data involved are exposed through an unqualifier much like :ref:`LoadFile <hobbes_loadfile_unqualifier>`, in a manner mostly invisible to the user.

Setting up the receiver
=======================

``hi`` can be set to receive messages over the network by opening a port:

::

  $ hi -p 8080
  [...]
  running a repl server at myhost:8080
  >

Opening a connection
====================

From another instance of *hi*, create a connection to the first using the ``Connect`` unqualifier:

::
  
  $ hi -s
  > c = connection :: (Connect "myhost:8080" p) => p

From here, you can inspect the details of the connection with ``printConnection``:

::

  > printConnection(c)
  id expr input output
  -- ---- ----- ------

  >

There's not much here yet, so let's create some functionality on the server.

Remote methods
==============

Back on the REPL server, create a new function called addOne:

::

  > addOne = \x.x+1
  >

Then on the client, let's make that functionality available remotely. We'll use the hobbes type negotiation mechanism to make sure all the types line up:

::

  > receive(invoke(c, `addOne`, 12))
  13

Wow! 

The ``invoke`` and ``receive`` functions allow Hobbes to execute commands remotely and interpret the results.

The quoted form of the invokation isn't a string - it's parsed but as-yet unexecuted Hobbes in a form which can be serialised and set to a remote Hobbes process for invocation. It's this mechanism that Hobbes uses to determine the return type of the method - on the remote process!

Inspection of the connection
----------------------------

If we use ``printConnection`` to take another look at ``c``, we'll see that the initial remote invocation of the function ``addOne`` has had some effects:

::

  > printConnection(c)
  id expr input output
  -- ---- ----- ------
  1  int  int   addOne

Firstly, we can see that Hobbes has given the remote ``addOne`` function a numeric ID - this means that future invocations will be much faster.

Secondly - Hobbes has used the connection to communicate with the remote host and find out the type of ``addOne`` - a function that takes an int and returns an int. 

Delayed Invocation
==================

In the above example the type information Hobbes gathered from the server was made available at the first invocation of the method, using ``receive``. However, Hobbes has the ability to query type information from the server using the unqualifier mechanism much earlier, before the method is even invoked.

Go back to the REPL and add another method, addTwo:

::

  addTwo = \x.x+2

Then on the client,

::

  > remoteAddTwo = \x.receive(invoke(c, `addTwo`, x::int))
  > printConnection(c)
  id expr input output
  -- ---- ----- ------
  1  int  int   addOne
  2  int  int   addTwo

In this example, ``remoteAddTwo`` is a function defined by a lambda - i.e. one that we haven't yet called! All we've passed is the information about the input type - the ``int`` argument to ``addTwo`` - and the Hobbes server process has done all the type inference and returned the structured type data for us.

Errors
======

Because all the type information is evaluated on the remote host, any processing errors or type mismatches will also come from the other server. For example, try to invoke a function that doesn't yet exist:

::

  > receive(invoke(c, `addSeven`, 3))
  stdin:1,1-33: Error from server: stdin:0,0-0: Undefined variable: 'addSeven'

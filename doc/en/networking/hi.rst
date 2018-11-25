.. _hobbes_networking_hi:

Networking with hi
******************

Hobbes supports native, typesafe client-server network programming in a very similar manner to its support for structured logfiles. Connection information alongside the types of data involved are exposed through an unqualifier much like :ref:`LoadFile <hobbes_loadfile_unqualifier>`, in a manner mostly invisible to the user.

You can use Hobbes networking to perform actions on another host, such as gathering usage statistics, or performing administrative actions such as changing the log level.

Setting up the receiver
=======================

*hi* can be set to receive messages over the network. If we invoke *hi* with the ``-p`` flag we can specify the port to listen on:

::

  $ hi -p 8080
  [...]
  running a repl server at myhost:8080
  >

We'll call this the *server*, and next we'll connect to it over the network.

Opening a connection
====================

From another instance of *hi*, create a connection to the *server* using the ``Connect`` unqualifier:

::
  
  $ hi -s
  > c = connection :: (Connect "myhost:8080" p) => p

.. note:: **Unqualifiers**

  For more information about how this works, have a look at the :ref:`LoadFile unqualifier <hobbes_loadfile_unqualifier>`, which we use to load data using the Hobbes persistence API.

We'll call this the *client*. From here, you can inspect the details of the connection with ``printConnection``:

::

  > printConnection(c)
  id expr input output
  -- ---- ----- ------

  >

There's not much here yet, so let's create some functionality on the *server*.

Remote methods
==============

Back on the *server*, create a new function called addOne:

::

  > addOne = \x.x+1
  >

Then on the *client*, let's make that functionality available remotely. We'll use the hobbes type negotiation mechanism to make sure all the types line up:

::

  > receive(invoke(c, `addOne`, 12))
  13

Wow! There's some new Hobbes here, so let's go through this line piece by piece.

The ``invoke`` and ``receive`` functions allow Hobbes to execute commands remotely and interpret the results. We call them together becuase the type information about the return value of the ``addOne`` function is passed between them.

Secondly, the strange 'quoted' form of ``addOne``. The quoted form of the invocation isn't a string - it's parsed but as-yet unexecuted Hobbes in a form which can be serialised and set to a remote Hobbes process for invocation. It's this mechanism that Hobbes uses to determine the return type of the method - on the remote process!

In this manner we're able to execute functions on the server from the client - without any of the complex type negotionation or serialisation that we'd otherwise have to do.

Once the work has been executed remotely, the result has been serialised, sent across the network, deserialised and displayed in our local *client* prompt.

Inspection of the connection
============================

If we use ``printConnection`` to take another look at ``c``, we'll see that the initial remote invocation of the function ``addOne`` has had some effects:

::

  > printConnection(c)
  id expr input output
  -- ---- ----- ------
  1  int  int   addOne

Firstly, we can see that Hobbes has given the remote ``addOne`` function a numeric ID - this means that future invocations will be much faster.

Secondly, Hobbes has used the connection to communicate with the remote host and find out the type of ``addOne`` - a function that takes an ``int`` and returns an ``int``. 

Delayed Invocation
==================

In the above example the type information Hobbes gathered from the server was made available at the first invocation of the method, using ``receive``. However, Hobbes has the ability to query type information from the server using the unqualifier mechanism much earlier, before the method is even invoked.

Go back to the *server* and add another method, addTwo:

::

  addTwo = \x.x+2

Then on the *client*,

::

  > remoteAddTwo = \x.receive(invoke(c, `addTwo`, x::int))
  > printConnection(c)
  id expr input output
  -- ---- ----- ------
  1  int  int   addOne
  2  int  int   addTwo

In this example, ``remoteAddTwo`` is a function defined by a lambda - that we haven't yet called! All we've passed is the information about the input type - the ``int`` argument to ``addTwo`` - and the Hobbes server process has done all the type inference and returned the structured type data for us.

We can invoke the remote function in the usual way, by passing parameters to the function name:

::

  > remoteAddTwo(3)
  5

Errors
======

Because all the type information is evaluated on the remote host, any processing errors or type mismatches will also come from the other server. For example, try to invoke a function that doesn't yet exist:

::

  > receive(invoke(c, `addSeven`, 3))
  stdin:1,1-33: Error from server: stdin:0,0-0: Undefined variable: 'addSeven'

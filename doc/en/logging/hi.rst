Processing logs with hi
***********************

We've already seen how to write C++ processes which log with the Hobbes logging API, and how we can use hog to pick up those log messages from shared memory and either store them locally or forward them on to another hog process running on a remote host.

In this chapter, we'll look at how we can read those persisted logs and process them in the Hobbes environment. 

You can read a logfile as it's being written to, or one that's been written to in the past. The name of the logfile is mentioned in the first few lines of the output from Hog. You'll see something like 

::

  finished preparing statements, writing data to './SimpleLogger/2018.09.26.data-0.log'

For simplicity, we'll use hi, the Hobbes REPL.

Reading the file
================

::

  $ hi
  > messages = inputFile :: (LoadFile "./SimpleLogger/2018.09.26/data-0.log" w) => w


That's how we open the file in hi. It looks a bit cryptic, so let's try to unpack it a little bit.

Firstly, this is simply an assignment of something called ``inputFile`` to the name ``messages``. The interesting bit is in the type annotation, which you might remember from :ref:`this previous section <type_annotations>`. This one is a little more long-winded than what we've seen before - but that's essentially what it is: a tighter specification for the *type* of what ``messages`` is.

And that's interesting, because it's a gentle hint at the fact that ``messages`` is a *type-safe* collection of all the structured data inside the file we've specified there, ``data.log``!

We'll come back to how that works a little further down, but right now we need to introduce the Hobbes logfile format.

Logfile format
--------------

Hobbes stores its persisted data in a binary format, which means the files it outputs aren't human readable. This makes them very space-efficient and means it's very quick for Hobbes to do file I/O, it just means you have to use tooling to read the files.

In practice, that's not so much of a problem because you either write the tools in advance, or else read a logfile in ``hi`` and use the appropriate queries to find the data you're looking for. For example, at Morgan Stanley there's a rich library of Hobbes tools which are used to hunt through logfiles to answer common queries.

The logfiles themselves have a *header/body* format, with a header describing the names and types (and by inference, the *sizes*) of each column. Immediately following the header is the body, which contains the actual persisted data. Therefore, performing an equality search on a member *n* of a struct of size *m* is trivial: you simply loop through every *m* bytes and compare the search value against the value at an offset of *n*.

The LoadFile unqualifier
------------------------

This brings us back to ``LoadFile``, and the type annotation in the code above. By now, you might have figured out what's actually happening. The syntax is inherited from Haskell, but what ``LoadFile`` is doing is this:

  1. Loading the file listed as its argument
  2. Reading the header and extracting the type information for each column
  3. Externalising the type information into the annotation

Therefore the annotation becomes, at compile time, a *type-safe* indication of the type of data contained in the file. In a sense, LoadFile is a hook into the Hobbes compiler, allowing the hi process to create a type at compile time, based on the type information available in the file header.

.. note:: **ORM: an analogy**

  If all this is a bit too hairy - don't worry! You can always just type the code and it'll work. Simply replace the name of the file with that of your persistence file and you're good to go.

  If you'd like a gentle push, you can think of ``LoadFile`` as a kind of backwards ORM. An Object-Relational Mapping tool can create a database schema for you based on a set of classes defined in code. You write your code and construct objects, and based on the ORM rules, these objects can be trivially persisted in a database table according to the schema.

  Well, what if that worked backwards? A reverse ORM tool could look at a database table and read the schema, and construct classes that you can code against. That's exactly what *LoadFile* is doing - except it all happens automatically, as part of the compilation process.

  Incidentally, the *same* process is happening when we use *hog* to send structured data over the network for persistence: the header is sent when the target hog process starts up, and the type information contained into the header is passed through to the persisted file before data starts to stream.

One-off data processing
=======================

We can query data inside a logfile using the comprehensions syntax we saw :ref:`previously<comprehensions>`. It's even possible to see updates to the file as it's being written - changes will be visible in hobbes code whilst elements are being added to the file. This allows for reactive programming but it also means that calculation results may change between invocations - so be careful!

Remember that our logger contains the following line:

::

  HSTORE(SimpleLogger, FirstEvent, "First", 0, 1, 2);

As a comprehension
------------------

Although the type of the data isn't quite an array, we can use comprehension syntax to collect data. In fact, this is a very common usecase for Hobbes in production. It allows us to filter and map across large amounts of data in a neat consistent manner:

::

  > [ x.1 | x <- messages.FirstEvent]
  ["12", "12", "12", "12", "12", "12", 
  ...


.. note:: **tuples?**
  
  Hobbes exposes this persisted element (the *line* of logged data, really) as a tuple, so you can unpack it using the numbered indexing syntax.

As an array
-----------

Similarly, we can use the "slice" notation to work with a subset of logged messages:

  > messages.SecondEvent[0:3]
  First 0 1 2 
  First 0 1 2 
  First 0 1 2 

.. note:: **ordering**
  
  The internal structure of Due to the internal structue of the persisted file, while elements may *look* ordered, this ordering We can force a 'most recent first' ordering of logged elements using the open-slice notation:

.. note:: **where's my data?!**
  
  If you have a process which is logging and you're not seeing any updates, it might be that you're writes from the sending process are being batched and haven't yet been flushed.

  This can be the case if you're not logging much data, and using auto-commit persistence in your :ref:`storage group definition <hobbes_define_storage_group>`.

  If that's the case, you can force a flush by calling the group's ``.commit()`` member in your logging code.

  e.g. for a storage group called *SimpleLogger* (like ours has been), you'd call

  ::
    
    SimpleLogger.commit();

Reactive processing
===================

Once we have a reference to a Hobbes file, we can perform realtime analysis of the data it contains with the ``signals`` API. If new data is written to the file, this event handler will be called:

::
  
  > signals(messages).FirstEvent <- (\_.do { putStrLn("message received!"); return true })
  > message received!
  message received!
  message received!
  [...]

This allows us to do reactive programming across Hobbes processes.
.. _hobbes_logging:

HLOG: The Hobbes Logger
***********************

As well as a programming language, Hobbes also comes packaged with a whole array of functionality to allow highly efficient structured logging from within C++ applications. 

Structured logging from within C++
==================================

Hobbes persistence is backed by a shared memory region which is initialised when the logging application starts up. In order to define some of the parameters for writing to this region, we'll use the ``DEFINE_STORAGE_GROUP`` macro.

DEFINE_STORAGE_GROUP
--------------------

.. code-block:: c++

  DEFINE_STORAGE_GROUP(
    MyGroupName,
    3000,
    hobbes::storage::Unreliable,
    hobbes::storage::AutoCommit
  );

LogGroup correlation
~~~~~~~~~~~~~~~~~~~~

Correlation of log messages is via the first parameter to the ``DEFINE_STORAGE_GROUP`` macro. The name must be unique - i.e. an application can't define two storage groups of the same name.

At the same time, we specify the number of shared memory pages to allocate for the storage group in the second macro parameter. This should reflect the maximum expected throughput for our log messages, because we need to take special action if we exceed the limit)

Reliability and drop-or-block
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The third parameter to ``DEFINE_STORAGE_GROUP`` is the behaviour to exhibit if we reach the capacity of the ringbuffer storage backed by our group's shared memory region. ``hobbes::storage::Unreliable`` means that any attempts to write to a full buffer (as we'll see with ``HSTORE`` and ``HLOG`` below) will fail, while ``hobbes::storage::Reliable`` means that such writes will block until the buffer is serviced by a consumer.

Manual vs Automatic commit
~~~~~~~~~~~~~~~~~~~~~~~~~~

The final parameter to ``DEFINE_STORAGE_GROUP`` allows us to specify the commit behaviour. ``hobbes::storage::ManualCommit`` means that all log statements up to the ``commit`` call will be grouped together in a transaction, which we can inspect later. Alternatively, specifying ``hobbes::storage::AutoCommit`` gives us uncorrelated log messages on the consumer side, but we don't need to call ``commit`` ourselves.

Once the LogGroup has been set up, we can start to log data. For that, we'll want to look at the ``HLOG`` and ``HSTORE`` macros.

HLOG and HSTORE
---------------

The ``HLOG`` macro logs a formatted message to a given LogGroup with an event name. It can be used for logging string output from applications. 

.. code-block:: c++

  HLOG(
    MyGroupName,
    EventName,
    "Format String with positional arguments $0, $1",
    12
    42.0
  );

Using the ``HLOG`` macro provides compile-time safety for the format string, and gcc will throw an error if you refer to a positional parameter that doesn't exist.

``HSTORE`` allows for richer structured logging of data types that we declare. In the following example we're simply logging an ``int``, but Hobbes has support for all the Hobbes primitive types, as well as ``std::string``, ``char *``, and arrays, vectors and tuples of its supported types.

.. code-block:: c++

  HLOG(
    MyGroupName,
    EventName,
    12
  );

Hobbes is also able to persist aggregate types we declare ourselves with ``DEFINE_HSTORE_STRUCT``, as well as custom types by specialising the ``hobbes::storage::store<YourType>`` class. Information about ``hobbes::storage:store`` can be found in the Hobbes GitHub repository.

Declaring structured types for HSTORE logging
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can create our own structured types for HSTORE logging with the ``DEFINE_HSTORE_STRUCT`` macro:

.. code-block:: c++

  DEFINE_HSTORE_STRUCT(
    StructName,
    (double, val1),
    (int, val2)
  );

Example
=======

An annotated example of a log producer in C++ can be found in the :ref:`examples <hobbes_logging_example>`
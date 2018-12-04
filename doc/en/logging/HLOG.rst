.. _hobbes_logging:

HLOG: The Logger
****************

As well as a programming language, Hobbes also comes packaged with a whole array of functionality to allow highly efficient structured logging from within C++ applications. 

Structured logging from within C++
==================================

Hobbes persistence is backed by a shared memory region which is initialised when the logging application starts up. In order to define some of the parameters for writing to this region, we'll use the ``DEFINE_STORAGE_GROUP`` macro.

.. _hobbes_define_storage_group:

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

The final parameter to ``DEFINE_STORAGE_GROUP`` allows us to specify the commit behaviour. ``hobbes::storage::ManualCommit`` means that all log statements up to the ``commit`` call will be grouped together in a transaction, which we can inspect later.

Alternatively, specifying ``hobbes::storage::AutoCommit`` gives us uncorrelated log messages on the consumer side, but we don't need to call ``commit`` ourselves.

Transactions which are manually committed will have a timestamp logged alongside them which can be used in out-of-band analysis and reporting, whereas autocommitted transactions have no timestamp. You can always call ``commit`` on an autocommit LogGroup, which will immediately persist the current transaction.

Finally, there's one more difference in how persisted data is made available to us in code - which we'll investigate in :ref:`logs and transactions <hobbes_logs_and_transactions>`

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

  HSTORE(
    MyGroupName,
    EventName,
    12
  );

Information about the types Hobbes is able to persist can be found in :ref:`hobbes persistable types <hobbes_persistable_types>`

.. warning:: **File size**

  Practically speaking, what we're discussing here is data *persistence* rather than logging. For that reason there's no model for output file rotation. That means that your persisted data files might very large grow in size, and you'll need to find a way to externally manage that.
  
  At Morgan Stanley, Hobbes persistence is used in production applications which might be bounced daily or even weekly, resulting in persisted files of many dozens of gigabytes.

Declaring structured types for HSTORE logging
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can create our own structured types for HSTORE logging with the ``DEFINE_HSTORE_STRUCT`` macro:

.. code-block:: c++

  DEFINE_HSTORE_STRUCT(
    StructName,
    (double, val1),
    (int, val2)
  );

.. _hobbes_simple_logging_example:

Example
=======

A simple example of a log producer is shown below. We initialise a small unreliable logger(!) then drop a few log messages with HLOG:

.. code-block:: c++

  #include <hobbes/storage.H>
  #include <chrono>
  #include <thread>

  using namespace std;
  using namespace std::chrono;
  using namespace hobbes::storage;

  DEFINE_STORAGE_GROUP(
    SimpleLogger,
    1,
    Unreliable,
    AutoCommit
  );

  int main() {

    while(true){
      HSTORE(SimpleLogger, FirstEvent, "First", 0, 1, 2);
      HSTORE(SimpleLogger, SecondEvent, "Second", "data", 3.4);

      this_thread::sleep_for(milliseconds(500));
    }
  }

A worked example of a log producer in C++ can be found in the :ref:`examples <hobbes_logging_example>`
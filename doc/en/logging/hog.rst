.. _hog:

Consuming logs with HOG
***********************

When our producer code runs and starts logging, it initialises a shared memory region of the appropriate size and with a name which is visible to Hobbes logging consumers.

Over time, as the application logs, the ringbuffer which backs onto the shared memory region will fill up, and depending on the reliability semantics specified in the ``DEFINE_STORAGE_GROUP`` invocation, further writes will either block or fail.

To prevent this from happening, a performant consumer must service the queue and provide further processing for log messages. One such consumer which is pre-written with some solid default behaviour is ``hog``.

HOG
---

::

  $ hog
  hog : record structured data locally or to a remote process

    usage: hog [-d <dir>] [-g group+] [-p t s host:port+] [-s port] [-c] [-m <dir>]
  where
    -d <dir>          : decides where structured data (or temporary data) is stored
    -g group+         : decides which data to record from memory on this machine
    -p t s host:port+ : decides to send data to remote process(es) every t time units or every s uncompressed bytes written
    -s port           : decides to receive data on the given port
    -c                : decides to store equally-typed data across processes in a single file
    -m <dir>          : decides where to place the domain socket for producer registration

Hog is reponsible for consuming messages from a particular LogGroup and coordinating their onward flow. Options are:

  #. To local disk. The default logfile directory is ``./$groupname``, and this can be overridden with ``-d``.
  #. To a remote process on a server described with ``-p``

Finally, Hog is also able to receive log messages from a remote process with ``-s``.

Logging to disk with HOG
------------------------

If we take our log message driver application from the :ref:`previous section <_hobbes_simple_logging_example>`, start a Hog listener on the same LogGroup and then start the driver, we'll see something like the following:

::
  
  $ hog -g SimpleLogger
  [2018-01-01T09:00:00.867323]: hog running in mode : |local={ dir="./$GROUP/$DATE/data", serverDir="/var/tmp" groups={"SimpleLogger"} }|
  [2018-01-01T09:00:00.867536]: install a monitor for the SimpleLogger group
  [2018-01-01T09:00:01.637614]: new connection for 'SimpleLogger'
  [2018-01-01T09:00:01.733374]: queue registered for group 'SimpleLogger' from 3817:3817, cmd 0
  [2018-01-01T09:00:01.848340]: error on connection for 'SimpleLogger': Process read error (closed pipe)
  [2018-01-01T09:00:01.912325]:  ==> FirstEvent :: () (#0)
  [2018-01-01T09:00:01.969274]:  ==> SecondEvent :: [:char|10L:] (#1)
  [2018-01-01T09:00:02.009241]:  ==> log :: <any of the above>
  [2018-01-01T09:00:02.129401]: finished preparing statements, writing data to './SimpleLogger/2018.01.01/data.log'
  

This output tells us a couple of things:

  #. Firstly, Hog is running in local mode, meaning that it's going to consume messages from the Hobbes ringbuffer in memory and write them out to disk.
  #. When the driver application starts we get some state information about the LogGroup, and the message types
  3. Hog has been able to determine the message names and, crucially, their types.

The fifth line isn't really a problem, it's just telling us that the logging application has shut down - which it has! Replacing ``return 0;`` with ``while(true);`` in the driver will keep it running until you kill it with ``ctrl-c``. You can make this change if you like, but for our purposes it doesn't really matter.



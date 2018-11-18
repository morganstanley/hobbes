.. _hog:

Consuming logs with hog
***********************

When our producer code runs and starts logging, it initialises a shared memory region of the appropriate size and with a name which is visible to Hobbes logging consumers.

Over time, as the application logs, the ringbuffer which backs onto the shared memory region will fill up, and depending on the reliability semantics specified in the ``DEFINE_STORAGE_GROUP`` invocation, further writes will either block or fail.

To prevent this from happening, a performant consumer must service the queue and provide further processing for log messages. One such consumer which is pre-written with some solid default behaviour is ``hog``.

hog
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

  #. Write messages to local disk. The default logfile directory is ``./$groupname``, and this can be overridden with ``-d``.
  #. Write messages to a remote hog process, running on a server described with ``-p``

Conversely, Hog is also able to receive log messages from a remote process with ``-s``.

.. _hog_logging_to_disk:

Logging to disk with hog
------------------------

If we take our log message driver application from the :ref:`previous section <hobbes_simple_logging_example>`, start a Hog listener on the same LogGroup and then start the driver, we'll see something like the following:

::
  
  $ hog -g SimpleLogger
  [2018-01-01T09:00:00.867323]: hog running in mode : |local={ dir="./$GROUP/$DATE/data", serverDir="/var/tmp" groups={"SimpleLogger"} }|
  [2018-01-01T09:00:00.867536]: install a monitor for the SimpleLogger group
  [2018-01-01T09:00:01.637614]: new connection for 'SimpleLogger'
  [2018-01-01T09:00:01.733374]: queue registered for group 'SimpleLogger' from 3817:3817, cmd 0
  [2018-01-01T09:00:01.912325]:  ==> FirstEvent :: () (#0)
  [2018-01-01T09:00:01.969274]:  ==> SecondEvent :: [:char|10L:] (#1)
  [2018-01-01T09:00:02.009241]:  ==> log :: <any of the above>
  [2018-01-01T09:00:02.129401]: finished preparing statements, writing data to './SimpleLogger/2018.01.01/data.log'

This output tells us a couple of things:

  #. Firstly, Hog is running in local mode, meaning that it's going to consume messages from the Hobbes ringbuffer in memory and write them out to disk.
  #. When the driver application starts we get some state information about the LogGroup, and the message types
  #. Hog has been able to determine the message names and, crucially, their types.

Reading logs from remote processes with hog
-------------------------------------------

You can use two instances of hog to send Hobbes log messages from one host to another. The setup is simple:

On the logging host
~~~~~~~~~~~~~~~~~~~

On the host where the application is logging, invoke ``hog`` with the ``-p`` parameter as follows:

::

  $ hog -g SimpleLogger -p $a $b $h:$p

Where:

  - ``$t`` is the time in seconds between sent messages
  - ``$s`` is the buffer size in *uncompressed* bytes before which a message is sent
  - ``$h:$p`` is the host and port on which the receiving instance of hog is running.

.. note::

  Messages are sent to the receiving host *at least* as often as ``$t``. If ``$s`` bytes are ready to be sent, this will happen regardless of the time since the last message.


For example:

::

  myhost $ hog -g SimpleLogger -p 1 100 anotherhost:10101
  [2018-09-26T15:03:12.126732]: hog running in mode : |batchsend={ dir="./$GROUP/$DATE/data", serverDir="/var/folders/pp/g8vs2j610l5fsy8lqlbh_8tm0000gn/T/", clevel=6, batchsendsize=100B, sendto=["anotherhost:10101"], groups={"SimpleLogger"} }|
  [2018-09-26T15:03:12.128300]: install a monitor for the 'SimpleLogger' group

On the receiving host
~~~~~~~~~~~~~~~~~~~~~

On the receiving host, the setup is very similar to the first case, "Logging to disk". The only difference is that rather than reading messages from the in-memory ring buffer, messages are instead received on a local port.

The setup is simple:

::

  $ hog -g SimpleLogger -s $p

Where:

  - ``$p`` is the port on which messages are to be received.

For example:

::

  anotherhost $ hog -g SimpleLogger -s 10101
  [2018-09-26T15:03:52.277627]: hog running in mode : |batchrecv={ dir="./$GROUP/$DATE/data", localport=10101 }|

Execute the logging driver
~~~~~~~~~~~~~~~~~~~~~~~~~~

Finally, on the logging host, execute your log driver application. You'll see both of the hog processes spring to life, and the receiving host will print the type information of the two messages we sent.
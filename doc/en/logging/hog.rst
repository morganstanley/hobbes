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



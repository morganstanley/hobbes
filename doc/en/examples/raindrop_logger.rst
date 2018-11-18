.. _hobbes_logging_example:

A Simple Logger
***************

.. code-block:: c++

  #include <hobbes/storage.H>
  #include <chrono>
  #include <thread>

  DEFINE_STORAGE_GROUP(
    RaindropLogger,
    1,
    Unreliable,
    ManualCommit
    );
  
  int main(){
    while(true){
      auto raindrop_size { rand () % 10 };
      
      switch( rand() % 5){
        case 0: HSTORE(RaindropLogger, FirstBucket, size);
                break;
        case 1: HSTORE(RaindropLogger, SecondBucket, size);
                break;
        case 2: HSTORE(RaindropLogger, ThirdBucket, size);
                break;
        case 4: HSTORE(RaindropLogger, FourthBucket, size);
                break;
      }

      RaindropLogger.commit();

      cout << "." << flush;
      this_thread::sleep_for(milliseconds(500));
    }
  }

In this example we're collecting raindrops of different sizes in four different buckets, and every time a raindrop falls in a bucket, we're logging that information.

We therefore have a storage group called "RaindropLogger" with unreliable manual-commit semantics (i.e. we'd rather lose an update than block on the publishing side, and writes to the persistence file are going to happen when we call ``flush``.

There are four event *names*, one for each bucket, and a drop falls every half a second. There's a one in five chance that a drop won't fall in any of the buckets, just to make things interesting!

Running locally
===============

Fire up an instance of *hog* and let it listen locally for updates to the RaindropLogger group:

::

  $ hog -g RaindropLogger
  [2018-11-07T14:19:20.812276]: hog running in mode : |local={ dir="./$GROUP/$DATE/data", serverDir="/var/tmp", groups={"RaindropLogger"} }|
  [2018-11-07T14:19:20.812276]: install a monitor for the 'RaindropLogger' group

In another shell on the same machine, build and run your RaindropLogger driver:

::

  $ ./RaindropLogger
  ..............

You'll see the *hog* output change to include the name of the written logfile. Follow the instructions in :ref:`Processing logs with hi <hobbes_log_processing_hi>` to read the file into the *hi* REPL


Queries
=======

Let's jump into *hi* again and play with our data. We can use the ``size`` function to see how many raindrops fell in a given bucket:

::
  
  $ hi
  > raindrops = inputFile :: (LoadFile "log_file_name" w) => w
  > size(raindrops.FirstBucket)
  51

Get the total amount of water that fell in the third bucket:

::

  > sum(raindrops.ThirdBucket[:0])
  614

.. note:: **Slice?**
  
  Why do we have to use the slice notation in the above example?

  Great question! If you use ``:t`` to inspect the type of *FirstBucket* you'll see that whatever it is, it's *not* an array!

  For performance reasons in order to allow the size of the group to grow over time whilst it's being monitored in memory, the type of a log group is a *stack of arrays*, meaning special functions need to be used to work with persisted messages.
  
  That's why the slice notation works but array indexing doesn't. It also explains why we're using the slice notation here: it's to force the log data into an array:

  ::
    
    > :t raindrops.ThirdBucket[1:2]
    [int]

  The ``sum`` function is overloaded for arrays, but not for the Hobbes internal *stack of arrays* type!

Let's get the count of all raindrops bigger than size 5:

  > size([x | x <- raindrops.FirstBucket[0:] ++ raindrops.SecondBucket[0:] ++ raindrops.ThirdBucket[0:] ++ raindrops.FourthBucket[0:], x > 5])
  193

Transactions
------------

Because we used a *manually* commited log group for our raindrop data (see :ref:`Logs and Transactions <hobbes_logs_and_transactions>`), we can see a list of all the raindrops in order, regardless of the bucket they fell in:

  ::

    > raindrops.transactions
    
(TODO)

This even shows us the committed transactions for where there was no logged event - i.e. the one in five chance that a raindrop missed a bucket!
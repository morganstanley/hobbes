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
-------


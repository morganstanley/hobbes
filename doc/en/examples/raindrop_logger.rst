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
    AutoCommit
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

      cout << "." << flush;

      RaindropLogger.commit();

      this_thread::sleep_for(milliseconds(500));
    }
  }


.. _hobbes_logging_example:

A Simple Logger
***************

.. code-block:: c++

  #include <stdlib.h>
  #include <hobbes/storage.H>

  DEFINE_STORAGE_GROUP(
    RaindropLogger,
    1,
    Unreliable,
    AutoCommit,
Processing logs with hi
=======================

We've already seen how to write C++ processes which log with the Hobbes logging API, and how we can use hog to pick up those log messages from shared memory and either store them locally or forward them on to another hog process running on a remote host.

In this chapter, we'll look at how we can read those persisted logs and process them in the Hobbes environment. For simplicity, we'll use hi, the Hobbes REPL.

Reading the file
----------------

::

  $ hi
  [...]
  > messages = inputFile :: (LoadFile "./SimpleLogger/2018.09.26/data.log" w) => w


That's how we open the file in hi. It looks a bit cryptic, so let's try to unpack it a little bit.

TODO
  - this is a typesafe process
  - hobbes persistence has a header format
  - this is read and a qualified type is created for us to use

One-off data processing
-----------------------

Realtime processing
-------------------

We can perform realtime analysis of persisted data with the ``signals`` API.

::
  
  > signals(messages).FirstMessage <- (\_.do { putStrLine("message received!"); return true })
  > message received!
  message received!
  message received!
  [...]

TODO
  -more
  
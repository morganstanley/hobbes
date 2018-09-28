Components
**********

Hobbes is comprised of two main components:

1: A programming language
-------------------------

Hobbes is a haskell-like programming language with a rich type system. Hobbes code can be embedded in a C++ program and data marshalled between the two. This means you can write C++ code which binds against a Hobbes environment and calls into Hobbes functions to execute functionality.

Over the day and as business requirements change, you can update the bound Hobbes code to give it different behaviour. The new code is compiled to highly efficient x86 instructions for later execution.

Similarly, you can make C++ functions available to the embedded Hobbes code. Ultimately, this gives you the power to choose the most efficient and effective format for different parts of your codebase: Highly structured and well-defined parts of your application can be built using C++, whilst the dynamic business logic can be written in Hobbes. 

For more details about hosting Hobbes in a C++ application, see :ref:`Embedding Hobbes <hobbes_hosting>`.
     

2: A persistence format
-----------------------
    
A data storage and expression format for out-of-band data processing. Hobbes comes with a typesafe, space-efficient persistence format for realtime storage and retrieval of application data.

This can be used for inter-process communication over TCP, quering and filtering of daily application logs, or fast post-hoc analysis of application behaviour based on Hobbes' internal decision tree structure.

For more details about Hobbes' persistence format, see :ref:`Structured logging in Hobbes <hobbes_logging>`.
  
Rationale
---------

Why create a custom programming language and persistence format? For a deeper look into the minds of its developers, you'll want to read :ref:`Digging into the domain <domain>`
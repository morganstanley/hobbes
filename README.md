# hobbes

a language, embedded compiler, and runtime for efficient dynamic expression evaluation, data storage and analysis

|section                             |description                                                   |
|------------------------------------|:------------------------------------------------------------:|
|[Building](#building)               |how to build and install hobbes                               |
|[Embedding](#embedding)             |use hobbes in C++ programs                                    |
|[Evaluation](#evaluation)           |evaluate basic hobbes expressions                             |
|[Storage](#storage)                 |record data for out-of-band analysis                          |
|[Networking](#networking)           |interact with remote hobbes processes                         |
|[Comprehensions](#comprehensions)   |transform/sort/join/filter/group sequences for data analysis  |
|[Pattern Matching](#pat-match)      |efficiently classify and destructure data                     |
|[Parsing](#parsing)                 |parse text with LALR(1) grammars                              |
|[Type Classes](#type-classes)       |overloading and compile-time calculation                      |
|[Unqualifier Modules](#unqualifiers)|user-defined "compiler plugins" for custom constraint handling|

## Building <a name="building"></a>

To build hobbes, you will need [LLVM](http://llvm.org/) 3.3 or later, [cmake](http://cmake.org/) 3.4 or later, [GNU gcc](https://gcc.gnu.org/) 4.8 or later, and a version 2.5 or later Linux kernel.

With LLVM, cmake, and g++ installed, after downloading this code you should be able to build and install hobbes just by running:

```
$ cmake .
$ make
$ make install
```

Depending on where you've installed LLVM, this might not work until you give cmake the path to LLVM's cmake modules.  In particular, you want the path to LLVM's `LLVMConfig.cmake` file.  If you set the environment variable `LLVM_DIR` to point to that directory, then the previous steps should complete successfully.

The build process will produce a static library, `libhobbes.a`, which can be linked into a C++ executable (if you want to use hobbes in a .so instead, the build produces a different static library to use, `libhobbes-pic.a`).

In addition, the build process will produce two utility programs, `hi` and `hog`.  The `hi` program is a basic interactive interpreter for hobbes expressions and the `hog` program will efficiently record structured data produced by applications into data files (these files can be loaded and queried at the same time by `hi`).  The source code for these programs can be informative as well, because they demonstrate many different aspects of the hobbes API.

## Embedding <a name="embedding"></a>

Let's consider how to embed hobbes in a simple C++ program.  This code implements a very basic shell, similar to `hi`:

```C++
#include <iostream>
#include <stdexcept>
#include <hobbes/hobbes.H>

int main() {
  hobbes::cc c;

  while (std::cin) {
    std::cout << "> " << std::flush;

    std::string line;
    std::getline(std::cin, line);
    if (line == ":q") break;

    try {
      c.compileFn<void()>("print(" + line + ")")();
    } catch (std::exception& ex) {
      std::cout << "*** " << ex.what();
    }

    std::cout << std::endl;
    hobbes::resetMemoryPool();
  }

  return 0;
}
```

First, to compile any expression we need to construct a `hobbes::cc` object.  Then, in the context of an exception handler, we can compile functions out of this `hobbes::cc` object with the `compileFn` method, giving it the type we expect back (`void()` in this case) and also a string that we expect can compile to a value of that type.  If any stage of compilation fails (parse error, type mismatch, etc) then an exception with details about the failure will be raised.  Finally, we call `hobbes::resetMemoryPool()` to release any memory that might have been dynamically allocated by our compiled expression (that is, memory allocated _by_ compiled expressions but not the compiled functions themselves -- those are released only when `hobbes::cc` is destroyed or when `hobbes::cc::releaseMachineCode` is used to destroy them).

When a compiled function decides to allocate memory, that allocation happens out of a "memory region".  A memory region is a dynamically-growable sequence of bytes where allocations come from, and it is deallocated in bulk when `hobbes::resetMemoryPool()` is called.  This makes allocation and deallocation very efficient, but requires some thought to establish "logical transaction" boundaries.  The active memory region is thread-local, so you can use the same function pointer in several threads concurrently without worrying about synchronization issues.

Finally, if we put the above program in a file called "test.cpp" then we can build it like this:

```
$ g++ -pthread -std=c++11 -I <path-to-hobbes-headers> -I <path-to-llvm-headers> test.cpp -o test -L <path-to-hobbes-libs> -lhobbes -ldl -lrt -ltinfo -lz -L <path-to-llvm-libs> `llvm-config --libs x86asmparser x86codegen x86 mcjit passes`
```

The explicit path statements may not be necessary depending on where/how LLVM and hobbes have been installed on your system.  The inline invocation of the `llvm-config` program is typical with users of LLVM, to avoid explicitly listing several libraries.

If everything worked correctly, we should now have a simple shell where we can evaluate hobbes expressions.

## Evaluation <a name="evaluation"></a>

Now that we have a working shell, we can experiment with expressions to get a sense of how the hobbes language works.  To start with, we have some typical constant values:

```
> 'c'
'c'
> 42
42
> 3.14159
3.14159
```

In total, these are the set of supported primitive types/constants:

|name  |example|description                                         |
|------|-------|:--------------------------------------------------:|
|unit  |`()`   |like 'void' in C, a trivial type with just one value|
|bool  |`false`|either true or false                                |
|char  |`'c'`  |a single character of text                          |
|byte  |`0Xff` |a single byte (0-255)                               |
|short |`42S`  |a 2-byte number                                     |
|int   |`42`   |a 4-byte number                                     |
|long  |`42L`  |an 8-byte number                                    |
|float |`42.0f`|a 4-byte floating point value                       |
|double|`42.0` |an 8-byte floating point value                      |

These primitives can be combined with arrays:

```
> [1, 2, 3]
[1, 2, 3]
> "foobar"
"foobar"
> 0xdeadbeef
0xdeadbeef
```

They can also be combined with records/tuples:

```
> {name="Jimmy", age=45, job="programmer"}
{name="Jimmy", age=45, job="programmer"}
> ("Jimmy", 45, "programmer")
("Jimmy", 45, "programmer")
```

Or with arrays of records/tuples, where they will print conveniently as a table:

```
> [{name="Jimmy", age=45, job="programmer"}, {name="Chicken", age=40, job="programmer"}]
   name age        job
------- --- ----------
  Jimmy  45 programmer
Chicken  40 programmer
> [("Jimmy", 45, "programmer"),("Chicken", 40, "programmer")]
  Jimmy 45 programmer
Chicken 40 programmer
```

We can combine types with variants or sums (the "nameless" form of variants, as tuples are to records).  Because "naked" variant introduction is incomplete by definition, we may sometimes need to introduce explicit type annotations:

```
> |food="pizza"|::|vehicle:int,food:[char]|
|food="pizza"|
> |1="pizza"|::(int+[char])
|1="pizza"|
```

Also, variants can be combined with [isorecursive types](https://en.wikipedia.org/wiki/Recursive_data_type#Isorecursive_types) to make linked lists.  These have a convenient form when printed, and built-in functions to avoid the recursive and variant type boilerplate:

```
> roll(|1=("chicken", roll(|1=("hot dog", roll(|1=("pizza", roll(|0=()|))|))|))|) :: ^x.(()+([char]*x))
"chicken":"hot dog":"pizza":[]
> cons("chicken", cons("hot dog", cons("pizza", nil())))
"chicken":"hot dog":"pizza":[]
```

These types and type constructors together make up the "algebraic data types" common in the ML family of programming languages (SML, ocaml, Haskell, ...).  The syntax and names for these types also come from ML and common academic programming language textbooks as in [TaPL](https://www.cis.upenn.edu/~bcpierce/tapl/) and [PFPL](http://www.cs.cmu.edu/~rwh/pfpl.html).

As in Haskell, hobbes supports a form of _qualified types_ with type classes and even user-defined constraint resolvers (which we will see in more detail later).  Among many other uses, this allows both mixed-type arithmetic and type inference to coexist:

```
> 0X01+2+3.0+4L+5S
15
> (\x y z u v.x+y+z+u+v)(0X01,2,3S,4L,5.0)
15
```

We can also use the `hi` program (a slightly more complex interpreter than the example earlier, distributed with hobbes) to evaluate expressions like this and also inspect their types.  For example, the types for the primitive expressions we considered earlier can be queried:

```
$ hi
hi : an interactive shell for hobbes
      type ':h' for help on commands

> :t ()
()
> :t false
bool
> :t 'c'
char
> :t 0Xff
byte
> :t 42S
short
> :t 42
int
> :t 42L
long
> :t 42.0f
float
> :t 42.0
double
```

And the types for record, array, variant and recursive values:

```
> :t [1..10]
[int]
> :t "foobar"
[char]
> :t 0xdeadbeef
[byte]
> :t {name="Jimmy", age=45, job="programmer"}
{ name:[char], age:int, job:[char] }
> :t ("Jimmy", 45, "programmer")
([char] * int * [char])
> :t cons("foo",nil())
^x.(() + ([char] * x))
```

Also for qualified types, we can use this feature to view the set of open/unresolved type constraints:

```
> :t \x.x.foo
(a.foo::b) => (a) -> b
> :t .foo
(a.foo::b) => (a) -> b
```

Here we see that the type of a simple function which just returns the value of the "foo" field in its input (both in the fully explicit `\x.x.foo` form as well as the equivalent `.foo` shorthand) has a _constraint_ (the part to the left of the `=>` in its type -- the same as in Haskell).  In this case, the entire type states that if a type `a` has a field named `foo` with type `b`, the function has type `a -> b`.  The additional complexity of this constraint allows this function to work with _any_ type `a` meeting that condition.  This particular constraint allows a kind of [duck typing](https://en.wikipedia.org/wiki/Duck_typing) for functions.

## Binding

If we're using hobbes inside of a C++ process (as opposed to just running scripts with `hi`), at some point we will need to _bind_ C++ values (typically functions) to a `hobbes::cc` instance so that they can be used in dynamic expressions at a later stage.  Much of this work is automated, often allowing us to avoid thinking about binding logic.

For example, consider a C++ function that we might have already defined:

```C++
int applyDiscreteWeighting(int x, int y) { return x * y; }
```

Now we can import this definition into our previous program, and bind it to our `hobbes::cc` instance by adding this line:

```C++
  c.bind("applyDiscreteWeighting", &applyDiscreteWeighting);
```

Then when we run our program, its shell will allow use of this function and will reject any expression using it in a way inconsistent with its type structure:

```
> applyDiscreteWeighting(3, 4)
12
> applyDiscreteWeighting(3, "pizza")
*** stdin:1,1-22: Cannot unify types: int != [char]
```

In other cases, it may be more convenient to use a type understood natively by hobbes and with dynamic memory allocation:

```C++
typedef std::pair<size_t, const hobbes::array<char>*> MyRecord;

const hobbes::array<MyRecord>* loadRecords(int key) {
  static const char* names[] = { "chicken", "hot dog", "foobar", "jimmy" };

  auto rs = hobbes::makeArray<MyRecord>(key);
  for (size_t i = 0; i < rs->size; ++i) {
    rs->data[i].first  = i;
    rs->data[i].second = hobbes::makeString(names[i % 4]);
  }

  return rs;
}
// [...]
  c.bind("loadRecords", &loadRecords);
// [...]
```

These calls to `hobbes::makeArray` and `hobbes::makeString` will allocate out of the calling thread's memory region and will produce data that hobbes knows how to generically destructure and print in a convenient form, as we can see by running this in our test program again:

```
> loadRecords(10)
0 chicken
1 hot dog
2  foobar
3   jimmy
4 chicken
5 hot dog
6  foobar
7   jimmy
8 chicken
9 hot dog
```

If we'd prefer to represent records by structs with significant field names rather than as tuples, we can instead define our type with the `DEFINE_STRUCT` macro (this is purely a syntactic difference -- the final runtime representation of tuples and structs is identical in hobbes).  For example, if we'd defined the `MyRecord` type in the previous example this way:

```C++
DEFINE_STRUCT(MyRecord,
  (size_t, index),
  (const hobbes::array<char>*, name)
);
```

And change the initialization code in `loadRecords` to refer to these fields by name:

```C++
// [...]
    rs->data[i].index = i;
    rs->data[i].name  = hobbes::makeString(names[i % 4]);
// [...]
```

Then we can access those fields by name from hobbes and e.g. tables of such records will be printed with a header showing these field names:

```
> loadRecords(10)
index    name
----- -------
    0 chicken
    1 hot dog
    2  foobar
    3   jimmy
    4 chicken
    5 hot dog
    6  foobar
    7   jimmy
    8 chicken
    9 hot dog
```

We can also bind "in reverse" using C function types, so that rather than making our C++ code appear to be valid hobbes symbols, we can make hobbes expressions appear to be valid C++ symbols.  For example, consider a C++ binding like:

```C++
int iter2(int (*pf)(int,int), int x) {
  return pf(pf(x, x), pf(x, x));
}
// [...]
  c.bind("iter2", &iter2);
// [...]
```

Here we've bound a function that itself takes a function as input, and we can use the hobbes syntax for anonymous functions to decide logic that this C++ function will use:

```
> iter2(\x y.x*x+y, 2)
42
```

Variants can be passed between C++ and hobbes as the `hobbes::variant<...>` type:

```C++
typedef hobbes::variant<int, const hobbes::array<char>*> classification;

const classification* classifyParameter(int x) {
  if (x < 42) {
    return hobbes::make<classification>(42 - x);
  } else {
    return hobbes::make<classification>(hobbes::makeString("achievement unlocked"));
  }
}
// [...]
  c.bind("classifyParameter", &classifyParameter);
// [...]
```

These should also print in a standard way (showing the constructor name "0" for the first case and "1" for the second case, since the constructors for this variant have no names), assuming that the payload types of your variant can be printed:

```
> classifyParameter(16)
|0=26|
> classifyParameter(8675309)
|1="achievement unlocked"|
```

In the process of binding application variables and functions to a `hobbes::cc` instance, hobbes will decide how to "lift" every C++ type to an equivalent hobbes type.  Many common cases are already handled transparently, as above where types like `size_t` and `std::pair` and `hobbes::array` are "lifted" without any special work required on our part.  If hobbes doesn't know how to lift a type, it will be lifted as an "opaque C++ type" such that hobbes just checks for consistent usage (and will respect class hierarchies and derived/base conversions as necessary) but will otherwise know nothing about the structure of the type.

For example, consider a binding case like this (meant to represent a complex type hierarchy):

```C++
class Top {
public:
  Top(int x, double y, int z) : x(x), y(y), z(z) { }

  int    getX() const { return this->x; }
  double getY() const { return this->y; }
  int    getZ() const { return this->z; }
private:
  int    x;
  double y;
  int    z;
};

class Left : public virtual Top {
public:
  Left(int x, double y, int z) : Top(x, y, z) { }
};

class Right : public virtual Top {
public:
  Right(int x, double y, int z) : Top(x, y, z) { }
};

class Bottom : public Left, public Right {
public:
  Bottom(int x, double y, int z) : Left(x*z, y*y, z*x), Right(x*x, y*y, z*z), Top(x, y, z) { }
};
// [...]
  c.bind("getX", memberfn(&Top::getX));
  c.bind("getY", memberfn(&Top::getY));
  c.bind("getZ", memberfn(&Top::getZ));

  static Bottom b(1, 3.14159, 42);
  c.bind("b", &b);
// [...]
```

Now back in our shell, we can invoke the bound `Top` methods on our bound `Bottom` instance (hobbes will coordinate with gcc to apply the correct pointer adjustments):

```
> b.getX()
1
> b.getY()
3.14159
> b.getZ()
42
```

If the default logic for lifting your C++ type is insufficient, the `hobbes::lift<T>` type can be specialized to determine a more useful description (the code in `hobbes/lang/tylift.H` has many examples of this; this is where the default set of lift definitions are made).

## Storage <a name="storage"></a>

Rather than using hobbes to "pull" application data with ad-hoc dynamic queries, we can also use hobbes to "push" large volumes of application data to local or remote storage for real-time or historical analysis.  In this case, we don't need a compiler instance but instead just need to define some basic quality-of-service parameters for the storage process.  For example, let's consider a simple mock weather monitor:

```C++
#include <hobbes/storage.H>
#include <stdlib.h>

DEFINE_STORAGE_GROUP(
  WeatherMonitor,              /* an arbitrary name for our data set */
  3000,                        /* our maximum expected throughput in system pages (if we record up to this limit, we either drop or block) */
  hobbes::storage::Unreliable, /* we'd rather drop than block */
  hobbes::storage::AutoCommit  /* we don't need to correlate multiple records in a batch (non-transactional) */
);

// a set of sensor readings
DEFINE_HSTORE_STRUCT(
  Sensor,
  (double, temp),
  (double, humidity)
);

int main() {
  while (true) {
    // record a random sensor reading from somewhere
    const char* locations[] = { "AZ", "CO", "HI" };
    const char* location    = locations[rand() % 3];

    Sensor s;
    s.temp     = 1.0 + 50.0 * ((double)(rand() % 100)) / 100.0;
    s.humidity = ((double)(rand() % 100)) / 100.0;

    HSTORE(WeatherMonitor, sensor, location, s);

    // sometimes record a passing car
    if (rand() % 100 == 0) {
      HSTORE(WeatherMonitor, carPassed, location);
    }
  }
  return 0;
}
```

We first use `DEFINE_STORAGE_GROUP` to decide a name for the set of data we'll record (the name we've chosen will be important later), how many memory pages to allocate for the ring buffer where data is temporarily stored (on most systems a page will be 4KB), whether to wait or discard data when it can't be stored as quickly as we're producing it, and whether we need to correlate data in batches/transactions or not.

Then we use `DEFINE_HSTORE_STRUCT` to define a simple `Sensor` structure.

Finally we run a simple loop, generating and recording mock sensor data.  The `HSTORE` macro, which we use to record data, takes the name of our storage group, an arbitrary name to identify this data, and then any subsequent arguments as data to store.  If our storage group is used in "unreliable" mode (as in the above example), then the `HSTORE` macro will return `false` if the data was dropped due to a consumer falling behind.  Many common types can be stored automatically with `HSTORE` (primitives, `std::string`, `std::vector`, ...) and as with binding to C++ symbols, it is possible to define storage for custom types by specializing the `hobbes::storage::store<T>` type (here the definitions in `hobbes/storage.H` can be instructive).

Now, at the same time that this program runs, we will also want to run another program to consume this data.  The `hog` program is distributed with hobbes for this purpose -- its source code may be instructive if you'd like to understand the storage consumer API or customize its logic in some way.  To get an overview of our recording options with this program, we can run it without arguments:

```
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
$
```

This shows that we can use this program to consume and store data in one of three modes:

1. immediate storage into a local disk file
2. temporary local storage (batched, compressed) pending successful sends to a remote process
3. a network server, storing into local files from remote processes

For our test program above, it will be enough to just store data locally.  We must run the `hog` consumer first before starting the producer (our test program).  If we run `hog` and then start start our test program, we should see `hog` produce output that looks like:

```
$ hog -g WeatherMonitor
[2018-01-01T09:00:00.867323]: hog running in mode : |local={ dir="./", groups={"WeatherMonitor"} }|
[2018-01-01T09:00:00.867536]: polling for creation of memory regions
[2018-01-01T09:00:00.873862]: group 'WeatherMonitor' ready, preparing to consume its data
[2018-01-01T09:00:01.637614]:  ==> carPassed :: ([char]) (#1)
[2018-01-01T09:00:01.733374]:  ==> sensor :: ([char] * { temp:double, humidity:double }) (#0)
[2018-01-01T09:00:01.848340]: finished preparing statements, writing data to './WeatherMonitor/data-2018.01.01-0.log'
```

Now while these two programs (both `hog` and our test program) are left running, we can simultaneously run a third program to load and query this data as it is being recorded.  This is a good use-case for the `hi` program seen previously.  If we enter the directory where `hog` is writing its output files, we can take a look at the data we're recording this way:

```
$ hi
hi : an interactive shell for hobbes
      type ':h' for help on commands

> wm = inputFile :: (LoadFile "./WeatherMonitor/data-2018.01.01-0.log" w) => w
> wm.sensor
AZ {temp=44.572017, humidity=44.582017}
AZ   {temp=3.575808, humidity=3.585808}
HI {temp=39.150116, humidity=39.160116}
...
> wm.carPassed
AZ
AZ
CO
AZ
...
> 
```

Although the syntax for loading this file may appear somewhat unusual (or maybe not, after we've covered qualified types and type classes in more detail), this method of loading data in `hi` should let us look at what we're recording fairly quickly.  If we repeatedly query the same data while it's being written, we should see different results as our view of the data is "live".  It's also possible to "tail" specific data as it is written by registering a function to handle updates:

```
> signals(wm).sensor <- (\_.do { putStrLn("sensor batch updated"); return true })
> sensor batch updated
sensor batch updated
sensor batch updated
sensor batch updated
```

In more complex applications, we might use this ability to accumulate statistics, index recorded data, alert on some condition, etc.

We could also want to record batched or "transactional" data where our storage statements need to be correlated.  For example, consider a mock cheeseburger order management application:

```C++
#include <hobbes/storage.H>
#include <stdlib.h>

DEFINE_STORAGE_GROUP(
  Orders,
  3000,
  hobbes::storage::Reliable,    /* we _must_ track all orders */
  hobbes::storage::ManualCommit /* we need to correlate all events in an order */
);

int main() {
  while (true) {
    // a customer enters the store
    const char* names[] = { "Harv", "Beatrice", "Pat" };
    const char* name    = names[rand() % 3];

    HSTORE(Orders, customerEntered, name);

    // he/she orders a cheeseburger
    const char* products[] = { "BBQ Attack", "Cheese Quake", "Bacon Salad" };
    const char* product    = products[rand() % 3];

    HSTORE(Orders, productOrdered, product);

    // perhaps he/she decides to add a drink
    if (rand() % 5 == 0) {
      const char* drinks[] = { "Soda", "Lemonade", "Water" };
      const char* drink    = drinks[rand() % 3];

      HSTORE(Orders, drinkOrdered, drink);
    }

    // sometimes the customer has a change of heart, else they pay up and go
    if (rand() % 10 == 0) {
      HSTORE(Orders, orderCanceled);
    } else {
      HSTORE(Orders, paymentReceived, 10.0 * ((double)(rand() % 100)) / 100.0);
    }

    // now that the order is finished, end the transaction
    Orders.commit();
  }
  return 0;
}
```

Now that we have indicated that we're recording meaningful transactions, when we run `hog` for this storage group, it will also record a "transactions" sequence along with the sequences it logs for each storage statement:

```
$ hog -g Orders
[2018-01-01T09:00:00.371394]: hog running in mode : |local={ dir="./", groups={"Orders"} }|
[2018-01-01T09:00:00.371633]: polling for creation of memory regions
[2018-01-01T09:00:00.371697]: group 'Orders' ready, preparing to consume its data
[2018-01-01T09:00:01.615395]:  ==> paymentReceived :: (double) (#4)
[2018-01-01T09:00:01.736183]:  ==> orderCanceled :: () (#3)
[2018-01-01T09:00:01.753279]:  ==> drinkOrdered :: ([char]) (#2)
[2018-01-01T09:00:01.838780]:  ==> productOrdered :: ([char]) (#1)
[2018-01-01T09:00:01.880418]:  ==> customerEntered :: ([char]) (#0)
[2018-01-01T09:00:01.927219]:  ==> transactions :: <any of the above>
[2018-01-01T09:00:02.136798]: finished preparing statements, writing data to './Orders/data-2018.01.01-0.log'
```

And if we simultaneously load this file, we can see that all of our storage statements can be queried as well as this new "transactions" data.  Each transaction is stored as a timestamp and an array of a variant over all possible storage statements so that we can tell which statements were recorded and in what order:

```
$ hi
hi : an interactive shell for hobbes
      type ':h' for help on commands

> orders = inputFile :: (LoadFile "./Orders/data-2018.01.01-0.log" w) => w
> orders.transactions
                      time                                                                                                              entries
-------------------------- --------------------------------------------------------------------------------------------------------------------
2018-01-01T12:19:03.244206                                       [|customerEntered=("Pat")|, |productOrdered=("Bacon Salad")|, |orderCanceled|]
2018-01-01T12:19:03.244172                                 [|customerEntered=("Beatrice")|, |productOrdered=("Cheese Quake")|, |orderCanceled|]
2018-01-01T12:19:03.244127    [|customerEntered=("Pat")|, |productOrdered=("Cheese Quake")|, |drinkOrdered=("Water")|, |paymentReceived=(6.4)|]
2018-01-01T12:19:03.244092                               [|customerEntered=("Pat")|, |productOrdered=("Bacon Salad")|, |paymentReceived=(6.4)|]
2018-01-01T12:19:03.244047 [|customerEntered=("Pat")|, |productOrdered=("Cheese Quake")|, |drinkOrdered=("Lemonade")|, |paymentReceived=(2.1)|]
2018-01-01T12:19:03.244013                               [|customerEntered=("Pat")|, |productOrdered=("Bacon Salad")|, |paymentReceived=(7.6)|]
2018-01-01T12:19:03.243978                           [|customerEntered=("Beatrice")|, |productOrdered=("BBQ Attack")|, |paymentReceived=(3.4)|]
2018-01-01T12:19:03.243944                          [|customerEntered=("Beatrice")|, |productOrdered=("Bacon Salad")|, |paymentReceived=(3.5)|]
2018-01-01T12:19:03.243910                             [|customerEntered=("Harv")|, |productOrdered=("Cheese Quake")|, |paymentReceived=(6.3)|]
...
```

Although the content of these transactions appears to possibly be very large, it's actually just a minor addition due to the way that this data is stored.  It can also be queried very efficiently.

## Networking <a name="networking"></a>

Just as we may need a lightweight, efficient method to record structured application data from C++, we may also need such a method to efficiently query plain C++ data from a remote process that understands hobbes.  This is made possible with a macro `DEFINE_NET_CLIENT` that takes a _signature_ for interaction and produces a type to implement it.

We can cut to the chase with a simple test program:

```C++
#include <iostream>
#include <hobbes/net.H>

// our RPC signature
DEFINE_NET_CLIENT(
  Connection,                       // a name for this connection profile
  (add, int(int,int), "\\x y.x+y"), // a function "add" of type int(int,int) remotely implemented by the expression '\x y.x+y'
  (mul, int(int,int), "\\x y.x*y")  // as above but "mul" for the expression '\x y.x*y'
);

int main(int, char**) {
  try {
    Connection c("localhost:8080");

    std::cout << "c.add(1,2) = " << c.add(1,2) << std::endl
              << "c.mul(1,2) = " << c.mul(1,2) << std::endl;

  } catch (std::exception& e) {
    std::cout << "*** " << e.what() << std::endl;
  }
  return 0;
}
```

Here we assume that a process is running at `localhost:8080` to accept our requests.  Once the connection is made, we initialize the remote end with our expected expressions/types (so that we can efficiently invoke them later just by ID).  If any part of this process fails (connection or remote type-checking) then an exception will be raised describing the failure.  Otherwise, execution proceeds in this case just to call both functions and report the results.

We can run a `hi` process to quietly listen on port 8080 like this:

```
$ hi -s -p 8080
>
```

And if we've compiled our test program above to an executable named `test` then we can invoke and run it against our running server to observe results like:

```
$ ./test
c.add(1,2) = 3
c.mul(1,2) = 2
$
```

Just as lightweight storage definitions can be added by specializing `hobbes::storage::store<T>`, also lightweight RPC codecs can be added by specializing `hobbes::net::io<T>` (and many types and type families are supported in `hobbes/net.H`, for example).

This networking/remote-evaluation method can be accessed purely from hobbes as well.  For example, with the previous `hi` server process still running, we can start a new `hi` process to talk to it like this:

```
$ hi -s
> c = connection :: (Connect "localhost:8080" p) => p
>
```

Now we have a connection made _at compile-time_.  We can determine the (current) static structure of this connection with the `printConnection` function, which currently shows this:

```
> printConnection(c)
localhost:8080

id expr input output
-- ---- ----- ------

>
```

Now we can make a basic function to perform some interaction between these processes.  Say we want a function to record a set of data about employees:

```
> sendData = \x.receive(invoke(c, `\x.println(x)`, x :: [{name:[char], age:int, salary:double}]))

> :t sendData
([{ name:[char], age:int, salary:double }]) -> ()
```

Here we see a couple of new functions, `invoke` and `receive` to perform remote evaluation and parse results (respectively).  We haven't invoked `sendData` yet, but because we have kept this connection at compile-time, `hi` was able to communicate with the remote process enough to know what its result type will be (though we've only specified the input type), and if we try `printConnection` again we should see this static protocol detail that's been negotiated:

```
> printConnection(c)
localhost:8080

id                                                       expr                                     input output
-- ---------------------------------------------------------- ----------------------------------------- ------
 0 \(.arg0).(let .t13409.rv0 = .arg0 in println(.t13409.rv0)) [{ name:[char], age:int, salary:double }]     ()
```

This shows that the remote (desugared) expression is known statically, as well as input and output types, and a unique ID has been determined for the expression.  At a later stage ("run-time" or "on the critical path"), to invoke this remote expression our sender program just has to write this value `0` followed by the input data that the remote side expects for this particular expression (but no further "self-describing" data).

This also critically relies on a feature of our type representation in hobbes, that it can carry statically-known expressions.  In this example, we use a "quoted expression" form to carry this expression so that it can be used at compile-time:

```
> :t `\x.println(x)`
(quote \(.arg0).(let .t14200.rv0 = .arg0 in println(.t14200.rv0)))
```

This type is equivalent to `()` at run-time (i.e.: it doesn't exist at run-time), but carrying its quoted expression in this way we can reason about and send/receive expression details at compile-time.

And if we invoke the function we've defined:

```
> sendData([{name="bob", age=40, salary=23.2}, {name="jane", age=51, salary=53.1}])
```

The remote side will print it, as expected:

```
name age salary
---- --- ------
 bob  40   23.2
jane  51   53.1
```

## Comprehensions <a name="comprehensions"></a>

Whether we are running queries directly against application bindings, against out-of-band live/historical data, or as prepared remote queries, we will eventually need to sort, join, group, filter, and transform data.  Traditionally we might think of SQL queries for problems like this, but hobbes (inspired as it is by Haskell) uses [comprehensions](https://en.wikipedia.org/wiki/List_comprehension) and basic functions.

A comprehension has the general form:

```
[E | p <- S, C]
```

Where `E` is a _transform_ or _map_ expression, `S` is an expression producing a sequence (of some type), `p` is a _pattern_ matched against each value in `S` (possibly introducing variables used in `E`), and `C` is a boolean _filter expression_ that decides which values in `S` to select.  The `C` expression is optional and may be omitted.  The pattern `p` is usually just a name for a variable, but can be more complex if necessary.

For example, we might want string representations of numbers evenly divisible by 3, out of the first 20 natural numbers:

```
> [show(x) | x <- [0..20], x % 3 == 0]
["0", "3", "6", "9", "12", "15", "18"]
```

If we've loaded the storage file from our mock weather monitor example earlier, we might want to get readings for AZ:

```
> [p.1 | p <- wm.sensor, p.0 == "AZ"]
temp humidity
---- --------
12.5    12.51
  33    33.01
  16    16.01
  48    48.01
 5.5     5.51
...
```

Alternately, we could use pattern binding both to simplify tuple indexing and to do the equality check (any values that don't match our pattern will be excluded):

```
> [s | ("AZ", s) <- wm.sensor]
temp humidity
---- --------
12.5    12.51
  33    33.01
  16    16.01
  48    48.01
 5.5     5.51
...
```

We can use the function `groupBy` to categorize a sequence, and then use a comprehension with the result to summarize it:

```
> [(k, sum(vs)) | (k, vs) <- groupBy(\x.x % 3, [0..20])]
0 63
1 70
2 77
```

We can use the function `joinBy` to combine two sequences of values on a common key:

```
> joinBy(.name, [{name="bob", score=20}, {name="jim", score=50}], .id, [{id="jim", age=6}, {id="bob", age=42}])
name score  id age
---- ----- --- ---
 bob    20 bob  42
 jim    50 jim   6
```

We can also use a _slice notation_ to select subsequences.  The syntax `S[i:e]` can be used to select values out of `S` starting at `i` and ending at `e`.  Both `i` and `e` are interpreted as indexes in the sequence but can be empty to indicate the end of sequence or negative to indicate an offset from the end of the sequence.  Indexes that are out of bounds will not be included in the result.  If `i` is greater than `e` then the result will be reversed compared to its original order in `S`.  Let's try a few examples:

```
> "foobar"[0:3]
"foo"
> "foobar"[3:0]
"oof"
> "foobar"[-3:]
"bar"
> "foobar"[:-3]
"rab"
> "foobar"[0:]
"foobar"
> "foobar"[:0]
"raboof"
```

We can sort a sequence with the `sort` or `sortWith` functions, assuming that either the sequence type is orderable or we can provide a function on values in the sequence to a type that's orderable.  Values will be given in ascending order, but we can get descending order just by reversing this sequence:

```
> sort([0..10])
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
> sortWith(.age, [{name="jimmy", age=30, state="AZ"}, {name="bob", age=42, state="CO"}, {name="frank", age=37, state="HI"}])
 name age state
----- --- -----
jimmy  30    AZ
frank  37    HI
  bob  42    CO
> sortWith(.age, [{name="jimmy", age=30, state="AZ"}, {name="bob", age=42, state="CO"}, {name="frank", age=37, state="HI"}])[:0]
 name age state
----- --- -----
  bob  42    CO
frank  37    HI
jimmy  30    AZ
```

Together, these methods of consuming sequences can be especially useful to inspect the state of a hobbes-enabled process or to analyze recorded application data.

## Pattern Matching <a name="pat-match"></a>

The hobbes language supports a form of [pattern matching](http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora016.html) for classifying and destructuring data.  The most general form of pattern matching is in the _match expression_, and we will consider how other uses of matching, like in sequence comprehensions, can be rewritten (or "desugared") into this general form.

An important special case of match expressions are C++ `switch` statements, as the C++ code:

```C++
switch (x) {
case 0:  return "foo";
case 1:  return "bar";
case 2:  return "chicken";
default: return "beats me";
}
```

is equivalent to the hobbes match expression (because the pattern `_` matches any value):

```
match x with
| 0 -> "foo"
| 1 -> "bar"
| 2 -> "chicken"
| _ -> "beats me"
```

And in general, for `r` distinct cases, both will efficiently determine a result in `O(log(r))` time (although some expressions, like the one above, can be arranged to run in `O(1)` time).

But match expressions generalize `switch` statements in a few key ways.

First, match expressions allow you to match on multiple values at once (so that for `c` values and `r` cases, a match can be determined in `O(c*log(r))` time -- in the `switch` example we have `c=1` and the expected time complexity).  For example, we could write a match expression like:

```
match x y with
| 0 0 -> "foo"
| 0 1 -> "foobar"
| 1 0 -> "bar"
| 1 1 -> "barbar"
| 2 0 -> "chicken"
| 2 1 -> "chicken bar!"
| _ _ -> "beats me"
```

where we'd otherwise have to write a `switch` statement like:

```C++
switch (x) {
case 0:
  switch (y) {
  case 0:  return "foo";
  case 1:  return "foobar";
  default: return "beats me";
  }
case 1:
  switch (y) {
  case 0:  return "bar";
  case 1:  return "barbar";
  default: return "beats me";
  }
case 2:
  switch (y) {
  case 0:  return "chicken";
  case 1:  return "chicken bar!";
  default: return "beats me";
  }
default:
  return "beats me";
}
```

and although both code fragments will compute the same result in the same amount of time (likely in exactly the same way), the match expression is more concise and so offers some _notational convenience_.

Another way that match expressions generalize `switch` statements is that they allow binding variables to some portion of a matched value.  For example, we might match on a complex variant type like the order transaction entry type from the storage example we looked at earlier:

```
match e with
| |paymentReceived=x| -> "received " ++ show(x)
| _                   -> "something happened"
```

In this case, the pattern `|paymentReceived=x|` checks that the variant in `e` is the `paymentReceived` case and if so, binds its payload to the variable `x` where it can be used subsequently.  This matching and binding process can be nested to any depth, so for example this expression:

```
match [(1,2),(3,4),(5,6)] with
| [_, (3, x), _] -> x
| _              -> -1
```

will evaluate to `4` because the first pattern row checks for an array of length 3 where the second value is a pair where the first value is `3` and the second value we will bind to `x` (given the input then, `x=4`) and then `x` is immediately returned.

As a special case of this, when we match on character arrays, we can use regular expressions with typical binding syntax as a form of pattern as well.  For example, this expression:

```
match "foobar" with
| 'f(?<us>u*)bar' -> us
| 'f(?<os>o*)bar' -> os
| _               -> "???"
```

will evaluate to `"oo"` because it matches the second regular expression, which binds to the variable `os` the sub-sequence of the input matching `o*`.

Finally, another way that match expressions generalize `switch` statements is that cases may contain a _guard expression_ which makes the final determination whether or not to follow a branch.  For example, this expression:

```
match 1 2 with
| 1 x where x > 10 -> 1
| _ _              -> 2
```

will return `2` because, although the input does match the first row at the patterns `1 x`, the _guard expression_ `x > 10` (in this case, `2 > 10`) evaluates to `false` and so the branch for that row is not followed, causing just the final row to match.

In order to be considered valid (and subsequently compiled), match tables must be _exhaustive_ and all rows _reachable_.  To be exhaustive, it just means that there is a matching row for every possible input to the match table (this is why the examples so far always end with a "match any" row like `| _ -> -1`).  For a row to be reachable, it means that no prior row fully subsumes it and so makes it redundant (this check can be disabled with a compiler flag but it is on by default under the assumption that programmers introduce pattern rows because they intend for them to be relevant).  This interpretation of matching is slightly different than in ocaml and Haskell, where e.g. inexhaustive matches may be flagged with a warning and then abort program execution at runtime when some input is given that doesn't match any row.

The fully general form of match expressions may be written:

```
match I0 I1 ... IN with
| p00 p10 ... pN0 where C0 -> E0
| p01 p11 ... pN1 where C1 -> E1
| ...
| p0R p1R ... pNR          -> ER
```

such that each of `I0`, `I1`, ..., `IN` is an expression whose value will be matched, each `pij` is a pattern to match against the value `Ii`, `Cr` is a condition to evaluate against the variables in `p0r` to `pNr`, and each `Er` is a result expression to return iff row `r` is selected (by implication, the type of each such `Er` must be the same).

And a pattern is any term freely generated by the grammar:

```
pattern := constant
        |  variable
        |  [pattern, pattern, ..., pattern]
        |  regex
        |  (pattern, pattern, ..., pattern)
        |  {f0=pattern, f1=pattern, ..., fN=pattern}
        |  |C=pattern|
```

where `constant` is any constant value (`()`, `true`, `42`, `'c'`, etc), `variable` is any name (unique across a row), `f0`, `f1`, ..., `fN` are record field names, and `C` is a variant constructor name.

For convenience, the expression `E matches p` desugars to `match E with | p -> true | _ -> false`.  If the pattern `p` is _irrefutable_ (ie: never fails to match) then `let p = E in B` desugars to `match E with | p -> B`.  If the pattern `p` is irrefutable then `\p.E` desugars to `\x.match x with | p -> E`.  If the pattern `p` is _refutable_ then the expression `\p.E` desugars to `\x.match x with | p -> just(E) | _ -> nothing`.  If the pattern `p` is irrefutable, then `[E | p <- S]` desugars to `map(\p.E, S)`.  If the pattern `p` is refutable, then `[E | p <- S]` desugars to `dropNulls(map(\p.E, S))`.

Pattern matching is very useful in many different kinds of applications.  For some uses of hobbes, it even functions as a kind of "rules engine" (where rows are "rules") such that users can derive very efficient machine code from very large match tables.

## Parsing <a name="parsing"></a>

Although many parsing problems can be solved with match expressions, there are many other common parsing problems for which the approach is insufficient.  In particular, basic expression languages [can't be parsed](https://en.wikipedia.org/wiki/Pumping_lemma_for_regular_languages) by the simple regular expressions supported in match expressions.

For these cases, hobbes defines a syntax for the introduction of parsers based on context-free grammars (technically, we produce [LALR(1) parsers](https://en.wikipedia.org/wiki/LALR_parser)).  As with many such methods (e.g. [yacc](https://en.wikipedia.org/wiki/Yacc), [GNU bison](https://www.gnu.org/software/bison/), [happy](https://www.haskell.org/happy/), ...) parsing rules define both syntax as well as "actions" for producing semantic values out of each rule.

As an example, we can build up to a basic calculator for arithmetic expressions.  Let's start with the simplest first step, a parser for recognizing single digits:

```
$ cat p.hob

calc = parse {
  D := "0" {0} | "1" {1} | "2" {2} | "3" {3} | "4" {4}
    |  "5" {5} | "6" {6} | "7" {7} | "8" {8} | "9" {9}
}

```

Here we have a file with just a single definition (to define the `calc` variable).  With this definition, we use the syntax `parse { RULES }` to construct a parser based on all of the rules in `RULES` and starting from the first rule in the sequence (in this case, the rule for `D`).  Here our syntax terms are simple (just the constant lexical values as in `"0"`, `"4"`, etc) and the actions are also simple (returning `0`, `4`, etc).  These actions can be arbitrary hobbes code, which will be useful as we develop this parser into something more useful.

For now, if we load this file with the `hi` program, we can first check the type of this new definition:

```
$ hi p.hob
hi : an interactive shell for hobbes
      type ':h' for help on commands

loaded module 'p.hob'
> :t calc
Array a char => (a) -> (() + int)
```

This tells us that the prepared parser is actually an overloaded function, taking any type that can be interpreted as an array of characters to a "maybe int" (it's _maybe_ because the parser input might not be recognized).

We can interact with this parser to see its simple behavior:

```
> calc("9")
9
> calc("42")

>
```

Now we can see that a string with a single digit _is_ recognized, but trying to read multi-digit input fails to produce a value.  So we can go back to our file `p.hob` and extend the grammar slightly to accept multiple digits:

```
calc = parse {
  V := v:V d:D { v*10 + d }
    |  d:D     { d }

  D := "0" {0} | "1" {1} | "2" {2} | "3" {3} | "4" {4}
    |  "5" {5} | "6" {6} | "7" {7} | "8" {8} | "9" {9}
}
```

Here we've introduced a few new ideas.  First, the rules for `V` define syntax in terms of the rule `D` (and `V` itself) rather than directly specifying lexical syntax.  Also, the rules for `V` _bind values_ to matched sub-rules (in the variables `v` and `d`) which are available for use in actions for those rules.  Finally, the first rule for `V` uses [left recursion](https://en.wikipedia.org/wiki/Left_recursion) to accept an indefinite-length sequence of lexical digits that will be incrementally translated to a single integer value in the usual way.  We can see this with another interaction:

```
> calc("42")
42
> calc("8675309")
8675309
```

But nonsensical input (still including input involving arithmetic operators) will be rejected:

```
> calc("1+2")

> calc("cheeseburger")

>
```

Finally, we can add support for arithmetic operators (respecting standard order of operations) by the [typical arithmetic grammar](https://en.wikipedia.org/wiki/Syntax_diagram#Example):

```
calc = parse {
  E := x:E "+" y:T { x + y }
    |  x:E "-" y:T { x - y }
    |  x:T         { x }

  T := x:T "*" y:F { x * y }
    |  x:T "/" y:F { x / y }
    |  x:F         { x }

  F := "(" x:E ")" { x }
    |  x:V         { x }

  V := v:V d:D { v*10 + d }
    |  d:D     { d }

  D := "0" {0} | "1" {1} | "2" {2} | "3" {3} | "4" {4}
    |  "5" {5} | "6" {6} | "7" {7} | "8" {8} | "9" {9}
}
```

And we have a parser that can recognize and evaluate simple arithmetic expressions:

```
> calc("9*(7+(8-4*3)/2)-3")
42
```

Not every syntactically valid parser definition will produce a valid parser.  Some parser definitions introduce _ambiguous choice_ (where the parser must choose between two equally valid paths).  In hobbes, these are treated as errors rather than accepting unpredictable performance (all parse times should be `O(n)` for `n` input characters).

For example, consider a parser definition like this:

```
$ cat e.hob

err = parse {
  S := "a" { 1 }
    |  z:Z { z }

  Z := "a" { 2 }
}
```

Here, parsing the `S` definition must be ambiguous for the input `"a"` because we could either choose the first path through `S := "a"` (giving `1` as the result) or else through `z:Z` we could choose `Z := "a"` (giving `2` as the result).  Because this choice is ambiguous, hobbes will reject the parser with an error indicating where its grammar went wrong:

```
$ hi -s e.hob
e.hob:2,7-7,1: Conflict between r0 and r0 on $
S -> 'a'# [$]
Z -> 'a'# [$]
```

In this error message, the symbol `#` is meant to indicate a position in a basic grammar rule (as in [parser items](https://en.wikipedia.org/wiki/Canonical_LR_parser#Parser_items)), and the symbols in brackets indicate the [lookahead](https://en.wikipedia.org/wiki/Parsing#Lookahead) values that allow the parser to conclude that a rule can be completed or _reduced_.  So the error message above is telling us that in the given state, we could either reduce with `S -> 'a'#` or `Z -> 'a'#` on the end-of-input terminal (aka `$`).

## Type Classes <a name="type-classes"></a>

In hobbes, user-defined overloading and ad-hoc type analysis can be defined through the use of [type classes](https://en.wikipedia.org/wiki/Type_class).  Type classes were [originally introduced](https://people.csail.mit.edu/dnj/teaching/6898/papers/wadler88.pdf) in Haskell as a way to combine overloading and type inference.  They can also be viewed as a specialized use of [qualified types](http://web.cecs.pdx.edu/~mpj/pubs/rev-qual-types.pdf), which is also the view taken in hobbes (although the majority of type constraints in hobbes programs so far designate type classes).  To get the intuition for the idea prior to a formal statement, let's consider some simple examples.

One of the simplest type classes to consider is the `Show` class, which has the job of converting a value to a string.  In hobbes, `Show` is defined like this:

```
class Show a where
  show :: a -> [char]
```

This definition can be read as a statement that the constraint `Show a` is true if and only if there is an associated function `show` of type `a -> [char]`.  We introduce these associated functions with `instance` definitions (also as in Haskell), as in for example:

```
instance Show int where
  show = showInt
```

Because there happens to be a function `showInt :: int -> [char]` defined by default with hobbes, it works perfectly as a `show` function for `int` values.  With this definition in place, we can evaluate:

```
> show(42)
"42"
```

But hobbes also supports regular families of types, and although it works well enough for primitive types to define each `Show` instance on a case-by-case basis, it would be very frustrating if we had to do the same thing to show a value of type `[int]` and again to show a value of type `[[int]]` or `[[[int]]]` etc.  We can use _instance generators_ where there is a regular rule to decide how to construct a type class instance for a set of types.  In the case of arrays, for example, we can introduce an instance generator for `Show` like this:

```
instance (Show a) => Show [a] where
  show xs = "[" ++ cdelim(map(show, xs), ", ") ++ "]"
```

This definition appears almost identical to a regular `instance` definition as before, but it has a constraint to the left of the `=>` as in a qualified type.  In this case, a good way to read `(Show a) => Show [a]` is "if it's true that there's a `Show a` instance, then we can make a `Show [a]` instance".  Then we just introduce a definition of `show` that can work for any such value of type `[a]`.  Because there happens to be a function `cdelim :: ([[char]]*[char]) -> [char]` around to concatenate an array of strings interleaved with another string, it's pretty easy to write this function so that we show every value in the input array and then join them together into one larger string.

Experimenting at the prompt, we can see that multiple levels of array nesting will uniformly apply this definition:

```
> :t [[[1,2],[3,4]], [[5,6],[7,8]]]
[[[int]]]
> show([[[1,2],[3,4]], [[5,6],[7,8]]])
"[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]"

> :t [[1,2,3,4], [5,6,7,8]]
[[int]]
> show([[1,2,3,4], [5,6,7,8]])
"[[1, 2, 3, 4], [5, 6, 7, 8]]"

> :t [1,2,3,4,5,6,7,8]
[int]
> show([1,2,3,4,5,6,7,8])
"[1, 2, 3, 4, 5, 6, 7, 8]"
```

We can further expand the set of types in `Show` if we consider tuples in a similar way to arrays.  In this case, tuples are intentionally designed to be heterogeneously-typed unlike arrays, so we don't have a function as simple as `map` to evaluate across them.  However, we can make a new type class to fill a similar role:

```
class ShowTuple p where
  showTuple :: p -> [[char]]
```

And we can use a special constraint `p=(h*t)` to deconstruct the tuple within an instance generator (where we bottom out in the unit type `()` when the tuple is fully considered):

```
instance (p=(h*t), Show h, ShowTuple t) => ShowTuple p where
  showTuple p = [show(p.0)] ++ showTuple(tupleTail(p))
instance ShowTuple () where
  showTuple _ = []
```

Now with these definitions, we can determine the same intermediate string array that we determined for arrays.  All that we need to bring this into `Show` is to use the same `cdelim` method:

```
instance (ShowTuple p) => Show p where
  show p = "(" ++ cdelim(showTuple(p), ", ") ++ ")"
```

And running this at the prompt, together with the prior definitions for `Show`, we can show even more complex types:

```
> show(42)
"42"
> show((42,[1,2,3]))
"(42, [1, 2, 3])"
> show((42,[1,2,3],[(4,5),(6,7)]))
"(42, [1, 2, 3], [(4, 5), (6, 7)])"
```

So far we have considered `Show` as an example of a typical type class, but sometimes we need to consider multiple types in combination.  For example, we might try a simple type class for overloaded addition:

```
class Addable a b c where
  (+) :: (a,b) -> c
```

And assuming that we have a function `iadd :: (int,int) -> int` for the primitive CPU instruction for addition of integers, we might try introducing the instance:

```
instance Addable int int int where
  (+) = iadd
```

And this would certainly work, but if we try to use this at the prompt we should get an error:

```
> 1 + 2
stdin:1,1-5: Failed to compile expression due to unresolved type constraints:
  Addable int int a
  Print a
>
```

The problem here is that hobbes wasn't able to infer a result type.  If we explicitly annotate the result type it will work:

```
> (1+2)::int
3
> (3+4)::int
7
```

However, this is an extremely inconvenient restriction and could easily become even more absurd for more complex types or multiple successive sums.  In short, we need some way to communicate to type inference and instance selection that although the result type of addition may vary, it is uniquely determined by the first two types.  Currently in hobbes, the way to address this problem is with [functional dependencies](http://web.cecs.pdx.edu/~mpj/pubs/fundeps-esop2000.pdf).  In short, when we define our type class we include with it the fact that the third type is _uniquely determined_ by the first two:

```
class Addable a b c | a b -> c where
  (+) :: (a,b) -> c
```

With this change, we no longer need to introduce an explicit type annotation for the result -- instance selection will search just based on the first two types and then will communicate back to type inference that `c=int` for the single instance that it finds.

More examples and applications of type classes can be found in the `hobbes/boot/` scripts, which are loaded whenever a `hobbes::cc` instance is constructed in C++ or when the `hi` program is initially run.

An important distinction between the interpretation of type classes in hobbes and in Haskell is that hobbes does not use the common [dictionary-passing transform](http://okmij.org/ftp/Computation/typeclass.html#dict) for overloaded identifiers.  Instead, hobbes requires that any type constraint must also come with some logic that, for any _satisfied_ constraint, can _rewrite_ an expression involving that constraint into an equivalent expression without it.  For example, at the `hi` prompt with our `Show` example, we can use the `:u` command to show the result of this process just on the expression level (here with obvious type annotations removed):

```
> :u show(42)
showInt(42)
```

This distinction is important because is allows hobbes to derive more efficient machine code than if dictionaries of indirect functions were passed around (though the approach is well known and has its drawbacks as well).

## Unqualifier Modules <a name="unqualifiers"></a>

The C++ interface to hobbes supports the introduction of custom "unqualifiers" (and the system of type classes described earlier is just one such "unqualifier" implementation).  The standard set of unqualifiers is defined in `hobbes/lang/preds/` but it's possible to introduce your own by implementing the `Unqualifier` interface, which allows all of the mechanisms described so far.  This definition is located in `hobbes/lang/tyunqualify.H`:

```C++
struct Unqualifier {
  // bind any implied type variables in a constraint
  virtual bool refine(const TEnvPtr& tenv, const ConstraintPtr& cst, MonoTypeUnifier* u, Definitions* ds) = 0;

  // determine whether or not this constraint is satisfied
  virtual bool satisfied(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const = 0;

  // determine whether or not it's possible to satisfy this constraint
  virtual bool satisfiable(const TEnvPtr& tenv, const ConstraintPtr& cst, Definitions* ds) const = 0;

  // why couldn't this constraint be eliminated?
  virtual void explain(const TEnvPtr&, const ConstraintPtr&, const ExprPtr&, Definitions*, annmsgs*) = 0;

  // resolve a qualified expression
  virtual ExprPtr unqualify(const TEnvPtr& tenv, const ConstraintPtr& cst, const ExprPtr& e, Definitions* ds) const = 0;

  // allow overloaded symbols (as in type classes)
  virtual PolyTypePtr lookup(const std::string& vn) const = 0;
  
  // list overloaded symbols (if any)
  virtual SymSet bindings() const = 0;

  // list functional dependencies between constraint parameters (if any)
  virtual FunDeps dependencies(const ConstraintPtr&) const = 0;
};
```

The key functions to implement for your own constraint unqualifiers here are:

* `refine`, which can infer parts of a constraint from other parts
* `satisfied`, which decides whether a constraint is fully satisfied
* `unqualify`, which rewrites an expression with a satisfied constraint into an equivalent expression without that constraint

It is usually not necessary to introduce new unqualifiers this way, but it can be a very useful tool when all other options are insufficient.  Once you've defined an implementation of this interface for your own compiler module, you can install it with `hobbes::cc::typeEnv()->bind("ConstraintName", UnqualifierPtr(new P()))` assuming that your type is `P` and you want it to resolve constraints named `"ConstraintName"`.

Why might you decide to implement your own unqualifier module?  Ultimately this will come down to the particulars of your use-case, but in general this is a good option if you need to configure an external resource at an initial stage in order to query it arbitrarily at a subsequent stage.

For example, we've seen earlier that hobbes has a `LoadFile` constraint that takes a compile-time string (the path to a structured data file) and a type variable.  The `Unqualifier` implementation that handles this constraint (in `hobbes/db/bindings.C`) will load the file _at compile-time_, determine its type structure, and will `refine` (or unify) the constraint's second argument to this type structure to allow subsequent type checking and compilation against the file's contents.  There's also an `AddDBFieldSignal` constraint defined in `hobbes/db/signals.C` to handle the previously-mentioned installation of callbacks or "tails" to react to updates in structured file data (eventually reducing to an interaction between `epoll` and `inotify` on Linux).  The addition of these unqualifier modules together allows a kind of flexible "dynamic, duck-typing" scripting against recorded data without giving up type checking or compilation of efficient code to react/query recorded data.

Another example we've seen earlier is the hobbes `Connect` constraint.  This also takes a compile-time string and produces a unique type representing the connection made at compile-time.  This is used further (and with other supporting constraints/unqualifiers) to negotiate a protocol and derive efficient code to communicate with a remote process.

These are just some examples of extensions to hobbes through the `Unqualifier` interface, but there are undoubtedly many other ways that this option can be useful to applications using hobbes.


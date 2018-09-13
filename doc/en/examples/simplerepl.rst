A Simple Hobbes REPL
********************

In the Embedding Hobbes chapter we built a simple Hobbes interpreter with two-way function bindings.

This allowed us to call C++ functions from within the Hobbes environment, and also, via the function pointer, to execute Hobbes code in the containing C++ code. 

.. code-block:: c++

  #include <iostream>
  #include <stdexcept>
  #include <hobbes/hobbes.H>

  int addTwo(int i){ return 2 + i; }

  DEFINE_STRUCT(Writer,
    (size_t, age),
    (const hobbes::array<char>*, name)
  );

  typedef std::pair<int, const hobbes::array<char>*> Writer1;

  Writer1* getWriter(){
    return hobbes::make<Writer1>(34, hobbes::makeString("Sam"));
  }

  hobbes::array<Writer>* getWriters(){
    auto writers = hobbes::makeArray<Writer>(4);

    writers->data[0].age = 21;
    writers->data[0].name = hobbes::makeString("John");

    writers->data[1].age = 22;
    writers->data[1].name = hobbes::makeString("Paul");

    writers->data[2].age = 22;
    writers->data[2].name = hobbes::makeString("George");

    writers->data[3].age = 42;
    writers->data[3].name = hobbes::makeString("Sam");

    return writers;
  }

  typedef hobbes::variant<int, const hobbes::array<char>*> CountOrMessage;

  CountOrMessage* classify(int i){
    if(i < 22){
      return hobbes::make<CountOrMessage>(i);
    }else{
      return hobbes::make<CountOrMessage>(hobbes::makeString("haha!"));
    }
  }

  int binaryIntFn(int (*pf)(int, int), int x){
    return pf(x, x);
  }

  int main() {
    hobbes::cc c;

    c.bind("addTwo", &addTwo);
    c.bind("getWriters", &getWriters);
    c.bind("getWriter", &getWriter);
    c.bind("classify", &classify);
    c.bind("binaryIntFn", &binaryIntFn);

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

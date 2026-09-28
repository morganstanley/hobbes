
#ifndef HOBBES_EVAL_HLOCK_HPP_INCLUDED
#define HOBBES_EVAL_HLOCK_HPP_INCLUDED

namespace hobbes {

// protect access to hobbes/llvm resources between threads
//
// This is one process-wide recursive lock. Every compiler takes it for the
// whole of each compile, and compacting the type memo (compactMTypeMemory,
// lang/type.C) takes it too: type inference reads through plain pointers and
// references into types it holds no reference to, which only the memo keeps
// alive, so the memo must not be compacted while any thread is compiling.
class hlock {
public:
  hlock();
  ~hlock();
};
class phlock {
public:
  phlock();
  void release();
  ~phlock();

private:
  bool f;
};

}

#endif

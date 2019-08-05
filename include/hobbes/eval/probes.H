
#ifndef HOBBES_EVAL_PROBES_HPP_INCLUDED
#define HOBBES_EVAL_PROBES_HPP_INCLUDED

#include <cstdint>
#include <string>

namespace hobbes {

class cc;

void initProbeDefs(cc&);

struct InjectedProbeMetadata {
  size_t id;
  std::string name;
  std::string file;
  size_t line;
  std::string function;
  void* callSite;
  void* returnSite;

  static constexpr uint64_t typeTag = static_cast<uint64_t>(0x01) << 56;
  static constexpr size_t caveSize = 5;
};

}

#endif

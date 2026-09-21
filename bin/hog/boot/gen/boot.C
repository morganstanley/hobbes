#include <hobbes/read/parser.H>
#include <hobbes/eval/cmodule.H>
#include <hobbes/eval/cc.H>

namespace hog {

#include <hog/boot/gen/bootdata.H>

void compileBootCode(hobbes::cc& ctx) {
  for (size_t i = 0; module_defs[i] != nullptr; ++i) {
    hobbes::compile(&ctx, ctx.readModule(std::string(reinterpret_cast<const char*>(module_defs[i]), module_lens[i])));
  }
}

}

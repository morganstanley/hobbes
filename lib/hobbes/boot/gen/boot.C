#include <hobbes/read/parser.H>
#include <hobbes/eval/cmodule.H>
#include <hobbes/eval/cc.H>

namespace hobbes {

#include <hobbes/boot/gen/bootdata.H>

void compileBootCode(cc& ctx) {
  size_t i = 0;
  for (size_t i = 0; module_defs[i] != 0; ++i) {
    compile(&ctx, ctx.readModule(std::string((const char*)module_defs[i], module_lens[i])));
  }
}

}

#include <hobbes/read/parser.H>
#include <hobbes/eval/cmodule.H>
#include <hobbes/eval/cc.H>

namespace hobbes {

#include <hobbes/boot/gen/bootdata.H>

void compileBootCode(cc& ctx) {
  compile(&ctx, ctx.readModule(std::string((const char*)___bootdata, ___bootdata_len)));
}

}

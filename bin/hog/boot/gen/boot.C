#include <hobbes/read/parser.H>
#include <hobbes/eval/cmodule.H>
#include <hobbes/eval/cc.H>

namespace hog {

#include "bootdata.H"

void compileBootCode(hobbes::cc& ctx) {
  hobbes::compile(&ctx, ctx.readModule(std::string(reinterpret_cast<const char*>(___bootdata), ___bootdata_len)));
}

}

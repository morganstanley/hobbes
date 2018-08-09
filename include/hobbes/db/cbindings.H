
#ifndef HOBBES_DB_CBINDINGS_HPP_INCLUDED
#define HOBBES_DB_CBINDINGS_HPP_INCLUDED

#include <hobbes/eval/cc.H>

#include <hobbes/lang/tyunqualify.H>
#include <hobbes/lang/preds/hasfield.H>

namespace hobbes {

void initCStorageFileDefs(FieldVerifier*, cc&);

}

#endif


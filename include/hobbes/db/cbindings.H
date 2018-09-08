
#ifndef HOBBES_DB_CBINDINGS_HPP_INCLUDED
#define HOBBES_DB_CBINDINGS_HPP_INCLUDED

#include <hobbes/eval/cc.H>

#include <hobbes/lang/tyunqualify.H>
#include <hobbes/lang/preds/hasfield.H>
#include <hobbes/db/file.H>
#include <hobbes/cfregion.H>

namespace hobbes {

class DynDModel0;

class UCWriter {
public:
  UCWriter(writer*, size_t root, size_t batchSize, size_t modelSize);
  void write(DynDModel0* dm, uint8_t c);
  bool step();
  size_t currentInitModel() const;
  uint8_t* currentInitModelData() const;
private:
  size_t   batchSize;
  size_t   modelSize;

  uint64_t scratchModelRef;
  uint8_t* scratchModel;

  fregion::cwbitstream out;
};


void initCStorageFileDefs(FieldVerifier*, cc&);

}

#endif


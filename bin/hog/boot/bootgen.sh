#!/bin/bash

# Regenerate gen/bootdata.H from the *.hob boot sources. The array symbol name
# that xxd emits is derived from the input filename, and boot.C expects
# '_bootdata' / '_bootdata_len', so the staging file must be named '_bootdata'.
cat *.hob > _bootdata
xxd -i _bootdata > gen/bootdata.H
rm _bootdata

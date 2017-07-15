#!/bin/bash

cd `dirname ${BASH_SOURCE[0]}`

GENFILE=../../../include/hobbes/boot/gen/bootdata.H

rm -f $GENFILE

for script in `ls ./*.hob`
do
  xxd -i $script >> $GENFILE
done

echo "unsigned char* module_defs[] = {" >> $GENFILE
for script in `ls ./*.hob`
do
  echo "$script," | sed 's/\./_/g' | sed 's/\//_/g' >> $GENFILE
done
echo "0 };" >> $GENFILE

echo "unsigned int module_lens[] = {" >> $GENFILE
for script in `ls ./*.hob`
do
  echo "$script""_len," | sed 's/\./_/g' | sed 's/\//_/g' >> $GENFILE
done
echo "0 };" >> $GENFILE


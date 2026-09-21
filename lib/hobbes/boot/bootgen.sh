#!/usr/bin/env bash

cd `dirname ${BASH_SOURCE[0]}`

GENFILE=../../../include/hobbes/boot/gen/bootdata.H

rm -f $GENFILE

for script in `ls ./*.hob`
do
  xxd -i $script | sed 's/ __/ _/' >> $GENFILE
done

echo "unsigned char* module_defs[] = {" >> $GENFILE
for script in `ls *.hob`
do
  echo _"$script," | sed 's/\./_/g' >> $GENFILE
done
echo "nullptr };" >> $GENFILE

echo "unsigned int module_lens[] = {" >> $GENFILE
for script in `ls *.hob`
do
  echo _"$script""_len," | sed 's/\./_/g' >> $GENFILE
done
echo "0u };" >> $GENFILE


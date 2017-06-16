#!/bin/bash

cd `dirname ${BASH_SOURCE[0]}`

cat ./*.hob > ./.bootdata
xxd -i ./.bootdata > ./gen/bootdata.H
rm ./.bootdata


#!/bin/bash

cat *.hob > ./.bootdata
xxd -i ./.bootdata > gen/bootdata.H
rm ./.bootdata


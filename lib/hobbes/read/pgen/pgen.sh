#!/bin/bash

cd `dirname ${BASH_SOURCE[0]}`

# generate the LALR(1) parser and token definitions
${BISON:-bison} -d -ohexpr.parse.C hexpr.y

# obey internal convention for division of source and header files
mv hexpr.parse.H ../../../../include/hobbes/read/pgen/

# generate the lexer to tokenize string input
${LEX:-flex} -ohexpr.lex.C hexpr.l

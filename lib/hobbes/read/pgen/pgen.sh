#!/bin/bash

#modulecmd bash load fsf/bison/3.0.2  # <-- won't module load
modulecmd bash load fsf/flex/2.5.4
modulecmd bash load perl5/core/5.14

cd `dirname ${BASH_SOURCE[0]}`

# generate the LALR(1) parser and token definitions
/ms/dist/fsf/PROJ/bison/3.0.2/bin/bison -d -ohexpr.parse.C hexpr.y

# generate the lexer to tokenize string input
flex -ohexpr.lex.C hexpr.l


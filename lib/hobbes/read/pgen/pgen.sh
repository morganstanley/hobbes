#!/bin/bash

cd `dirname ${BASH_SOURCE[0]}`

# generate the LALR(1) parser and token definitions
bison -d -ohexpr.parse.C hexpr.y

# generate the lexer to tokenize string input
flex -ohexpr.lex.C hexpr.l


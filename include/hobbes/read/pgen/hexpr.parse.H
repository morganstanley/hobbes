/* A Bison parser, made by GNU Bison 3.3.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2019 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_HEXPR_PARSE_H_INCLUDED
# define YY_YY_HEXPR_PARSE_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    TPARSEMODULE = 258,
    TPARSEDEFN = 259,
    TPARSEEXPR = 260,
    TOPTION = 261,
    TMODULE = 262,
    TWHERE = 263,
    TIMPORT = 264,
    TTYPE = 265,
    TDATA = 266,
    TCLASS = 267,
    TINST = 268,
    TINDENT = 269,
    TBOOL = 270,
    TCHAR = 271,
    TBYTE = 272,
    TBYTES = 273,
    TSHORT = 274,
    TINT = 275,
    TLONG = 276,
    TINT128 = 277,
    TFLOAT = 278,
    TDOUBLE = 279,
    TIDENT = 280,
    TSTRING = 281,
    TREGEX = 282,
    TTIMEINTERVAL = 283,
    TTIME = 284,
    TDATETIME = 285,
    TTUPSECTION = 286,
    TCSTARROW = 287,
    TARROW = 288,
    TCOLON = 289,
    TEXISTS = 290,
    TASSIGN = 291,
    TPARROW = 292,
    TEQUALS = 293,
    TASSUMP = 294,
    TAPPEND = 295,
    TPLUS = 296,
    TMINUS = 297,
    TTIMES = 298,
    TDIVIDE = 299,
    TREM = 300,
    TDOT = 301,
    TEQUIV = 302,
    TEQ = 303,
    TCIEQ = 304,
    TNEQ = 305,
    TLT = 306,
    TLTE = 307,
    TGT = 308,
    TGTE = 309,
    TNOT = 310,
    TLET = 311,
    TCASE = 312,
    TDEFAULT = 313,
    TMATCH = 314,
    TMATCHES = 315,
    TPARSE = 316,
    TWITH = 317,
    TOF = 318,
    TAND = 319,
    TOR = 320,
    TIF = 321,
    TTHEN = 322,
    TELSE = 323,
    TIN = 324,
    TPACK = 325,
    TUNPACK = 326,
    TDO = 327,
    TRETURN = 328,
    TLPAREN = 329,
    TRPAREN = 330,
    TLBRACKET = 331,
    TRBRACKET = 332,
    TLBRACE = 333,
    TRBRACE = 334,
    TBAR = 335,
    TCOMMA = 336,
    TSEMICOLON = 337,
    TFN = 338,
    TFNL = 339,
    TCOMPOSE = 340,
    TUPTO = 341,
    TCARET = 342,
    TAT = 343,
    TDOLLAR = 344,
    TQUESTION = 345,
    TSQUOTE = 346,
    TEQUOTE = 347
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 262 "hexpr.y" /* yacc.c:1927  */

  hobbes::Module*              module;
  hobbes::ModuleDefs*          mdefs;
  hobbes::ModuleDef*           mdef;
  hobbes::CFunDepDef*          fundep;
  hobbes::CFunDepDefs*         fundeps;
  hobbes::MVarTypeDefs*        mvtydefs;
  hobbes::MVarTypeDef*         mvtydef;
  hobbes::MVarDefs*            mvdefs;
  hobbes::MVarDef*             mvdef;
  hobbes::str::seq*            strings;

  hobbes::Expr*                exp;
  hobbes::Exprs*               exps;
  hobbes::MkRecord::FieldDefs* rfields;
  hobbes::Case::Bindings*      vfields;
  hobbes::Case::Binding*       vbind;

  hobbes::PatternRows*         patternexps;
  hobbes::PatternRow*          patternexp;
  hobbes::Patterns*            patterns;
  hobbes::Pattern*             pattern;
  hobbes::CSelection*          cselection;
  hobbes::CSelections*         cselections;
  hobbes::MatchRecord::Fields* recpatfields;
  hobbes::MatchRecord::Field*  recpatfield;

  typedef std::pair<hobbes::PatternPtr, hobbes::ExprPtr> LetBinding;
  typedef std::vector<LetBinding>                        LetBindings;

  LetBinding*                             letbinding;
  LetBindings*                            letbindings;

  hobbes::QualType*            qualtype;
  hobbes::Constraints*         tconstraints;
  hobbes::Constraint*          tconstraint;
  const hobbes::MonoTypePtr*   mtype;
  hobbes::MonoTypes*           mtypes;
  hobbes::Record::Members*     mreclist;
  hobbes::Variant::Members*    mvarlist;

  std::string*                            string;
  bool                                    boolv;
  char                                    charv;
  short                                   shortv;
  int                                     intv;
  long                                    longv;
  __int128                                int128v;
  float                                   floatv;
  double                                  doublev;

  hobbes::Grammar*             prules;
  hobbes::Grammar::value_type* prule;
  hobbes::GrammarRules*        prdefs;
  hobbes::GrammarRule*         prdef;
  hobbes::BoundGrammarValues*  pbelems;
  hobbes::BoundGrammarValue*   pbelem;
  hobbes::GrammarValue*        pvalue;

#line 211 "hexpr.parse.H" /* yacc.c:1927  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE yylval;
extern YYLTYPE yylloc;
int yyparse (void);

#endif /* !YY_YY_HEXPR_PARSE_H_INCLUDED  */

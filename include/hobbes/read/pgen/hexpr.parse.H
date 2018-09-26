/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

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
    TMODULE = 261,
    TWHERE = 262,
    TIMPORT = 263,
    TTYPE = 264,
    TDATA = 265,
    TCLASS = 266,
    TINST = 267,
    TINDENT = 268,
    TBOOL = 269,
    TCHAR = 270,
    TBYTE = 271,
    TBYTES = 272,
    TSHORT = 273,
    TINT = 274,
    TLONG = 275,
    TINT128 = 276,
    TFLOAT = 277,
    TDOUBLE = 278,
    TIDENT = 279,
    TSTRING = 280,
    TREGEX = 281,
    TTIMEINTERVAL = 282,
    TTIME = 283,
    TDATETIME = 284,
    TTUPSECTION = 285,
    TCSTARROW = 286,
    TARROW = 287,
    TCOLON = 288,
    TEXISTS = 289,
    TASSIGN = 290,
    TPARROW = 291,
    TEQUALS = 292,
    TASSUMP = 293,
    TAPPEND = 294,
    TPLUS = 295,
    TMINUS = 296,
    TTIMES = 297,
    TDIVIDE = 298,
    TREM = 299,
    TDOT = 300,
    TEQUIV = 301,
    TEQ = 302,
    TCIEQ = 303,
    TNEQ = 304,
    TLT = 305,
    TLTE = 306,
    TGT = 307,
    TGTE = 308,
    TNOT = 309,
    TLET = 310,
    TCASE = 311,
    TDEFAULT = 312,
    TMATCH = 313,
    TMATCHES = 314,
    TPARSE = 315,
    TWITH = 316,
    TOF = 317,
    TAND = 318,
    TOR = 319,
    TIF = 320,
    TTHEN = 321,
    TELSE = 322,
    TIN = 323,
    TPACK = 324,
    TUNPACK = 325,
    TDO = 326,
    TRETURN = 327,
    TLPAREN = 328,
    TRPAREN = 329,
    TLBRACKET = 330,
    TRBRACKET = 331,
    TLBRACE = 332,
    TRBRACE = 333,
    TBAR = 334,
    TCOMMA = 335,
    TSEMICOLON = 336,
    TFN = 337,
    TFNL = 338,
    TCOMPOSE = 339,
    TUPTO = 340,
    TCARET = 341,
    TAT = 342,
    TDOLLAR = 343,
    TQUESTION = 344,
    TSQUOTE = 345,
    TEQUOTE = 346
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 274 "hexpr.y" /* yacc.c:1909  */

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

#line 203 "hexpr.parse.H" /* yacc.c:1909  */
};
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

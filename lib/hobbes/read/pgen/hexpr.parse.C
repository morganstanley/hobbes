/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"
#pragma GCC diagnostic ignored "-Wunused-label"
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wall"

#ifdef __clang__
#pragma clang diagnostic ignored "-Wdeprecated-register"
#pragma clang diagnostic ignored "-Wold-style-cast"
#pragma clang diagnostic ignored "-Wunused-label"
#pragma clang diagnostic ignored "-Wunused-function"
#pragma clang diagnostic ignored "-Wall"
#endif

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "hexpr.y" /* yacc.c:339  */

typedef struct YYLTYPE {
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
#define YYLTYPE_IS_DECLARED 1

#define YYLLOC_DEFAULT(L, R, N) \
  if (N) { \
    (L).first_line   = YYRHSLOC(R, 1).first_line; \
    (L).first_column = YYRHSLOC(R, 1).first_column; \
    (L).last_line    = YYRHSLOC(R, N).last_line; \
    (L).last_column  = YYRHSLOC(R, N).last_column; \
  } else { \
    (L).first_line = (L).last_line = YYRHSLOC(R, 0).last_line; \
    (L).first_column = (L).last_column = YYRHSLOC(R, 0).last_column; \
  }
#line 22 "hexpr.y" /* yacc.c:339  */

#include <hobbes/lang/module.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/pat/pattern.H>
#include <hobbes/lang/type.H>
#include <hobbes/lang/typepreds.H>
#include <hobbes/db/bindings.H>
#include <hobbes/util/autorelease.H>
#include <hobbes/util/str.H>
#include <hobbes/util/array.H>
#include <hobbes/parse/grammar.H>
#include <hobbes/parse/lalr.H>
#include <hobbes/read/pgen/hexpr.parse.H>
#include <string>
#include <stdexcept>
#include <vector>
#include <stdio.h>

using namespace hobbes;

typedef std::pair<PatternPtr, ExprPtr> LetBinding;
typedef std::vector<LetBinding>        LetBindings;

cc*         yyParseCC;
Module*     yyParsedModule = 0;
std::string yyParsedVar;
Expr*       yyParsedExpr   = 0;
std::string yyMatchDiagram;
std::string yyModulePath;
YYLTYPE     yyErrPos;

extern std::string yyVexpLexError;

extern void wantIndent(bool f);

extern int yylex();
void yyerror(const char* s) {
  yyVexpLexError = std::string(s);
  yyErrPos       = yylloc;
}

LexicalAnnotation m(const YYLTYPE& p) {
  return LexicallyAnnotated::make(Pos(p.first_line, p.first_column), Pos(p.last_line, p.last_column));
}

LexicalAnnotation m(const YYLTYPE& p0, const YYLTYPE& p1) {
  return LexicallyAnnotated::make(Pos(p0.first_line, p0.first_column), Pos(p1.last_line, p1.last_column));
}

#define TAPP0(fn,la)          new App(fn, list<ExprPtr>(), la)
#define TAPP1(fn,x0,la)       new App(fn, list(ExprPtr(x0)), la)
#define TAPP2(fn,x0,x1,la)    new App(fn, list(ExprPtr(x0),ExprPtr(x1)), la)
#define TAPP3(fn,x0,x1,x2,la) new App(fn, list(ExprPtr(x0),ExprPtr(x1),ExprPtr(x2)), la)

Expr* pickNestedExp(Exprs* exprs, const LexicalAnnotation& la) {
  if (exprs->size() == 0) {
    return new Unit(la);
  } else if (exprs->size() == 1) {
    return (*exprs)[0]->clone();
  } else {
    MkRecord::FieldDefs fds;
    for (size_t i = 0; i < exprs->size(); ++i) {
      fds.push_back(MkRecord::FieldDef(".f" + str::from(i), (*exprs)[i]));
    }
    return new MkRecord(fds, la);
  }
}

Pattern* pickNestedPat(Patterns* pats, const LexicalAnnotation& la) {
  if (pats->size() == 0) {
    return new MatchLiteral(PrimitivePtr(new Unit(la)), la);
  } else {
    MatchRecord::Fields fds;
    for (size_t i = 0; i < pats->size(); ++i) {
      fds.push_back(MatchRecord::Field(".f" + str::from(i), (*pats)[i]));
    }
    return new MatchRecord(fds, la);
  }
}

PatternRows normPatternRules(PatternRows rs, const LexicalAnnotation& la) {
  if (rs.size() > 0 && !rs.back().result) {
    throw annotated_error(la, "match table can't end with fall-through row");
  }

  for (size_t i = rs.size(); i > 0; --i) {
    if (!rs[i-1].result) {
      rs[i-1].result = rs[i].result;
    }
  }
  return rs;
}

Expr* compileNestedLetMatch(const LetBindings& bs, const ExprPtr& e, const LexicalAnnotation& la) {
  ExprPtr r = e;
  for (LetBindings::const_reverse_iterator b = bs.rbegin(); b != bs.rend(); ++b) {
    r = compileMatch(yyParseCC, list(b->second), list(PatternRow(list(b->first), r)), la);
  }
  return r->clone();
}

ExprPtr irpatFunc(const PatternPtr& pat, const ExprPtr& body, const LexicalAnnotation& la) {
  return fn(str::strings(".arg"), compileMatch(yyParseCC, list(var(".arg", la)), list(PatternRow(list(pat), body)), la), la);
}

ExprPtr rpatFunc(const PatternPtr& pat, const ExprPtr& cond, const ExprPtr& body, const LexicalAnnotation& la) {
  MonoTypePtr rty   = freshTypeVar();
  MonoTypePtr mrty  = sumtype(primty("unit"), rty);
  ExprPtr     fbody = assume(ExprPtr(new MkVariant(".f0", mktunit(la), la)), mrty, la);
  ExprPtr     sbody = assume(ExprPtr(new MkVariant(".f1", body, la)), mrty, la);
  PatternRow  pr    = cond ? PatternRow(list(pat), cond, sbody) : PatternRow(list(pat), sbody);

  return fn(str::strings(".arg"), compileMatch(yyParseCC, list(var(".arg", la)), list(pr, PatternRow(list(PatternPtr(new MatchAny("_", la))), fbody)), la), la);
}

Expr* compileArrayComprehension(const ExprPtr& body, const PatternPtr& pat, const ExprPtr& arr, const LexicalAnnotation& la) {
  if (refutable(pat)) {
    return new App(var("ffilterMMap", la), list(rpatFunc(pat, ExprPtr(), body, la), arr), la);
  } else {
    return new App(var("fmap", la), list(irpatFunc(pat, body, la), arr), la);
  }
}

Expr* compileArrayComprehension(const ExprPtr& body, const PatternPtr& pat, const ExprPtr& arr, const ExprPtr& cond, const LexicalAnnotation& la) {
  if (refutable(pat)) {
    return new App(var("ffilterMMap", la), list(rpatFunc(pat, cond, body, la), arr), la);
  } else {
    return new App(var("ffilterMap", la), list(irpatFunc(pat, cond, la), irpatFunc(pat, body, la), arr), la);
  }
}

Expr* makeRefutablePatternFn(const Patterns& ps, const ExprPtr& e, const LexicalAnnotation& la) {
  str::seq vns;
  Exprs    vnes;
  Patterns els;
  for (size_t i = 0; i < ps.size(); ++i) {
    std::string vname = ".arg" + str::from(i);
    vns.push_back(vname);
    vnes.push_back(isUnitPat(ps[i]) ? assume(var(vname, la), primty("unit"), la) : var(vname, la));
    els.push_back(PatternPtr(new MatchAny("_", la)));
  }
  return
    new Fn(vns,
      compileMatch(yyParseCC, vnes, list(
        PatternRow(ps,  justE(e, la)),
        PatternRow(els, nothingE(la))
      ), la),
      la
    );
}

Expr* makeIrrefutablePatternFn(const Patterns& ps, const ExprPtr& e, const LexicalAnnotation& la) {
  str::seq vns;
  Exprs    vnes;
  for (size_t i = 0; i < ps.size(); ++i) {
    std::string vname = ".arg" + str::from(i);
    vns.push_back(vname);
    vnes.push_back(isUnitPat(ps[i]) ? assume(var(vname, la), primty("unit"), la) : var(vname, la));
  }
  return new Fn(vns, compileMatch(yyParseCC, vnes, list(PatternRow(ps, e)), la), la);
}

Expr* makePatternFn(const Patterns& ps, const ExprPtr& e, const LexicalAnnotation& la) {
  for (const auto p : ps) {
    if (refutable(p)) {
      return makeRefutablePatternFn(ps, e, la);
    }
  }
  return makeIrrefutablePatternFn(ps, e, la);
}

Expr* makeProjSeq(Expr* rec, const str::seq& fields, const LexicalAnnotation& la) {
  for (const auto& f : fields) {
    rec = new Proj(ExprPtr(rec), f, la);
  }
  return rec;
}

MonoTypePtr monoTypeByName(const std::string& tn) {
  if (isPrimName(tn) || yyParseCC->isTypeAliasName(tn)) {
    return yyParseCC->replaceTypeAliases(Prim::make(tn));
  } else if (yyParseCC->isTypeName(tn)) {
    return Prim::make(tn, yyParseCC->namedTypeRepresentation(tn));
  } else {
    return TVar::make(tn);
  }
}

MonoTypePtr accumTApp(const MonoTypes& ts) {
  if (ts.size() == 0) {
    throw std::runtime_error("Internal parser error for type applications");
  } else if (ts.size() == 1) {
    return ts[0];
  } else {
    return MonoTypePtr(TApp::make(ts[0], drop(ts, 1)));
  }
}

MonoTypePtr makeTupleType(const MonoTypes& mts) {
  if (mts.size() == 1) {
    return clone(mts[0]);
  } else {
    Record::Members tms;
    for (unsigned int i = 0; i < mts.size(); ++i) {
      tms.push_back(Record::Member(".f" + str::from(i), mts[i]));
    }
    return Record::make(tms);
  }
}

MonoTypePtr makeSumType(const MonoTypes& mts) {
  Variant::Members ms;
  for (unsigned int i = 0; i < mts.size(); ++i) {
    ms.push_back(Variant::Member(".f" + str::from(i), mts[i], i));
  }
  return Variant::make(ms);
}

MonoTypePtr makeRecType(const Record::Members& tms) {
  return Record::make(tms);
}

MonoTypePtr makeVarType(const Variant::Members& vms) {
  Variant::Members tvms = vms;
  for (unsigned int i = 0; i < tvms.size(); ++i) {
    tvms[i].id = i;
  }
  return Variant::make(tvms);
}

str::seq tupSectionFields(const std::string& x) {
  str::seq r = str::csplit(x.substr(1), ".");
  for (std::string& f : r) {
    f = ".f" + f;
  }
  return r;
}

// override var and pat-var construction
namespace hobbes {
typedef Expr* (*VarCtorFn)(const std::string&, const LexicalAnnotation&);
typedef Pattern* (*PatVarCtorFn)(const std::string&, const LexicalAnnotation&);
extern VarCtorFn varCtorFn;
extern PatVarCtorFn patVarCtorFn;
}


#line 334 "hexpr.parse.C" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "hexpr.parse.H".  */
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
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 274 "hexpr.y" /* yacc.c:355  */

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

#line 525 "hexpr.parse.C" /* yacc.c:355  */
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

/* Copy the second part of user declarations.  */

#line 554 "hexpr.parse.C" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  71
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2871

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  94
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  76
/* YYNRULES -- Number of rules.  */
#define YYNRULES  345
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  764

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   348

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   492,   492,   493,   494,   495,   498,   499,   500,   502,
     503,   504,   506,   507,   508,   509,   510,   512,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   526,
     529,   532,   533,   536,   538,   541,   542,   543,   544,   545,
     546,   547,   548,   550,   551,   553,   555,   556,   558,   561,
     562,   563,   564,   566,   567,   569,   572,   574,   576,   577,
     579,   581,   583,   584,   585,   586,   587,   588,   589,   590,
     591,   592,   593,   594,   595,   596,   597,   598,   599,   600,
     601,   603,   604,   606,   607,   610,   611,   612,   613,   614,
     615,   616,   617,   618,   620,   621,   623,   624,   625,   626,
     627,   628,   629,   630,   631,   633,   634,   635,   636,   637,
     639,   640,   641,   642,   644,   647,   648,   651,   654,   657,
     669,   670,   673,   675,   676,   678,   680,   681,   683,   684,
     687,   688,   691,   692,   693,   694,   695,   696,   697,   698,
     699,   702,   703,   704,   705,   706,   709,   710,   711,   714,
     717,   720,   721,   724,   725,   726,   727,   728,   729,   730,
     731,   732,   733,   734,   735,   736,   737,   740,   743,   744,
     745,   746,   747,   748,   749,   750,   751,   752,   753,   754,
     755,   756,   757,   758,   759,   760,   763,   765,   766,   768,
     770,   771,   773,   775,   776,   778,   779,   781,   782,   783,
     785,   786,   788,   789,   791,   792,   794,   795,   798,   799,
     801,   802,   803,   804,   805,   806,   807,   808,   809,   810,
     811,   812,   813,   814,   815,   816,   817,   818,   819,   820,
     821,   822,   823,   824,   826,   827,   828,   829,   830,   832,
     834,   835,   837,   838,   840,   841,   843,   845,   846,   847,
     849,   850,   851,   852,   853,   854,   855,   856,   857,   858,
     859,   860,   861,   862,   864,   865,   866,   867,   869,   870,
     872,   873,   874,   876,   877,   878,   880,   881,   884,   886,
     887,   889,   890,   891,   892,   893,   894,   895,   896,   897,
     898,   900,   901,   902,   903,   905,   906,   907,   908,   910,
     911,   912,   914,   915,   917,   918,   920,   921,   922,   924,
     925,   926,   927,   928,   929,   930,   931,   932,   933,   934,
     935,   936,   937,   938,   939,   940,   941,   943,   944,   946,
     947,   949,   950,   952,   953,   955,   956,   958,   959,   961,
     962,   964,   965,   966,   967,   969
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "\"domodule\"", "\"dodefn\"",
  "\"doexpr\"", "\"option\"", "\"module\"", "\"where\"", "\"import\"",
  "\"type\"", "\"data\"", "\"class\"", "\"instance\"", "\"indent\"",
  "\"boolV\"", "\"charV\"", "\"byteV\"", "\"bytesV\"", "\"shortV\"",
  "\"intV\"", "\"longV\"", "\"int128V\"", "\"floatV\"", "\"doubleV\"",
  "\"id\"", "\"stringV\"", "\"regexV\"", "\"timespanV\"", "\"timeV\"",
  "\"dateTimeV\"", "\"tupSection\"", "\"=>\"", "\"->\"", "\":\"",
  "\"exists\"", "\"<-\"", "\":=\"", "\"=\"", "\"::\"", "\"++\"", "\"+\"",
  "\"-\"", "\"*\"", "\"/\"", "\"%\"", "\".\"", "\"==\"", "\"===\"",
  "\"~\"", "\"!=\"", "\"<\"", "\"<=\"", "\">\"", "\">=\"", "\"!\"",
  "\"let\"", "\"case\"", "\"default\"", "\"match\"", "\"matches\"",
  "\"parse\"", "\"with\"", "\"of\"", "\"and\"", "\"or\"", "\"if\"",
  "\"then\"", "\"else\"", "\"in\"", "\"pack\"", "\"unpack\"", "\"do\"",
  "\"return\"", "\"(\"", "\")\"", "\"[\"", "\"]\"", "\"{\"", "\"}\"",
  "\"|\"", "\",\"", "\";\"", "\"\\\\\"", "\"fn\"", "\"o\"", "\"..\"",
  "\"^\"", "\"@\"", "\"$\"", "\"?\"", "\"'\"", "\"`\"", "\"=~\"",
  "$accept", "s", "module", "defs", "def", "importdef", "tydef",
  "vartybind", "vardef", "classdef", "fundeps", "fundep", "cmembers",
  "cmember", "instdef", "imembers", "imember", "names", "nameseq", "name",
  "opname", "idseq", "types", "l0expr", "l1expr", "l2expr", "l3expr",
  "l4expr", "l5expr", "letbindings", "letbinding", "dobindings",
  "dobinding", "l6expr", "prules", "prule", "prdefs", "prdef", "pbelems",
  "pbelem", "pvalue", "tsseq", "l6exprs", "patternexps", "patternexp",
  "patterns", "refutablep", "irrefutablep", "pattern", "patternseq",
  "patternseqn", "recpatfields", "recpatfield", "recfields",
  "recfieldname", "recfieldpath", "varfields", "varbind", "cargs", "qtype",
  "cst", "tpreds", "tpred", "l1mtargl", "ltmtype", "l0mtype", "l1mtype",
  "tyind", "cppid", "l0mtargl", "l0mtarglt", "mtuplist", "msumlist",
  "mreclist", "mvarlist", "id", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348
};
# endif

#define YYPACT_NINF -578

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-578)))

#define YYTABLE_NINF -345

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      47,  1234,  2000,  2000,    88,    24,    24,    24,    96,    96,
     118,   118,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,
    -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,   534,
      61,  2000,  2702,    30,  2702,    24,    73,  1453,  2000,   534,
     416,  2000,  -578,  1318,  -578,  -578,  -578,  -578,  -578,  -578,
     187,  -578,   252,   273,    32,   226,  2468,  2312,  2000,  1532,
    2791,  2791,   451,   150,   633,   594,   595,  -578,   306,   451,
    -578,  -578,  1234,   324,   325,  -578,  2620,    81,  -578,  -578,
      98,  1001,   315,    96,   366,  1114,  -578,  -578,  -578,  -578,
    -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,
    -578,  2791,    24,   179,  -578,   316,  -578,   287,   138,  2624,
      24,   138,   372,  2078,   362,   380,  2390,   439,   463,   473,
     534,   476,   480,   487,   493,   497,   511,   513,   523,  2234,
     545,   550,   554,  -578,  -578,   562,   451,   120,   277,   367,
     345,   436,   604,     9,   157,  -578,  1136,  1136,  2791,  2000,
    1766,    32,  -578,  -578,   534,  2000,   228,  -578,  -578,   229,
     362,   380,  2390,   439,   463,   473,   476,   480,   487,   497,
     511,   513,   523,   545,   550,   554,  -578,  -578,  -578,  -578,
    -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  2791,
    2791,    24,   501,   273,   824,  -578,  -578,  -578,  2725,  2000,
    2000,  2000,  2312,  2312,  2468,  2468,  2468,  2468,  2468,  2468,
    2468,  2468,  2468,  2468,  2468,  2546,  2546,  2546,  2000,  -578,
    1318,    24,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,
    -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  1136,
    -578,  1136,  -578,  -578,  -578,    24,    24,   212,   314,  1804,
    1804,    24,  2000,   225,  -578,   385,  1804,    24,    13,    96,
    2620,    24,   212,    24,    24,    18,  -578,    -1,   498,   446,
     606,  -578,  -578,   371,   581,   531,  -578,   630,  2000,   102,
    2312,   590,   597,   138,    12,  -578,   656,  2702,  1610,   534,
     414,  1688,  -578,   659,   662,  -578,  -578,  -578,  -578,  -578,
    -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,
    -578,  -578,  -578,  -578,  -578,  2000,  2791,  1844,  -578,  -578,
     198,  2000,  2000,  2000,  -578,  -578,   502,  -578,   673,  -578,
    -578,  -578,   402,  2000,   245,  -578,   451,  2000,   231,  2000,
     427,   411,   547,   669,   122,  2000,  -578,  2000,   624,   624,
    -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,
    -578,  -578,  -578,  -578,  -578,  -578,  -578,   451,  1318,  -578,
    -578,  -578,   665,   171,   639,  -578,   628,  -578,    27,  1114,
    -578,   707,   212,    71,   571,   682,   108,   236,   113,   671,
     193,  -578,  1001,   507,  1804,  1804,   534,  1804,  1804,  1804,
     905,  1804,   635,    96,   704,    24,    24,  1114,   640,   690,
     691,   714,  -578,  1804,  1804,  1804,  1804,  -578,   654,  2791,
    -578,    21,  2791,   451,  2000,  -578,  -578,   504,  2791,   597,
    -578,  -578,  -578,  -578,   249,  -578,  -578,  -578,  -578,  -578,
    -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  1610,  2156,
     534,   589,   273,  -578,   630,  -578,  2000,  -578,  -578,  2000,
     451,   695,  -578,   291,  -578,   696,   451,   322,   421,   212,
     320,  1114,  -578,   433,  1922,  -578,   451,  2000,   243,   175,
    -578,   660,  -578,   661,  -578,    33,  2791,  2791,  -578,   451,
     451,  1804,  -578,  -578,  -578,  -578,  1804,   657,  -578,  1804,
    -578,    24,  1114,  1804,  1114,  -578,    24,  1114,  1804,  -578,
    -578,  1804,  1804,  1804,     7,    89,   464,   635,   635,   635,
    -578,   635,   635,    22,    96,   704,  -578,    20,  -578,    49,
    -578,  -578,    46,  1114,  1114,  1114,    96,   714,  -578,   635,
     635,   635,   635,  -578,  -578,  -578,  -578,  -578,   451,   705,
     584,  -578,   428,  1411,  -578,   663,  -578,    39,  2702,   703,
     124,   440,   485,  2000,  -578,  2000,  -578,  -578,  -578,  -578,
    -578,   475,   451,  2000,   286,  2000,  -578,  -578,  -578,   664,
     666,   635,    -8,   599,    99,   711,  -578,   101,   148,   670,
     715,   672,    42,   635,   115,   142,   717,   125,   719,  1804,
    1804,  1804,  1804,  1804,   704,    24,  -578,  -578,   704,    24,
      24,  -578,   714,  -578,   432,  -578,  -578,   718,  -578,    24,
     697,   504,    24,  2000,  2000,  2000,  -578,  -578,  -578,  2000,
    -578,  -578,   725,   138,  2156,  2156,  -578,  -578,  -578,   399,
     451,  -578,   451,  2000,   321,   451,  -578,  -578,   731,  -578,
     736,  -578,   733,  1114,  1804,   734,   735,  1114,   737,  1804,
    1804,  1804,  1804,  1804,  1804,   635,   635,   635,   635,   635,
     704,    26,   704,  -578,    24,   714,  -578,  1114,  2000,   739,
    2000,  -578,   741,   451,    76,   451,  -578,   532,   549,  -578,
    2000,   451,  2000,   355,  1804,   698,  1804,  -578,   141,  1804,
    1804,  -578,  1804,   166,   308,   196,   147,   294,   127,   704,
    -578,   451,  2000,   451,  2000,  2000,  -578,  -578,  -578,   528,
     451,  2000,   389,   635,  -578,   635,   742,   635,   635,   635,
     744,  -578,  -578,  1804,  -578,  1804,   704,   451,   451,   451,
    -578,   451,  2000,   413,  1804,  1804,   248,   301,   451,  2000,
     431,   635,   635,  -578,  -578,   451,  2000,   456,   451,  2000,
     750,   451,  2000,   451
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,     9,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   153,   154,   155,   156,   157,   158,   159,   160,
     161,   162,   345,   163,   150,   200,   165,   166,   267,     0,
       0,     0,     0,     0,     0,     0,     0,   273,   273,   247,
       0,     0,     2,     8,    10,    12,    13,    14,    15,    16,
       0,    29,   114,   164,   149,   131,     0,     0,     0,   273,
       0,     0,     4,    93,    95,   104,   109,   113,   131,     5,
     131,     1,     9,     0,    30,   329,     0,     0,    58,    60,
       0,     0,     0,     0,     0,     0,   258,   253,   257,   252,
     251,   254,   255,   263,   256,   259,   260,   261,   262,   266,
     250,   241,     0,     0,   124,     0,   234,     0,   203,     0,
       0,   151,     0,     0,     0,     0,     0,     0,     0,     0,
      65,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    64,    67,     0,   274,     0,   274,     0,
       0,     0,     0,     0,     0,    11,     0,     0,     0,   273,
       0,   148,   201,   265,     0,     0,     0,   108,    87,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   210,   211,   212,   218,
     213,   214,   215,   216,   217,   219,   223,   221,   222,   241,
     241,     0,     0,   220,     0,   239,   209,   233,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     6,
       9,     0,    75,    76,    77,    78,    79,    80,    65,    69,
      68,    66,    70,    71,    72,    73,    62,    63,    74,     0,
      59,     0,   320,   319,   325,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   279,     0,   309,     0,    39,    56,
      60,     0,     0,     0,     0,    49,    83,   335,     0,   307,
     308,   309,   243,     0,   240,     0,   245,     0,     0,     0,
       0,     0,     0,   202,     0,   188,     0,     0,   241,   247,
       0,     0,   127,     0,   131,   168,   169,   170,   171,   172,
     173,   176,   175,   174,   177,   178,   181,   179,   180,   185,
     182,   183,   184,    61,   167,     0,     0,     0,   136,   146,
       0,     0,     0,     0,   143,   186,     0,    33,     0,   277,
     122,   118,     0,     0,     0,   264,    17,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   208,     0,    88,    89,
      90,    91,    92,    98,    97,    96,    99,   100,   101,   102,
     103,   107,   105,   106,   110,   111,   112,     3,     7,   330,
      31,    32,     0,     0,     0,   318,     0,   305,   335,     0,
     311,     0,     0,     0,     0,   309,     0,     0,   309,     0,
       0,   278,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   281,   302,     0,     0,     0,     0,     0,   305,     0,
     344,     0,    84,     0,     0,     0,     0,   235,     0,     0,
     237,     0,     0,   115,     0,   123,   125,     0,     0,   117,
     205,   119,   187,   194,     0,   153,   154,   155,   156,   157,
     158,   159,   160,   162,   163,   150,   165,   166,   241,   241,
     247,     0,   164,   131,     0,   129,     0,   120,   126,     0,
     275,     0,   133,     0,   147,     0,   248,     0,     0,     0,
     335,     0,   130,     0,     0,   137,    18,     0,     0,     0,
     229,     0,   224,     0,   231,     0,     0,     0,   226,    85,
      86,     0,   310,   314,   315,   304,     0,     0,   312,     0,
     316,     0,     0,     0,     0,   317,     0,     0,     0,   326,
     280,     0,     0,     0,     0,     0,     0,   282,   284,   283,
     323,   322,   303,    35,     0,    41,    46,    40,    43,     0,
      81,    57,    50,     0,     0,     0,     0,    51,    53,   337,
     306,   336,   338,   236,   242,   238,   244,   246,   116,     0,
       0,   268,     0,     0,   204,   189,   191,     0,     0,     0,
       0,     0,     0,     0,   132,     0,   142,   141,   276,   140,
     139,     0,    19,     0,     0,     0,   230,   225,   232,     0,
       0,   321,     0,     0,     0,     0,   340,   335,     0,     0,
     342,   343,   335,   324,     0,     0,   309,     0,   309,     0,
       0,     0,     0,     0,     0,     0,    48,    47,     0,     0,
       0,    82,     0,   333,     0,   343,    55,     0,    54,     0,
     144,     0,     0,     0,     0,     0,   194,   199,   198,     0,
     193,   196,   197,   152,     0,     0,   143,   121,   128,     0,
     249,   138,    20,     0,     0,    94,   228,   227,     0,   328,
       0,   327,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   301,   294,   293,   292,   291,
      37,    36,    42,    44,    45,    52,   332,     0,     0,     0,
       0,   269,     0,   270,     0,   206,   190,     0,     0,   134,
       0,    21,     0,     0,     0,     0,     0,   339,     0,     0,
       0,   341,     0,   337,     0,     0,     0,     0,     0,     0,
     334,    34,     0,   145,     0,     0,   192,   195,   197,     0,
      22,     0,     0,   290,   313,   288,     0,   296,   300,   299,
       0,   287,   285,     0,   295,     0,    38,   272,   271,   207,
     135,    23,     0,     0,     0,     0,     0,     0,    24,     0,
       0,   289,   298,   286,   297,    25,     0,     0,    26,     0,
       0,    27,     0,    28
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -578,  -578,   701,   570,   -30,  -578,  -578,   269,  -578,  -578,
     190,   188,  -577,  -505,  -578,   184,  -518,  -380,   683,    -4,
     538,   191,   393,    -2,   -39,   557,   -31,   429,     5,  -578,
     533,  -578,   527,   -25,  -578,   530,  -578,   194,  -578,  -578,
     131,   -44,  -578,  -578,   392,   -51,  -578,   -91,   -19,  -175,
    -578,  -183,  -386,  -578,   -17,   -49,  -578,   201,   -36,  -123,
     685,  -578,   435,  -578,   580,   -74,   722,  -578,   587,  -578,
    -578,  -578,  -578,  -578,  -578,   410
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,    42,    43,    44,    45,    46,    47,   616,    48,
     527,   528,   525,   526,    49,   537,   538,   258,   259,    50,
     135,   529,   265,   136,    63,    64,    65,    66,    67,   103,
     104,   291,   292,    52,   284,   285,   555,   556,   557,   630,
     631,    53,   109,   429,   430,   194,   195,   105,   272,   273,
     274,   275,   276,   140,   141,    54,   550,   551,   137,   327,
     328,   253,   254,   401,   376,   329,   267,   650,    74,   268,
     614,   269,   270,   384,   387,    70
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      62,    69,   139,   151,    78,    78,    51,   108,   342,   111,
     198,   266,    99,   145,   340,   341,   193,   193,   158,   618,
     607,   404,   293,   523,   330,   157,   411,   670,   608,   107,
     604,   672,  -331,   242,   709,   546,   138,    22,   243,   144,
     413,   196,   196,    22,   244,   599,    22,   323,    51,    22,
       1,     2,     3,   245,   612,   627,   159,   193,    22,   151,
    -331,   242,   151,   153,    22,   628,   243,   648,   413,   246,
     496,    22,   244,   240,    22,  -331,   240,    51,   154,    78,
     400,   245,   610,   659,   283,   157,    22,   400,    71,   324,
     158,   431,   262,   405,   248,   400,   263,   246,   264,   546,
     545,   609,   605,    99,   193,   251,    22,   609,   110,   715,
     252,   290,   578,   332,   499,   400,   370,   629,   371,   239,
     262,    22,   248,    22,   263,   600,   264,    22,   601,   331,
     400,   157,   736,   251,  -331,   101,   241,   335,   252,   102,
     199,   200,   413,    22,   654,   193,   193,   507,   334,   503,
     193,   113,   504,   336,   193,    76,   617,   618,   660,   400,
     487,   201,   635,   351,   352,   607,   663,   607,   735,    28,
      76,   424,    76,   377,   377,   346,   101,   400,   652,   346,
     102,   361,   362,   363,    29,   661,   202,   400,   408,   400,
     733,   412,    81,  -344,  -344,   314,   400,   348,   349,   350,
     293,   315,   488,   400,   636,    86,    87,    88,    89,    90,
      91,    92,   149,   400,   150,   400,   367,   221,    93,   203,
     726,   199,   200,    22,   492,    51,   146,   242,   655,   400,
     400,   607,   243,    94,   151,   400,   400,    22,   244,   199,
     200,   426,   201,   575,   452,   374,   730,   245,   278,   325,
     390,    22,   408,    22,   400,   240,    22,   199,   200,    95,
     201,   279,   434,   246,   155,   -60,   337,   342,    22,   477,
      96,    97,   193,   340,   341,   732,   423,   464,   201,   474,
      28,   573,    98,    28,   400,   509,   262,   375,   248,   290,
     263,   147,   264,   199,   200,    29,   339,   461,    29,   251,
     391,   152,   495,   465,   252,   497,   392,   495,   377,   199,
     200,    22,   148,   460,   201,   463,   505,   506,   558,   466,
     467,   468,   475,   149,   643,   150,   149,   753,   150,   242,
     201,   473,   220,   266,   243,   476,   400,   479,   145,    22,
     244,   199,   200,   489,   218,   490,    22,   257,   379,   245,
     281,   199,   200,  -331,   280,   199,   200,   316,   393,   692,
     394,   413,   201,   317,   395,   246,   396,   397,   564,   398,
     399,   221,   201,    51,   734,   193,   201,   553,   193,   516,
      22,   754,   400,   731,   193,   151,   199,   200,   262,   400,
     248,   380,   263,   721,   264,   408,   400,   568,   261,    78,
     544,   251,   566,   547,   452,   452,   252,   201,   400,   196,
     287,    55,    68,   139,    22,    72,    73,    75,    79,    79,
      83,    85,   548,   393,   319,   394,   320,   742,   586,   395,
     589,   396,   397,   591,   398,   399,   142,   295,    22,   100,
     106,    22,   193,   193,   318,   112,   417,   138,   315,   100,
     143,   749,   418,    55,   561,   296,    22,   562,   412,   613,
     586,   615,   622,   199,   200,   156,   623,   579,   580,   756,
     197,   197,   571,   400,   321,   572,   689,   472,   199,   200,
     690,    22,    55,   315,   201,   199,   200,    79,   482,   415,
      79,   256,   483,   260,   759,   271,   455,   199,   200,   201,
     602,   567,   480,   603,   199,   200,   201,   676,   481,   193,
     569,   197,   277,   677,   298,   199,   200,   242,   201,   637,
     286,   343,   243,   294,   549,   201,    22,    22,   244,    22,
     100,   414,    78,   633,   346,   374,   201,   245,   299,   199,
     200,    86,    87,    88,    89,    90,    91,    92,   300,   199,
     200,   301,   641,   246,    93,   302,   271,   271,   197,    22,
     201,   639,   303,   640,   100,   627,   338,   638,   304,    94,
     201,   642,   305,   645,    22,   628,   469,   375,   248,   697,
     249,   511,   250,   701,   151,   512,   306,   513,   307,   251,
     452,   452,   199,   200,   252,    95,   199,   200,   308,   197,
     197,   277,   344,   710,   197,   740,    96,    97,   197,   559,
     420,   716,   421,   201,    22,   579,   580,   201,    98,   649,
     310,   683,   684,   685,    22,   311,   484,   687,   485,   312,
      55,   369,   467,   468,   212,   213,   214,   313,   215,   216,
     217,   691,   322,   242,   364,   365,   366,   416,   243,   271,
     500,   271,   501,    22,   244,   372,    75,   271,   271,   385,
     388,   389,   419,   245,   620,   621,   271,   403,   422,    79,
     427,   407,   271,   409,   410,   271,   711,   428,   713,   246,
     204,   205,   206,   207,   208,   209,   210,   211,   719,   106,
     720,    77,    80,   433,   286,    82,    84,   459,   453,   454,
    -234,   294,   262,   494,   248,   471,   263,   486,   264,   201,
     737,   491,   738,   739,   493,   251,   502,   508,   524,   741,
     252,   533,   242,   400,   534,   535,   197,   243,   536,   543,
     100,   563,    22,   244,   565,   576,   256,   583,   577,   619,
     748,   634,   245,   626,   646,   653,   647,   755,   478,   657,
     656,   662,   658,   664,   758,   680,   678,   761,   246,   688,
     763,   353,   354,   355,   356,   357,   358,   359,   360,   694,
     695,   696,   699,   219,   700,   724,   702,   712,    55,   714,
     744,   262,   745,   248,   498,   263,   271,   264,   762,   271,
     368,   271,   271,   606,   251,   671,   675,   673,   406,   252,
     532,   674,   256,   255,   271,   271,   100,   271,   271,   271,
     271,   271,   425,   260,   432,   530,   531,   271,   458,   717,
     686,   554,   681,   271,   271,   271,   271,   510,   381,   197,
       0,   277,   197,   373,     0,     0,     0,   552,   197,   176,
     177,   178,   179,   180,   181,   182,   183,     0,   184,    22,
     185,   186,    25,   187,   188,     0,     0,     0,   453,   453,
     454,   560,     0,     0,     0,     0,     0,     0,     0,     0,
     345,     0,     0,     0,     0,     0,     0,     0,     0,   271,
       0,   271,     0,     0,     0,     0,     0,     0,   574,     0,
       0,     0,     0,     0,     0,   277,   197,   197,   189,     0,
     190,   271,   191,     0,   192,     0,   271,     0,     0,   271,
       0,   585,   271,   271,   271,     0,   590,   271,   271,     0,
     242,   271,   596,   598,     0,   243,     0,     0,     0,     0,
      22,   244,     0,     0,    79,     0,     0,     0,     0,   611,
     245,     0,   271,   271,   271,   271,   260,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   246,     0,     0,     0,
       0,     0,     0,   197,     0,     0,     0,   632,     0,   378,
       0,   383,   386,     0,     0,     0,     0,     0,   402,   382,
       0,   248,     0,   263,   644,   264,     0,     0,     0,     0,
       0,     0,   251,   651,     0,   520,     0,   252,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   271,
     271,   271,   271,   271,     0,   530,   242,     0,     0,   530,
     530,   243,     0,     0,     0,     0,    22,   244,     0,   679,
       0,   552,   682,     0,     0,     0,   245,     0,     0,     0,
       0,     0,     0,     0,   453,   453,     0,     0,   470,     0,
       0,     0,   246,     0,   693,     0,     0,     0,     0,     0,
       0,     0,     0,   271,   271,     0,     0,   271,     0,   271,
     271,   271,   271,   271,   271,   247,     0,   248,     0,   249,
       0,   250,     0,     0,   611,     0,     0,   271,   251,     0,
       0,     0,     0,   252,     0,     0,     0,     0,   718,     0,
       0,     0,     0,   722,   271,     0,   271,     0,     0,   271,
     271,     0,   271,     0,   255,     0,   514,   515,     0,   517,
     518,   519,   521,   522,     0,     0,     0,     0,     0,   242,
       0,     0,   743,     0,   243,   539,   540,   541,   542,    22,
     244,     0,     0,   271,     0,   271,     0,     0,     0,   245,
       0,   242,     0,   750,   271,   271,   243,     0,     0,     0,
     757,    22,   244,     0,     0,   246,     0,   760,     0,     0,
       0,   245,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   246,   262,     0,
     248,   378,   263,     0,   264,     0,     0,     0,     0,     0,
       0,   251,     0,     0,     0,     0,   252,     0,     0,     0,
     326,     0,   248,   581,   263,     0,   264,     0,   582,     0,
       0,   584,     0,   251,   587,   588,     0,     0,   252,   592,
     593,     0,     0,   594,   595,   597,     0,     0,     0,     0,
       5,     6,     0,     7,     8,     9,    10,    11,     0,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      29,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,     0,    32,     0,    33,     0,     0,     0,     0,
       0,     0,     0,     0,    34,    35,    36,     0,    37,     0,
      38,     0,    39,     0,    40,     0,     0,     0,     0,     0,
       0,   665,   666,   667,   668,   669,    41,     7,     8,     9,
      10,    11,     0,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    29,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,   698,    32,     0,    33,
       0,   703,   704,   705,   706,   707,   708,     0,    34,    35,
      36,     0,    37,     0,    38,     0,    39,     0,    40,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      41,     0,     0,     0,     0,     0,   723,     0,   725,   624,
       0,   727,   728,     0,   729,     0,   176,   177,   178,   179,
     180,   181,   182,   183,     0,   184,    22,   185,   186,    25,
     187,   188,     0,     0,   625,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   746,     0,   747,     0,     0,
       0,     0,     0,     0,     0,     0,   751,   752,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,   189,     0,   190,     0,   191,
       0,   192,     0,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,    30,
      31,     0,    32,     0,    33,     0,     0,   130,   131,    58,
       0,     0,   132,    34,    35,    36,     0,    59,     0,    38,
       0,    39,     0,    40,     0,     0,    60,    61,   133,     0,
       0,     0,     0,     0,     0,    41,   134,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,     0,     0,     0,     0,
       0,     0,   160,   161,   162,   163,   164,   165,    29,   166,
     167,   168,   124,   169,   170,   171,   172,   129,    30,    31,
       0,    32,     0,    33,     0,     0,   173,   174,    58,     0,
       0,   175,    34,    35,    36,     0,    59,     0,    38,     0,
      39,     0,    40,     0,     0,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    41,   435,   436,   437,   438,   439,
     440,   441,   442,    20,   443,    22,   444,   445,    25,   446,
     447,    28,     0,     0,     0,     0,     0,     0,     0,     0,
     160,   161,   162,   163,   164,   165,    29,   166,   167,   168,
     124,   169,   170,   171,   172,   129,    30,    31,     0,    32,
       0,    33,     0,     0,   173,   174,    58,     0,     0,   175,
      34,    35,    36,     0,   448,     0,   449,     0,   450,     0,
     451,     0,     0,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    41,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,     0,     0,     0,    29,     0,     0,     0,     0,     0,
       0,     0,     0,    57,    30,    31,     0,    32,     0,    33,
       0,     0,     0,     0,    58,     0,     0,     0,    34,    35,
      36,   456,   288,     0,    38,     0,   289,   457,    40,     0,
       0,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      41,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,     0,     0,
     333,     0,     0,     0,     0,     0,     0,     0,    56,     0,
       0,     0,    29,     0,     0,     0,     0,     0,     0,   242,
       0,    57,    30,    31,   243,    32,     0,    33,     0,    22,
     244,     0,    58,     0,     0,     0,    34,    35,    36,   245,
      59,     0,    38,     0,    39,     0,    40,     0,     0,    60,
      61,     0,     0,     0,     0,   246,     0,     0,    41,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,   382,     0,
     248,     0,   263,     0,   264,     0,    56,     0,     0,     0,
      29,   251,     0,     0,     0,     0,   252,     0,     0,    57,
      30,    31,     0,    32,     0,    33,     0,     0,     0,     0,
      58,     0,     0,     0,    34,    35,    36,     0,    59,     0,
      38,   462,    39,     0,    40,     0,     0,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    41,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    56,     0,     0,     0,    29,     0,
       0,     0,     0,     0,     0,     0,     0,    57,    30,    31,
       0,    32,     0,    33,     0,     0,     0,     0,    58,     0,
       0,     0,    34,    35,    36,     0,    59,     0,    38,   570,
      39,     0,    40,     0,     0,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    41,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    56,     0,     0,     0,    29,     0,     0,     0,
       0,     0,     0,     0,     0,    57,    30,    31,     0,    32,
       0,    33,     0,     0,     0,     0,    58,     0,     0,     0,
      34,    35,    36,     0,    59,     0,    38,     0,    39,     0,
      40,     0,     0,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    41,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,     0,     0,     0,    29,     0,     0,     0,     0,     0,
       0,     0,     0,    57,    30,    31,     0,    32,     0,    33,
       0,     0,     0,     0,    58,     0,     0,     0,    34,    35,
      36,     0,   288,     0,    38,     0,   289,     0,    40,     0,
       0,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      41,   435,   436,   437,   438,   439,   440,   441,   442,    20,
     443,    22,   444,   445,    25,   446,   447,    28,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    56,     0,
       0,     0,    29,     0,     0,     0,     0,     0,     0,     0,
       0,    57,    30,    31,     0,    32,     0,    33,     0,     0,
       0,     0,    58,     0,     0,     0,    34,    35,    36,     0,
     448,     0,   449,     0,   450,     0,   451,     0,     0,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    41,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    56,     0,     0,     0,
      29,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,     0,    32,     0,    33,     0,     0,     0,     0,
      58,     0,     0,     0,    34,    35,    36,     0,    59,   309,
      38,     0,    39,     0,    40,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    41,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    56,     0,     0,     0,    29,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
       0,    32,     0,    33,     0,     0,     0,     0,    58,     0,
       0,     0,    34,    35,    36,     0,    59,     0,    38,     0,
      39,     0,    40,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    41,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    56,     0,     0,     0,    29,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    30,    31,     0,    32,
       0,    33,     0,     0,     0,     0,     0,     0,     0,     0,
      34,    35,    36,     0,    59,   297,    38,     0,    39,     0,
      40,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    41,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,     0,     0,     0,    29,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,     0,    32,     0,    33,
       0,     0,     0,     0,     0,     0,     0,     0,    34,    35,
      36,     0,    59,     0,    38,     0,    39,     0,    40,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      41,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    29,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    30,    31,     0,    32,     0,    33,     0,     0,
       0,     0,     0,     0,     0,     0,    34,    35,    36,     0,
      59,     0,    38,     0,    39,     0,    40,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    41,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,     0,     0,
     222,   223,   224,   225,   226,   227,   228,   229,   230,   231,
      29,   232,   233,   234,   235,     0,     0,     0,     0,     0,
       0,    31,     0,     0,   236,   237,   282,     0,     0,   238,
       0,     0,     0,     0,    34,    35,     0,     0,    59,     0,
      38,     0,    39,     0,    40,   133,     0,     0,     0,     0,
       0,     0,     0,   134,     0,     0,    41,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,     0,     0,     0,     0,
     176,   177,   178,   179,   180,   181,   182,   183,    29,   184,
      22,   185,   186,    25,   187,   188,     0,     0,     0,    31,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   347,    34,    35,     0,     0,    59,     0,    38,     0,
      39,     0,    40,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    41,     0,     0,     0,     0,   189,
       0,   190,     0,   191,     0,   192,   176,   177,   178,   179,
     180,   181,   182,   183,     0,   184,    22,   185,   186,    25,
     187,   188,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   189,     0,   190,     0,   191,
       0,   192
};

static const yytype_int16 yycheck[] =
{
       2,     3,    38,    52,     8,     9,     1,    32,   191,    34,
      61,    85,    29,    43,   189,   190,    60,    61,    57,   537,
     525,     8,   113,   403,   147,    56,     8,   604,     8,    31,
       8,   608,    33,    15,     8,   421,    38,    25,    20,    41,
      41,    60,    61,    25,    26,    38,    25,    38,    43,    25,
       3,     4,     5,    35,     8,    16,    58,   101,    25,   108,
      33,    15,   111,    31,    25,    26,    20,    75,    41,    51,
      43,    25,    26,    77,    25,    33,    80,    72,    46,    83,
      88,    35,    33,    41,   109,   116,    25,    88,     0,    80,
     129,    79,    74,    80,    76,    88,    78,    51,    80,   485,
      79,    81,    80,   120,   148,    87,    25,    81,    78,    33,
      92,   113,    79,   149,    43,    88,   239,    78,   241,    38,
      74,    25,    76,    25,    78,    36,    80,    25,    39,   148,
      88,   162,   709,    87,    33,    74,    38,   154,    92,    78,
      64,    65,    41,    25,    43,   189,   190,    34,   150,    41,
     194,    78,    44,   155,   198,    74,   536,   675,    43,    88,
      38,    85,    38,   202,   203,   670,    41,   672,    41,    31,
      74,    69,    74,   247,   248,   194,    74,    88,    79,   198,
      78,   212,   213,   214,    46,    43,    36,    88,   262,    88,
      43,   265,    74,    80,    81,    75,    88,   199,   200,   201,
     291,    81,    80,    88,    80,     7,     8,     9,    10,    11,
      12,    13,    74,    88,    76,    88,   218,    46,    20,    69,
      79,    64,    65,    25,    53,   220,    39,    15,    80,    88,
      88,   736,    20,    35,   283,    88,    88,    25,    26,    64,
      65,   280,    85,    68,   288,    33,    80,    35,    69,    92,
     252,    25,   326,    25,    88,   259,    25,    64,    65,    61,
      85,    82,   287,    51,    38,    39,    38,   450,    25,    38,
      72,    73,   316,   448,   449,    79,   278,    79,    85,    34,
      31,    38,    84,    31,    88,    92,    74,    75,    76,   291,
      78,    39,    80,    64,    65,    46,    67,   316,    46,    87,
      75,    28,   376,   320,    92,   379,    81,   381,   382,    64,
      65,    25,    60,   315,    85,   317,    80,    81,    69,   321,
     322,   323,    77,    74,    38,    76,    74,    79,    76,    15,
      85,   333,     8,   407,    20,   337,    88,   339,   368,    25,
      26,    64,    65,   345,    38,   347,    25,    32,    34,    35,
      63,    64,    65,    33,    38,    64,    65,    80,    38,    38,
      40,    41,    85,    86,    44,    51,    46,    47,    77,    49,
      50,    46,    85,   368,    80,   419,    85,   428,   422,   396,
      25,    80,    88,    75,   428,   434,    64,    65,    74,    88,
      76,    77,    78,    38,    80,   469,    88,   471,    32,   403,
     419,    87,    80,   422,   448,   449,    92,    85,    88,   428,
      38,     1,     2,   449,    25,     5,     6,     7,     8,     9,
      10,    11,   424,    38,    79,    40,    81,    38,   502,    44,
     504,    46,    47,   507,    49,    50,    20,    75,    25,    29,
      30,    25,   486,   487,    77,    35,    75,   449,    81,    39,
      40,    38,    81,    43,   456,    75,    25,   459,   532,   533,
     534,   535,    34,    64,    65,    55,    38,   486,   487,    38,
      60,    61,   474,    88,    38,   477,    77,    75,    64,    65,
      81,    25,    72,    81,    85,    64,    65,    77,    77,    43,
      80,    81,    81,    83,    38,    85,    82,    64,    65,    85,
      36,    80,    75,    39,    64,    65,    85,    75,    81,   553,
      77,   101,   102,    81,    75,    64,    65,    15,    85,    79,
     110,    20,    20,   113,    20,    85,    25,    25,    26,    25,
     120,    33,   536,   558,   553,    33,    85,    35,    75,    64,
      65,     7,     8,     9,    10,    11,    12,    13,    75,    64,
      65,    75,    77,    51,    20,    75,   146,   147,   148,    25,
      85,   563,    75,   565,   154,    16,   156,    82,    75,    35,
      85,   573,    75,   575,    25,    26,    74,    75,    76,   653,
      78,    74,    80,   657,   633,    78,    75,    80,    75,    87,
     634,   635,    64,    65,    92,    61,    64,    65,    75,   189,
     190,   191,   192,   677,   194,    77,    72,    73,   198,    20,
      79,    79,    81,    85,    25,   634,   635,    85,    84,    20,
      75,   623,   624,   625,    25,    75,    79,   629,    81,    75,
     220,   221,   634,   635,    40,    41,    42,    75,    43,    44,
      45,   643,    38,    15,   215,   216,   217,    41,    20,   239,
      79,   241,    81,    25,    26,   245,   246,   247,   248,   249,
     250,   251,    81,    35,    80,    81,   256,   257,    38,   259,
      80,   261,   262,   263,   264,   265,   678,    80,   680,    51,
      47,    48,    49,    50,    51,    52,    53,    54,   690,   279,
     692,     8,     9,    37,   284,    10,    11,    38,   288,   289,
      38,   291,    74,    75,    76,    32,    78,    38,    80,    85,
     712,    46,   714,   715,    75,    87,    34,    46,    14,   721,
      92,    81,    15,    88,    34,    34,   316,    20,    14,    75,
     320,    36,    25,    26,    38,    75,   326,    80,    77,    34,
     742,    38,    35,    80,    80,    34,    80,   749,   338,    34,
      80,    34,    80,    34,   756,    58,    38,   759,    51,    34,
     762,   204,   205,   206,   207,   208,   209,   210,   211,    38,
      34,    38,    38,    72,    39,    77,    39,    38,   368,    38,
      38,    74,    38,    76,    77,    78,   376,    80,    38,   379,
     220,   381,   382,   524,    87,   605,   612,   609,   260,    92,
     407,   610,   392,    81,   394,   395,   396,   397,   398,   399,
     400,   401,   279,   403,   284,   405,   406,   407,   291,   688,
     626,   429,   621,   413,   414,   415,   416,   392,   248,   419,
      -1,   421,   422,   246,    -1,    -1,    -1,   427,   428,    15,
      16,    17,    18,    19,    20,    21,    22,    -1,    24,    25,
      26,    27,    28,    29,    30,    -1,    -1,    -1,   448,   449,
     450,   451,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   469,
      -1,   471,    -1,    -1,    -1,    -1,    -1,    -1,   478,    -1,
      -1,    -1,    -1,    -1,    -1,   485,   486,   487,    74,    -1,
      76,   491,    78,    -1,    80,    -1,   496,    -1,    -1,   499,
      -1,   501,   502,   503,   504,    -1,   506,   507,   508,    -1,
      15,   511,   512,   513,    -1,    20,    -1,    -1,    -1,    -1,
      25,    26,    -1,    -1,   524,    -1,    -1,    -1,    -1,   529,
      35,    -1,   532,   533,   534,   535,   536,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    -1,    -1,    -1,
      -1,    -1,    -1,   553,    -1,    -1,    -1,   557,    -1,   247,
      -1,   249,   250,    -1,    -1,    -1,    -1,    -1,   256,    74,
      -1,    76,    -1,    78,   574,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    87,   583,    -1,    90,    -1,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   599,
     600,   601,   602,   603,    -1,   605,    15,    -1,    -1,   609,
     610,    20,    -1,    -1,    -1,    -1,    25,    26,    -1,   619,
      -1,   621,   622,    -1,    -1,    -1,    35,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   634,   635,    -1,    -1,   326,    -1,
      -1,    -1,    51,    -1,   644,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   653,   654,    -1,    -1,   657,    -1,   659,
     660,   661,   662,   663,   664,    74,    -1,    76,    -1,    78,
      -1,    80,    -1,    -1,   674,    -1,    -1,   677,    87,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,   688,    -1,
      -1,    -1,    -1,   693,   694,    -1,   696,    -1,    -1,   699,
     700,    -1,   702,    -1,   392,    -1,   394,   395,    -1,   397,
     398,   399,   400,   401,    -1,    -1,    -1,    -1,    -1,    15,
      -1,    -1,   722,    -1,    20,   413,   414,   415,   416,    25,
      26,    -1,    -1,   733,    -1,   735,    -1,    -1,    -1,    35,
      -1,    15,    -1,   743,   744,   745,    20,    -1,    -1,    -1,
     750,    25,    26,    -1,    -1,    51,    -1,   757,    -1,    -1,
      -1,    35,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    74,    -1,
      76,   469,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,
      74,    -1,    76,   491,    78,    -1,    80,    -1,   496,    -1,
      -1,   499,    -1,    87,   502,   503,    -1,    -1,    92,   507,
     508,    -1,    -1,   511,   512,   513,    -1,    -1,    -1,    -1,
       6,     7,    -1,     9,    10,    11,    12,    13,    -1,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      56,    57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    -1,    74,    -1,
      76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,   599,   600,   601,   602,   603,    92,     9,    10,    11,
      12,    13,    -1,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    56,    57,   654,    59,    -1,    61,
      -1,   659,   660,   661,   662,   663,   664,    -1,    70,    71,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      92,    -1,    -1,    -1,    -1,    -1,   694,    -1,   696,     8,
      -1,   699,   700,    -1,   702,    -1,    15,    16,    17,    18,
      19,    20,    21,    22,    -1,    24,    25,    26,    27,    28,
      29,    30,    -1,    -1,    33,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   733,    -1,   735,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   744,   745,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    74,    -1,    76,    -1,    78,
      -1,    80,    -1,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    -1,    61,    -1,    -1,    64,    65,    66,
      -1,    -1,    69,    70,    71,    72,    -1,    74,    -1,    76,
      -1,    78,    -1,    80,    -1,    -1,    83,    84,    85,    -1,
      -1,    -1,    -1,    -1,    -1,    92,    93,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    -1,    61,    -1,    -1,    64,    65,    66,    -1,
      -1,    69,    70,    71,    72,    -1,    74,    -1,    76,    -1,
      78,    -1,    80,    -1,    -1,    83,    84,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      -1,    61,    -1,    -1,    64,    65,    66,    -1,    -1,    69,
      70,    71,    72,    -1,    74,    -1,    76,    -1,    78,    -1,
      80,    -1,    -1,    83,    84,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      42,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    55,    56,    57,    -1,    59,    -1,    61,
      -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    -1,    76,    -1,    78,    79,    80,    -1,
      -1,    83,    84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      92,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    -1,    -1,
      34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,
      -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,    15,
      -1,    55,    56,    57,    20,    59,    -1,    61,    -1,    25,
      26,    -1,    66,    -1,    -1,    -1,    70,    71,    72,    35,
      74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,    83,
      84,    -1,    -1,    -1,    -1,    51,    -1,    -1,    92,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    74,    -1,
      76,    -1,    78,    -1,    80,    -1,    42,    -1,    -1,    -1,
      46,    87,    -1,    -1,    -1,    -1,    92,    -1,    -1,    55,
      56,    57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    70,    71,    72,    -1,    74,    -1,
      76,    77,    78,    -1,    80,    -1,    -1,    83,    84,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    56,    57,
      -1,    59,    -1,    61,    -1,    -1,    -1,    -1,    66,    -1,
      -1,    -1,    70,    71,    72,    -1,    74,    -1,    76,    77,
      78,    -1,    80,    -1,    -1,    83,    84,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    42,    -1,    -1,    -1,    46,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    55,    56,    57,    -1,    59,
      -1,    61,    -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,
      70,    71,    72,    -1,    74,    -1,    76,    -1,    78,    -1,
      80,    -1,    -1,    83,    84,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      42,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    55,    56,    57,    -1,    59,    -1,    61,
      -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,    70,    71,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      -1,    83,    84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      92,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,
      -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    55,    56,    57,    -1,    59,    -1,    61,    -1,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    70,    71,    72,    -1,
      74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,    83,
      84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      56,    57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    70,    71,    72,    -1,    74,    75,
      76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    56,    57,
      -1,    59,    -1,    61,    -1,    -1,    -1,    -1,    66,    -1,
      -1,    -1,    70,    71,    72,    -1,    74,    -1,    76,    -1,
      78,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    42,    -1,    -1,    -1,    46,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    56,    57,    -1,    59,
      -1,    61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    -1,    74,    75,    76,    -1,    78,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      42,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    56,    57,    -1,    59,    -1,    61,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      92,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    56,    57,    -1,    59,    -1,    61,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    -1,
      74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      46,    51,    52,    53,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    57,    -1,    -1,    64,    65,    62,    -1,    -1,    69,
      -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    74,    -1,
      76,    -1,    78,    -1,    80,    85,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    93,    -1,    -1,    92,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    17,    18,    19,    20,    21,    22,    46,    24,
      25,    26,    27,    28,    29,    30,    -1,    -1,    -1,    57,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    70,    71,    -1,    -1,    74,    -1,    76,    -1,
      78,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,    74,
      -1,    76,    -1,    78,    -1,    80,    15,    16,    17,    18,
      19,    20,    21,    22,    -1,    24,    25,    26,    27,    28,
      29,    30,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    74,    -1,    76,    -1,    78,
      -1,    80
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,    95,     6,     7,     9,    10,    11,
      12,    13,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    46,
      56,    57,    59,    61,    70,    71,    72,    74,    76,    78,
      80,    92,    96,    97,    98,    99,   100,   101,   103,   108,
     113,   122,   127,   135,   149,   169,    42,    55,    66,    74,
      83,    84,   117,   118,   119,   120,   121,   122,   169,   117,
     169,     0,   169,   169,   162,   169,    74,   112,   113,   169,
     112,    74,   154,   169,   154,   169,     7,     8,     9,    10,
      11,    12,    13,    20,    35,    61,    72,    73,    84,   148,
     169,    74,    78,   123,   124,   141,   169,   117,   127,   136,
      78,   127,   169,    78,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      64,    65,    69,    85,    93,   114,   117,   152,   117,   152,
     147,   148,    20,   169,   117,    98,    39,    39,    60,    74,
      76,   149,    28,    31,    46,    38,   169,   120,   118,   117,
      40,    41,    42,    43,    44,    45,    47,    48,    49,    51,
      52,    53,    54,    64,    65,    69,    15,    16,    17,    18,
      19,    20,    21,    22,    24,    26,    27,    29,    30,    74,
      76,    78,    80,   135,   139,   140,   142,   169,   139,    64,
      65,    85,    36,    69,    47,    48,    49,    50,    51,    52,
      53,    54,    40,    41,    42,    43,    44,    45,    38,    96,
       8,    46,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    51,    52,    53,    54,    64,    65,    69,    38,
     113,    38,    15,    20,    26,    35,    51,    74,    76,    78,
      80,    87,    92,   155,   156,   160,   169,    32,   111,   112,
     169,    32,    74,    78,    80,   116,   159,   160,   163,   165,
     166,   169,   142,   143,   144,   145,   146,   169,    69,    82,
      38,    63,    62,   127,   128,   129,   169,    38,    74,    78,
     117,   125,   126,   141,   169,    75,    75,    75,    75,    75,
      75,    75,    75,    75,    75,    75,    75,    75,    75,    75,
      75,    75,    75,    75,    75,    81,    80,    86,    77,    79,
      81,    38,    38,    38,    80,    92,    74,   153,   154,   159,
     153,   142,   152,    34,   117,   148,   117,    38,   169,    67,
     143,   143,   145,    20,   169,    46,   142,    46,   117,   117,
     117,   118,   118,   119,   119,   119,   119,   119,   119,   119,
     119,   120,   120,   120,   121,   121,   121,   117,    97,   169,
     153,   153,   169,   162,    33,    75,   158,   159,   160,    34,
      77,   158,    74,   160,   167,   169,   160,   168,   169,   169,
     117,    75,    81,    38,    40,    44,    46,    47,    49,    50,
      88,   157,   160,   169,     8,    80,   114,   169,   159,   169,
     169,     8,   159,    41,    33,    43,    41,    75,    81,    81,
      79,    81,    38,   117,    69,   124,   118,    80,    80,   137,
     138,    79,   129,    37,   127,    15,    16,    17,    18,    19,
      20,    21,    22,    24,    26,    27,    29,    30,    74,    76,
      78,    80,   135,   169,   169,    82,    73,    79,   126,    38,
     117,   142,    77,   117,    79,   148,   117,   117,   117,    74,
     160,    32,    75,   117,    34,    77,   117,    38,   169,   117,
      75,    81,    77,    81,    79,    81,    38,    38,    80,   117,
     117,    46,    53,    75,    75,   159,    43,   159,    77,    43,
      79,    81,    34,    41,    44,    80,    81,    34,    46,    92,
     156,    74,    78,    80,   160,   160,   148,   160,   160,   160,
      90,   160,   160,   111,    14,   106,   107,   104,   105,   115,
     169,   169,   116,    81,    34,    34,    14,   109,   110,   160,
     160,   160,   160,    75,   142,    79,   146,   142,   117,    20,
     150,   151,   169,   139,   138,   130,   131,   132,    69,    20,
     169,   117,   117,    36,    77,    38,    80,    80,   159,    77,
      77,   117,   117,    38,   169,    68,    75,    77,    79,   142,
     142,   160,   160,    80,   160,   169,   159,   160,   160,   159,
     169,   159,   160,   160,   160,   160,   169,   160,   169,    38,
      36,    39,    36,    39,     8,    80,   101,   107,     8,    81,
      33,   169,     8,   159,   164,   159,   102,   111,   110,    34,
      80,    81,    34,    38,     8,    33,    80,    16,    26,    78,
     133,   134,   169,   127,    38,    38,    80,    79,    82,   117,
     117,    77,   117,    38,   169,   117,    80,    80,    75,    20,
     161,   169,    79,    34,    43,    80,    80,    34,    80,    41,
      43,    43,    34,    41,    34,   160,   160,   160,   160,   160,
     106,   104,   106,   105,   115,   109,    75,    81,    38,   169,
      58,   151,   169,   117,   117,   117,   131,   117,    34,    77,
      81,   117,    38,   169,    38,    34,    38,   159,   160,    38,
      39,   159,    39,   160,   160,   160,   160,   160,   160,     8,
     159,   117,    38,   117,    38,    33,    79,   134,   169,   117,
     117,    38,   169,   160,    77,   160,    79,   160,   160,   160,
      80,    75,    79,    43,    80,    41,   106,   117,   117,   117,
      77,   117,    38,   169,    38,    38,   160,   160,   117,    38,
     169,   160,   160,    79,    80,   117,    38,   169,   117,    38,
     169,   117,    38,   117
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    94,    95,    95,    95,    95,    96,    96,    96,    97,
      97,    97,    98,    98,    98,    98,    98,    98,    98,    98,
      98,    98,    98,    98,    98,    98,    98,    98,    98,    98,
      99,   100,   100,   101,   102,   103,   103,   103,   103,   103,
     103,   103,   103,   104,   104,   105,   106,   106,   107,   108,
     108,   108,   108,   109,   109,   110,   111,   111,   112,   112,
     113,   113,   114,   114,   114,   114,   114,   114,   114,   114,
     114,   114,   114,   114,   114,   114,   114,   114,   114,   114,
     114,   115,   115,   116,   116,   117,   117,   117,   117,   117,
     117,   117,   117,   117,   118,   118,   119,   119,   119,   119,
     119,   119,   119,   119,   119,   120,   120,   120,   120,   120,
     121,   121,   121,   121,   122,   122,   122,   122,   122,   122,
     122,   122,   122,   123,   123,   124,   125,   125,   126,   126,
     127,   127,   127,   127,   127,   127,   127,   127,   127,   127,
     127,   127,   127,   127,   127,   127,   127,   127,   127,   127,
     127,   127,   127,   127,   127,   127,   127,   127,   127,   127,
     127,   127,   127,   127,   127,   127,   127,   127,   127,   127,
     127,   127,   127,   127,   127,   127,   127,   127,   127,   127,
     127,   127,   127,   127,   127,   127,   127,   128,   128,   129,
     130,   130,   131,   132,   132,   133,   133,   134,   134,   134,
     135,   135,   136,   136,   137,   137,   138,   138,   139,   139,
     140,   140,   140,   140,   140,   140,   140,   140,   140,   140,
     140,   140,   140,   140,   140,   140,   140,   140,   140,   140,
     140,   140,   140,   140,   141,   141,   141,   141,   141,   142,
     143,   143,   144,   144,   145,   145,   146,   147,   147,   147,
     148,   148,   148,   148,   148,   148,   148,   148,   148,   148,
     148,   148,   148,   148,   149,   149,   149,   149,   150,   150,
     151,   151,   151,   152,   152,   152,   153,   153,   154,   155,
     155,   156,   156,   156,   156,   156,   156,   156,   156,   156,
     156,   156,   156,   156,   156,   156,   156,   156,   156,   156,
     156,   156,   157,   157,   158,   158,   159,   159,   159,   160,
     160,   160,   160,   160,   160,   160,   160,   160,   160,   160,
     160,   160,   160,   160,   160,   160,   160,   161,   161,   162,
     162,   163,   163,   164,   164,   165,   165,   166,   166,   167,
     167,   168,   168,   168,   168,   169
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     4,     2,     2,     3,     4,     1,     0,
       1,     2,     1,     1,     1,     1,     1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     1,
       2,     4,     4,     3,     3,     5,     7,     7,     9,     3,
       5,     5,     7,     1,     3,     3,     1,     2,     2,     3,
       5,     5,     7,     1,     2,     2,     1,     3,     1,     2,
       1,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     2,     4,     4,     2,     3,     3,
       3,     3,     3,     1,     6,     1,     3,     3,     3,     3,
       3,     3,     3,     3,     1,     3,     3,     3,     2,     1,
       3,     3,     3,     1,     1,     4,     5,     4,     3,     4,
       4,     6,     3,     3,     1,     3,     2,     1,     4,     2,
       4,     1,     5,     4,     7,     9,     3,     4,     6,     5,
       5,     5,     5,     3,     6,     8,     3,     4,     2,     1,
       1,     2,     6,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     2,     1,     3,
       3,     1,     4,     2,     0,     3,     1,     1,     1,     1,
       1,     2,     2,     1,     2,     1,     4,     6,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     4,     3,     5,     5,     3,
       4,     3,     4,     1,     1,     3,     4,     3,     4,     1,
       1,     0,     3,     1,     3,     1,     3,     0,     3,     5,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     2,     2,     1,     1,     3,
       3,     5,     5,     0,     1,     3,     3,     1,     3,     1,
       3,     2,     3,     3,     3,     7,     9,     7,     7,     9,
       7,     5,     5,     5,     5,     7,     7,     9,     9,     7,
       7,     5,     1,     2,     2,     1,     3,     1,     1,     1,
       3,     2,     3,     7,     3,     3,     3,     3,     2,     1,
       1,     4,     3,     3,     4,     1,     3,     1,     1,     1,
       3,     1,     5,     1,     3,     1,     3,     3,     3,     5,
       3,     5,     3,     3,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static unsigned
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  unsigned res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
 }

#  define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (yylocationp);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                       , &(yylsp[(yyi + 1) - (yynrhs)])                       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.
       'yyls': related to locations.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

    /* The locations where the error started and ended.  */
    YYLTYPE yyerror_range[3];

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  yylsp[0] = yylloc;
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yyls1, yysize * sizeof (*yylsp),
                    &yystacksize);

        yyls = yyls1;
        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 492 "hexpr.y" /* yacc.c:1646  */
    { yyParsedModule = (yyvsp[0].module);                     }
#line 2677 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 3:
#line 493 "hexpr.y" /* yacc.c:1646  */
    { yyParsedVar    = *(yyvsp[-2].string); yyParsedExpr = (yyvsp[0].exp); }
#line 2683 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 4:
#line 494 "hexpr.y" /* yacc.c:1646  */
    { yyParsedVar    = "";  yyParsedExpr = (yyvsp[0].exp); }
#line 2689 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 5:
#line 495 "hexpr.y" /* yacc.c:1646  */
    { yyParsedExpr   = (yyvsp[0].exp);                     }
#line 2695 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 6:
#line 498 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = (yyvsp[0].module); (yyval.module)->setOption(*(yyvsp[-1].string), m((yylsp[-2]))); }
#line 2701 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 7:
#line 499 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = new Module(*(yyvsp[-2].string), *(yyvsp[0].mdefs)); }
#line 2707 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 8:
#line 500 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = new Module(freshName(), *(yyvsp[0].mdefs)); }
#line 2713 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 9:
#line 502 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); }
#line 2719 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 10:
#line 503 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2725 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 11:
#line 504 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = (yyvsp[-1].mdefs);                            (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2731 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 12:
#line 506 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2737 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 13:
#line 507 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2743 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 14:
#line 508 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mvtydef); }
#line 2749 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 15:
#line 509 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2755 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 16:
#line 510 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2761 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 17:
#line 512 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2767 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 18:
#line 513 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 2773 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 19:
#line 514 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 2779 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 20:
#line 515 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 2785 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 21:
#line 516 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 2791 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 22:
#line 517 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 2797 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 23:
#line 518 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 2803 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 24:
#line 519 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-9]), (yylsp[0]))); }
#line 2809 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 25:
#line 520 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-10]), (yylsp[0]))); }
#line 2815 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 26:
#line 521 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-11]), (yylsp[0]))); }
#line 2821 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 27:
#line 522 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-12]), (yylsp[0]))); }
#line 2827 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 28:
#line 523 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-13]), (yylsp[0]))); }
#line 2833 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 29:
#line 526 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(freshName()), let(freshName(), ExprPtr((yyvsp[0].exp)), mktunit(m((yylsp[0]))), m((yylsp[0]))), m((yylsp[0]))); }
#line 2839 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 30:
#line 529 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MImport(yyModulePath, *(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 2845 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 31:
#line 532 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MTypeDef(MTypeDef::Transparent, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 2851 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 32:
#line 533 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MTypeDef(MTypeDef::Opaque, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 2857 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 33:
#line 536 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydef) = new MVarTypeDef(*(yyvsp[-2].string), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]), (yylsp[0]))); }
#line 2863 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 34:
#line 538 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdef) = new MVarDef(*(yyvsp[-2].strings), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2869 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 35:
#line 541 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-3].tconstraints), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2875 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 36:
#line 542 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-6]), (yylsp[0]))); wantIndent(false); }
#line 2881 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 37:
#line 543 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2887 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 38:
#line 544 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-7].tconstraints), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-8]), (yylsp[0])));            wantIndent(false); }
#line 2893 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 39:
#line 545 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2899 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 40:
#line 546 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2905 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 41:
#line 547 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-4]), (yylsp[0])));            wantIndent(false); }
#line 2911 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 42:
#line 548 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2917 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 43:
#line 550 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundeps) = autorelease(new CFunDepDefs()); (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2923 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 44:
#line 551 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundeps) = (yyvsp[-2].fundeps);                             (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2929 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 45:
#line 553 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundep) = autorelease(new CFunDepDef(*(yyvsp[-2].strings), *(yyvsp[0].strings))); }
#line 2935 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 46:
#line 555 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydefs) = autorelease(new MVarTypeDefs()); (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2941 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 47:
#line 556 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydefs) = (yyvsp[-1].mvtydefs);                              (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2947 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 48:
#line 558 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydef) = (yyvsp[0].mvtydef); }
#line 2953 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 49:
#line 561 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2959 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 50:
#line 562 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-3].tconstraints),           *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2965 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 51:
#line 563 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-4]), (yylsp[0])));        wantIndent(false); }
#line 2971 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 52:
#line 564 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-5].tconstraints),           *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-6]), (yylsp[0])));        wantIndent(false); }
#line 2977 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 53:
#line 566 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdefs) = autorelease(new MVarDefs()); (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2983 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 54:
#line 567 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdefs) = (yyvsp[-1].mvdefs);                          (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2989 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 55:
#line 569 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdef) = (yyvsp[0].mvdef); }
#line 2995 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 56:
#line 572 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[0].strings); }
#line 3001 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 57:
#line 574 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[-1].string)); (yyval.strings)->push_back(*(yyvsp[-2].string)); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3007 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 58:
#line 576 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3013 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 59:
#line 577 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3019 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 60:
#line 579 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 3025 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 61:
#line 581 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[-1].string); }
#line 3031 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 62:
#line 583 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("and")); }
#line 3037 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 63:
#line 584 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("or")); }
#line 3043 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 64:
#line 585 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3049 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 65:
#line 586 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3055 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 66:
#line 587 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("~")); }
#line 3061 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 67:
#line 588 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("=~")); }
#line 3067 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 68:
#line 589 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("===")); }
#line 3073 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 69:
#line 590 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("==")); }
#line 3079 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 70:
#line 591 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("<")); }
#line 3085 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 71:
#line 592 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("<=")); }
#line 3091 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 72:
#line 593 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(">")); }
#line 3097 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 73:
#line 594 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(">=")); }
#line 3103 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 74:
#line 595 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("in")); }
#line 3109 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 75:
#line 596 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("append")); }
#line 3115 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 76:
#line 597 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("+")); }
#line 3121 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 77:
#line 598 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("-")); }
#line 3127 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 78:
#line 599 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("*")); }
#line 3133 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 79:
#line 600 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("/")); }
#line 3139 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 80:
#line 601 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("%")); }
#line 3145 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 81:
#line 603 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3151 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 82:
#line 604 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3157 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 83:
#line 606 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3163 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 84:
#line 607 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes);                           (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3169 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 85:
#line 610 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3175 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 86:
#line 611 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3181 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 87:
#line 612 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), (yyvsp[0].exp), m((yylsp[-1]),(yylsp[0]))); }
#line 3187 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 88:
#line 613 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("and",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3193 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 89:
#line 614 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("or",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3199 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 90:
#line 615 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("compose",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3205 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 91:
#line 616 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assign(ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3211 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 92:
#line 617 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("in",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3217 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 93:
#line 618 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3223 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 94:
#line 620 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP3(var("if",m((yylsp[-5]))), (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-5]), (yylsp[0]))); }
#line 3229 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 95:
#line 621 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3235 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 96:
#line 623 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("~",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3241 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 97:
#line 624 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("===",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3247 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 98:
#line 625 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3253 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 99:
#line 626 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))), m((yylsp[-2]),(yylsp[0]))); }
#line 3259 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 100:
#line 627 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("<",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3265 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 101:
#line 628 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("<=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3271 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 102:
#line 629 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var(">",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3277 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 103:
#line 630 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var(">=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3283 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 104:
#line 631 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3289 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 105:
#line 633 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("+",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3295 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 106:
#line 634 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("-",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3301 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 107:
#line 635 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("append",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3307 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 108:
#line 636 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("neg",m((yylsp[-1]))), ExprPtr((yyvsp[0].exp)), m((yylsp[-1]),(yylsp[0]))); }
#line 3313 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 109:
#line 637 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3319 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 110:
#line 639 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("*", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3325 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 111:
#line 640 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("/", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3331 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 112:
#line 641 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("%", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3337 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 113:
#line 642 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3343 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 114:
#line 644 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3349 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 115:
#line 647 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-2].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3355 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 116:
#line 648 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]),(yylsp[0])))->clone(); }
#line 3361 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 117:
#line 651 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileMatch(yyParseCC, *(yyvsp[-2].exps), normPatternRules(*(yyvsp[0].patternexps), m((yylsp[-3]),(yylsp[0]))), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3367 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 118:
#line 654 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileMatchTest(yyParseCC, ExprPtr((yyvsp[-2].exp)), PatternPtr((yyvsp[0].pattern)), m((yylsp[-2]),(yylsp[0])))->clone(); }
#line 3373 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 119:
#line 657 "hexpr.y" /* yacc.c:1646  */
    {
        try {
          (yyval.exp) = makeParser(yyParseCC, *(yyvsp[-1].prules), m((yylsp[-3]),(yylsp[0])))->clone();
        } catch (hobbes::compile_table_failure& ctf) {
          std::ostringstream ss;
          ss << ctf.what() << std::endl;
          ctf.print(ss);
          throw annotated_error(m((yylsp[-3]),(yylsp[0])), ss.str());
        }
      }
#line 3388 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 120:
#line 669 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-1].letbindings), ExprPtr(new Unit(m((yylsp[-3]),(yylsp[0])))), m((yylsp[-3]),(yylsp[0]))); }
#line 3394 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 121:
#line 670 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[-1].exp)), m((yylsp[-5]),(yylsp[0]))); }
#line 3400 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 122:
#line 673 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assump(ExprPtr((yyvsp[-2].exp)), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]),(yylsp[0]))); }
#line 3406 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 123:
#line 675 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].letbindings)->push_back(*(yyvsp[0].letbinding)); (yyval.letbindings) = (yyvsp[-2].letbindings); }
#line 3412 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 124:
#line 676 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3418 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 125:
#line 678 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-2].pattern)), ExprPtr((yyvsp[0].exp)))); }
#line 3424 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 126:
#line 680 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = (yyvsp[-1].letbindings); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3430 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 127:
#line 681 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3436 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 128:
#line 683 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)))); }
#line 3442 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 129:
#line 684 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr(new MatchAny("_",m((yylsp[-1])))), ExprPtr((yyvsp[-1].exp)))); }
#line 3448 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 130:
#line 687 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].exps), m((yylsp[-3]), (yylsp[0]))); }
#line 3454 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 131:
#line 688 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = varCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3460 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 132:
#line 691 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("range", m((yylsp[-2]))), list(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]), (yylsp[0]))); }
#line 3466 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 133:
#line 692 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("iterateS", m((yylsp[-1]))), list(ExprPtr((yyvsp[-2].exp)), fn(str::strings(".x"), fncall(var("+", m((yylsp[-1]))), list(var(".x", m((yylsp[-1]))), ExprPtr(new Int(1, m((yylsp[-1]))))), m((yylsp[-1]))), m((yylsp[-1])))), m((yylsp[-3]), (yylsp[0]))); }
#line 3472 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 134:
#line 693 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileArrayComprehension(ExprPtr((yyvsp[-5].exp)), PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 3478 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 135:
#line 694 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileArrayComprehension(ExprPtr((yyvsp[-7].exp)), PatternPtr((yyvsp[-5].pattern)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 3484 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 136:
#line 695 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkArray(*(yyvsp[-1].exps), m((yylsp[-2]), (yylsp[0]))); }
#line 3490 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 137:
#line 696 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new AIndex(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3496 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 138:
#line 697 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("slice", m((yylsp[-2]))), list(ExprPtr((yyvsp[-5].exp)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-5]), (yylsp[0]))); }
#line 3502 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 139:
#line 698 "hexpr.y" /* yacc.c:1646  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-1]))), list(var(vn,m((yylsp[-4]))), ExprPtr((yyvsp[-2].exp)), fncall(var("size",m((yylsp[-1]))), list(var(vn,m((yylsp[-4])))),m((yylsp[-4])))),m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3508 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 140:
#line 699 "hexpr.y" /* yacc.c:1646  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-2]))), list(var(vn,m((yylsp[-4]))), fncall(var("size",m((yylsp[-2]))), list(var(vn,m((yylsp[-2])))),m((yylsp[-4]))), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3514 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 141:
#line 702 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-3].string), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3520 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 142:
#line 703 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(".f" + str::from((yyvsp[-3].intv)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3526 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 143:
#line 704 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-1].string), ExprPtr(new Unit(m((yylsp[-1])))), m((yylsp[-2]), (yylsp[0]))); }
#line 3532 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 144:
#line 705 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-4].exp)), *(yyvsp[-1].vfields), m((yylsp[-5]), (yylsp[0]))); }
#line 3538 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 145:
#line 706 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-6].exp)), *(yyvsp[-3].vfields), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3544 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 146:
#line 709 "hexpr.y" /* yacc.c:1646  */
    { if ((yyvsp[-1].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-1].rfields), m((yylsp[-2]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-2]), (yylsp[0]))); } }
#line 3550 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 147:
#line 710 "hexpr.y" /* yacc.c:1646  */
    { if ((yyvsp[-2].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-2].rfields), m((yylsp[-3]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-3]), (yylsp[0]))); } }
#line 3556 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 148:
#line 711 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makeProjSeq((yyvsp[-1].exp), *(yyvsp[0].strings), m((yylsp[-1]), (yylsp[0]))); }
#line 3562 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 149:
#line 714 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Fn(str::strings("x"), proj(var("x", m((yylsp[0]))), *(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3568 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 150:
#line 717 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileRegexFn(yyParseCC, std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0])))->clone(); }
#line 3574 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 151:
#line 720 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Pack(ExprPtr((yyvsp[0].exp)), m((yylsp[-1]), (yylsp[0]))); }
#line 3580 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 152:
#line 721 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Unpack(*(yyvsp[-4].string), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3586 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 153:
#line 724 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Bool((yyvsp[0].boolv), m((yylsp[0]))); }
#line 3592 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 154:
#line 725 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3598 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 155:
#line 726 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3604 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 156:
#line 727 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3610 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 157:
#line 728 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Short((yyvsp[0].shortv), m((yylsp[0]))); }
#line 3616 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 158:
#line 729 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Int((yyvsp[0].intv), m((yylsp[0]))); }
#line 3622 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 159:
#line 730 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Long((yyvsp[0].longv), m((yylsp[0]))); }
#line 3628 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 160:
#line 731 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Int128((yyvsp[0].int128v), m((yylsp[0]))); }
#line 3634 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 161:
#line 732 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Float((yyvsp[0].floatv), m((yylsp[0]))); }
#line 3640 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 162:
#line 733 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Double((yyvsp[0].doublev), m((yylsp[0]))); }
#line 3646 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 163:
#line 734 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3652 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 164:
#line 735 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0])))->clone(); }
#line 3658 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 165:
#line 736 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3664 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 166:
#line 737 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3670 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 167:
#line 740 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = pickNestedExp((yyvsp[-1].exps), m((yylsp[-2]),(yylsp[0]))); }
#line 3676 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 168:
#line 743 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("append", m((yylsp[-1]))); }
#line 3682 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 169:
#line 744 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("+",      m((yylsp[-1]))); }
#line 3688 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 170:
#line 745 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("-",      m((yylsp[-1]))); }
#line 3694 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 171:
#line 746 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("*",      m((yylsp[-1]))); }
#line 3700 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 172:
#line 747 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("/",      m((yylsp[-1]))); }
#line 3706 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 173:
#line 748 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("%",      m((yylsp[-1]))); }
#line 3712 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 174:
#line 749 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("~",      m((yylsp[-1]))); }
#line 3718 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 175:
#line 750 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("===",    m((yylsp[-1]))); }
#line 3724 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 176:
#line 751 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("==",     m((yylsp[-1]))); }
#line 3730 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 177:
#line 752 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("!=",     m((yylsp[-1]))); }
#line 3736 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 178:
#line 753 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("<",      m((yylsp[-1]))); }
#line 3742 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 179:
#line 754 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var(">",      m((yylsp[-1]))); }
#line 3748 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 180:
#line 755 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var(">=",     m((yylsp[-1]))); }
#line 3754 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 181:
#line 756 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("<=",     m((yylsp[-1]))); }
#line 3760 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 182:
#line 757 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("and",    m((yylsp[-1]))); }
#line 3766 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 183:
#line 758 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("or",     m((yylsp[-1]))); }
#line 3772 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 184:
#line 759 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("in",     m((yylsp[-1]))); }
#line 3778 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 185:
#line 760 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("not",    m((yylsp[-1]))); }
#line 3784 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 186:
#line 763 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assump(fncall(var("unsafeCast", m((yylsp[-1]))), list(mktunit(m((yylsp[-1])))), m((yylsp[-1]))), qualtype(tapp(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp)))))), m((yylsp[-1]))); }
#line 3790 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 187:
#line 765 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prules) = (yyvsp[-1].prules); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3796 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 188:
#line 766 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prules) = autorelease(new Grammar()); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3802 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 189:
#line 768 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prule) = autorelease(new Grammar::value_type(*(yyvsp[-2].string), *(yyvsp[0].prdefs))); }
#line 3808 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 190:
#line 770 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdefs) = (yyvsp[-2].prdefs); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3814 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 191:
#line 771 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdefs) = autorelease(new GrammarRules()); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3820 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 192:
#line 773 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdef) = autorelease(new GrammarRule(*(yyvsp[-3].pbelems), ExprPtr((yyvsp[-1].exp)))); }
#line 3826 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 193:
#line 775 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelems) = (yyvsp[-1].pbelems); (yyval.pbelems)->push_back(*(yyvsp[0].pbelem)); }
#line 3832 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 194:
#line 776 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelems) = autorelease(new BoundGrammarValues()); }
#line 3838 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 195:
#line 778 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue(*(yyvsp[-2].string), GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3844 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 196:
#line 779 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue("_", GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3850 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 197:
#line 781 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GSymRef(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3856 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 198:
#line 782 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GStr(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3862 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 199:
#line 783 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GStr(std::string(1, str::readCharDef(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3868 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 200:
#line 785 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3874 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 201:
#line 786 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3880 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 202:
#line 788 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = (yyvsp[-1].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3886 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 203:
#line 789 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3892 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 204:
#line 791 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexps) = (yyvsp[-1].patternexps); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3898 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 205:
#line 792 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexps) = autorelease(new PatternRows()); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3904 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 206:
#line 794 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)))); }
#line 3910 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 207:
#line 795 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-4].patterns), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)))); }
#line 3916 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 208:
#line 798 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[-1].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3922 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 209:
#line 799 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = autorelease(new Patterns()); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3928 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 210:
#line 801 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Bool((yyvsp[0].boolv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3934 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 211:
#line 802 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3940 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 212:
#line 803 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3946 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 213:
#line 804 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Short((yyvsp[0].shortv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3952 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 214:
#line 805 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int((yyvsp[0].intv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3958 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 215:
#line 806 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Long((yyvsp[0].longv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3964 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 216:
#line 807 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int128((yyvsp[0].int128v), m((yylsp[0])))), m((yylsp[0]))); }
#line 3970 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 217:
#line 808 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Double((yyvsp[0].doublev), m((yylsp[0])))), m((yylsp[0]))); }
#line 3976 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 218:
#line 809 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = mkpatarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3982 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 219:
#line 810 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = mkpatarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3988 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 220:
#line 811 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkTimespanPrim(*(yyvsp[0].strings), m((yylsp[0]))), mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3994 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 221:
#line 812 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4000 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 222:
#line 813 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkDateTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4006 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 223:
#line 814 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRegex(std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0]))); }
#line 4012 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 224:
#line 815 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4018 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 225:
#line 816 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4024 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 226:
#line 817 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-1].string), PatternPtr(new MatchLiteral(PrimitivePtr(new Unit(m((yylsp[-1])))), m((yylsp[-1])))), m((yylsp[-2]),(yylsp[0]))); }
#line 4030 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 227:
#line 818 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-3].string), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4036 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 228:
#line 819 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(".f" + str::from((yyvsp[-3].intv)), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4042 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 229:
#line 820 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4048 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 230:
#line 821 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4054 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 231:
#line 822 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4060 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 232:
#line 823 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4066 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 233:
#line 824 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = patVarCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4072 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 234:
#line 826 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchAny(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4078 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 235:
#line 827 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4084 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 236:
#line 828 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4090 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 237:
#line 829 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4096 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 238:
#line 830 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4102 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 239:
#line 832 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = (yyvsp[0].pattern); }
#line 4108 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 240:
#line 834 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[0].patterns); }
#line 4114 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 241:
#line 835 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = new Patterns(); }
#line 4120 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 242:
#line 837 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[-2].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4126 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 243:
#line 838 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = new Patterns(); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4132 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 244:
#line 840 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfields) = (yyvsp[-2].recpatfields); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4138 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 245:
#line 841 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfields) = new MatchRecord::Fields(); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4144 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 246:
#line 843 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfield) = new MatchRecord::Field(*(yyvsp[-2].string), PatternPtr((yyvsp[0].pattern))); }
#line 4150 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 247:
#line 845 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); }
#line 4156 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 248:
#line 846 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4162 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 249:
#line 847 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = (yyvsp[-4].rfields);                                     (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4168 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 250:
#line 849 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4174 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 251:
#line 850 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("data")); }
#line 4180 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 252:
#line 851 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("type")); }
#line 4186 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 253:
#line 852 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("where")); }
#line 4192 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 254:
#line 853 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("class")); wantIndent(false); }
#line 4198 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 255:
#line 854 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("instance")); wantIndent(false); }
#line 4204 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 256:
#line 855 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("exists")); }
#line 4210 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 257:
#line 856 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("import")); }
#line 4216 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 258:
#line 857 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("module")); }
#line 4222 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 259:
#line 858 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("parse")); }
#line 4228 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 260:
#line 859 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("do")); }
#line 4234 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 261:
#line 860 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("return")); }
#line 4240 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 262:
#line 861 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("fn")); }
#line 4246 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 263:
#line 862 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(".f" + str::from((yyvsp[0].intv)))); }
#line 4252 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 264:
#line 864 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-2].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4258 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 265:
#line 865 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings); str::seq x = tupSectionFields(*(yyvsp[0].string)); (yyval.strings)->insert((yyval.strings)->end(), x.begin(), x.end()); }
#line 4264 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 266:
#line 866 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4270 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 267:
#line 867 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); *(yyval.strings) = tupSectionFields(*(yyvsp[0].string)); }
#line 4276 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 268:
#line 869 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vfields) = autorelease(new Case::Bindings()); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4282 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 269:
#line 870 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vfields) = (yyvsp[-2].vfields); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4288 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 270:
#line 872 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-2].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4294 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 271:
#line 873 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-4].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4300 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 272:
#line 874 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(".f" + str::from((yyvsp[-4].intv)), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4306 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 273:
#line 876 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); }
#line 4312 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 274:
#line 877 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4318 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 275:
#line 878 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].exps)->push_back(ExprPtr((yyvsp[0].exp))); (yyval.exps) = (yyvsp[-2].exps); }
#line 4324 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 276:
#line 880 "hexpr.y" /* yacc.c:1646  */
    { (yyval.qualtype) = new QualType(*(yyvsp[-2].tconstraints), *(yyvsp[0].mtype)); }
#line 4330 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 277:
#line 881 "hexpr.y" /* yacc.c:1646  */
    { (yyval.qualtype) = new QualType(Constraints(), *(yyvsp[0].mtype)); }
#line 4336 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 278:
#line 884 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraints) = (yyvsp[-1].tconstraints); }
#line 4342 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 279:
#line 886 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraints) = autorelease(new Constraints()); (yyval.tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); }
#line 4348 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 280:
#line 887 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); (yyval.tconstraints) = (yyvsp[-2].tconstraints); }
#line 4354 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 281:
#line 889 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(*(yyvsp[-1].string), *(yyvsp[0].mtypes)); }
#line 4360 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 282:
#line 890 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(EqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4366 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 283:
#line 891 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(NotEqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4372 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 284:
#line 892 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(FixIsoRecur::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4378 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 285:
#line 893 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4384 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 286:
#line 894 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4390 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 287:
#line 895 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4396 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 288:
#line 896 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4402 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 289:
#line 897 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4408 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 290:
#line 898 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(1), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4414 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 291:
#line 900 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4420 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 292:
#line 901 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4426 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 293:
#line 902 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4432 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 294:
#line 903 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4438 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 295:
#line 905 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4444 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 296:
#line 906 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4450 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 297:
#line 907 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4456 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 298:
#line 908 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4462 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 299:
#line 910 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), TString::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype))); }
#line 4468 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 300:
#line 911 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), *(yyvsp[-5].mtype),                *(yyvsp[-3].mtype))); }
#line 4474 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 301:
#line 912 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(AppendsToUnqualifier::constraintName(), list(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4480 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 302:
#line 914 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4486 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 303:
#line 915 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-1].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4492 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 304:
#line 917 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4498 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 305:
#line 918 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4504 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 306:
#line 920 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Func::make(tuplety(*(yyvsp[-2].mtypes)), *(yyvsp[0].mtype)))); }
#line 4510 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 307:
#line 921 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeTupleType(*(yyvsp[0].mtypes)))); }
#line 4516 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 308:
#line 922 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeSumType(*(yyvsp[0].mtypes)))); }
#line 4522 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 309:
#line 924 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(monoTypeByName(*(yyvsp[0].string)))); }
#line 4528 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 310:
#line 925 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(OpaquePtr::make(str::replace<char>(*(yyvsp[-1].string), ".", "::"), 0, false))); }
#line 4534 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 311:
#line 926 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("[]"))); }
#line 4540 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 312:
#line 927 "hexpr.y" /* yacc.c:1646  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(Array::make(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4546 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 313:
#line 928 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(FixedArray::make(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype)))); }
#line 4552 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 314:
#line 929 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("->"))); }
#line 4558 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 315:
#line 930 "hexpr.y" /* yacc.c:1646  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(clone(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4564 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 316:
#line 931 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeRecType(*(yyvsp[-1].mreclist)))); }
#line 4570 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 317:
#line 932 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeVarType(*(yyvsp[-1].mvarlist)))); }
#line 4576 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 318:
#line 933 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("unit"))); }
#line 4582 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 319:
#line 934 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(((yyvsp[0].intv) == 0) ? Prim::make("void") : TLong::make((yyvsp[0].intv)))); }
#line 4588 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 320:
#line 935 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr((yyvsp[0].boolv) ? TLong::make(1) : TLong::make(0))); }
#line 4594 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 321:
#line 936 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Exists::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4600 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 322:
#line 937 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype), *(yyvsp[0].mtype)))); }
#line 4606 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 323:
#line 938 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype)))); }
#line 4612 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 324:
#line 939 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Recursive::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4618 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 325:
#line 940 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TString::make(str::unescape(str::trimq(*(yyvsp[0].string)))))); }
#line 4624 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 326:
#line 941 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp))))))); }
#line 4630 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 327:
#line 943 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TVar::make(*(yyvsp[0].string)))); }
#line 4636 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 328:
#line 944 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TLong::make((yyvsp[0].intv)))); }
#line 4642 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 329:
#line 946 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4648 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 330:
#line 947 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[-2].string); *(yyval.string) += "."; *(yyval.string) += *(yyvsp[0].string); }
#line 4654 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 331:
#line 949 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4660 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 332:
#line 950 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-1].mtypes)->insert((yyvsp[-1].mtypes)->begin(), *(yyvsp[-3].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4666 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 333:
#line 952 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4672 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 334:
#line 953 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-2].mtypes); }
#line 4678 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 335:
#line 955 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4684 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 336:
#line 956 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4690 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 337:
#line 958 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[-2].mtype)); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4696 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 338:
#line 959 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4702 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 339:
#line 961 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mreclist) = (yyvsp[-4].mreclist);                                 (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4708 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 340:
#line 962 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mreclist) = autorelease(new Record::Members()); (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4714 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 341:
#line 964 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = (yyvsp[-4].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4720 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 342:
#line 965 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = (yyvsp[-2].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4726 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 343:
#line 966 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4732 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 344:
#line 967 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4738 "hexpr.parse.C" /* yacc.c:1646  */
    break;


#line 4742 "hexpr.parse.C" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }

  yyerror_range[1] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  yyerror_range[1] = yylsp[1-yylen];
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, yyerror_range, 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 971 "hexpr.y" /* yacc.c:1906  */

#pragma GCC diagnostic pop


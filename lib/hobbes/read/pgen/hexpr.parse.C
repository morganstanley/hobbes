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

#line 524 "hexpr.parse.C" /* yacc.c:355  */
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

#line 553 "hexpr.parse.C" /* yacc.c:358  */

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
#define YYFINAL  70
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2855

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  93
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  76
/* YYNRULES -- Number of rules.  */
#define YYNRULES  344
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  761

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   347

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
      85,    86,    87,    88,    89,    90,    91,    92
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   491,   491,   492,   493,   494,   497,   498,   500,   501,
     502,   504,   505,   506,   507,   508,   510,   511,   512,   513,
     514,   515,   516,   517,   518,   519,   520,   521,   524,   527,
     530,   531,   534,   536,   539,   540,   541,   542,   543,   544,
     545,   546,   548,   549,   551,   553,   554,   556,   559,   560,
     561,   562,   564,   565,   567,   570,   572,   574,   575,   577,
     579,   581,   582,   583,   584,   585,   586,   587,   588,   589,
     590,   591,   592,   593,   594,   595,   596,   597,   598,   599,
     601,   602,   604,   605,   608,   609,   610,   611,   612,   613,
     614,   615,   616,   618,   619,   621,   622,   623,   624,   625,
     626,   627,   628,   629,   631,   632,   633,   634,   635,   637,
     638,   639,   640,   642,   645,   646,   649,   652,   655,   667,
     668,   671,   673,   674,   676,   678,   679,   681,   682,   685,
     686,   689,   690,   691,   692,   693,   694,   695,   696,   697,
     700,   701,   702,   703,   704,   707,   708,   709,   712,   715,
     718,   719,   722,   723,   724,   725,   726,   727,   728,   729,
     730,   731,   732,   733,   734,   735,   738,   741,   742,   743,
     744,   745,   746,   747,   748,   749,   750,   751,   752,   753,
     754,   755,   756,   757,   758,   761,   763,   764,   766,   768,
     769,   771,   773,   774,   776,   777,   779,   780,   781,   783,
     784,   786,   787,   789,   790,   792,   793,   796,   797,   799,
     800,   801,   802,   803,   804,   805,   806,   807,   808,   809,
     810,   811,   812,   813,   814,   815,   816,   817,   818,   819,
     820,   821,   822,   824,   825,   826,   827,   828,   830,   832,
     833,   835,   836,   838,   839,   841,   843,   844,   845,   847,
     848,   849,   850,   851,   852,   853,   854,   855,   856,   857,
     858,   859,   860,   862,   863,   864,   865,   867,   868,   870,
     871,   872,   874,   875,   876,   878,   879,   882,   884,   885,
     887,   888,   889,   890,   891,   892,   893,   894,   895,   896,
     898,   899,   900,   901,   903,   904,   905,   906,   908,   909,
     910,   912,   913,   915,   916,   918,   919,   920,   922,   923,
     924,   925,   926,   927,   928,   929,   930,   931,   932,   933,
     934,   935,   936,   937,   938,   939,   941,   942,   944,   945,
     947,   948,   950,   951,   953,   954,   956,   957,   959,   960,
     962,   963,   964,   965,   967
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "\"domodule\"", "\"dodefn\"",
  "\"doexpr\"", "\"module\"", "\"where\"", "\"import\"", "\"type\"",
  "\"data\"", "\"class\"", "\"instance\"", "\"indent\"", "\"boolV\"",
  "\"charV\"", "\"byteV\"", "\"bytesV\"", "\"shortV\"", "\"intV\"",
  "\"longV\"", "\"int128V\"", "\"floatV\"", "\"doubleV\"", "\"id\"",
  "\"stringV\"", "\"regexV\"", "\"timespanV\"", "\"timeV\"",
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
     345,   346,   347
};
# endif

#define YYPACT_NINF -579

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-579)))

#define YYTABLE_NINF -344

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     411,  1219,  1985,  1985,   100,   128,   128,    77,    77,    84,
      84,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,    39,     8,
    1985,  2687,   102,  2687,   128,   123,  1438,  1985,    39,   129,
    1985,  -579,  1303,  -579,  -579,  -579,  -579,  -579,  -579,   196,
    -579,   254,   214,   154,   239,  2453,  2297,  1985,  1517,  2776,
    2776,   359,     7,   551,   523,   530,  -579,   207,   359,  -579,
    -579,   249,   270,  -579,  2605,   105,  -579,  -579,   107,  1182,
     288,    77,   322,  1789,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  2776,
     128,   164,  -579,   351,  -579,   260,   194,  2609,   128,   194,
     358,  2063,   343,   368,  2375,   379,   393,   415,    39,   451,
     466,   482,   501,   518,   533,   538,   539,  2219,   540,   547,
     549,  -579,  -579,   552,   359,   394,   267,   106,   -24,   546,
     588,     4,   216,  -579,  1867,  1867,  2776,  1985,  1751,   154,
    -579,  -579,    39,  1985,   211,  -579,  -579,   242,   343,   368,
    2375,   379,   393,   415,   451,   466,   482,   518,   533,   538,
     539,   540,   547,   549,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,  -579,  2776,  2776,   128,
     509,   214,   810,  -579,  -579,  -579,  2710,  1985,  1985,  1985,
    2297,  2297,  2453,  2453,  2453,  2453,  2453,  2453,  2453,  2453,
    2453,  2453,  2453,  2531,  2531,  2531,  1985,  1303,   128,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,  1867,  -579,  1867,  -579,
    -579,  -579,   128,   128,   505,   689,  1945,  1945,   128,  1985,
     410,  -579,   248,  1945,   128,    24,    77,  2605,   128,   505,
     128,   128,    70,  -579,    34,   399,   590,   593,  -579,  -579,
     419,   559,   223,  -579,   610,  1985,   178,  2297,   510,   569,
     194,    10,  -579,   613,  2687,  1595,    39,   284,  1673,  -579,
     632,   633,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  1985,  2776,  1829,  -579,  -579,   182,  1985,  1985,
    1985,  -579,  -579,   886,  -579,   619,  -579,  -579,  -579,   420,
    1985,   186,  -579,   359,  1985,   301,  1985,   446,   157,   491,
     634,   134,  1985,  -579,  1985,   597,   597,  -579,  -579,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  -579,  -579,   359,  1303,  -579,  -579,  -579,   627,
     158,   614,  -579,   603,  -579,    27,  1789,  -579,   987,   505,
      83,   515,   657,   165,   298,    60,   646,   218,  -579,  1182,
     472,  1945,  1945,    39,  1945,  1945,  1945,  1099,  1945,   605,
      77,   680,   128,   128,  1789,   615,   663,   664,   685,  -579,
    1945,  1945,  1945,  1945,  -579,   625,  2776,  -579,    19,  2776,
     359,  1985,  -579,  -579,   517,  2776,   569,  -579,  -579,  -579,
    -579,   193,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  -579,  -579,  -579,  1595,  2141,    39,   529,   214,
    -579,   610,  -579,  1985,  -579,  -579,  1985,   359,   665,  -579,
     310,  -579,   667,   359,   334,   346,   505,   172,  1789,  -579,
     357,  1907,  -579,   359,  1985,   312,   371,  -579,   628,  -579,
     629,  -579,    28,  2776,  2776,  -579,   359,   359,  1945,  -579,
    -579,  -579,  -579,  1945,   622,  -579,  1945,  -579,   128,  1789,
    1945,  1789,  -579,   128,  1789,  1945,  -579,  -579,  1945,  1945,
    1945,    25,    81,    30,   605,   605,   605,  -579,   605,   605,
      26,    77,   680,  -579,    18,  -579,   140,  -579,  -579,   264,
    1789,  1789,  1789,    77,   685,  -579,   605,   605,   605,   605,
    -579,  -579,  -579,  -579,  -579,   359,   673,   467,  -579,   524,
    1396,  -579,   638,  -579,    15,  2687,   675,   148,   303,   382,
    1985,  -579,  1985,  -579,  -579,  -579,  -579,  -579,   407,   359,
    1985,   321,  1985,  -579,  -579,  -579,   639,   640,   605,   282,
     535,   -17,   687,  -579,    94,   305,   645,   692,   647,    40,
     605,    96,   120,   694,    31,   695,  1945,  1945,  1945,  1945,
    1945,   680,   128,  -579,  -579,   680,   128,   128,  -579,   685,
    -579,   458,  -579,  -579,   693,  -579,   128,   672,   517,   128,
    1985,  1985,  1985,  -579,  -579,  -579,  1985,  -579,  -579,   698,
     194,  2141,  2141,  -579,  -579,  -579,   299,   359,  -579,   359,
    1985,   387,   359,  -579,  -579,   697,  -579,   699,  -579,   701,
    1789,  1945,   703,   704,  1789,   705,  1945,  1945,  1945,  1945,
    1945,  1945,   605,   605,   605,   605,   605,   680,    22,   680,
    -579,   128,   685,  -579,  1789,  1985,   708,  1985,  -579,   710,
     359,   103,   359,  -579,   423,   296,  -579,  1985,   359,  1985,
     438,  1945,   659,  1945,  -579,   302,  1945,  1945,  -579,  1945,
     377,   391,   361,   135,   409,    67,   680,  -579,   359,  1985,
     359,  1985,  1985,  -579,  -579,  -579,   439,   359,  1985,   468,
     605,  -579,   605,   711,   605,   605,   605,   712,  -579,  -579,
    1945,  -579,  1945,   680,   359,   359,   359,  -579,   359,  1985,
     480,  1945,  1945,   372,   435,   359,  1985,   484,   605,   605,
    -579,  -579,   359,  1985,   494,   359,  1985,   713,   359,  1985,
     359
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,     8,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   152,   153,   154,   155,   156,   157,   158,   159,   160,
     161,   344,   162,   149,   199,   164,   165,   266,     0,     0,
       0,     0,     0,     0,     0,     0,   272,   272,   246,     0,
       0,     2,     7,     9,    11,    12,    13,    14,    15,     0,
      28,   113,   163,   148,   130,     0,     0,     0,   272,     0,
       0,     4,    92,    94,   103,   108,   112,   130,     5,   130,
       1,     0,    29,   328,     0,     0,    57,    59,     0,     0,
       0,     0,     0,     0,   257,   252,   256,   251,   250,   253,
     254,   262,   255,   258,   259,   260,   261,   265,   249,   240,
       0,     0,   123,     0,   233,     0,   202,     0,     0,   150,
       0,     0,     0,     0,     0,     0,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    66,     0,   273,     0,   273,     0,     0,     0,
       0,     0,     0,    10,     0,     0,     0,   272,     0,   147,
     200,   264,     0,     0,     0,   107,    86,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   209,   210,   211,   217,   212,   213,
     214,   215,   216,   218,   222,   220,   221,   240,   240,     0,
       0,   219,     0,   238,   208,   232,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     8,     0,    74,
      75,    76,    77,    78,    79,    64,    68,    67,    65,    69,
      70,    71,    72,    61,    62,    73,     0,    58,     0,   319,
     318,   324,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   278,     0,   308,     0,    38,    55,    59,     0,     0,
       0,     0,    48,    82,   334,     0,   306,   307,   308,   242,
       0,   239,     0,   244,     0,     0,     0,     0,     0,     0,
     201,     0,   187,     0,     0,   240,   246,     0,     0,   126,
       0,   130,   167,   168,   169,   170,   171,   172,   175,   174,
     173,   176,   177,   180,   178,   179,   184,   181,   182,   183,
      60,   166,     0,     0,     0,   135,   145,     0,     0,     0,
       0,   142,   185,     0,    32,     0,   276,   121,   117,     0,
       0,     0,   263,    16,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   207,     0,    87,    88,    89,    90,    91,
      97,    96,    95,    98,    99,   100,   101,   102,   106,   104,
     105,   109,   110,   111,     3,     6,   329,    30,    31,     0,
       0,     0,   317,     0,   304,   334,     0,   310,     0,     0,
       0,     0,   308,     0,     0,   308,     0,     0,   277,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   280,   301,
       0,     0,     0,     0,     0,   304,     0,   343,     0,    83,
       0,     0,     0,     0,   234,     0,     0,   236,     0,     0,
     114,     0,   122,   124,     0,     0,   116,   204,   118,   186,
     193,     0,   152,   153,   154,   155,   156,   157,   158,   159,
     161,   162,   149,   164,   165,   240,   240,   246,     0,   163,
     130,     0,   128,     0,   119,   125,     0,   274,     0,   132,
       0,   146,     0,   247,     0,     0,     0,   334,     0,   129,
       0,     0,   136,    17,     0,     0,     0,   228,     0,   223,
       0,   230,     0,     0,     0,   225,    84,    85,     0,   309,
     313,   314,   303,     0,     0,   311,     0,   315,     0,     0,
       0,     0,   316,     0,     0,     0,   325,   279,     0,     0,
       0,     0,     0,     0,   281,   283,   282,   322,   321,   302,
      34,     0,    40,    45,    39,    42,     0,    80,    56,    49,
       0,     0,     0,     0,    50,    52,   336,   305,   335,   337,
     235,   241,   237,   243,   245,   115,     0,     0,   267,     0,
       0,   203,   188,   190,     0,     0,     0,     0,     0,     0,
       0,   131,     0,   141,   140,   275,   139,   138,     0,    18,
       0,     0,     0,   229,   224,   231,     0,     0,   320,     0,
       0,     0,     0,   339,   334,     0,     0,   341,   342,   334,
     323,     0,     0,   308,     0,   308,     0,     0,     0,     0,
       0,     0,     0,    47,    46,     0,     0,     0,    81,     0,
     332,     0,   342,    54,     0,    53,     0,   143,     0,     0,
       0,     0,     0,   193,   198,   197,     0,   192,   195,   196,
     151,     0,     0,   142,   120,   127,     0,   248,   137,    19,
       0,     0,    93,   227,   226,     0,   327,     0,   326,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   300,   293,   292,   291,   290,    36,    35,    41,
      43,    44,    51,   331,     0,     0,     0,     0,   268,     0,
     269,     0,   205,   189,     0,     0,   133,     0,    20,     0,
       0,     0,     0,     0,   338,     0,     0,     0,   340,     0,
     336,     0,     0,     0,     0,     0,     0,   333,    33,     0,
     144,     0,     0,   191,   194,   196,     0,    21,     0,     0,
     289,   312,   287,     0,   295,   299,   298,     0,   286,   284,
       0,   294,     0,    37,   271,   270,   206,   134,    22,     0,
       0,     0,     0,     0,     0,    23,     0,     0,   288,   297,
     285,   296,    24,     0,     0,    25,     0,     0,    26,     0,
      27
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -579,  -579,  -579,   519,   -32,  -579,  -579,   220,  -579,  -579,
     144,   146,  -578,  -504,  -579,   147,  -514,  -378,   598,     1,
     496,   151,   355,    -2,   -41,   459,   -36,   352,    11,  -579,
     479,  -579,   473,   -20,  -579,   486,  -579,   137,  -579,  -579,
      85,   -55,  -579,  -579,   347,   -53,  -579,   -90,   -23,  -171,
    -579,  -175,  -392,  -579,   -22,   -49,  -579,   160,   -34,  -121,
     601,  -579,   380,  -579,   527,   -19,   709,  -579,   536,  -579,
    -579,  -579,  -579,  -579,  -579,   398
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,    41,    42,    43,    44,    45,    46,   613,    47,
     524,   525,   522,   523,    48,   534,   535,   255,   256,    49,
     133,   526,   262,   134,    62,    63,    64,    65,    66,   101,
     102,   288,   289,    51,   281,   282,   552,   553,   554,   627,
     628,    52,   107,   426,   427,   192,   193,   103,   269,   270,
     271,   272,   273,   138,   139,    53,   547,   548,   135,   324,
     325,   250,   251,   398,   373,   326,   264,   647,    72,   265,
     611,   266,   267,   381,   384,    69
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      61,    68,   149,   137,   191,   191,    97,   196,    76,    76,
     143,   106,    50,   109,   339,   156,   337,   338,   604,   155,
     615,   290,   520,   667,   327,   605,   543,   669,   105,   706,
     624,   401,    21,   601,    21,   136,   194,   194,   142,    21,
     625,   320,   200,    21,   191,    84,    85,    86,    87,    88,
      89,    90,    21,    50,   316,   157,   317,   149,    91,  -330,
     149,   649,   596,    21,   263,   599,  -330,   410,   600,   493,
     397,   660,  -330,    92,   410,   201,   237,   408,   155,   237,
     656,    99,    76,   321,   239,   100,   156,   280,   428,   240,
     543,   191,   626,   504,    21,   241,    97,   542,   606,    93,
      70,    21,   606,   402,   242,   602,   575,   732,    21,   287,
      94,    95,   397,   329,   397,   367,   597,   368,   397,   598,
     243,   397,    96,   328,   155,   496,  -330,   397,   733,    21,
     332,    21,   191,   191,   410,   712,   651,   191,   657,  -343,
    -343,   191,   236,   259,   238,   245,   331,   260,   140,   261,
      74,   333,    21,    21,   397,   614,   248,    79,   615,   348,
     349,   249,   658,   604,    21,   604,   197,   198,   397,   343,
     397,   484,   607,   343,   358,   359,   360,   730,    74,   108,
      74,   397,   315,   397,   151,   632,   312,   199,    84,    85,
      86,    87,    88,    89,    90,   345,   346,   347,   290,   152,
     111,    91,    21,   218,  -330,   500,    21,   397,   501,   390,
     489,   391,   410,   485,   364,   392,    92,   393,   394,   471,
     395,   396,   397,    27,    27,   374,   374,   633,    50,   604,
     449,   149,   275,   479,   144,    21,   423,   480,    28,    28,
     405,   150,    93,   409,   216,   276,   421,   387,   334,   197,
     198,    99,   397,    94,    95,   100,   217,   237,   191,   397,
     461,   555,   472,    21,   431,    96,   147,   147,   148,   148,
     199,   609,   339,   420,   337,   338,   153,   -59,   239,   197,
     198,   197,   198,   240,    27,   390,   287,   391,    21,   241,
     458,   392,   145,   393,   394,   462,   395,   396,   242,    28,
     199,   417,   199,   418,   405,   197,   198,   322,   336,   506,
     457,   624,   460,   146,   243,   218,   463,   464,   465,   254,
      21,   625,   278,   197,   198,    21,   199,   147,   470,   148,
     197,   198,   473,   143,   476,   397,    21,   259,   474,   245,
     486,   260,   487,   261,   199,    21,   313,   197,   198,   570,
     248,   199,   314,   258,   492,   249,   645,   494,   640,   492,
     374,   191,   197,   198,   191,   452,   197,   198,   199,   397,
     191,   513,   550,   197,   198,   686,    50,   502,   503,   687,
     723,   634,   149,   199,   652,   263,   561,   199,   277,   397,
     449,   449,   397,   541,   199,   284,   544,   197,   198,    54,
      67,    76,   194,    71,    73,    77,    77,    81,    83,   197,
     198,    21,   137,   563,     1,     2,     3,   292,   199,   545,
     197,   198,   197,   198,   689,   564,    98,   104,   191,   191,
     199,   411,   110,   566,   197,   198,    98,   141,   572,   729,
      54,   199,   293,   199,   136,   197,   198,   405,   397,   565,
     750,   558,   154,   295,   559,   199,   727,   195,   195,   397,
     576,   577,    21,   635,   397,   728,   199,   296,   311,   568,
     197,   198,   569,    77,   312,   718,    77,   253,   397,   257,
     583,   268,   586,   638,   388,   588,   197,   198,   731,   297,
     389,   199,    21,   414,   469,   191,   397,   195,   274,   415,
     312,   713,   197,   198,    21,   739,   283,   199,    21,   291,
     409,   610,   583,   612,   751,   737,    98,   746,    21,   239,
     477,   753,   397,   199,   240,   298,   478,   343,   340,    21,
     241,   756,   673,    21,    76,   630,   546,   371,   674,   242,
     299,    21,   268,   268,   195,   508,   617,   618,   556,   509,
      98,   510,   335,    21,   646,   243,   300,   619,   636,    21,
     637,   620,   210,   211,   212,   361,   362,   363,   639,   481,
     642,   482,   213,   214,   215,   301,   449,   449,   259,   372,
     245,   149,   260,   318,   261,   195,   195,   274,   341,   424,
     195,   248,   302,   497,   195,   498,   249,   202,   203,   204,
     205,   206,   207,   208,   209,    75,    78,   303,   576,   577,
      80,    82,   304,   305,   307,    54,   366,   239,   680,   681,
     682,   308,   240,   309,   684,   319,   310,    21,   241,   464,
     465,   694,   412,   413,   268,   698,   268,   242,   688,   416,
     369,    73,   268,   268,   382,   385,   386,   419,   425,   430,
     468,   268,   400,   243,    77,   707,   404,   268,   406,   407,
     268,   350,   351,   352,   353,   354,   355,   356,   357,   456,
    -233,   483,   488,   708,   104,   710,   259,   491,   245,   283,
     260,   199,   261,   450,   451,   716,   291,   717,   490,   248,
     499,   505,   397,   521,   249,   530,   531,   532,   533,   540,
     560,   580,   573,   239,   562,   574,   616,   734,   240,   735,
     736,   195,   631,    21,   241,    98,   738,   623,   643,   644,
     650,   253,   376,   242,   653,   654,   655,   659,   661,   677,
     675,   685,   692,   475,   691,   721,   365,   745,   693,   243,
     696,   603,   697,   699,   752,   709,   668,   711,   741,   742,
     759,   755,   670,   403,   758,   422,   672,   760,   671,   529,
     683,   455,   259,    54,   245,   377,   260,   429,   261,   507,
     714,   268,   378,   551,   268,   248,   268,   268,   678,   370,
     249,     0,     0,     0,     0,     0,     0,   253,   252,   268,
     268,    98,   268,   268,   268,   268,   268,     0,   257,     0,
     527,   528,   268,     0,     0,     0,     0,     0,   268,   268,
     268,   268,     0,     0,   195,     0,   274,   195,     0,     0,
       0,     0,   549,   195,   174,   175,   176,   177,   178,   179,
     180,   181,     0,   182,    21,   183,   184,    24,   185,   186,
       0,     0,     0,   450,   450,   451,   557,     0,     0,     0,
       0,     0,     0,     0,     0,   342,     0,     0,     0,     0,
       0,     0,     0,     0,   268,     0,   268,     0,     0,     0,
       0,     0,     0,   571,     0,     0,     0,     0,     0,     0,
     274,   195,   195,   187,     0,   188,   268,   189,     0,   190,
       0,   268,     0,     0,   268,     0,   582,   268,   268,   268,
     239,   587,   268,   268,     0,   240,   268,   593,   595,     0,
      21,   241,     0,     0,     0,     0,     0,     0,   371,    77,
     242,     0,     0,     0,   608,     0,     0,   268,   268,   268,
     268,   257,     0,     0,     0,     0,   243,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   195,     0,
       0,     0,   629,   375,     0,   380,   383,     0,     0,   466,
     372,   245,   399,   246,     0,   247,     0,     0,     0,   641,
       0,     0,   248,     0,     0,     0,     0,   249,   648,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   268,   268,   268,   268,   268,     0,
     527,   239,     0,     0,   527,   527,   240,     0,     0,     0,
       0,    21,   241,     0,   676,     0,   549,   679,     0,     0,
       0,   242,     0,     0,     0,     0,     0,     0,     0,   450,
     450,     0,   467,     0,     0,     0,     0,   243,     0,   690,
       0,     0,     0,     0,     0,     0,     0,     0,   268,   268,
       0,     0,   268,     0,   268,   268,   268,   268,   268,   268,
     259,     0,   245,   495,   260,     0,   261,     0,     0,   608,
       0,     0,   268,   248,     0,     0,     0,     0,   249,     0,
       0,     0,     0,   715,     0,     0,     0,     0,   719,   268,
       0,   268,     0,     0,   268,   268,     0,   268,   252,     0,
     511,   512,     0,   514,   515,   516,   518,   519,     0,     0,
       0,     0,     0,   239,     0,     0,     0,   740,   240,   536,
     537,   538,   539,    21,   241,     0,     0,     0,   268,     0,
     268,     0,     0,   242,     0,     0,     0,     0,   747,   268,
     268,     0,     0,     0,     0,   754,     0,     0,     0,   243,
       0,     0,   757,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   379,     0,   245,   375,   260,     0,   261,     0,
       0,     0,     0,     0,     0,   248,     0,     0,   517,     0,
     249,     0,     0,     0,     0,     0,   239,   578,     0,     0,
       0,   240,   579,     0,     0,   581,    21,   241,   584,   585,
       0,     0,     0,   589,   590,     0,   242,   591,   592,   594,
       0,     0,     0,     0,     0,     5,     0,     6,     7,     8,
       9,    10,   243,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
       0,     0,     0,     0,     0,   244,     0,   245,     0,   246,
       0,   247,     0,     0,    28,     0,     0,     0,   248,     0,
       0,     0,     0,   249,    29,    30,     0,    31,     0,    32,
       0,     0,     0,     0,     0,     0,     0,     0,    33,    34,
      35,     0,    36,     0,    37,     0,    38,     0,    39,     0,
       0,     0,     0,     0,     0,   662,   663,   664,   665,   666,
      40,     6,     7,     8,     9,    10,     0,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    28,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    29,    30,
     695,    31,     0,    32,     0,   700,   701,   702,   703,   704,
     705,     0,    33,    34,    35,     0,    36,     0,    37,     0,
      38,     0,    39,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    40,     0,     0,     0,     0,     0,
     720,     0,   722,   621,     0,   724,   725,     0,   726,     0,
     174,   175,   176,   177,   178,   179,   180,   181,     0,   182,
      21,   183,   184,    24,   185,   186,     0,     0,   622,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   743,
       0,   744,     0,     0,     0,     0,     0,     0,     0,     0,
     748,   749,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,   187,
       0,   188,     0,   189,     0,   190,     0,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,    29,    30,     0,    31,     0,    32,     0,
       0,   128,   129,    57,     0,     0,   130,    33,    34,    35,
       0,    58,     0,    37,     0,    38,     0,    39,     0,     0,
      59,    60,   131,     0,     0,     0,     0,     0,     0,    40,
     132,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,     0,     0,
       0,     0,     0,     0,     0,     0,   158,   159,   160,   161,
     162,   163,    28,   164,   165,   166,   122,   167,   168,   169,
     170,   127,    29,    30,     0,    31,     0,    32,     0,     0,
     171,   172,    57,     0,     0,   173,    33,    34,    35,     0,
      58,     0,    37,     0,    38,     0,    39,     0,     0,    59,
      60,     0,     0,     0,     0,     0,     0,     0,    40,   432,
     433,   434,   435,   436,   437,   438,   439,    19,   440,    21,
     441,   442,    24,   443,   444,    27,     0,     0,     0,     0,
       0,     0,     0,     0,   158,   159,   160,   161,   162,   163,
      28,   164,   165,   166,   122,   167,   168,   169,   170,   127,
      29,    30,     0,    31,     0,    32,     0,     0,   171,   172,
      57,     0,     0,   173,    33,    34,    35,     0,   445,     0,
     446,     0,   447,     0,   448,     0,     0,    59,    60,     0,
       0,     0,     0,     0,     0,     0,    40,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    55,     0,     0,     0,    28,     0,
       0,     0,     0,     0,     0,     0,     0,    56,    29,    30,
       0,    31,     0,    32,     0,     0,     0,     0,    57,     0,
       0,     0,    33,    34,    35,   453,   285,     0,    37,     0,
     286,   454,    39,     0,     0,    59,    60,     0,     0,     0,
       0,     0,     0,     0,    40,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,     0,     0,   330,     0,     0,     0,     0,     0,
       0,     0,    55,     0,     0,     0,    28,     0,     0,     0,
       0,     0,     0,   239,     0,    56,    29,    30,   240,    31,
       0,    32,     0,    21,   241,     0,    57,     0,     0,     0,
      33,    34,    35,   242,    58,     0,    37,     0,    38,     0,
      39,     0,     0,    59,    60,     0,     0,     0,     0,   243,
       0,     0,    40,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
       0,     0,   259,     0,   245,     0,   260,     0,   261,     0,
      55,     0,     0,     0,    28,   248,     0,     0,     0,     0,
     249,   239,     0,    56,    29,    30,   240,    31,     0,    32,
       0,    21,   241,     0,    57,     0,     0,     0,    33,    34,
      35,   242,    58,     0,    37,   459,    38,     0,    39,     0,
       0,    59,    60,     0,     0,     0,     0,   243,     0,     0,
      40,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,     0,     0,
     323,     0,   245,     0,   260,     0,   261,     0,    55,     0,
       0,     0,    28,   248,     0,     0,     0,     0,   249,   239,
       0,    56,    29,    30,   240,    31,     0,    32,     0,    21,
     241,     0,    57,     0,     0,     0,    33,    34,    35,   242,
      58,     0,    37,   567,    38,     0,    39,     0,     0,    59,
      60,     0,     0,     0,     0,   243,     0,     0,    40,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,     0,     0,   379,     0,
     245,     0,   260,     0,   261,     0,    55,     0,     0,     0,
      28,   248,     0,     0,     0,     0,   249,     0,     0,    56,
      29,    30,     0,    31,     0,    32,     0,     0,     0,     0,
      57,     0,     0,     0,    33,    34,    35,     0,    58,     0,
      37,     0,    38,     0,    39,     0,     0,    59,    60,     0,
       0,     0,     0,     0,     0,     0,    40,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    55,     0,     0,     0,    28,     0,
       0,     0,     0,     0,     0,     0,     0,    56,    29,    30,
       0,    31,     0,    32,     0,     0,     0,     0,    57,     0,
       0,     0,    33,    34,    35,     0,   285,     0,    37,     0,
     286,     0,    39,     0,     0,    59,    60,     0,     0,     0,
       0,     0,     0,     0,    40,   432,   433,   434,   435,   436,
     437,   438,   439,    19,   440,    21,   441,   442,    24,   443,
     444,    27,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    55,     0,     0,     0,    28,     0,     0,     0,
       0,     0,     0,     0,     0,    56,    29,    30,     0,    31,
       0,    32,     0,     0,     0,     0,    57,     0,     0,     0,
      33,    34,    35,     0,   445,     0,   446,     0,   447,     0,
     448,     0,     0,    59,    60,     0,     0,     0,     0,     0,
       0,     0,    40,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      55,     0,     0,     0,    28,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    29,    30,     0,    31,     0,    32,
       0,     0,     0,     0,    57,     0,     0,     0,    33,    34,
      35,     0,    58,   306,    37,     0,    38,     0,    39,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      40,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    55,     0,
       0,     0,    28,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    29,    30,     0,    31,     0,    32,     0,     0,
       0,     0,    57,     0,     0,     0,    33,    34,    35,     0,
      58,     0,    37,     0,    38,     0,    39,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    40,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    55,     0,     0,     0,
      28,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      29,    30,     0,    31,     0,    32,     0,     0,     0,     0,
       0,     0,     0,     0,    33,    34,    35,     0,    58,   294,
      37,     0,    38,     0,    39,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    40,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    55,     0,     0,     0,    28,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    29,    30,
       0,    31,     0,    32,     0,     0,     0,     0,     0,     0,
       0,     0,    33,    34,    35,     0,    58,     0,    37,     0,
      38,     0,    39,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    40,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    28,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    29,    30,     0,    31,
       0,    32,     0,     0,     0,     0,     0,     0,     0,     0,
      33,    34,    35,     0,    58,     0,    37,     0,    38,     0,
      39,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    40,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
       0,     0,     0,     0,   219,   220,   221,   222,   223,   224,
     225,   226,   227,   228,    28,   229,   230,   231,   232,     0,
       0,     0,     0,     0,     0,    30,     0,     0,   233,   234,
     279,     0,     0,   235,     0,     0,     0,     0,    33,    34,
       0,     0,    58,     0,    37,     0,    38,     0,    39,   131,
       0,     0,     0,     0,     0,     0,     0,   132,     0,     0,
      40,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,     0,     0,
       0,     0,     0,     0,   174,   175,   176,   177,   178,   179,
     180,   181,    28,   182,    21,   183,   184,    24,   185,   186,
       0,     0,     0,    30,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   344,    33,    34,     0,     0,
      58,     0,    37,     0,    38,     0,    39,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    40,     0,
       0,     0,     0,   187,     0,   188,     0,   189,     0,   190,
     174,   175,   176,   177,   178,   179,   180,   181,     0,   182,
      21,   183,   184,    24,   185,   186,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   187,
       0,   188,     0,   189,     0,   190
};

static const yytype_int16 yycheck[] =
{
       2,     3,    51,    37,    59,    60,    28,    60,     7,     8,
      42,    31,     1,    33,   189,    56,   187,   188,   522,    55,
     534,   111,   400,   601,   145,     7,   418,   605,    30,     7,
      15,     7,    24,     7,    24,    37,    59,    60,    40,    24,
      25,    37,    35,    24,    99,     6,     7,     8,     9,    10,
      11,    12,    24,    42,    78,    57,    80,   106,    19,    32,
     109,    78,    37,    24,    83,    35,    32,    40,    38,    42,
      87,    40,    32,    34,    40,    68,    75,     7,   114,    78,
      40,    73,    81,    79,    14,    77,   127,   107,    78,    19,
     482,   146,    77,    33,    24,    25,   118,    78,    80,    60,
       0,    24,    80,    79,    34,    79,    78,    40,    24,   111,
      71,    72,    87,   147,    87,   236,    35,   238,    87,    38,
      50,    87,    83,   146,   160,    42,    32,    87,   706,    24,
     152,    24,   187,   188,    40,    32,    42,   192,    42,    79,
      80,   196,    37,    73,    37,    75,   148,    77,    19,    79,
      73,   153,    24,    24,    87,   533,    86,    73,   672,   200,
     201,    91,    42,   667,    24,   669,    63,    64,    87,   192,
      87,    37,    32,   196,   210,   211,   212,    42,    73,    77,
      73,    87,    76,    87,    30,    37,    80,    84,     6,     7,
       8,     9,    10,    11,    12,   197,   198,   199,   288,    45,
      77,    19,    24,    45,    32,    40,    24,    87,    43,    37,
      52,    39,    40,    79,   216,    43,    34,    45,    46,    33,
      48,    49,    87,    30,    30,   244,   245,    79,   217,   733,
     285,   280,    68,    76,    38,    24,   277,    80,    45,    45,
     259,    27,    60,   262,    37,    81,    68,   249,    37,    63,
      64,    73,    87,    71,    72,    77,     7,   256,   313,    87,
      78,    68,    76,    24,   284,    83,    73,    73,    75,    75,
      84,     7,   447,   275,   445,   446,    37,    38,    14,    63,
      64,    63,    64,    19,    30,    37,   288,    39,    24,    25,
     313,    43,    38,    45,    46,   317,    48,    49,    34,    45,
      84,    78,    84,    80,   323,    63,    64,    91,    66,    91,
     312,    15,   314,    59,    50,    45,   318,   319,   320,    31,
      24,    25,    62,    63,    64,    24,    84,    73,   330,    75,
      63,    64,   334,   365,   336,    87,    24,    73,    37,    75,
     342,    77,   344,    79,    84,    24,    79,    63,    64,    37,
      86,    84,    85,    31,   373,    91,    74,   376,    37,   378,
     379,   416,    63,    64,   419,    81,    63,    64,    84,    87,
     425,   393,   425,    63,    64,    76,   365,    79,    80,    80,
      78,    78,   431,    84,    79,   404,    76,    84,    37,    87,
     445,   446,    87,   416,    84,    37,   419,    63,    64,     1,
       2,   400,   425,     5,     6,     7,     8,     9,    10,    63,
      64,    24,   446,    79,     3,     4,     5,    74,    84,   421,
      63,    64,    63,    64,    37,    79,    28,    29,   483,   484,
      84,    32,    34,    76,    63,    64,    38,    39,    67,    78,
      42,    84,    74,    84,   446,    63,    64,   466,    87,   468,
      78,   453,    54,    74,   456,    84,    79,    59,    60,    87,
     483,   484,    24,    81,    87,    74,    84,    74,    74,   471,
      63,    64,   474,    75,    80,    37,    78,    79,    87,    81,
     499,    83,   501,    76,    74,   504,    63,    64,    79,    74,
      80,    84,    24,    74,    74,   550,    87,    99,   100,    80,
      80,    78,    63,    64,    24,    37,   108,    84,    24,   111,
     529,   530,   531,   532,    79,    76,   118,    37,    24,    14,
      74,    37,    87,    84,    19,    74,    80,   550,    19,    24,
      25,    37,    74,    24,   533,   555,    19,    32,    80,    34,
      74,    24,   144,   145,   146,    73,    79,    80,    19,    77,
     152,    79,   154,    24,    19,    50,    74,    33,   560,    24,
     562,    37,    39,    40,    41,   213,   214,   215,   570,    78,
     572,    80,    42,    43,    44,    74,   631,   632,    73,    74,
      75,   630,    77,    37,    79,   187,   188,   189,   190,    79,
     192,    86,    74,    78,   196,    80,    91,    46,    47,    48,
      49,    50,    51,    52,    53,     7,     8,    74,   631,   632,
       9,    10,    74,    74,    74,   217,   218,    14,   620,   621,
     622,    74,    19,    74,   626,    37,    74,    24,    25,   631,
     632,   650,    42,    40,   236,   654,   238,    34,   640,    80,
     242,   243,   244,   245,   246,   247,   248,    37,    79,    36,
      31,   253,   254,    50,   256,   674,   258,   259,   260,   261,
     262,   202,   203,   204,   205,   206,   207,   208,   209,    37,
      37,    37,    45,   675,   276,   677,    73,    74,    75,   281,
      77,    84,    79,   285,   286,   687,   288,   689,    74,    86,
      33,    45,    87,    13,    91,    80,    33,    33,    13,    74,
      35,    79,    74,    14,    37,    76,    33,   709,    19,   711,
     712,   313,    37,    24,    25,   317,   718,    79,    79,    79,
      33,   323,    33,    34,    79,    33,    79,    33,    33,    57,
      37,    33,    33,   335,    37,    76,   217,   739,    37,    50,
      37,   521,    38,    38,   746,    37,   602,    37,    37,    37,
      37,   753,   606,   257,   756,   276,   609,   759,   607,   404,
     623,   288,    73,   365,    75,    76,    77,   281,    79,   389,
     685,   373,   245,   426,   376,    86,   378,   379,   618,   243,
      91,    -1,    -1,    -1,    -1,    -1,    -1,   389,    79,   391,
     392,   393,   394,   395,   396,   397,   398,    -1,   400,    -1,
     402,   403,   404,    -1,    -1,    -1,    -1,    -1,   410,   411,
     412,   413,    -1,    -1,   416,    -1,   418,   419,    -1,    -1,
      -1,    -1,   424,   425,    14,    15,    16,    17,    18,    19,
      20,    21,    -1,    23,    24,    25,    26,    27,    28,    29,
      -1,    -1,    -1,   445,   446,   447,   448,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   466,    -1,   468,    -1,    -1,    -1,
      -1,    -1,    -1,   475,    -1,    -1,    -1,    -1,    -1,    -1,
     482,   483,   484,    73,    -1,    75,   488,    77,    -1,    79,
      -1,   493,    -1,    -1,   496,    -1,   498,   499,   500,   501,
      14,   503,   504,   505,    -1,    19,   508,   509,   510,    -1,
      24,    25,    -1,    -1,    -1,    -1,    -1,    -1,    32,   521,
      34,    -1,    -1,    -1,   526,    -1,    -1,   529,   530,   531,
     532,   533,    -1,    -1,    -1,    -1,    50,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   550,    -1,
      -1,    -1,   554,   244,    -1,   246,   247,    -1,    -1,    73,
      74,    75,   253,    77,    -1,    79,    -1,    -1,    -1,   571,
      -1,    -1,    86,    -1,    -1,    -1,    -1,    91,   580,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   596,   597,   598,   599,   600,    -1,
     602,    14,    -1,    -1,   606,   607,    19,    -1,    -1,    -1,
      -1,    24,    25,    -1,   616,    -1,   618,   619,    -1,    -1,
      -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   631,
     632,    -1,   323,    -1,    -1,    -1,    -1,    50,    -1,   641,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   650,   651,
      -1,    -1,   654,    -1,   656,   657,   658,   659,   660,   661,
      73,    -1,    75,    76,    77,    -1,    79,    -1,    -1,   671,
      -1,    -1,   674,    86,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    -1,    -1,   685,    -1,    -1,    -1,    -1,   690,   691,
      -1,   693,    -1,    -1,   696,   697,    -1,   699,   389,    -1,
     391,   392,    -1,   394,   395,   396,   397,   398,    -1,    -1,
      -1,    -1,    -1,    14,    -1,    -1,    -1,   719,    19,   410,
     411,   412,   413,    24,    25,    -1,    -1,    -1,   730,    -1,
     732,    -1,    -1,    34,    -1,    -1,    -1,    -1,   740,   741,
     742,    -1,    -1,    -1,    -1,   747,    -1,    -1,    -1,    50,
      -1,    -1,   754,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    75,   466,    77,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,    89,    -1,
      91,    -1,    -1,    -1,    -1,    -1,    14,   488,    -1,    -1,
      -1,    19,   493,    -1,    -1,   496,    24,    25,   499,   500,
      -1,    -1,    -1,   504,   505,    -1,    34,   508,   509,   510,
      -1,    -1,    -1,    -1,    -1,     6,    -1,     8,     9,    10,
      11,    12,    50,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    75,    -1,    77,
      -1,    79,    -1,    -1,    45,    -1,    -1,    -1,    86,    -1,
      -1,    -1,    -1,    91,    55,    56,    -1,    58,    -1,    60,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    70,
      71,    -1,    73,    -1,    75,    -1,    77,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,   596,   597,   598,   599,   600,
      91,     8,     9,    10,    11,    12,    -1,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    45,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    56,
     651,    58,    -1,    60,    -1,   656,   657,   658,   659,   660,
     661,    -1,    69,    70,    71,    -1,    73,    -1,    75,    -1,
      77,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,
     691,    -1,   693,     7,    -1,   696,   697,    -1,   699,    -1,
      14,    15,    16,    17,    18,    19,    20,    21,    -1,    23,
      24,    25,    26,    27,    28,    29,    -1,    -1,    32,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   730,
      -1,   732,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     741,   742,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    73,
      -1,    75,    -1,    77,    -1,    79,    -1,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    -1,    60,    -1,
      -1,    63,    64,    65,    -1,    -1,    68,    69,    70,    71,
      -1,    73,    -1,    75,    -1,    77,    -1,    79,    -1,    -1,
      82,    83,    84,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      92,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    -1,    60,    -1,    -1,
      63,    64,    65,    -1,    -1,    68,    69,    70,    71,    -1,
      73,    -1,    75,    -1,    77,    -1,    79,    -1,    -1,    82,
      83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    -1,    60,    -1,    -1,    63,    64,
      65,    -1,    -1,    68,    69,    70,    71,    -1,    73,    -1,
      75,    -1,    77,    -1,    79,    -1,    -1,    82,    83,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    41,    -1,    -1,    -1,    45,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    54,    55,    56,
      -1,    58,    -1,    60,    -1,    -1,    -1,    -1,    65,    -1,
      -1,    -1,    69,    70,    71,    72,    73,    -1,    75,    -1,
      77,    78,    79,    -1,    -1,    82,    83,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    91,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    -1,    33,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    41,    -1,    -1,    -1,    45,    -1,    -1,    -1,
      -1,    -1,    -1,    14,    -1,    54,    55,    56,    19,    58,
      -1,    60,    -1,    24,    25,    -1,    65,    -1,    -1,    -1,
      69,    70,    71,    34,    73,    -1,    75,    -1,    77,    -1,
      79,    -1,    -1,    82,    83,    -1,    -1,    -1,    -1,    50,
      -1,    -1,    91,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    -1,    73,    -1,    75,    -1,    77,    -1,    79,    -1,
      41,    -1,    -1,    -1,    45,    86,    -1,    -1,    -1,    -1,
      91,    14,    -1,    54,    55,    56,    19,    58,    -1,    60,
      -1,    24,    25,    -1,    65,    -1,    -1,    -1,    69,    70,
      71,    34,    73,    -1,    75,    76,    77,    -1,    79,    -1,
      -1,    82,    83,    -1,    -1,    -1,    -1,    50,    -1,    -1,
      91,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    -1,    -1,
      73,    -1,    75,    -1,    77,    -1,    79,    -1,    41,    -1,
      -1,    -1,    45,    86,    -1,    -1,    -1,    -1,    91,    14,
      -1,    54,    55,    56,    19,    58,    -1,    60,    -1,    24,
      25,    -1,    65,    -1,    -1,    -1,    69,    70,    71,    34,
      73,    -1,    75,    76,    77,    -1,    79,    -1,    -1,    82,
      83,    -1,    -1,    -1,    -1,    50,    -1,    -1,    91,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    -1,    -1,    73,    -1,
      75,    -1,    77,    -1,    79,    -1,    41,    -1,    -1,    -1,
      45,    86,    -1,    -1,    -1,    -1,    91,    -1,    -1,    54,
      55,    56,    -1,    58,    -1,    60,    -1,    -1,    -1,    -1,
      65,    -1,    -1,    -1,    69,    70,    71,    -1,    73,    -1,
      75,    -1,    77,    -1,    79,    -1,    -1,    82,    83,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    41,    -1,    -1,    -1,    45,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    54,    55,    56,
      -1,    58,    -1,    60,    -1,    -1,    -1,    -1,    65,    -1,
      -1,    -1,    69,    70,    71,    -1,    73,    -1,    75,    -1,
      77,    -1,    79,    -1,    -1,    82,    83,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    91,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    41,    -1,    -1,    -1,    45,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    54,    55,    56,    -1,    58,
      -1,    60,    -1,    -1,    -1,    -1,    65,    -1,    -1,    -1,
      69,    70,    71,    -1,    73,    -1,    75,    -1,    77,    -1,
      79,    -1,    -1,    82,    83,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      41,    -1,    -1,    -1,    45,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    55,    56,    -1,    58,    -1,    60,
      -1,    -1,    -1,    -1,    65,    -1,    -1,    -1,    69,    70,
      71,    -1,    73,    74,    75,    -1,    77,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      91,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    41,    -1,
      -1,    -1,    45,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    55,    56,    -1,    58,    -1,    60,    -1,    -1,
      -1,    -1,    65,    -1,    -1,    -1,    69,    70,    71,    -1,
      73,    -1,    75,    -1,    77,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    41,    -1,    -1,    -1,
      45,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      55,    56,    -1,    58,    -1,    60,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    70,    71,    -1,    73,    74,
      75,    -1,    77,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    41,    -1,    -1,    -1,    45,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    56,
      -1,    58,    -1,    60,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    70,    71,    -1,    73,    -1,    75,    -1,
      77,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    91,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    45,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    55,    56,    -1,    58,
      -1,    60,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    70,    71,    -1,    73,    -1,    75,    -1,    77,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    -1,    -1,    -1,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    45,    50,    51,    52,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    56,    -1,    -1,    63,    64,
      61,    -1,    -1,    68,    -1,    -1,    -1,    -1,    69,    70,
      -1,    -1,    73,    -1,    75,    -1,    77,    -1,    79,    84,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,
      91,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    -1,    -1,
      -1,    -1,    -1,    -1,    14,    15,    16,    17,    18,    19,
      20,    21,    45,    23,    24,    25,    26,    27,    28,    29,
      -1,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    45,    69,    70,    -1,    -1,
      73,    -1,    75,    -1,    77,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    -1,    -1,    73,    -1,    75,    -1,    77,    -1,    79,
      14,    15,    16,    17,    18,    19,    20,    21,    -1,    23,
      24,    25,    26,    27,    28,    29,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    75,    -1,    77,    -1,    79
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,    94,     6,     8,     9,    10,    11,
      12,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    45,    55,
      56,    58,    60,    69,    70,    71,    73,    75,    77,    79,
      91,    95,    96,    97,    98,    99,   100,   102,   107,   112,
     121,   126,   134,   148,   168,    41,    54,    65,    73,    82,
      83,   116,   117,   118,   119,   120,   121,   168,   116,   168,
       0,   168,   161,   168,    73,   111,   112,   168,   111,    73,
     153,   168,   153,   168,     6,     7,     8,     9,    10,    11,
      12,    19,    34,    60,    71,    72,    83,   147,   168,    73,
      77,   122,   123,   140,   168,   116,   126,   135,    77,   126,
     168,    77,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    63,    64,
      68,    84,    92,   113,   116,   151,   116,   151,   146,   147,
      19,   168,   116,    97,    38,    38,    59,    73,    75,   148,
      27,    30,    45,    37,   168,   119,   117,   116,    39,    40,
      41,    42,    43,    44,    46,    47,    48,    50,    51,    52,
      53,    63,    64,    68,    14,    15,    16,    17,    18,    19,
      20,    21,    23,    25,    26,    28,    29,    73,    75,    77,
      79,   134,   138,   139,   141,   168,   138,    63,    64,    84,
      35,    68,    46,    47,    48,    49,    50,    51,    52,    53,
      39,    40,    41,    42,    43,    44,    37,     7,    45,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    50,
      51,    52,    53,    63,    64,    68,    37,   112,    37,    14,
      19,    25,    34,    50,    73,    75,    77,    79,    86,    91,
     154,   155,   159,   168,    31,   110,   111,   168,    31,    73,
      77,    79,   115,   158,   159,   162,   164,   165,   168,   141,
     142,   143,   144,   145,   168,    68,    81,    37,    62,    61,
     126,   127,   128,   168,    37,    73,    77,   116,   124,   125,
     140,   168,    74,    74,    74,    74,    74,    74,    74,    74,
      74,    74,    74,    74,    74,    74,    74,    74,    74,    74,
      74,    74,    80,    79,    85,    76,    78,    80,    37,    37,
      37,    79,    91,    73,   152,   153,   158,   152,   141,   151,
      33,   116,   147,   116,    37,   168,    66,   142,   142,   144,
      19,   168,    45,   141,    45,   116,   116,   116,   117,   117,
     118,   118,   118,   118,   118,   118,   118,   118,   119,   119,
     119,   120,   120,   120,   116,    96,   168,   152,   152,   168,
     161,    32,    74,   157,   158,   159,    33,    76,   157,    73,
     159,   166,   168,   159,   167,   168,   168,   116,    74,    80,
      37,    39,    43,    45,    46,    48,    49,    87,   156,   159,
     168,     7,    79,   113,   168,   158,   168,   168,     7,   158,
      40,    32,    42,    40,    74,    80,    80,    78,    80,    37,
     116,    68,   123,   117,    79,    79,   136,   137,    78,   128,
      36,   126,    14,    15,    16,    17,    18,    19,    20,    21,
      23,    25,    26,    28,    29,    73,    75,    77,    79,   134,
     168,   168,    81,    72,    78,   125,    37,   116,   141,    76,
     116,    78,   147,   116,   116,   116,    73,   159,    31,    74,
     116,    33,    76,   116,    37,   168,   116,    74,    80,    76,
      80,    78,    80,    37,    37,    79,   116,   116,    45,    52,
      74,    74,   158,    42,   158,    76,    42,    78,    80,    33,
      40,    43,    79,    80,    33,    45,    91,   155,    73,    77,
      79,   159,   159,   147,   159,   159,   159,    89,   159,   159,
     110,    13,   105,   106,   103,   104,   114,   168,   168,   115,
      80,    33,    33,    13,   108,   109,   159,   159,   159,   159,
      74,   141,    78,   145,   141,   116,    19,   149,   150,   168,
     138,   137,   129,   130,   131,    68,    19,   168,   116,   116,
      35,    76,    37,    79,    79,   158,    76,    76,   116,   116,
      37,   168,    67,    74,    76,    78,   141,   141,   159,   159,
      79,   159,   168,   158,   159,   159,   158,   168,   158,   159,
     159,   159,   159,   168,   159,   168,    37,    35,    38,    35,
      38,     7,    79,   100,   106,     7,    80,    32,   168,     7,
     158,   163,   158,   101,   110,   109,    33,    79,    80,    33,
      37,     7,    32,    79,    15,    25,    77,   132,   133,   168,
     126,    37,    37,    79,    78,    81,   116,   116,    76,   116,
      37,   168,   116,    79,    79,    74,    19,   160,   168,    78,
      33,    42,    79,    79,    33,    79,    40,    42,    42,    33,
      40,    33,   159,   159,   159,   159,   159,   105,   103,   105,
     104,   114,   108,    74,    80,    37,   168,    57,   150,   168,
     116,   116,   116,   130,   116,    33,    76,    80,   116,    37,
     168,    37,    33,    37,   158,   159,    37,    38,   158,    38,
     159,   159,   159,   159,   159,   159,     7,   158,   116,    37,
     116,    37,    32,    78,   133,   168,   116,   116,    37,   168,
     159,    76,   159,    78,   159,   159,   159,    79,    74,    78,
      42,    79,    40,   105,   116,   116,   116,    76,   116,    37,
     168,    37,    37,   159,   159,   116,    37,   168,   159,   159,
      78,    79,   116,    37,   168,   116,    37,   168,   116,    37,
     116
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    93,    94,    94,    94,    94,    95,    95,    96,    96,
      96,    97,    97,    97,    97,    97,    97,    97,    97,    97,
      97,    97,    97,    97,    97,    97,    97,    97,    97,    98,
      99,    99,   100,   101,   102,   102,   102,   102,   102,   102,
     102,   102,   103,   103,   104,   105,   105,   106,   107,   107,
     107,   107,   108,   108,   109,   110,   110,   111,   111,   112,
     112,   113,   113,   113,   113,   113,   113,   113,   113,   113,
     113,   113,   113,   113,   113,   113,   113,   113,   113,   113,
     114,   114,   115,   115,   116,   116,   116,   116,   116,   116,
     116,   116,   116,   117,   117,   118,   118,   118,   118,   118,
     118,   118,   118,   118,   119,   119,   119,   119,   119,   120,
     120,   120,   120,   121,   121,   121,   121,   121,   121,   121,
     121,   121,   122,   122,   123,   124,   124,   125,   125,   126,
     126,   126,   126,   126,   126,   126,   126,   126,   126,   126,
     126,   126,   126,   126,   126,   126,   126,   126,   126,   126,
     126,   126,   126,   126,   126,   126,   126,   126,   126,   126,
     126,   126,   126,   126,   126,   126,   126,   126,   126,   126,
     126,   126,   126,   126,   126,   126,   126,   126,   126,   126,
     126,   126,   126,   126,   126,   126,   127,   127,   128,   129,
     129,   130,   131,   131,   132,   132,   133,   133,   133,   134,
     134,   135,   135,   136,   136,   137,   137,   138,   138,   139,
     139,   139,   139,   139,   139,   139,   139,   139,   139,   139,
     139,   139,   139,   139,   139,   139,   139,   139,   139,   139,
     139,   139,   139,   140,   140,   140,   140,   140,   141,   142,
     142,   143,   143,   144,   144,   145,   146,   146,   146,   147,
     147,   147,   147,   147,   147,   147,   147,   147,   147,   147,
     147,   147,   147,   148,   148,   148,   148,   149,   149,   150,
     150,   150,   151,   151,   151,   152,   152,   153,   154,   154,
     155,   155,   155,   155,   155,   155,   155,   155,   155,   155,
     155,   155,   155,   155,   155,   155,   155,   155,   155,   155,
     155,   156,   156,   157,   157,   158,   158,   158,   159,   159,
     159,   159,   159,   159,   159,   159,   159,   159,   159,   159,
     159,   159,   159,   159,   159,   159,   160,   160,   161,   161,
     162,   162,   163,   163,   164,   164,   165,   165,   166,   166,
     167,   167,   167,   167,   168
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     4,     2,     2,     4,     1,     0,     1,
       2,     1,     1,     1,     1,     1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     1,     2,
       4,     4,     3,     3,     5,     7,     7,     9,     3,     5,
       5,     7,     1,     3,     3,     1,     2,     2,     3,     5,
       5,     7,     1,     2,     2,     1,     3,     1,     2,     1,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     2,     4,     4,     2,     3,     3,     3,
       3,     3,     1,     6,     1,     3,     3,     3,     3,     3,
       3,     3,     3,     1,     3,     3,     3,     2,     1,     3,
       3,     3,     1,     1,     4,     5,     4,     3,     4,     4,
       6,     3,     3,     1,     3,     2,     1,     4,     2,     4,
       1,     5,     4,     7,     9,     3,     4,     6,     5,     5,
       5,     5,     3,     6,     8,     3,     4,     2,     1,     1,
       2,     6,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     2,     1,     3,     3,
       1,     4,     2,     0,     3,     1,     1,     1,     1,     1,
       2,     2,     1,     2,     1,     4,     6,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     4,     3,     5,     5,     3,     4,
       3,     4,     1,     1,     3,     4,     3,     4,     1,     1,
       0,     3,     1,     3,     1,     3,     0,     3,     5,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     2,     2,     1,     1,     3,     3,
       5,     5,     0,     1,     3,     3,     1,     3,     1,     3,
       2,     3,     3,     3,     7,     9,     7,     7,     9,     7,
       5,     5,     5,     5,     7,     7,     9,     9,     7,     7,
       5,     1,     2,     2,     1,     3,     1,     1,     1,     3,
       2,     3,     7,     3,     3,     3,     3,     2,     1,     1,
       4,     3,     3,     4,     1,     3,     1,     1,     1,     3,
       1,     5,     1,     3,     1,     3,     3,     3,     5,     3,
       5,     3,     3,     1,     1
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
#line 491 "hexpr.y" /* yacc.c:1646  */
    { yyParsedModule = (yyvsp[0].module);                     }
#line 2672 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 3:
#line 492 "hexpr.y" /* yacc.c:1646  */
    { yyParsedVar    = *(yyvsp[-2].string); yyParsedExpr = (yyvsp[0].exp); }
#line 2678 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 4:
#line 493 "hexpr.y" /* yacc.c:1646  */
    { yyParsedVar    = "";  yyParsedExpr = (yyvsp[0].exp); }
#line 2684 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 5:
#line 494 "hexpr.y" /* yacc.c:1646  */
    { yyParsedExpr   = (yyvsp[0].exp);                     }
#line 2690 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 6:
#line 497 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = new Module(*(yyvsp[-2].string), *(yyvsp[0].mdefs)); }
#line 2696 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 7:
#line 498 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = new Module(freshName(), *(yyvsp[0].mdefs)); }
#line 2702 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 8:
#line 500 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); }
#line 2708 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 9:
#line 501 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2714 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 10:
#line 502 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = (yyvsp[-1].mdefs);                            (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2720 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 11:
#line 504 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2726 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 12:
#line 505 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2732 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 13:
#line 506 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mvtydef); }
#line 2738 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 14:
#line 507 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2744 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 15:
#line 508 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2750 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 16:
#line 510 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2756 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 17:
#line 511 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 2762 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 18:
#line 512 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 2768 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 19:
#line 513 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 2774 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 20:
#line 514 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 2780 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 21:
#line 515 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 2786 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 22:
#line 516 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 2792 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 23:
#line 517 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-9]), (yylsp[0]))); }
#line 2798 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 24:
#line 518 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-10]), (yylsp[0]))); }
#line 2804 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 25:
#line 519 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-11]), (yylsp[0]))); }
#line 2810 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 26:
#line 520 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-12]), (yylsp[0]))); }
#line 2816 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 27:
#line 521 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-13]), (yylsp[0]))); }
#line 2822 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 28:
#line 524 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(freshName()), let(freshName(), ExprPtr((yyvsp[0].exp)), mktunit(m((yylsp[0]))), m((yylsp[0]))), m((yylsp[0]))); }
#line 2828 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 29:
#line 527 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MImport(yyModulePath, *(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 2834 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 30:
#line 530 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MTypeDef(MTypeDef::Transparent, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 2840 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 31:
#line 531 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MTypeDef(MTypeDef::Opaque, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 2846 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 32:
#line 534 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydef) = new MVarTypeDef(*(yyvsp[-2].string), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]), (yylsp[0]))); }
#line 2852 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 33:
#line 536 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdef) = new MVarDef(*(yyvsp[-2].strings), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2858 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 34:
#line 539 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-3].tconstraints), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2864 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 35:
#line 540 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-6]), (yylsp[0]))); wantIndent(false); }
#line 2870 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 36:
#line 541 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2876 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 37:
#line 542 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-7].tconstraints), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-8]), (yylsp[0])));            wantIndent(false); }
#line 2882 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 38:
#line 543 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2888 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 39:
#line 544 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2894 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 40:
#line 545 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-4]), (yylsp[0])));            wantIndent(false); }
#line 2900 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 41:
#line 546 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2906 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 42:
#line 548 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundeps) = autorelease(new CFunDepDefs()); (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2912 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 43:
#line 549 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundeps) = (yyvsp[-2].fundeps);                             (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2918 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 44:
#line 551 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundep) = autorelease(new CFunDepDef(*(yyvsp[-2].strings), *(yyvsp[0].strings))); }
#line 2924 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 45:
#line 553 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydefs) = autorelease(new MVarTypeDefs()); (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2930 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 46:
#line 554 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydefs) = (yyvsp[-1].mvtydefs);                              (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2936 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 47:
#line 556 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydef) = (yyvsp[0].mvtydef); }
#line 2942 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 48:
#line 559 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2948 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 49:
#line 560 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-3].tconstraints),           *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2954 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 50:
#line 561 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-4]), (yylsp[0])));        wantIndent(false); }
#line 2960 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 51:
#line 562 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-5].tconstraints),           *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-6]), (yylsp[0])));        wantIndent(false); }
#line 2966 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 52:
#line 564 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdefs) = autorelease(new MVarDefs()); (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2972 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 53:
#line 565 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdefs) = (yyvsp[-1].mvdefs);                          (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2978 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 54:
#line 567 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdef) = (yyvsp[0].mvdef); }
#line 2984 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 55:
#line 570 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[0].strings); }
#line 2990 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 56:
#line 572 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[-1].string)); (yyval.strings)->push_back(*(yyvsp[-2].string)); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 2996 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 57:
#line 574 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3002 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 58:
#line 575 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3008 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 59:
#line 577 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 3014 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 60:
#line 579 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[-1].string); }
#line 3020 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 61:
#line 581 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("and")); }
#line 3026 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 62:
#line 582 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("or")); }
#line 3032 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 63:
#line 583 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3038 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 64:
#line 584 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3044 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 65:
#line 585 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("~")); }
#line 3050 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 66:
#line 586 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("=~")); }
#line 3056 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 67:
#line 587 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("===")); }
#line 3062 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 68:
#line 588 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("==")); }
#line 3068 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 69:
#line 589 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("<")); }
#line 3074 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 70:
#line 590 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("<=")); }
#line 3080 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 71:
#line 591 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(">")); }
#line 3086 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 72:
#line 592 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(">=")); }
#line 3092 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 73:
#line 593 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("in")); }
#line 3098 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 74:
#line 594 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("append")); }
#line 3104 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 75:
#line 595 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("+")); }
#line 3110 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 76:
#line 596 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("-")); }
#line 3116 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 77:
#line 597 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("*")); }
#line 3122 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 78:
#line 598 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("/")); }
#line 3128 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 79:
#line 599 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("%")); }
#line 3134 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 80:
#line 601 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3140 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 81:
#line 602 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3146 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 82:
#line 604 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3152 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 83:
#line 605 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes);                           (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3158 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 84:
#line 608 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3164 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 85:
#line 609 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3170 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 86:
#line 610 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), (yyvsp[0].exp), m((yylsp[-1]),(yylsp[0]))); }
#line 3176 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 87:
#line 611 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("and",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3182 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 88:
#line 612 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("or",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3188 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 89:
#line 613 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("compose",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3194 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 90:
#line 614 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assign(ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3200 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 91:
#line 615 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("in",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3206 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 92:
#line 616 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3212 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 93:
#line 618 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP3(var("if",m((yylsp[-5]))), (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-5]), (yylsp[0]))); }
#line 3218 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 94:
#line 619 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3224 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 95:
#line 621 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("~",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3230 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 96:
#line 622 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("===",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3236 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 97:
#line 623 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3242 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 98:
#line 624 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))), m((yylsp[-2]),(yylsp[0]))); }
#line 3248 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 99:
#line 625 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("<",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3254 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 100:
#line 626 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("<=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3260 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 101:
#line 627 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var(">",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3266 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 102:
#line 628 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var(">=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3272 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 103:
#line 629 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3278 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 104:
#line 631 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("+",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3284 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 105:
#line 632 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("-",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3290 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 106:
#line 633 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("append",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3296 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 107:
#line 634 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("neg",m((yylsp[-1]))), ExprPtr((yyvsp[0].exp)), m((yylsp[-1]),(yylsp[0]))); }
#line 3302 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 108:
#line 635 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3308 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 109:
#line 637 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("*", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3314 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 110:
#line 638 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("/", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3320 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 111:
#line 639 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("%", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3326 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 112:
#line 640 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3332 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 113:
#line 642 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3338 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 114:
#line 645 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-2].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3344 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 115:
#line 646 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]),(yylsp[0])))->clone(); }
#line 3350 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 116:
#line 649 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileMatch(yyParseCC, *(yyvsp[-2].exps), normPatternRules(*(yyvsp[0].patternexps), m((yylsp[-3]),(yylsp[0]))), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3356 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 117:
#line 652 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileMatchTest(yyParseCC, ExprPtr((yyvsp[-2].exp)), PatternPtr((yyvsp[0].pattern)), m((yylsp[-2]),(yylsp[0])))->clone(); }
#line 3362 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 118:
#line 655 "hexpr.y" /* yacc.c:1646  */
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
#line 3377 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 119:
#line 667 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-1].letbindings), ExprPtr(new Unit(m((yylsp[-3]),(yylsp[0])))), m((yylsp[-3]),(yylsp[0]))); }
#line 3383 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 120:
#line 668 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[-1].exp)), m((yylsp[-5]),(yylsp[0]))); }
#line 3389 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 121:
#line 671 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assump(ExprPtr((yyvsp[-2].exp)), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]),(yylsp[0]))); }
#line 3395 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 122:
#line 673 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].letbindings)->push_back(*(yyvsp[0].letbinding)); (yyval.letbindings) = (yyvsp[-2].letbindings); }
#line 3401 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 123:
#line 674 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3407 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 124:
#line 676 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-2].pattern)), ExprPtr((yyvsp[0].exp)))); }
#line 3413 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 125:
#line 678 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = (yyvsp[-1].letbindings); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3419 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 126:
#line 679 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3425 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 127:
#line 681 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)))); }
#line 3431 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 128:
#line 682 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr(new MatchAny("_",m((yylsp[-1])))), ExprPtr((yyvsp[-1].exp)))); }
#line 3437 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 129:
#line 685 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].exps), m((yylsp[-3]), (yylsp[0]))); }
#line 3443 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 130:
#line 686 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = varCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3449 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 131:
#line 689 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("range", m((yylsp[-2]))), list(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]), (yylsp[0]))); }
#line 3455 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 132:
#line 690 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("iterateS", m((yylsp[-1]))), list(ExprPtr((yyvsp[-2].exp)), fn(str::strings(".x"), fncall(var("+", m((yylsp[-1]))), list(var(".x", m((yylsp[-1]))), ExprPtr(new Int(1, m((yylsp[-1]))))), m((yylsp[-1]))), m((yylsp[-1])))), m((yylsp[-3]), (yylsp[0]))); }
#line 3461 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 133:
#line 691 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileArrayComprehension(ExprPtr((yyvsp[-5].exp)), PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 3467 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 134:
#line 692 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileArrayComprehension(ExprPtr((yyvsp[-7].exp)), PatternPtr((yyvsp[-5].pattern)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 3473 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 135:
#line 693 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkArray(*(yyvsp[-1].exps), m((yylsp[-2]), (yylsp[0]))); }
#line 3479 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 136:
#line 694 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new AIndex(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3485 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 137:
#line 695 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("slice", m((yylsp[-2]))), list(ExprPtr((yyvsp[-5].exp)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-5]), (yylsp[0]))); }
#line 3491 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 138:
#line 696 "hexpr.y" /* yacc.c:1646  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-1]))), list(var(vn,m((yylsp[-4]))), ExprPtr((yyvsp[-2].exp)), fncall(var("size",m((yylsp[-1]))), list(var(vn,m((yylsp[-4])))),m((yylsp[-4])))),m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3497 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 139:
#line 697 "hexpr.y" /* yacc.c:1646  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-2]))), list(var(vn,m((yylsp[-4]))), fncall(var("size",m((yylsp[-2]))), list(var(vn,m((yylsp[-2])))),m((yylsp[-4]))), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3503 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 140:
#line 700 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-3].string), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3509 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 141:
#line 701 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(".f" + str::from((yyvsp[-3].intv)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3515 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 142:
#line 702 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-1].string), ExprPtr(new Unit(m((yylsp[-1])))), m((yylsp[-2]), (yylsp[0]))); }
#line 3521 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 143:
#line 703 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-4].exp)), *(yyvsp[-1].vfields), m((yylsp[-5]), (yylsp[0]))); }
#line 3527 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 144:
#line 704 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-6].exp)), *(yyvsp[-3].vfields), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3533 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 145:
#line 707 "hexpr.y" /* yacc.c:1646  */
    { if ((yyvsp[-1].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-1].rfields), m((yylsp[-2]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-2]), (yylsp[0]))); } }
#line 3539 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 146:
#line 708 "hexpr.y" /* yacc.c:1646  */
    { if ((yyvsp[-2].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-2].rfields), m((yylsp[-3]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-3]), (yylsp[0]))); } }
#line 3545 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 147:
#line 709 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makeProjSeq((yyvsp[-1].exp), *(yyvsp[0].strings), m((yylsp[-1]), (yylsp[0]))); }
#line 3551 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 148:
#line 712 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Fn(str::strings("x"), proj(var("x", m((yylsp[0]))), *(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3557 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 149:
#line 715 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileRegexFn(yyParseCC, std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0])))->clone(); }
#line 3563 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 150:
#line 718 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Pack(ExprPtr((yyvsp[0].exp)), m((yylsp[-1]), (yylsp[0]))); }
#line 3569 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 151:
#line 719 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Unpack(*(yyvsp[-4].string), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3575 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 152:
#line 722 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Bool((yyvsp[0].boolv), m((yylsp[0]))); }
#line 3581 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 153:
#line 723 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3587 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 154:
#line 724 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3593 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 155:
#line 725 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3599 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 156:
#line 726 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Short((yyvsp[0].shortv), m((yylsp[0]))); }
#line 3605 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 157:
#line 727 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Int((yyvsp[0].intv), m((yylsp[0]))); }
#line 3611 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 158:
#line 728 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Long((yyvsp[0].longv), m((yylsp[0]))); }
#line 3617 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 159:
#line 729 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Int128((yyvsp[0].int128v), m((yylsp[0]))); }
#line 3623 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 160:
#line 730 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Float((yyvsp[0].floatv), m((yylsp[0]))); }
#line 3629 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 161:
#line 731 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Double((yyvsp[0].doublev), m((yylsp[0]))); }
#line 3635 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 162:
#line 732 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3641 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 163:
#line 733 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0])))->clone(); }
#line 3647 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 164:
#line 734 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3653 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 165:
#line 735 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3659 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 166:
#line 738 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = pickNestedExp((yyvsp[-1].exps), m((yylsp[-2]),(yylsp[0]))); }
#line 3665 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 167:
#line 741 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("append", m((yylsp[-1]))); }
#line 3671 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 168:
#line 742 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("+",      m((yylsp[-1]))); }
#line 3677 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 169:
#line 743 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("-",      m((yylsp[-1]))); }
#line 3683 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 170:
#line 744 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("*",      m((yylsp[-1]))); }
#line 3689 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 171:
#line 745 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("/",      m((yylsp[-1]))); }
#line 3695 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 172:
#line 746 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("%",      m((yylsp[-1]))); }
#line 3701 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 173:
#line 747 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("~",      m((yylsp[-1]))); }
#line 3707 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 174:
#line 748 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("===",    m((yylsp[-1]))); }
#line 3713 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 175:
#line 749 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("==",     m((yylsp[-1]))); }
#line 3719 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 176:
#line 750 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("!=",     m((yylsp[-1]))); }
#line 3725 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 177:
#line 751 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("<",      m((yylsp[-1]))); }
#line 3731 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 178:
#line 752 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var(">",      m((yylsp[-1]))); }
#line 3737 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 179:
#line 753 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var(">=",     m((yylsp[-1]))); }
#line 3743 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 180:
#line 754 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("<=",     m((yylsp[-1]))); }
#line 3749 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 181:
#line 755 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("and",    m((yylsp[-1]))); }
#line 3755 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 182:
#line 756 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("or",     m((yylsp[-1]))); }
#line 3761 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 183:
#line 757 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("in",     m((yylsp[-1]))); }
#line 3767 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 184:
#line 758 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("not",    m((yylsp[-1]))); }
#line 3773 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 185:
#line 761 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assump(fncall(var("unsafeCast", m((yylsp[-1]))), list(mktunit(m((yylsp[-1])))), m((yylsp[-1]))), qualtype(tapp(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp)))))), m((yylsp[-1]))); }
#line 3779 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 186:
#line 763 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prules) = (yyvsp[-1].prules); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3785 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 187:
#line 764 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prules) = autorelease(new Grammar()); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3791 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 188:
#line 766 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prule) = autorelease(new Grammar::value_type(*(yyvsp[-2].string), *(yyvsp[0].prdefs))); }
#line 3797 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 189:
#line 768 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdefs) = (yyvsp[-2].prdefs); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3803 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 190:
#line 769 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdefs) = autorelease(new GrammarRules()); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3809 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 191:
#line 771 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdef) = autorelease(new GrammarRule(*(yyvsp[-3].pbelems), ExprPtr((yyvsp[-1].exp)))); }
#line 3815 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 192:
#line 773 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelems) = (yyvsp[-1].pbelems); (yyval.pbelems)->push_back(*(yyvsp[0].pbelem)); }
#line 3821 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 193:
#line 774 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelems) = autorelease(new BoundGrammarValues()); }
#line 3827 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 194:
#line 776 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue(*(yyvsp[-2].string), GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3833 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 195:
#line 777 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue("_", GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3839 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 196:
#line 779 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GSymRef(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3845 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 197:
#line 780 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GStr(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3851 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 198:
#line 781 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GStr(std::string(1, str::readCharDef(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3857 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 199:
#line 783 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3863 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 200:
#line 784 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3869 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 201:
#line 786 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = (yyvsp[-1].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3875 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 202:
#line 787 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3881 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 203:
#line 789 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexps) = (yyvsp[-1].patternexps); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3887 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 204:
#line 790 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexps) = autorelease(new PatternRows()); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3893 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 205:
#line 792 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)))); }
#line 3899 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 206:
#line 793 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-4].patterns), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)))); }
#line 3905 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 207:
#line 796 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[-1].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3911 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 208:
#line 797 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = autorelease(new Patterns()); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3917 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 209:
#line 799 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Bool((yyvsp[0].boolv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3923 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 210:
#line 800 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3929 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 211:
#line 801 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3935 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 212:
#line 802 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Short((yyvsp[0].shortv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3941 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 213:
#line 803 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int((yyvsp[0].intv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3947 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 214:
#line 804 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Long((yyvsp[0].longv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3953 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 215:
#line 805 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int128((yyvsp[0].int128v), m((yylsp[0])))), m((yylsp[0]))); }
#line 3959 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 216:
#line 806 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Double((yyvsp[0].doublev), m((yylsp[0])))), m((yylsp[0]))); }
#line 3965 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 217:
#line 807 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = mkpatarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3971 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 218:
#line 808 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = mkpatarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3977 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 219:
#line 809 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkTimespanPrim(*(yyvsp[0].strings), m((yylsp[0]))), mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3983 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 220:
#line 810 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 3989 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 221:
#line 811 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkDateTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 3995 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 222:
#line 812 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRegex(std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0]))); }
#line 4001 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 223:
#line 813 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4007 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 224:
#line 814 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4013 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 225:
#line 815 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-1].string), PatternPtr(new MatchLiteral(PrimitivePtr(new Unit(m((yylsp[-1])))), m((yylsp[-1])))), m((yylsp[-2]),(yylsp[0]))); }
#line 4019 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 226:
#line 816 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-3].string), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4025 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 227:
#line 817 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(".f" + str::from((yyvsp[-3].intv)), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4031 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 228:
#line 818 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4037 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 229:
#line 819 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4043 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 230:
#line 820 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4049 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 231:
#line 821 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4055 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 232:
#line 822 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = patVarCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4061 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 233:
#line 824 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchAny(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4067 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 234:
#line 825 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4073 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 235:
#line 826 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4079 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 236:
#line 827 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4085 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 237:
#line 828 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4091 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 238:
#line 830 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = (yyvsp[0].pattern); }
#line 4097 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 239:
#line 832 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[0].patterns); }
#line 4103 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 240:
#line 833 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = new Patterns(); }
#line 4109 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 241:
#line 835 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[-2].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4115 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 242:
#line 836 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = new Patterns(); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4121 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 243:
#line 838 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfields) = (yyvsp[-2].recpatfields); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4127 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 244:
#line 839 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfields) = new MatchRecord::Fields(); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4133 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 245:
#line 841 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfield) = new MatchRecord::Field(*(yyvsp[-2].string), PatternPtr((yyvsp[0].pattern))); }
#line 4139 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 246:
#line 843 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); }
#line 4145 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 247:
#line 844 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4151 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 248:
#line 845 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = (yyvsp[-4].rfields);                                     (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4157 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 249:
#line 847 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4163 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 250:
#line 848 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("data")); }
#line 4169 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 251:
#line 849 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("type")); }
#line 4175 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 252:
#line 850 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("where")); }
#line 4181 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 253:
#line 851 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("class")); wantIndent(false); }
#line 4187 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 254:
#line 852 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("instance")); wantIndent(false); }
#line 4193 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 255:
#line 853 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("exists")); }
#line 4199 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 256:
#line 854 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("import")); }
#line 4205 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 257:
#line 855 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("module")); }
#line 4211 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 258:
#line 856 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("parse")); }
#line 4217 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 259:
#line 857 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("do")); }
#line 4223 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 260:
#line 858 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("return")); }
#line 4229 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 261:
#line 859 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("fn")); }
#line 4235 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 262:
#line 860 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(".f" + str::from((yyvsp[0].intv)))); }
#line 4241 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 263:
#line 862 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-2].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4247 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 264:
#line 863 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings); str::seq x = tupSectionFields(*(yyvsp[0].string)); (yyval.strings)->insert((yyval.strings)->end(), x.begin(), x.end()); }
#line 4253 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 265:
#line 864 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4259 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 266:
#line 865 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); *(yyval.strings) = tupSectionFields(*(yyvsp[0].string)); }
#line 4265 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 267:
#line 867 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vfields) = autorelease(new Case::Bindings()); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4271 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 268:
#line 868 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vfields) = (yyvsp[-2].vfields); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4277 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 269:
#line 870 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-2].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4283 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 270:
#line 871 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-4].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4289 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 271:
#line 872 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(".f" + str::from((yyvsp[-4].intv)), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4295 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 272:
#line 874 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); }
#line 4301 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 273:
#line 875 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4307 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 274:
#line 876 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].exps)->push_back(ExprPtr((yyvsp[0].exp))); (yyval.exps) = (yyvsp[-2].exps); }
#line 4313 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 275:
#line 878 "hexpr.y" /* yacc.c:1646  */
    { (yyval.qualtype) = new QualType(*(yyvsp[-2].tconstraints), *(yyvsp[0].mtype)); }
#line 4319 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 276:
#line 879 "hexpr.y" /* yacc.c:1646  */
    { (yyval.qualtype) = new QualType(Constraints(), *(yyvsp[0].mtype)); }
#line 4325 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 277:
#line 882 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraints) = (yyvsp[-1].tconstraints); }
#line 4331 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 278:
#line 884 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraints) = autorelease(new Constraints()); (yyval.tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); }
#line 4337 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 279:
#line 885 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); (yyval.tconstraints) = (yyvsp[-2].tconstraints); }
#line 4343 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 280:
#line 887 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(*(yyvsp[-1].string), *(yyvsp[0].mtypes)); }
#line 4349 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 281:
#line 888 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(EqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4355 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 282:
#line 889 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(NotEqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4361 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 283:
#line 890 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(FixIsoRecur::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4367 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 284:
#line 891 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4373 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 285:
#line 892 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4379 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 286:
#line 893 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4385 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 287:
#line 894 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4391 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 288:
#line 895 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4397 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 289:
#line 896 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(1), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4403 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 290:
#line 898 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4409 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 291:
#line 899 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4415 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 292:
#line 900 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4421 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 293:
#line 901 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4427 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 294:
#line 903 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4433 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 295:
#line 904 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4439 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 296:
#line 905 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4445 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 297:
#line 906 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4451 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 298:
#line 908 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), TString::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype))); }
#line 4457 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 299:
#line 909 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), *(yyvsp[-5].mtype),                *(yyvsp[-3].mtype))); }
#line 4463 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 300:
#line 910 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(AppendsToUnqualifier::constraintName(), list(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4469 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 301:
#line 912 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4475 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 302:
#line 913 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-1].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4481 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 303:
#line 915 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4487 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 304:
#line 916 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4493 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 305:
#line 918 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Func::make(tuplety(*(yyvsp[-2].mtypes)), *(yyvsp[0].mtype)))); }
#line 4499 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 306:
#line 919 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeTupleType(*(yyvsp[0].mtypes)))); }
#line 4505 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 307:
#line 920 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeSumType(*(yyvsp[0].mtypes)))); }
#line 4511 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 308:
#line 922 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(monoTypeByName(*(yyvsp[0].string)))); }
#line 4517 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 309:
#line 923 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(OpaquePtr::make(str::replace<char>(*(yyvsp[-1].string), ".", "::"), 0, false))); }
#line 4523 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 310:
#line 924 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("[]"))); }
#line 4529 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 311:
#line 925 "hexpr.y" /* yacc.c:1646  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(Array::make(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4535 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 312:
#line 926 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(FixedArray::make(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype)))); }
#line 4541 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 313:
#line 927 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("->"))); }
#line 4547 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 314:
#line 928 "hexpr.y" /* yacc.c:1646  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(clone(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4553 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 315:
#line 929 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeRecType(*(yyvsp[-1].mreclist)))); }
#line 4559 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 316:
#line 930 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeVarType(*(yyvsp[-1].mvarlist)))); }
#line 4565 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 317:
#line 931 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("unit"))); }
#line 4571 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 318:
#line 932 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(((yyvsp[0].intv) == 0) ? Prim::make("void") : TLong::make((yyvsp[0].intv)))); }
#line 4577 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 319:
#line 933 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr((yyvsp[0].boolv) ? TLong::make(1) : TLong::make(0))); }
#line 4583 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 320:
#line 934 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Exists::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4589 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 321:
#line 935 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype), *(yyvsp[0].mtype)))); }
#line 4595 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 322:
#line 936 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype)))); }
#line 4601 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 323:
#line 937 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Recursive::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4607 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 324:
#line 938 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TString::make(str::unescape(str::trimq(*(yyvsp[0].string)))))); }
#line 4613 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 325:
#line 939 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp))))))); }
#line 4619 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 326:
#line 941 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TVar::make(*(yyvsp[0].string)))); }
#line 4625 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 327:
#line 942 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TLong::make((yyvsp[0].intv)))); }
#line 4631 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 328:
#line 944 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4637 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 329:
#line 945 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[-2].string); *(yyval.string) += "."; *(yyval.string) += *(yyvsp[0].string); }
#line 4643 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 330:
#line 947 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4649 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 331:
#line 948 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-1].mtypes)->insert((yyvsp[-1].mtypes)->begin(), *(yyvsp[-3].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4655 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 332:
#line 950 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4661 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 333:
#line 951 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-2].mtypes); }
#line 4667 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 334:
#line 953 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4673 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 335:
#line 954 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4679 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 336:
#line 956 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[-2].mtype)); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4685 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 337:
#line 957 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4691 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 338:
#line 959 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mreclist) = (yyvsp[-4].mreclist);                                 (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4697 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 339:
#line 960 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mreclist) = autorelease(new Record::Members()); (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4703 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 340:
#line 962 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = (yyvsp[-4].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4709 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 341:
#line 963 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = (yyvsp[-2].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4715 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 342:
#line 964 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4721 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 343:
#line 965 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4727 "hexpr.parse.C" /* yacc.c:1646  */
    break;


#line 4731 "hexpr.parse.C" /* yacc.c:1646  */
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
#line 969 "hexpr.y" /* yacc.c:1906  */

#pragma GCC diagnostic pop


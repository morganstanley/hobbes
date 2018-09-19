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
  float                                   floatv;
  double                                  doublev;

  hobbes::Grammar*             prules;
  hobbes::Grammar::value_type* prule;
  hobbes::GrammarRules*        prdefs;
  hobbes::GrammarRule*         prdef;
  hobbes::BoundGrammarValues*  pbelems;
  hobbes::BoundGrammarValue*   pbelem;
  hobbes::GrammarValue*        pvalue;

#line 523 "hexpr.parse.C" /* yacc.c:355  */
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

#line 552 "hexpr.parse.C" /* yacc.c:358  */

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
#define YYFINAL  69
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2820

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  93
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  76
/* YYNRULES -- Number of rules.  */
#define YYNRULES  343
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  759

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
       0,   490,   490,   491,   492,   493,   496,   497,   499,   500,
     501,   503,   504,   505,   506,   507,   509,   510,   511,   512,
     513,   514,   515,   516,   517,   518,   519,   520,   523,   526,
     529,   530,   533,   535,   538,   539,   540,   541,   542,   543,
     544,   545,   547,   548,   550,   552,   553,   555,   558,   559,
     560,   561,   563,   564,   566,   569,   571,   573,   574,   576,
     578,   580,   581,   582,   583,   584,   585,   586,   587,   588,
     589,   590,   591,   592,   593,   594,   595,   596,   597,   598,
     600,   601,   603,   604,   607,   608,   609,   610,   611,   612,
     613,   614,   615,   617,   618,   620,   621,   622,   623,   624,
     625,   626,   627,   628,   630,   631,   632,   633,   634,   636,
     637,   638,   639,   641,   644,   645,   648,   651,   654,   666,
     667,   670,   672,   673,   675,   677,   678,   680,   681,   684,
     685,   688,   689,   690,   691,   692,   693,   694,   695,   696,
     699,   700,   701,   702,   703,   706,   707,   708,   711,   714,
     715,   718,   719,   720,   721,   722,   723,   724,   725,   726,
     727,   728,   729,   730,   731,   734,   737,   738,   739,   740,
     741,   742,   743,   744,   745,   746,   747,   748,   749,   750,
     751,   752,   753,   754,   757,   759,   760,   762,   764,   765,
     767,   769,   770,   772,   773,   775,   776,   777,   779,   780,
     782,   783,   785,   786,   788,   789,   792,   793,   795,   796,
     797,   798,   799,   800,   801,   802,   803,   804,   805,   806,
     807,   808,   809,   810,   811,   812,   813,   814,   815,   816,
     817,   818,   820,   821,   822,   823,   824,   826,   828,   829,
     831,   832,   834,   835,   837,   839,   840,   841,   843,   844,
     845,   846,   847,   848,   849,   850,   851,   852,   853,   854,
     855,   856,   858,   859,   860,   861,   863,   864,   866,   867,
     868,   870,   871,   872,   874,   875,   878,   880,   881,   883,
     884,   885,   886,   887,   888,   889,   890,   891,   892,   894,
     895,   896,   897,   899,   900,   901,   902,   904,   905,   906,
     908,   909,   911,   912,   914,   915,   916,   918,   919,   920,
     921,   922,   923,   924,   925,   926,   927,   928,   929,   930,
     931,   932,   933,   934,   935,   937,   938,   940,   941,   943,
     944,   946,   947,   949,   950,   952,   953,   955,   956,   958,
     959,   960,   961,   963
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

#define YYPACT_NINF -576

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-576)))

#define YYTABLE_NINF -343

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     604,  1168,  2060,  2060,   126,    23,    23,    88,    88,   111,
     111,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,
    -576,  -576,  -576,  -576,  -576,  -576,  -576,   655,    45,  2060,
    2606,    66,  2606,    23,   103,  1435,  2060,   655,   134,  2060,
    -576,  1252,  -576,  -576,  -576,  -576,  -576,  -576,   159,  -576,
     384,   180,    16,   242,  2372,  2216,  2060,  1592,  2741,  2741,
     348,     3,   644,   580,   656,  -576,   222,   348,  -576,  -576,
     276,   261,  -576,   783,    12,  -576,  -576,   125,   478,   335,
      88,   347,  1786,  -576,  -576,  -576,  -576,  -576,  -576,  -576,
    -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  2741,    23,
     -26,  -576,   292,  -576,   205,   161,  2528,    23,   161,   372,
    2138,   363,   377,  2294,   444,   453,   463,   655,   475,   488,
     508,   513,   517,   522,   524,   529,  1355,   531,   538,   541,
    -576,  -576,   549,   348,   281,   331,   164,   107,   421,   435,
     182,   237,  -576,  1864,  1864,  2741,  2060,  1748,    16,  -576,
    -576,   655,  2060,   152,  -576,  -576,   357,   363,   377,  2294,
     444,   453,   463,   475,   488,   508,   517,   522,   524,   529,
     531,   538,   541,  -576,  -576,  -576,  -576,  -576,  -576,  -576,
    -576,  -576,  -576,  -576,  -576,  -576,  2741,  2741,    23,   343,
     180,  2675,  -576,  -576,  -576,  2708,  2060,  2060,  2060,  2216,
    2216,  2372,  2372,  2372,  2372,  2372,  2372,  2372,  2372,  2372,
    2372,  2372,  2450,  2450,  2450,  2060,  1252,    23,  -576,  -576,
    -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,
    -576,  -576,  -576,  -576,  -576,  1864,  -576,  1864,  -576,  -576,
    -576,    23,    23,   520,   687,  1942,  1942,    23,  2060,   386,
    -576,   206,  1942,    23,    22,    88,   783,    23,   520,    23,
      23,    34,  -576,    60,   568,   593,   586,  -576,  -576,   414,
     566,   212,  -576,   614,  2060,   104,  2216,   590,   591,   161,
      28,  -576,   623,  2606,  1514,   655,   400,  1670,  -576,   638,
     639,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,
    -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,
    -576,  2060,  2741,  1826,  -576,  -576,   193,  2060,  2060,  2060,
    -576,  -576,   888,  -576,   647,  -576,  -576,  -576,   425,  2060,
     146,  -576,   348,  2060,   211,  2060,   455,   389,   485,   643,
     183,  2060,  -576,  2060,   618,   618,  -576,  -576,  -576,  -576,
    -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,
    -576,  -576,  -576,   348,  1252,  -576,  -576,  -576,   658,   -12,
     630,  -576,   984,  -576,    40,  1786,  -576,   300,   520,     8,
     499,   677,   171,   410,    72,   671,   301,  -576,   478,   440,
    1942,  1942,   655,  1942,  1942,  1942,   263,  1942,   631,    88,
     704,    23,    23,  1786,   642,   690,   691,   712,  -576,  1942,
    1942,  1942,  1942,  -576,   654,  2741,  -576,    32,  2741,   348,
    2060,  -576,  -576,   449,  2741,   591,  -576,  -576,  -576,  -576,
     148,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,
    -576,  -576,  -576,  1514,  1904,   655,   507,   180,  -576,   614,
    -576,  2060,  -576,  -576,  2060,   348,   694,  -576,   422,  -576,
     693,   348,   437,   459,   520,   259,  1786,  -576,   483,  1982,
    -576,   348,  2060,   213,   284,  -576,   659,  -576,   660,  -576,
      43,  2741,  2741,  -576,   348,   348,  1942,  -576,  -576,  -576,
    -576,  1942,   653,  -576,  1942,  -576,    23,  1786,  1942,  1786,
    -576,    23,  1786,  1942,  -576,  -576,  1942,  1942,  1942,    37,
     121,   401,   631,   631,   631,  -576,   631,   631,    25,    88,
     704,  -576,    18,  -576,    38,  -576,  -576,    69,  1786,  1786,
    1786,    88,   712,  -576,   631,   631,   631,   631,  -576,  -576,
    -576,  -576,  -576,   348,   701,   493,  -576,   454,  2640,  -576,
     661,  -576,    20,  2606,   702,   196,   398,   497,  2060,  -576,
    2060,  -576,  -576,  -576,  -576,  -576,   561,   348,  2060,   257,
    2060,  -576,  -576,  -576,   662,   664,   631,   236,   552,   -14,
     711,  -576,    47,   199,   666,   713,   668,   101,   631,    95,
     141,   715,    99,   717,  1942,  1942,  1942,  1942,  1942,   704,
      23,  -576,  -576,   704,    23,    23,  -576,   712,  -576,   456,
    -576,  -576,   714,  -576,    23,   696,   449,    23,  2060,  2060,
    2060,  -576,  -576,  -576,  2060,  -576,  -576,   721,   161,  1904,
    1904,  -576,  -576,  -576,   317,   348,  -576,   348,  2060,   298,
     348,  -576,  -576,   719,  -576,   724,  -576,   722,  1786,  1942,
     728,   720,  1786,   729,  1942,  1942,  1942,  1942,  1942,  1942,
     631,   631,   631,   631,   631,   704,    19,   704,  -576,    23,
     712,  -576,  1786,  2060,   731,  2060,  -576,   733,   348,   106,
     348,  -576,   526,   500,  -576,  2060,   348,  2060,   306,  1942,
     695,  1942,  -576,   160,  1942,  1942,  -576,  1942,   233,   258,
     330,   173,   239,   131,   704,  -576,   348,  2060,   348,  2060,
    2060,  -576,  -576,  -576,   584,   348,  2060,   320,   631,  -576,
     631,   739,   631,   631,   631,   740,  -576,  -576,  1942,  -576,
    1942,   704,   348,   348,   348,  -576,   348,  2060,   346,  1942,
    1942,   341,   361,   348,  2060,   407,   631,   631,  -576,  -576,
     348,  2060,   409,   348,  2060,   742,   348,  2060,   348
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,     8,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   151,   152,   153,   154,   155,   156,   157,   158,   159,
     160,   343,   161,   198,   163,   164,   265,     0,     0,     0,
       0,     0,     0,     0,     0,   271,   271,   245,     0,     0,
       2,     7,     9,    11,    12,    13,    14,    15,     0,    28,
     113,   162,   148,   130,     0,     0,     0,   271,     0,     0,
       4,    92,    94,   103,   108,   112,   130,     5,   130,     1,
       0,    29,   327,     0,     0,    57,    59,     0,     0,     0,
       0,     0,     0,   256,   251,   255,   250,   249,   252,   253,
     261,   254,   257,   258,   259,   260,   264,   248,   239,     0,
       0,   123,     0,   232,     0,   201,     0,     0,   149,     0,
       0,     0,     0,     0,     0,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    66,     0,   272,     0,   272,     0,     0,     0,     0,
       0,     0,    10,     0,     0,     0,   271,     0,   147,   199,
     263,     0,     0,     0,   107,    86,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   208,   209,   210,   216,   211,   212,   213,
     214,   215,   217,   221,   219,   220,   239,   239,     0,     0,
     218,     0,   237,   207,   231,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     8,     0,    74,    75,
      76,    77,    78,    79,    64,    68,    67,    65,    69,    70,
      71,    72,    61,    62,    73,     0,    58,     0,   318,   317,
     323,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     277,     0,   307,     0,    38,    55,    59,     0,     0,     0,
       0,    48,    82,   333,     0,   305,   306,   307,   241,     0,
     238,     0,   243,     0,     0,     0,     0,     0,     0,   200,
       0,   186,     0,     0,   239,   245,     0,     0,   126,     0,
     130,   166,   167,   168,   169,   170,   171,   174,   173,   172,
     175,   176,   179,   177,   178,   183,   180,   181,   182,    60,
     165,     0,     0,     0,   135,   145,     0,     0,     0,     0,
     142,   184,     0,    32,     0,   275,   121,   117,     0,     0,
       0,   262,    16,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   206,     0,    87,    88,    89,    90,    91,    97,
      96,    95,    98,    99,   100,   101,   102,   106,   104,   105,
     109,   110,   111,     3,     6,   328,    30,    31,     0,     0,
       0,   316,     0,   303,   333,     0,   309,     0,     0,     0,
       0,   307,     0,     0,   307,     0,     0,   276,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   279,   300,     0,
       0,     0,     0,     0,   303,     0,   342,     0,    83,     0,
       0,     0,     0,   233,     0,     0,   235,     0,     0,   114,
       0,   122,   124,     0,     0,   116,   203,   118,   185,   192,
       0,   151,   152,   153,   154,   155,   156,   157,   158,   160,
     161,   163,   164,   239,   239,   245,     0,   162,   130,     0,
     128,     0,   119,   125,     0,   273,     0,   132,     0,   146,
       0,   246,     0,     0,     0,   333,     0,   129,     0,     0,
     136,    17,     0,     0,     0,   227,     0,   222,     0,   229,
       0,     0,     0,   224,    84,    85,     0,   308,   312,   313,
     302,     0,     0,   310,     0,   314,     0,     0,     0,     0,
     315,     0,     0,     0,   324,   278,     0,     0,     0,     0,
       0,     0,   280,   282,   281,   321,   320,   301,    34,     0,
      40,    45,    39,    42,     0,    80,    56,    49,     0,     0,
       0,     0,    50,    52,   335,   304,   334,   336,   234,   240,
     236,   242,   244,   115,     0,     0,   266,     0,     0,   202,
     187,   189,     0,     0,     0,     0,     0,     0,     0,   131,
       0,   141,   140,   274,   139,   138,     0,    18,     0,     0,
       0,   228,   223,   230,     0,     0,   319,     0,     0,     0,
       0,   338,   333,     0,     0,   340,   341,   333,   322,     0,
       0,   307,     0,   307,     0,     0,     0,     0,     0,     0,
       0,    47,    46,     0,     0,     0,    81,     0,   331,     0,
     341,    54,     0,    53,     0,   143,     0,     0,     0,     0,
       0,   192,   197,   196,     0,   191,   194,   195,   150,     0,
       0,   142,   120,   127,     0,   247,   137,    19,     0,     0,
      93,   226,   225,     0,   326,     0,   325,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     299,   292,   291,   290,   289,    36,    35,    41,    43,    44,
      51,   330,     0,     0,     0,     0,   267,     0,   268,     0,
     204,   188,     0,     0,   133,     0,    20,     0,     0,     0,
       0,     0,   337,     0,     0,     0,   339,     0,   335,     0,
       0,     0,     0,     0,     0,   332,    33,     0,   144,     0,
       0,   190,   193,   195,     0,    21,     0,     0,   288,   311,
     286,     0,   294,   298,   297,     0,   285,   283,     0,   293,
       0,    37,   270,   269,   205,   134,    22,     0,     0,     0,
       0,     0,     0,    23,     0,     0,   287,   296,   284,   295,
      24,     0,     0,    25,     0,     0,    26,     0,    27
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -576,  -576,  -576,   564,   -27,  -576,  -576,   262,  -576,  -576,
     184,   178,  -575,  -499,  -576,   176,  -513,  -377,    58,     1,
     539,   192,   399,    -2,   -35,   651,   -36,   474,    10,  -576,
     528,  -576,   514,   -20,  -576,   525,  -576,   189,  -576,  -576,
     128,   -55,  -576,  -576,   379,   -52,  -576,   -95,   -28,  -170,
    -576,  -175,  -378,  -576,   -21,   -48,  -576,   197,   -31,  -121,
     621,  -576,   428,  -576,   573,   -19,   708,  -576,   576,  -576,
    -576,  -576,  -576,  -576,  -576,   397
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,    40,    41,    42,    43,    44,    45,   611,    46,
     522,   523,   520,   521,    47,   532,   533,   254,   255,    48,
     132,   524,   261,   133,    61,    62,    63,    64,    65,   100,
     101,   287,   288,    50,   280,   281,   550,   551,   552,   625,
     626,    51,   106,   425,   426,   191,   192,   102,   268,   269,
     270,   271,   272,   137,   138,    52,   545,   546,   134,   323,
     324,   249,   250,   397,   372,   325,   263,   645,    71,   264,
     609,   265,   266,   380,   383,    68
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      60,    67,   148,   190,   190,   136,    96,   195,    75,    75,
     105,    49,   108,   338,   142,   289,   336,   337,   154,   613,
     155,   602,   518,   326,   665,   603,   704,   104,   667,   400,
     193,   193,   599,   217,   135,   622,    21,   141,   199,   541,
     487,   407,   274,   190,    21,   623,   150,    21,   238,   235,
     494,    49,    21,   239,   156,   275,    21,   148,    21,   240,
     148,   151,    21,   262,   647,    74,    77,    21,   241,    21,
     605,   200,  -329,   396,   594,   236,   607,   154,   236,  -329,
     409,    75,   491,   238,   242,    73,   279,   409,   239,   649,
     190,   155,  -329,    21,   240,   396,    96,   624,   604,   604,
     409,   401,   541,   241,   600,   502,   427,   258,   286,   244,
     540,   259,    21,   260,   366,   328,   367,   327,    98,   242,
     247,   573,    99,   154,   396,   248,    69,   396,    21,   731,
     331,   190,   190,  -329,   396,    21,   190,   655,   710,   658,
     190,   654,   258,   107,   244,   330,   259,   396,   260,    21,
     332,  -342,  -342,   139,   612,   247,   595,   613,    21,   596,
     248,    73,   237,   342,   347,   348,   602,   342,   602,   196,
     197,   730,   420,   357,   358,   359,    21,    98,    26,   469,
     110,    99,   396,   656,    78,   315,   396,   316,   396,   333,
     198,    26,   289,    27,   344,   345,   346,   143,    73,    83,
      84,    85,    86,    87,    88,    89,    27,   149,   396,   196,
     197,   498,    90,   363,   499,   728,   553,    21,   396,   319,
     482,   146,   470,   147,   373,   373,    49,    91,   396,   447,
     198,   148,   602,   630,   146,    21,   147,    21,   721,   404,
     314,   422,   408,   389,   311,   390,   386,   396,   472,   391,
     568,   392,   393,    92,   394,   395,   236,   190,   396,   215,
     396,   320,   483,   430,    93,    94,    21,   277,   196,   197,
     338,   459,   419,   336,   337,   631,    95,   238,   650,   152,
     -59,    21,   239,   216,   456,   286,   396,    21,   240,   198,
     416,  -329,   417,   396,   638,   460,   389,   241,   390,   409,
     196,   197,   391,   404,   392,   393,   217,   394,   395,   455,
     643,   458,   725,   242,   238,   461,   462,   463,   729,   239,
     396,   198,    21,   396,    21,   240,   396,   468,   321,   276,
      21,   471,   726,   474,   241,   687,   378,   142,   244,   484,
     259,   485,   260,   716,    21,   396,   396,   196,   197,   247,
     242,   570,   515,   490,   248,   310,   492,   737,   490,   373,
     190,   311,   339,   190,   196,   197,   253,    21,   198,   190,
      21,   511,   548,   258,    49,   244,   493,   259,   257,   260,
     196,   197,   148,   744,   262,   198,   247,   539,   447,   447,
     542,   248,   504,   684,   196,   197,   193,   685,    53,    66,
      75,   198,    70,    72,    76,    76,    80,    82,   727,   283,
     312,   196,   197,   136,    26,   198,   313,   396,   543,   748,
     196,   197,   144,   335,    97,   103,   190,   190,   396,    27,
     109,    21,   198,    21,    97,   140,   597,   291,    53,   598,
     749,   198,   135,   145,   751,   404,   754,   563,   396,   556,
     153,   292,   557,   574,   575,   194,   194,   146,   317,   147,
     387,   196,   197,   196,   197,   477,   388,   566,   544,   478,
     567,    76,   318,    21,    76,   252,   632,   256,   581,   267,
     584,   450,   198,   586,   198,   196,   197,   617,   413,   500,
     501,   618,   238,   190,   414,   194,   273,   239,   559,   467,
     196,   197,    21,   240,   282,   311,   198,   290,   408,   608,
     581,   610,   241,   506,    97,   622,   561,   507,   294,   508,
     342,   198,   196,   197,    21,   623,   554,   295,   242,   475,
     671,    21,    75,   628,   238,   476,   672,   296,   562,   239,
     267,   267,   194,   198,    21,   240,   196,   197,    97,   297,
     334,   243,   370,   244,   241,   245,   634,   246,   635,   564,
     196,   197,   298,   479,   247,   480,   637,   198,   640,   248,
     242,   644,   615,   616,   447,   447,    21,   495,   633,   496,
     148,   198,   299,   194,   194,   273,   340,   300,   194,   196,
     197,   301,   194,   258,   371,   244,   302,   259,   303,   260,
     410,   574,   575,   304,   711,   306,   247,     1,     2,     3,
     198,   248,   307,    53,   365,   308,   678,   679,   680,   209,
     210,   211,   682,   309,   196,   197,   412,   462,   463,   692,
      79,    81,   267,   696,   267,   411,   686,   636,   368,    72,
     267,   267,   381,   384,   385,   198,   415,   196,   197,   267,
     399,   418,    76,   705,   403,   267,   405,   406,   267,   429,
     735,    83,    84,    85,    86,    87,    88,    89,   198,   423,
     424,   706,   103,   708,    90,   454,  -232,   282,   466,    21,
     481,   448,   449,   714,   290,   715,   360,   361,   362,    91,
     201,   202,   203,   204,   205,   206,   207,   208,   212,   213,
     214,   238,   198,   486,   488,   732,   239,   733,   734,   194,
     497,    21,   240,    97,   736,    92,   503,   519,   396,   252,
     375,   241,   528,   529,   530,   531,    93,    94,   538,   558,
     560,   473,   578,   571,   614,   743,   572,   242,    95,   629,
     621,   641,   750,   642,   648,   651,   652,   653,   657,   753,
     659,   673,   756,   675,   683,   758,   689,   690,   695,   691,
     258,    53,   244,   376,   259,   694,   260,   697,   707,   267,
     709,   719,   267,   247,   267,   267,   739,   740,   248,   757,
     364,   601,   668,   670,   666,   252,   251,   267,   267,    97,
     267,   267,   267,   267,   267,   402,   256,   669,   525,   526,
     267,   453,   527,   421,   549,   428,   267,   267,   267,   267,
     681,   712,   194,   676,   273,   194,   505,   377,   369,     0,
     547,   194,   218,   219,   220,   221,   222,   223,   224,   225,
     226,   227,     0,   228,   229,   230,   231,     0,     0,     0,
     448,   448,   449,   555,     0,     0,   232,   233,     0,     0,
       0,   234,   349,   350,   351,   352,   353,   354,   355,   356,
       0,   267,     0,   267,     0,     0,     0,   130,     0,     0,
     569,     0,     0,     0,     0,   131,     0,   273,   194,   194,
       0,     0,     0,   267,     0,     0,     0,     0,   267,     0,
       0,   267,     0,   580,   267,   267,   267,     0,   585,   267,
     267,     0,   238,   267,   591,   593,     0,   239,     0,     0,
       0,     0,    21,   240,     0,     0,    76,     0,     0,     0,
     370,   606,   241,     0,   267,   267,   267,   267,   256,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   242,     0,
       0,     0,     0,     0,     0,   194,     0,     0,     0,   627,
       0,   374,     0,   379,   382,     0,     0,     0,     0,     0,
     398,   464,   371,   244,     0,   245,   639,   246,     0,     0,
       0,     0,     0,     0,   247,   646,     0,     0,     0,   248,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   267,   267,   267,   267,   267,     0,   525,   238,     0,
       0,   525,   525,   239,     0,     0,     0,     0,    21,   240,
       0,   674,     0,   547,   677,     0,     0,     0,   241,     0,
       0,     0,     0,     0,     0,     0,   448,   448,     0,     0,
     465,     0,     0,     0,   242,     0,   688,     0,     0,     0,
       0,     0,     0,     0,     0,   267,   267,     0,     0,   267,
       0,   267,   267,   267,   267,   267,   267,   258,   489,   244,
       0,   259,     0,   260,     0,     0,   606,     0,     0,   267,
     247,     0,     0,     0,     0,   248,     0,     0,     0,     0,
     713,     0,     0,     0,     0,   717,   267,     0,   267,     0,
       0,   267,   267,     0,   267,     0,   251,     0,   509,   510,
       0,   512,   513,   514,   516,   517,     0,     0,     0,     0,
       0,     0,     0,     0,   738,     0,     0,   534,   535,   536,
     537,     0,     0,     0,     0,   267,     0,   267,     0,     0,
       0,     0,     0,     0,     0,   745,   267,   267,     0,     0,
       0,     0,   752,     0,     0,     0,     0,     0,     0,   755,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,     0,     5,     0,     6,     7,     8,     9,
      10,     0,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,   576,    23,    24,    25,    26,   577,
       0,     0,   579,     0,     0,   582,   583,     0,     0,     0,
     587,   588,     0,    27,   589,   590,   592,     0,     0,     0,
       0,     0,     0,    28,    29,     0,    30,     0,    31,     0,
       0,     0,     0,     0,     0,     0,     0,    32,    33,    34,
       0,    35,     0,    36,     0,    37,     0,    38,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    39,
       6,     7,     8,     9,    10,     0,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,     0,    23,
      24,    25,    26,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    27,     0,     0,
       0,     0,   660,   661,   662,   663,   664,    28,    29,     0,
      30,     0,    31,     0,     0,     0,     0,     0,     0,     0,
       0,    32,    33,    34,     0,    35,     0,    36,     0,    37,
       0,    38,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    39,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   693,     0,     0,
       0,     0,   698,   699,   700,   701,   702,   703,     0,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,     0,    23,    24,    25,    26,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    54,   718,     0,   720,
      27,     0,   722,   723,     0,   724,     0,     0,     0,     0,
      28,    29,     0,    30,     0,    31,     0,     0,     0,     0,
      56,     0,     0,     0,    32,    33,    34,     0,    57,   305,
      36,     0,    37,     0,    38,     0,   741,     0,   742,     0,
       0,     0,     0,     0,     0,     0,    39,   746,   747,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,     0,    23,    24,    25,    26,     0,     0,     0,     0,
       0,     0,     0,     0,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
      28,    29,     0,    30,     0,    31,     0,     0,   127,   128,
      56,     0,     0,   129,    32,    33,    34,     0,    57,     0,
      36,     0,    37,     0,    38,     0,     0,    58,    59,   130,
       0,     0,     0,     0,     0,     0,    39,   131,   431,   432,
     433,   434,   435,   436,   437,   438,    19,   439,    21,   440,
     183,    23,   441,   442,    26,     0,     0,     0,     0,     0,
       0,     0,     0,   157,   158,   159,   160,   161,   162,    27,
     163,   164,   165,   121,   166,   167,   168,   169,   126,    28,
      29,     0,    30,     0,    31,     0,     0,   170,   171,    56,
       0,     0,   172,    32,    33,    34,     0,   443,     0,   444,
       0,   445,     0,   446,     0,     0,    58,    59,     0,     0,
       0,     0,     0,     0,     0,    39,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,     0,    23,
      24,    25,    26,     0,     0,     0,     0,     0,     0,     0,
       0,   157,   158,   159,   160,   161,   162,    27,   163,   164,
     165,   121,   166,   167,   168,   169,   126,    28,    29,     0,
      30,     0,    31,     0,     0,   170,   171,    56,     0,     0,
     172,    32,    33,    34,     0,    57,     0,    36,     0,    37,
       0,    38,     0,     0,    58,    59,     0,     0,     0,     0,
       0,     0,     0,    39,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,     0,    23,    24,    25,
      26,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    54,     0,     0,     0,    27,     0,     0,     0,     0,
       0,     0,     0,     0,    55,    28,    29,     0,    30,     0,
      31,     0,     0,     0,     0,    56,     0,     0,     0,    32,
      33,    34,   451,   284,     0,    36,     0,   285,   452,    38,
       0,     0,    58,    59,     0,     0,     0,     0,     0,     0,
       0,    39,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,     0,    23,    24,    25,    26,     0,
       0,   329,     0,     0,     0,     0,     0,     0,     0,    54,
       0,     0,     0,    27,     0,     0,     0,     0,     0,     0,
     238,     0,    55,    28,    29,   239,    30,     0,    31,     0,
      21,   240,     0,    56,     0,     0,     0,    32,    33,    34,
     241,    57,     0,    36,     0,    37,     0,    38,     0,     0,
      58,    59,     0,     0,     0,     0,   242,     0,     0,    39,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,     0,    23,    24,    25,    26,     0,     0,   258,
       0,   244,     0,   259,     0,   260,     0,    54,     0,     0,
       0,    27,   247,     0,     0,     0,     0,   248,   238,     0,
      55,    28,    29,   239,    30,     0,    31,     0,    21,   240,
       0,    56,     0,     0,     0,    32,    33,    34,   241,    57,
       0,    36,   457,    37,     0,    38,     0,     0,    58,    59,
       0,     0,     0,     0,   242,     0,     0,    39,   431,   432,
     433,   434,   435,   436,   437,   438,    19,   439,    21,   440,
     183,    23,   441,   442,    26,     0,     0,   322,     0,   244,
       0,   259,     0,   260,     0,    54,     0,     0,     0,    27,
     247,     0,     0,     0,     0,   248,   238,     0,    55,    28,
      29,   239,    30,     0,    31,     0,    21,   240,     0,    56,
       0,     0,     0,    32,    33,    34,   241,   443,     0,   444,
       0,   445,     0,   446,     0,     0,    58,    59,     0,     0,
       0,     0,   242,     0,     0,    39,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,     0,    23,
      24,    25,    26,     0,     0,   378,     0,   244,     0,   259,
       0,   260,     0,    54,     0,     0,     0,    27,   247,     0,
       0,     0,     0,   248,     0,     0,    55,    28,    29,     0,
      30,     0,    31,     0,     0,     0,     0,    56,     0,     0,
       0,    32,    33,    34,     0,    57,     0,    36,   565,    37,
       0,    38,     0,     0,    58,    59,     0,     0,     0,     0,
       0,     0,     0,    39,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,     0,    23,    24,    25,
      26,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    54,     0,     0,     0,    27,     0,     0,     0,     0,
       0,     0,     0,     0,    55,    28,    29,     0,    30,     0,
      31,     0,     0,     0,     0,    56,     0,     0,     0,    32,
      33,    34,     0,    57,     0,    36,     0,    37,     0,    38,
       0,     0,    58,    59,     0,     0,     0,     0,     0,     0,
       0,    39,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,     0,    23,    24,    25,    26,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    54,
       0,     0,     0,    27,     0,     0,     0,     0,     0,     0,
       0,     0,    55,    28,    29,     0,    30,     0,    31,     0,
       0,     0,     0,    56,     0,     0,     0,    32,    33,    34,
       0,   284,     0,    36,     0,   285,     0,    38,     0,     0,
      58,    59,     0,     0,     0,     0,     0,     0,     0,    39,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,     0,    23,    24,    25,    26,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    54,     0,     0,
       0,    27,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    28,    29,     0,    30,     0,    31,     0,     0,     0,
       0,    56,     0,     0,     0,    32,    33,    34,     0,    57,
       0,    36,     0,    37,     0,    38,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    39,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
       0,    23,    24,    25,    26,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    54,     0,     0,     0,    27,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    28,
      29,     0,    30,     0,    31,     0,     0,     0,     0,     0,
       0,     0,     0,    32,    33,    34,     0,    57,   293,    36,
       0,    37,     0,    38,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    39,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,     0,    23,
      24,    25,    26,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    54,     0,     0,     0,    27,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    28,    29,     0,
      30,     0,    31,     0,     0,     0,     0,     0,     0,     0,
       0,    32,    33,    34,     0,    57,     0,    36,     0,    37,
       0,    38,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    39,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,     0,    23,    24,    25,
      26,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    27,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    28,    29,     0,    30,     0,
      31,     0,     0,     0,     0,     0,     0,     0,     0,    32,
      33,    34,     0,    57,     0,    36,     0,    37,     0,    38,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    39,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,     0,    23,    24,    25,    26,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    27,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    29,     0,     0,     0,     0,   278,
       0,     0,     0,     0,     0,     0,     0,    32,    33,     0,
       0,    57,     0,    36,     0,    37,     0,    38,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    39,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,     0,    23,    24,    25,    26,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   619,     0,     0,
       0,    27,     0,     0,   173,   174,   175,   176,   177,   178,
     179,   180,    29,   181,    21,   182,   183,    23,   184,   185,
       0,     0,   620,     0,     0,    32,    33,     0,     0,    57,
       0,    36,     0,    37,     0,    38,     0,     0,     0,   173,
     174,   175,   176,   177,   178,   179,   180,    39,   181,    21,
     182,   183,    23,   184,   185,     0,     0,     0,     0,     0,
       0,     0,     0,   186,     0,   187,     0,   188,     0,   189,
     341,     0,   173,   174,   175,   176,   177,   178,   179,   180,
       0,   181,    21,   182,   183,    23,   184,   185,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   186,     0,
     187,     0,   188,   343,   189,   173,   174,   175,   176,   177,
     178,   179,   180,     0,   181,    21,   182,   183,    23,   184,
     185,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   186,     0,   187,     0,   188,     0,   189,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   186,     0,   187,     0,   188,     0,
     189
};

static const yytype_int16 yycheck[] =
{
       2,     3,    50,    58,    59,    36,    27,    59,     7,     8,
      30,     1,    32,   188,    41,   110,   186,   187,    54,   532,
      55,   520,   399,   144,   599,     7,     7,    29,   603,     7,
      58,    59,     7,    45,    36,    15,    24,    39,    35,   417,
      52,     7,    68,    98,    24,    25,    30,    24,    14,    37,
      42,    41,    24,    19,    56,    81,    24,   105,    24,    25,
     108,    45,    24,    82,    78,     7,     8,    24,    34,    24,
      32,    68,    32,    87,    37,    74,     7,   113,    77,    32,
      40,    80,    42,    14,    50,    73,   106,    40,    19,    42,
     145,   126,    32,    24,    25,    87,   117,    77,    80,    80,
      40,    79,   480,    34,    79,    33,    78,    73,   110,    75,
      78,    77,    24,    79,   235,   146,   237,   145,    73,    50,
      86,    78,    77,   159,    87,    91,     0,    87,    24,   704,
     151,   186,   187,    32,    87,    24,   191,    42,    32,    40,
     195,    40,    73,    77,    75,   147,    77,    87,    79,    24,
     152,    79,    80,    19,   531,    86,    35,   670,    24,    38,
      91,    73,    37,   191,   199,   200,   665,   195,   667,    63,
      64,    40,    68,   209,   210,   211,    24,    73,    30,    33,
      77,    77,    87,    42,    73,    78,    87,    80,    87,    37,
      84,    30,   287,    45,   196,   197,   198,    38,    73,     6,
       7,     8,     9,    10,    11,    12,    45,    27,    87,    63,
      64,    40,    19,   215,    43,    42,    68,    24,    87,    37,
      37,    73,    76,    75,   243,   244,   216,    34,    87,   284,
      84,   279,   731,    37,    73,    24,    75,    24,    78,   258,
      76,   276,   261,    37,    80,    39,   248,    87,    37,    43,
      37,    45,    46,    60,    48,    49,   255,   312,    87,    37,
      87,    79,    79,   283,    71,    72,    24,    62,    63,    64,
     445,    78,   274,   443,   444,    79,    83,    14,    79,    37,
      38,    24,    19,     7,   312,   287,    87,    24,    25,    84,
      78,    32,    80,    87,    37,   316,    37,    34,    39,    40,
      63,    64,    43,   322,    45,    46,    45,    48,    49,   311,
      74,   313,    79,    50,    14,   317,   318,   319,    79,    19,
      87,    84,    24,    87,    24,    25,    87,   329,    91,    37,
      24,   333,    74,   335,    34,    37,    73,   364,    75,   341,
      77,   343,    79,    37,    24,    87,    87,    63,    64,    86,
      50,    67,    89,   372,    91,    74,   375,    37,   377,   378,
     415,    80,    19,   418,    63,    64,    31,    24,    84,   424,
      24,   392,   424,    73,   364,    75,    76,    77,    31,    79,
      63,    64,   430,    37,   403,    84,    86,   415,   443,   444,
     418,    91,    91,    76,    63,    64,   424,    80,     1,     2,
     399,    84,     5,     6,     7,     8,     9,    10,    78,    37,
      79,    63,    64,   444,    30,    84,    85,    87,   420,    78,
      63,    64,    38,    66,    27,    28,   481,   482,    87,    45,
      33,    24,    84,    24,    37,    38,    35,    74,    41,    38,
      79,    84,   444,    59,    37,   464,    37,   466,    87,   451,
      53,    74,   454,   481,   482,    58,    59,    73,    37,    75,
      74,    63,    64,    63,    64,    76,    80,   469,    19,    80,
     472,    74,    37,    24,    77,    78,    78,    80,   497,    82,
     499,    81,    84,   502,    84,    63,    64,    33,    74,    79,
      80,    37,    14,   548,    80,    98,    99,    19,    76,    74,
      63,    64,    24,    25,   107,    80,    84,   110,   527,   528,
     529,   530,    34,    73,   117,    15,    79,    77,    74,    79,
     548,    84,    63,    64,    24,    25,    19,    74,    50,    74,
      74,    24,   531,   553,    14,    80,    80,    74,    79,    19,
     143,   144,   145,    84,    24,    25,    63,    64,   151,    74,
     153,    73,    32,    75,    34,    77,   558,    79,   560,    76,
      63,    64,    74,    78,    86,    80,   568,    84,   570,    91,
      50,    19,    79,    80,   629,   630,    24,    78,    81,    80,
     628,    84,    74,   186,   187,   188,   189,    74,   191,    63,
      64,    74,   195,    73,    74,    75,    74,    77,    74,    79,
      32,   629,   630,    74,    78,    74,    86,     3,     4,     5,
      84,    91,    74,   216,   217,    74,   618,   619,   620,    39,
      40,    41,   624,    74,    63,    64,    40,   629,   630,   648,
       9,    10,   235,   652,   237,    42,   638,    76,   241,   242,
     243,   244,   245,   246,   247,    84,    80,    63,    64,   252,
     253,    37,   255,   672,   257,   258,   259,   260,   261,    36,
      76,     6,     7,     8,     9,    10,    11,    12,    84,    79,
      79,   673,   275,   675,    19,    37,    37,   280,    31,    24,
      37,   284,   285,   685,   287,   687,   212,   213,   214,    34,
      46,    47,    48,    49,    50,    51,    52,    53,    42,    43,
      44,    14,    84,    45,    74,   707,    19,   709,   710,   312,
      33,    24,    25,   316,   716,    60,    45,    13,    87,   322,
      33,    34,    80,    33,    33,    13,    71,    72,    74,    35,
      37,   334,    79,    74,    33,   737,    76,    50,    83,    37,
      79,    79,   744,    79,    33,    79,    33,    79,    33,   751,
      33,    37,   754,    57,    33,   757,    37,    33,    38,    37,
      73,   364,    75,    76,    77,    37,    79,    38,    37,   372,
      37,    76,   375,    86,   377,   378,    37,    37,    91,    37,
     216,   519,   604,   607,   600,   388,    78,   390,   391,   392,
     393,   394,   395,   396,   397,   256,   399,   605,   401,   402,
     403,   287,   403,   275,   425,   280,   409,   410,   411,   412,
     621,   683,   415,   616,   417,   418,   388,   244,   242,    -1,
     423,   424,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    52,    53,    -1,    -1,    -1,
     443,   444,   445,   446,    -1,    -1,    63,    64,    -1,    -1,
      -1,    68,   201,   202,   203,   204,   205,   206,   207,   208,
      -1,   464,    -1,   466,    -1,    -1,    -1,    84,    -1,    -1,
     473,    -1,    -1,    -1,    -1,    92,    -1,   480,   481,   482,
      -1,    -1,    -1,   486,    -1,    -1,    -1,    -1,   491,    -1,
      -1,   494,    -1,   496,   497,   498,   499,    -1,   501,   502,
     503,    -1,    14,   506,   507,   508,    -1,    19,    -1,    -1,
      -1,    -1,    24,    25,    -1,    -1,   519,    -1,    -1,    -1,
      32,   524,    34,    -1,   527,   528,   529,   530,   531,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    -1,
      -1,    -1,    -1,    -1,    -1,   548,    -1,    -1,    -1,   552,
      -1,   243,    -1,   245,   246,    -1,    -1,    -1,    -1,    -1,
     252,    73,    74,    75,    -1,    77,   569,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    86,   578,    -1,    -1,    -1,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   594,   595,   596,   597,   598,    -1,   600,    14,    -1,
      -1,   604,   605,    19,    -1,    -1,    -1,    -1,    24,    25,
      -1,   614,    -1,   616,   617,    -1,    -1,    -1,    34,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   629,   630,    -1,    -1,
     322,    -1,    -1,    -1,    50,    -1,   639,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   648,   649,    -1,    -1,   652,
      -1,   654,   655,   656,   657,   658,   659,    73,    74,    75,
      -1,    77,    -1,    79,    -1,    -1,   669,    -1,    -1,   672,
      86,    -1,    -1,    -1,    -1,    91,    -1,    -1,    -1,    -1,
     683,    -1,    -1,    -1,    -1,   688,   689,    -1,   691,    -1,
      -1,   694,   695,    -1,   697,    -1,   388,    -1,   390,   391,
      -1,   393,   394,   395,   396,   397,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   717,    -1,    -1,   409,   410,   411,
     412,    -1,    -1,    -1,    -1,   728,    -1,   730,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   738,   739,   740,    -1,    -1,
      -1,    -1,   745,    -1,    -1,    -1,    -1,    -1,    -1,   752,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   464,    -1,     6,    -1,     8,     9,    10,    11,
      12,    -1,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,   486,    27,    28,    29,    30,   491,
      -1,    -1,   494,    -1,    -1,   497,   498,    -1,    -1,    -1,
     502,   503,    -1,    45,   506,   507,   508,    -1,    -1,    -1,
      -1,    -1,    -1,    55,    56,    -1,    58,    -1,    60,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    70,    71,
      -1,    73,    -1,    75,    -1,    77,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,
       8,     9,    10,    11,    12,    -1,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    -1,    27,
      28,    29,    30,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    45,    -1,    -1,
      -1,    -1,   594,   595,   596,   597,   598,    55,    56,    -1,
      58,    -1,    60,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    69,    70,    71,    -1,    73,    -1,    75,    -1,    77,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   649,    -1,    -1,
      -1,    -1,   654,   655,   656,   657,   658,   659,    -1,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    -1,    27,    28,    29,    30,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    41,   689,    -1,   691,
      45,    -1,   694,   695,    -1,   697,    -1,    -1,    -1,    -1,
      55,    56,    -1,    58,    -1,    60,    -1,    -1,    -1,    -1,
      65,    -1,    -1,    -1,    69,    70,    71,    -1,    73,    74,
      75,    -1,    77,    -1,    79,    -1,   728,    -1,   730,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    91,   739,   740,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    -1,    27,    28,    29,    30,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    -1,    60,    -1,    -1,    63,    64,
      65,    -1,    -1,    68,    69,    70,    71,    -1,    73,    -1,
      75,    -1,    77,    -1,    79,    -1,    -1,    82,    83,    84,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    -1,    60,    -1,    -1,    63,    64,    65,
      -1,    -1,    68,    69,    70,    71,    -1,    73,    -1,    75,
      -1,    77,    -1,    79,    -1,    -1,    82,    83,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    -1,    27,
      28,    29,    30,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    -1,    60,    -1,    -1,    63,    64,    65,    -1,    -1,
      68,    69,    70,    71,    -1,    73,    -1,    75,    -1,    77,
      -1,    79,    -1,    -1,    82,    83,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    -1,    27,    28,    29,
      30,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    41,    -1,    -1,    -1,    45,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    54,    55,    56,    -1,    58,    -1,
      60,    -1,    -1,    -1,    -1,    65,    -1,    -1,    -1,    69,
      70,    71,    72,    73,    -1,    75,    -1,    77,    78,    79,
      -1,    -1,    82,    83,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    -1,    27,    28,    29,    30,    -1,
      -1,    33,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    41,
      -1,    -1,    -1,    45,    -1,    -1,    -1,    -1,    -1,    -1,
      14,    -1,    54,    55,    56,    19,    58,    -1,    60,    -1,
      24,    25,    -1,    65,    -1,    -1,    -1,    69,    70,    71,
      34,    73,    -1,    75,    -1,    77,    -1,    79,    -1,    -1,
      82,    83,    -1,    -1,    -1,    -1,    50,    -1,    -1,    91,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    -1,    27,    28,    29,    30,    -1,    -1,    73,
      -1,    75,    -1,    77,    -1,    79,    -1,    41,    -1,    -1,
      -1,    45,    86,    -1,    -1,    -1,    -1,    91,    14,    -1,
      54,    55,    56,    19,    58,    -1,    60,    -1,    24,    25,
      -1,    65,    -1,    -1,    -1,    69,    70,    71,    34,    73,
      -1,    75,    76,    77,    -1,    79,    -1,    -1,    82,    83,
      -1,    -1,    -1,    -1,    50,    -1,    -1,    91,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    -1,    -1,    73,    -1,    75,
      -1,    77,    -1,    79,    -1,    41,    -1,    -1,    -1,    45,
      86,    -1,    -1,    -1,    -1,    91,    14,    -1,    54,    55,
      56,    19,    58,    -1,    60,    -1,    24,    25,    -1,    65,
      -1,    -1,    -1,    69,    70,    71,    34,    73,    -1,    75,
      -1,    77,    -1,    79,    -1,    -1,    82,    83,    -1,    -1,
      -1,    -1,    50,    -1,    -1,    91,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    -1,    27,
      28,    29,    30,    -1,    -1,    73,    -1,    75,    -1,    77,
      -1,    79,    -1,    41,    -1,    -1,    -1,    45,    86,    -1,
      -1,    -1,    -1,    91,    -1,    -1,    54,    55,    56,    -1,
      58,    -1,    60,    -1,    -1,    -1,    -1,    65,    -1,    -1,
      -1,    69,    70,    71,    -1,    73,    -1,    75,    76,    77,
      -1,    79,    -1,    -1,    82,    83,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    -1,    27,    28,    29,
      30,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    41,    -1,    -1,    -1,    45,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    54,    55,    56,    -1,    58,    -1,
      60,    -1,    -1,    -1,    -1,    65,    -1,    -1,    -1,    69,
      70,    71,    -1,    73,    -1,    75,    -1,    77,    -1,    79,
      -1,    -1,    82,    83,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    -1,    27,    28,    29,    30,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    41,
      -1,    -1,    -1,    45,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    54,    55,    56,    -1,    58,    -1,    60,    -1,
      -1,    -1,    -1,    65,    -1,    -1,    -1,    69,    70,    71,
      -1,    73,    -1,    75,    -1,    77,    -1,    79,    -1,    -1,
      82,    83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    -1,    27,    28,    29,    30,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    41,    -1,    -1,
      -1,    45,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    55,    56,    -1,    58,    -1,    60,    -1,    -1,    -1,
      -1,    65,    -1,    -1,    -1,    69,    70,    71,    -1,    73,
      -1,    75,    -1,    77,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      -1,    27,    28,    29,    30,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    41,    -1,    -1,    -1,    45,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,
      56,    -1,    58,    -1,    60,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    70,    71,    -1,    73,    74,    75,
      -1,    77,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    -1,    27,
      28,    29,    30,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    41,    -1,    -1,    -1,    45,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    56,    -1,
      58,    -1,    60,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    69,    70,    71,    -1,    73,    -1,    75,    -1,    77,
      -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    -1,    27,    28,    29,
      30,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    55,    56,    -1,    58,    -1,
      60,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,
      70,    71,    -1,    73,    -1,    75,    -1,    77,    -1,    79,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    -1,    27,    28,    29,    30,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    56,    -1,    -1,    -1,    -1,    61,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    70,    -1,
      -1,    73,    -1,    75,    -1,    77,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    -1,    27,    28,    29,    30,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     7,    -1,    -1,
      -1,    45,    -1,    -1,    14,    15,    16,    17,    18,    19,
      20,    21,    56,    23,    24,    25,    26,    27,    28,    29,
      -1,    -1,    32,    -1,    -1,    69,    70,    -1,    -1,    73,
      -1,    75,    -1,    77,    -1,    79,    -1,    -1,    -1,    14,
      15,    16,    17,    18,    19,    20,    21,    91,    23,    24,
      25,    26,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    75,    -1,    77,    -1,    79,
      45,    -1,    14,    15,    16,    17,    18,    19,    20,    21,
      -1,    23,    24,    25,    26,    27,    28,    29,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      75,    -1,    77,    45,    79,    14,    15,    16,    17,    18,
      19,    20,    21,    -1,    23,    24,    25,    26,    27,    28,
      29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    75,    -1,    77,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    75,    -1,    77,    -1,
      79
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,    94,     6,     8,     9,    10,    11,
      12,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    27,    28,    29,    30,    45,    55,    56,
      58,    60,    69,    70,    71,    73,    75,    77,    79,    91,
      95,    96,    97,    98,    99,   100,   102,   107,   112,   121,
     126,   134,   148,   168,    41,    54,    65,    73,    82,    83,
     116,   117,   118,   119,   120,   121,   168,   116,   168,     0,
     168,   161,   168,    73,   111,   112,   168,   111,    73,   153,
     168,   153,   168,     6,     7,     8,     9,    10,    11,    12,
      19,    34,    60,    71,    72,    83,   147,   168,    73,    77,
     122,   123,   140,   168,   116,   126,   135,    77,   126,   168,
      77,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    63,    64,    68,
      84,    92,   113,   116,   151,   116,   151,   146,   147,    19,
     168,   116,    97,    38,    38,    59,    73,    75,   148,    27,
      30,    45,    37,   168,   119,   117,   116,    39,    40,    41,
      42,    43,    44,    46,    47,    48,    50,    51,    52,    53,
      63,    64,    68,    14,    15,    16,    17,    18,    19,    20,
      21,    23,    25,    26,    28,    29,    73,    75,    77,    79,
     134,   138,   139,   141,   168,   138,    63,    64,    84,    35,
      68,    46,    47,    48,    49,    50,    51,    52,    53,    39,
      40,    41,    42,    43,    44,    37,     7,    45,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    50,    51,
      52,    53,    63,    64,    68,    37,   112,    37,    14,    19,
      25,    34,    50,    73,    75,    77,    79,    86,    91,   154,
     155,   159,   168,    31,   110,   111,   168,    31,    73,    77,
      79,   115,   158,   159,   162,   164,   165,   168,   141,   142,
     143,   144,   145,   168,    68,    81,    37,    62,    61,   126,
     127,   128,   168,    37,    73,    77,   116,   124,   125,   140,
     168,    74,    74,    74,    74,    74,    74,    74,    74,    74,
      74,    74,    74,    74,    74,    74,    74,    74,    74,    74,
      74,    80,    79,    85,    76,    78,    80,    37,    37,    37,
      79,    91,    73,   152,   153,   158,   152,   141,   151,    33,
     116,   147,   116,    37,   168,    66,   142,   142,   144,    19,
     168,    45,   141,    45,   116,   116,   116,   117,   117,   118,
     118,   118,   118,   118,   118,   118,   118,   119,   119,   119,
     120,   120,   120,   116,    96,   168,   152,   152,   168,   161,
      32,    74,   157,   158,   159,    33,    76,   157,    73,   159,
     166,   168,   159,   167,   168,   168,   116,    74,    80,    37,
      39,    43,    45,    46,    48,    49,    87,   156,   159,   168,
       7,    79,   113,   168,   158,   168,   168,     7,   158,    40,
      32,    42,    40,    74,    80,    80,    78,    80,    37,   116,
      68,   123,   117,    79,    79,   136,   137,    78,   128,    36,
     126,    14,    15,    16,    17,    18,    19,    20,    21,    23,
      25,    28,    29,    73,    75,    77,    79,   134,   168,   168,
      81,    72,    78,   125,    37,   116,   141,    76,   116,    78,
     147,   116,   116,   116,    73,   159,    31,    74,   116,    33,
      76,   116,    37,   168,   116,    74,    80,    76,    80,    78,
      80,    37,    37,    79,   116,   116,    45,    52,    74,    74,
     158,    42,   158,    76,    42,    78,    80,    33,    40,    43,
      79,    80,    33,    45,    91,   155,    73,    77,    79,   159,
     159,   147,   159,   159,   159,    89,   159,   159,   110,    13,
     105,   106,   103,   104,   114,   168,   168,   115,    80,    33,
      33,    13,   108,   109,   159,   159,   159,   159,    74,   141,
      78,   145,   141,   116,    19,   149,   150,   168,   138,   137,
     129,   130,   131,    68,    19,   168,   116,   116,    35,    76,
      37,    79,    79,   158,    76,    76,   116,   116,    37,   168,
      67,    74,    76,    78,   141,   141,   159,   159,    79,   159,
     168,   158,   159,   159,   158,   168,   158,   159,   159,   159,
     159,   168,   159,   168,    37,    35,    38,    35,    38,     7,
      79,   100,   106,     7,    80,    32,   168,     7,   158,   163,
     158,   101,   110,   109,    33,    79,    80,    33,    37,     7,
      32,    79,    15,    25,    77,   132,   133,   168,   126,    37,
      37,    79,    78,    81,   116,   116,    76,   116,    37,   168,
     116,    79,    79,    74,    19,   160,   168,    78,    33,    42,
      79,    79,    33,    79,    40,    42,    42,    33,    40,    33,
     159,   159,   159,   159,   159,   105,   103,   105,   104,   114,
     108,    74,    80,    37,   168,    57,   150,   168,   116,   116,
     116,   130,   116,    33,    76,    80,   116,    37,   168,    37,
      33,    37,   158,   159,    37,    38,   158,    38,   159,   159,
     159,   159,   159,   159,     7,   158,   116,    37,   116,    37,
      32,    78,   133,   168,   116,   116,    37,   168,   159,    76,
     159,    78,   159,   159,   159,    79,    74,    78,    42,    79,
      40,   105,   116,   116,   116,    76,   116,    37,   168,    37,
      37,   159,   159,   116,    37,   168,   159,   159,    78,    79,
     116,    37,   168,   116,    37,   168,   116,    37,   116
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
     126,   126,   126,   126,   126,   127,   127,   128,   129,   129,
     130,   131,   131,   132,   132,   133,   133,   133,   134,   134,
     135,   135,   136,   136,   137,   137,   138,   138,   139,   139,
     139,   139,   139,   139,   139,   139,   139,   139,   139,   139,
     139,   139,   139,   139,   139,   139,   139,   139,   139,   139,
     139,   139,   140,   140,   140,   140,   140,   141,   142,   142,
     143,   143,   144,   144,   145,   146,   146,   146,   147,   147,
     147,   147,   147,   147,   147,   147,   147,   147,   147,   147,
     147,   147,   148,   148,   148,   148,   149,   149,   150,   150,
     150,   151,   151,   151,   152,   152,   153,   154,   154,   155,
     155,   155,   155,   155,   155,   155,   155,   155,   155,   155,
     155,   155,   155,   155,   155,   155,   155,   155,   155,   155,
     156,   156,   157,   157,   158,   158,   158,   159,   159,   159,
     159,   159,   159,   159,   159,   159,   159,   159,   159,   159,
     159,   159,   159,   159,   159,   160,   160,   161,   161,   162,
     162,   163,   163,   164,   164,   165,   165,   166,   166,   167,
     167,   167,   167,   168
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
       5,     5,     3,     6,     8,     3,     4,     2,     1,     2,
       6,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     2,     1,     3,     3,     1,
       4,     2,     0,     3,     1,     1,     1,     1,     1,     2,
       2,     1,     2,     1,     4,     6,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     4,     3,     5,     5,     3,     4,     3,
       4,     1,     1,     3,     4,     3,     4,     1,     1,     0,
       3,     1,     3,     1,     3,     0,     3,     5,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     2,     2,     1,     1,     3,     3,     5,
       5,     0,     1,     3,     3,     1,     3,     1,     3,     2,
       3,     3,     3,     7,     9,     7,     7,     9,     7,     5,
       5,     5,     5,     7,     7,     9,     9,     7,     7,     5,
       1,     2,     2,     1,     3,     1,     1,     1,     3,     2,
       3,     7,     3,     3,     3,     3,     2,     1,     1,     4,
       3,     3,     4,     1,     3,     1,     1,     1,     3,     1,
       5,     1,     3,     1,     3,     3,     3,     5,     3,     5,
       3,     3,     1,     1
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
#line 490 "hexpr.y" /* yacc.c:1646  */
    { yyParsedModule = (yyvsp[0].module);                     }
#line 2662 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 3:
#line 491 "hexpr.y" /* yacc.c:1646  */
    { yyParsedVar    = *(yyvsp[-2].string); yyParsedExpr = (yyvsp[0].exp); }
#line 2668 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 4:
#line 492 "hexpr.y" /* yacc.c:1646  */
    { yyParsedVar    = "";  yyParsedExpr = (yyvsp[0].exp); }
#line 2674 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 5:
#line 493 "hexpr.y" /* yacc.c:1646  */
    { yyParsedExpr   = (yyvsp[0].exp);                     }
#line 2680 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 6:
#line 496 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = new Module(*(yyvsp[-2].string), *(yyvsp[0].mdefs)); }
#line 2686 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 7:
#line 497 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = new Module(freshName(), *(yyvsp[0].mdefs)); }
#line 2692 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 8:
#line 499 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); }
#line 2698 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 9:
#line 500 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2704 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 10:
#line 501 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = (yyvsp[-1].mdefs);                            (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2710 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 11:
#line 503 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2716 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 12:
#line 504 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2722 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 13:
#line 505 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mvtydef); }
#line 2728 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 14:
#line 506 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2734 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 15:
#line 507 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2740 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 16:
#line 509 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2746 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 17:
#line 510 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 2752 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 18:
#line 511 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 2758 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 19:
#line 512 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 2764 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 20:
#line 513 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 2770 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 21:
#line 514 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 2776 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 22:
#line 515 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 2782 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 23:
#line 516 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-9]), (yylsp[0]))); }
#line 2788 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 24:
#line 517 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-10]), (yylsp[0]))); }
#line 2794 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 25:
#line 518 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-11]), (yylsp[0]))); }
#line 2800 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 26:
#line 519 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-12]), (yylsp[0]))); }
#line 2806 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 27:
#line 520 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-13]), (yylsp[0]))); }
#line 2812 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 28:
#line 523 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(freshName()), let(freshName(), ExprPtr((yyvsp[0].exp)), mktunit(m((yylsp[0]))), m((yylsp[0]))), m((yylsp[0]))); }
#line 2818 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 29:
#line 526 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MImport(yyModulePath, *(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 2824 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 30:
#line 529 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MTypeDef(MTypeDef::Transparent, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 2830 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 31:
#line 530 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MTypeDef(MTypeDef::Opaque, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 2836 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 32:
#line 533 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydef) = new MVarTypeDef(*(yyvsp[-2].string), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]), (yylsp[0]))); }
#line 2842 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 33:
#line 535 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdef) = new MVarDef(*(yyvsp[-2].strings), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2848 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 34:
#line 538 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-3].tconstraints), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2854 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 35:
#line 539 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-6]), (yylsp[0]))); wantIndent(false); }
#line 2860 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 36:
#line 540 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2866 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 37:
#line 541 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-7].tconstraints), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-8]), (yylsp[0])));            wantIndent(false); }
#line 2872 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 38:
#line 542 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2878 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 39:
#line 543 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2884 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 40:
#line 544 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-4]), (yylsp[0])));            wantIndent(false); }
#line 2890 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 41:
#line 545 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2896 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 42:
#line 547 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundeps) = autorelease(new CFunDepDefs()); (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2902 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 43:
#line 548 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundeps) = (yyvsp[-2].fundeps);                             (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2908 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 44:
#line 550 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundep) = autorelease(new CFunDepDef(*(yyvsp[-2].strings), *(yyvsp[0].strings))); }
#line 2914 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 45:
#line 552 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydefs) = autorelease(new MVarTypeDefs()); (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2920 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 46:
#line 553 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydefs) = (yyvsp[-1].mvtydefs);                              (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2926 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 47:
#line 555 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydef) = (yyvsp[0].mvtydef); }
#line 2932 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 48:
#line 558 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2938 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 49:
#line 559 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-3].tconstraints),           *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2944 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 50:
#line 560 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-4]), (yylsp[0])));        wantIndent(false); }
#line 2950 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 51:
#line 561 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-5].tconstraints),           *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-6]), (yylsp[0])));        wantIndent(false); }
#line 2956 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 52:
#line 563 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdefs) = autorelease(new MVarDefs()); (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2962 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 53:
#line 564 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdefs) = (yyvsp[-1].mvdefs);                          (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2968 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 54:
#line 566 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdef) = (yyvsp[0].mvdef); }
#line 2974 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 55:
#line 569 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[0].strings); }
#line 2980 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 56:
#line 571 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[-1].string)); (yyval.strings)->push_back(*(yyvsp[-2].string)); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 2986 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 57:
#line 573 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 2992 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 58:
#line 574 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 2998 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 59:
#line 576 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 3004 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 60:
#line 578 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[-1].string); }
#line 3010 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 61:
#line 580 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("and")); }
#line 3016 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 62:
#line 581 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("or")); }
#line 3022 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 63:
#line 582 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3028 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 64:
#line 583 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3034 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 65:
#line 584 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("~")); }
#line 3040 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 66:
#line 585 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("=~")); }
#line 3046 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 67:
#line 586 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("===")); }
#line 3052 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 68:
#line 587 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("==")); }
#line 3058 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 69:
#line 588 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("<")); }
#line 3064 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 70:
#line 589 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("<=")); }
#line 3070 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 71:
#line 590 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(">")); }
#line 3076 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 72:
#line 591 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(">=")); }
#line 3082 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 73:
#line 592 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("in")); }
#line 3088 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 74:
#line 593 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("append")); }
#line 3094 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 75:
#line 594 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("+")); }
#line 3100 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 76:
#line 595 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("-")); }
#line 3106 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 77:
#line 596 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("*")); }
#line 3112 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 78:
#line 597 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("/")); }
#line 3118 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 79:
#line 598 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("%")); }
#line 3124 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 80:
#line 600 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3130 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 81:
#line 601 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3136 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 82:
#line 603 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3142 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 83:
#line 604 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes);                           (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3148 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 84:
#line 607 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3154 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 85:
#line 608 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3160 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 86:
#line 609 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), (yyvsp[0].exp), m((yylsp[-1]),(yylsp[0]))); }
#line 3166 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 87:
#line 610 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("and",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3172 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 88:
#line 611 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("or",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3178 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 89:
#line 612 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("compose",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3184 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 90:
#line 613 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assign(ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3190 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 91:
#line 614 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("in",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3196 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 92:
#line 615 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3202 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 93:
#line 617 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP3(var("if",m((yylsp[-5]))), (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-5]), (yylsp[0]))); }
#line 3208 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 94:
#line 618 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3214 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 95:
#line 620 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("~",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3220 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 96:
#line 621 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("===",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3226 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 97:
#line 622 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3232 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 98:
#line 623 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))), m((yylsp[-2]),(yylsp[0]))); }
#line 3238 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 99:
#line 624 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("<",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3244 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 100:
#line 625 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("<=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3250 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 101:
#line 626 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var(">",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3256 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 102:
#line 627 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var(">=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3262 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 103:
#line 628 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3268 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 104:
#line 630 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("+",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3274 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 105:
#line 631 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("-",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3280 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 106:
#line 632 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("append",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3286 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 107:
#line 633 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("neg",m((yylsp[-1]))), ExprPtr((yyvsp[0].exp)), m((yylsp[-1]),(yylsp[0]))); }
#line 3292 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 108:
#line 634 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3298 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 109:
#line 636 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("*", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3304 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 110:
#line 637 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("/", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3310 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 111:
#line 638 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("%", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3316 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 112:
#line 639 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3322 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 113:
#line 641 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3328 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 114:
#line 644 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-2].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3334 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 115:
#line 645 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]),(yylsp[0])))->clone(); }
#line 3340 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 116:
#line 648 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileMatch(yyParseCC, *(yyvsp[-2].exps), normPatternRules(*(yyvsp[0].patternexps), m((yylsp[-3]),(yylsp[0]))), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3346 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 117:
#line 651 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileMatchTest(yyParseCC, ExprPtr((yyvsp[-2].exp)), PatternPtr((yyvsp[0].pattern)), m((yylsp[-2]),(yylsp[0])))->clone(); }
#line 3352 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 118:
#line 654 "hexpr.y" /* yacc.c:1646  */
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
#line 3367 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 119:
#line 666 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-1].letbindings), ExprPtr(new Unit(m((yylsp[-3]),(yylsp[0])))), m((yylsp[-3]),(yylsp[0]))); }
#line 3373 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 120:
#line 667 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[-1].exp)), m((yylsp[-5]),(yylsp[0]))); }
#line 3379 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 121:
#line 670 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assump(ExprPtr((yyvsp[-2].exp)), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]),(yylsp[0]))); }
#line 3385 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 122:
#line 672 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].letbindings)->push_back(*(yyvsp[0].letbinding)); (yyval.letbindings) = (yyvsp[-2].letbindings); }
#line 3391 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 123:
#line 673 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3397 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 124:
#line 675 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-2].pattern)), ExprPtr((yyvsp[0].exp)))); }
#line 3403 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 125:
#line 677 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = (yyvsp[-1].letbindings); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3409 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 126:
#line 678 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3415 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 127:
#line 680 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)))); }
#line 3421 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 128:
#line 681 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr(new MatchAny("_",m((yylsp[-1])))), ExprPtr((yyvsp[-1].exp)))); }
#line 3427 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 129:
#line 684 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].exps), m((yylsp[-3]), (yylsp[0]))); }
#line 3433 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 130:
#line 685 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = varCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3439 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 131:
#line 688 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("range", m((yylsp[-2]))), list(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]), (yylsp[0]))); }
#line 3445 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 132:
#line 689 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("iterateS", m((yylsp[-1]))), list(ExprPtr((yyvsp[-2].exp)), fn(str::strings(".x"), fncall(var("+", m((yylsp[-1]))), list(var(".x", m((yylsp[-1]))), ExprPtr(new Int(1, m((yylsp[-1]))))), m((yylsp[-1]))), m((yylsp[-1])))), m((yylsp[-3]), (yylsp[0]))); }
#line 3451 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 133:
#line 690 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileArrayComprehension(ExprPtr((yyvsp[-5].exp)), PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 3457 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 134:
#line 691 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileArrayComprehension(ExprPtr((yyvsp[-7].exp)), PatternPtr((yyvsp[-5].pattern)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 3463 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 135:
#line 692 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkArray(*(yyvsp[-1].exps), m((yylsp[-2]), (yylsp[0]))); }
#line 3469 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 136:
#line 693 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new AIndex(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3475 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 137:
#line 694 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("slice", m((yylsp[-2]))), list(ExprPtr((yyvsp[-5].exp)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-5]), (yylsp[0]))); }
#line 3481 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 138:
#line 695 "hexpr.y" /* yacc.c:1646  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-1]))), list(var(vn,m((yylsp[-4]))), ExprPtr((yyvsp[-2].exp)), fncall(var("size",m((yylsp[-1]))), list(var(vn,m((yylsp[-4])))),m((yylsp[-4])))),m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3487 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 139:
#line 696 "hexpr.y" /* yacc.c:1646  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-2]))), list(var(vn,m((yylsp[-4]))), fncall(var("size",m((yylsp[-2]))), list(var(vn,m((yylsp[-2])))),m((yylsp[-4]))), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3493 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 140:
#line 699 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-3].string), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3499 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 141:
#line 700 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(".f" + str::from((yyvsp[-3].intv)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3505 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 142:
#line 701 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-1].string), ExprPtr(new Unit(m((yylsp[-1])))), m((yylsp[-2]), (yylsp[0]))); }
#line 3511 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 143:
#line 702 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-4].exp)), *(yyvsp[-1].vfields), m((yylsp[-5]), (yylsp[0]))); }
#line 3517 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 144:
#line 703 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-6].exp)), *(yyvsp[-3].vfields), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3523 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 145:
#line 706 "hexpr.y" /* yacc.c:1646  */
    { if ((yyvsp[-1].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-1].rfields), m((yylsp[-2]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-2]), (yylsp[0]))); } }
#line 3529 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 146:
#line 707 "hexpr.y" /* yacc.c:1646  */
    { if ((yyvsp[-2].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-2].rfields), m((yylsp[-3]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-3]), (yylsp[0]))); } }
#line 3535 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 147:
#line 708 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makeProjSeq((yyvsp[-1].exp), *(yyvsp[0].strings), m((yylsp[-1]), (yylsp[0]))); }
#line 3541 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 148:
#line 711 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Fn(str::strings("x"), proj(var("x", m((yylsp[0]))), *(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3547 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 149:
#line 714 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Pack(ExprPtr((yyvsp[0].exp)), m((yylsp[-1]), (yylsp[0]))); }
#line 3553 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 150:
#line 715 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Unpack(*(yyvsp[-4].string), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3559 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 151:
#line 718 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Bool((yyvsp[0].boolv), m((yylsp[0]))); }
#line 3565 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 152:
#line 719 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3571 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 153:
#line 720 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3577 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 154:
#line 721 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3583 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 155:
#line 722 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Short((yyvsp[0].shortv), m((yylsp[0]))); }
#line 3589 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 156:
#line 723 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Int((yyvsp[0].intv), m((yylsp[0]))); }
#line 3595 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 157:
#line 724 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Long((yyvsp[0].longv), m((yylsp[0]))); }
#line 3601 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 158:
#line 725 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Int128((yyvsp[0].int128v), m((yylsp[0]))); }
#line 3607 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 159:
#line 726 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Float((yyvsp[0].floatv), m((yylsp[0]))); }
#line 3613 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 160:
#line 727 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Double((yyvsp[0].doublev), m((yylsp[0]))); }
#line 3619 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 161:
#line 728 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3625 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 162:
#line 729 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0])))->clone(); }
#line 3631 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 163:
#line 730 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3637 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 164:
#line 731 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3643 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 165:
#line 734 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = pickNestedExp((yyvsp[-1].exps), m((yylsp[-2]),(yylsp[0]))); }
#line 3649 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 166:
#line 737 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("append", m((yylsp[-1]))); }
#line 3655 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 167:
#line 738 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("+",      m((yylsp[-1]))); }
#line 3661 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 168:
#line 739 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("-",      m((yylsp[-1]))); }
#line 3667 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 169:
#line 740 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("*",      m((yylsp[-1]))); }
#line 3673 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 170:
#line 741 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("/",      m((yylsp[-1]))); }
#line 3679 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 171:
#line 742 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("%",      m((yylsp[-1]))); }
#line 3685 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 172:
#line 743 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("~",      m((yylsp[-1]))); }
#line 3691 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 173:
#line 744 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("===",    m((yylsp[-1]))); }
#line 3697 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 174:
#line 745 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("==",     m((yylsp[-1]))); }
#line 3703 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 175:
#line 746 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("!=",     m((yylsp[-1]))); }
#line 3709 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 176:
#line 747 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("<",      m((yylsp[-1]))); }
#line 3715 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 177:
#line 748 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var(">",      m((yylsp[-1]))); }
#line 3721 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 178:
#line 749 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var(">=",     m((yylsp[-1]))); }
#line 3727 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 179:
#line 750 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("<=",     m((yylsp[-1]))); }
#line 3733 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 180:
#line 751 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("and",    m((yylsp[-1]))); }
#line 3739 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 181:
#line 752 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("or",     m((yylsp[-1]))); }
#line 3745 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 182:
#line 753 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("in",     m((yylsp[-1]))); }
#line 3751 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 183:
#line 754 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("not",    m((yylsp[-1]))); }
#line 3757 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 184:
#line 757 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assump(fncall(var("unsafeCast", m((yylsp[-1]))), list(mktunit(m((yylsp[-1])))), m((yylsp[-1]))), qualtype(tapp(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp)))))), m((yylsp[-1]))); }
#line 3763 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 185:
#line 759 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prules) = (yyvsp[-1].prules); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3769 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 186:
#line 760 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prules) = autorelease(new Grammar()); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3775 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 187:
#line 762 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prule) = autorelease(new Grammar::value_type(*(yyvsp[-2].string), *(yyvsp[0].prdefs))); }
#line 3781 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 188:
#line 764 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdefs) = (yyvsp[-2].prdefs); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3787 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 189:
#line 765 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdefs) = autorelease(new GrammarRules()); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3793 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 190:
#line 767 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdef) = autorelease(new GrammarRule(*(yyvsp[-3].pbelems), ExprPtr((yyvsp[-1].exp)))); }
#line 3799 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 191:
#line 769 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelems) = (yyvsp[-1].pbelems); (yyval.pbelems)->push_back(*(yyvsp[0].pbelem)); }
#line 3805 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 192:
#line 770 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelems) = autorelease(new BoundGrammarValues()); }
#line 3811 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 193:
#line 772 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue(*(yyvsp[-2].string), GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3817 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 194:
#line 773 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue("_", GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3823 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 195:
#line 775 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GSymRef(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3829 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 196:
#line 776 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GStr(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3835 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 197:
#line 777 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GStr(std::string(1, str::readCharDef(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3841 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 198:
#line 779 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3847 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 199:
#line 780 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3853 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 200:
#line 782 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = (yyvsp[-1].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3859 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 201:
#line 783 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3865 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 202:
#line 785 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexps) = (yyvsp[-1].patternexps); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3871 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 203:
#line 786 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexps) = autorelease(new PatternRows()); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3877 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 204:
#line 788 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)))); }
#line 3883 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 205:
#line 789 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-4].patterns), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)))); }
#line 3889 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 206:
#line 792 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[-1].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3895 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 207:
#line 793 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = autorelease(new Patterns()); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3901 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 208:
#line 795 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Bool((yyvsp[0].boolv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3907 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 209:
#line 796 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3913 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 210:
#line 797 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3919 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 211:
#line 798 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Short((yyvsp[0].shortv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3925 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 212:
#line 799 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int((yyvsp[0].intv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3931 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 213:
#line 800 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Long((yyvsp[0].longv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3937 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 214:
#line 801 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int128((yyvsp[0].int128v), m((yylsp[0])))), m((yylsp[0]))); }
#line 3943 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 215:
#line 802 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Double((yyvsp[0].doublev), m((yylsp[0])))), m((yylsp[0]))); }
#line 3949 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 216:
#line 803 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = mkpatarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3955 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 217:
#line 804 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = mkpatarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3961 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 218:
#line 805 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkTimespanPrim(*(yyvsp[0].strings), m((yylsp[0]))), mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3967 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 219:
#line 806 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 3973 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 220:
#line 807 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkDateTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 3979 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 221:
#line 808 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRegex(std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0]))); }
#line 3985 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 222:
#line 809 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 3991 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 223:
#line 810 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 3997 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 224:
#line 811 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-1].string), PatternPtr(new MatchLiteral(PrimitivePtr(new Unit(m((yylsp[-1])))), m((yylsp[-1])))), m((yylsp[-2]),(yylsp[0]))); }
#line 4003 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 225:
#line 812 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-3].string), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4009 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 226:
#line 813 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(".f" + str::from((yyvsp[-3].intv)), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4015 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 227:
#line 814 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4021 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 228:
#line 815 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4027 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 229:
#line 816 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4033 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 230:
#line 817 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4039 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 231:
#line 818 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = patVarCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4045 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 232:
#line 820 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchAny(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4051 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 233:
#line 821 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4057 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 234:
#line 822 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4063 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 235:
#line 823 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4069 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 236:
#line 824 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4075 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 237:
#line 826 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = (yyvsp[0].pattern); }
#line 4081 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 238:
#line 828 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[0].patterns); }
#line 4087 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 239:
#line 829 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = new Patterns(); }
#line 4093 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 240:
#line 831 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[-2].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4099 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 241:
#line 832 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = new Patterns(); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4105 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 242:
#line 834 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfields) = (yyvsp[-2].recpatfields); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4111 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 243:
#line 835 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfields) = new MatchRecord::Fields(); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4117 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 244:
#line 837 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfield) = new MatchRecord::Field(*(yyvsp[-2].string), PatternPtr((yyvsp[0].pattern))); }
#line 4123 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 245:
#line 839 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); }
#line 4129 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 246:
#line 840 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4135 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 247:
#line 841 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = (yyvsp[-4].rfields);                                     (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4141 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 248:
#line 843 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4147 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 249:
#line 844 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("data")); }
#line 4153 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 250:
#line 845 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("type")); }
#line 4159 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 251:
#line 846 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("where")); }
#line 4165 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 252:
#line 847 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("class")); wantIndent(false); }
#line 4171 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 253:
#line 848 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("instance")); wantIndent(false); }
#line 4177 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 254:
#line 849 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("exists")); }
#line 4183 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 255:
#line 850 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("import")); }
#line 4189 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 256:
#line 851 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("module")); }
#line 4195 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 257:
#line 852 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("parse")); }
#line 4201 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 258:
#line 853 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("do")); }
#line 4207 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 259:
#line 854 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("return")); }
#line 4213 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 260:
#line 855 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("fn")); }
#line 4219 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 261:
#line 856 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(".f" + str::from((yyvsp[0].intv)))); }
#line 4225 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 262:
#line 858 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-2].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4231 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 263:
#line 859 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings); str::seq x = tupSectionFields(*(yyvsp[0].string)); (yyval.strings)->insert((yyval.strings)->end(), x.begin(), x.end()); }
#line 4237 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 264:
#line 860 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4243 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 265:
#line 861 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); *(yyval.strings) = tupSectionFields(*(yyvsp[0].string)); }
#line 4249 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 266:
#line 863 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vfields) = autorelease(new Case::Bindings()); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4255 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 267:
#line 864 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vfields) = (yyvsp[-2].vfields); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4261 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 268:
#line 866 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-2].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4267 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 269:
#line 867 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-4].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4273 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 270:
#line 868 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(".f" + str::from((yyvsp[-4].intv)), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4279 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 271:
#line 870 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); }
#line 4285 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 272:
#line 871 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4291 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 273:
#line 872 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].exps)->push_back(ExprPtr((yyvsp[0].exp))); (yyval.exps) = (yyvsp[-2].exps); }
#line 4297 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 274:
#line 874 "hexpr.y" /* yacc.c:1646  */
    { (yyval.qualtype) = new QualType(*(yyvsp[-2].tconstraints), *(yyvsp[0].mtype)); }
#line 4303 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 275:
#line 875 "hexpr.y" /* yacc.c:1646  */
    { (yyval.qualtype) = new QualType(Constraints(), *(yyvsp[0].mtype)); }
#line 4309 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 276:
#line 878 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraints) = (yyvsp[-1].tconstraints); }
#line 4315 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 277:
#line 880 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraints) = autorelease(new Constraints()); (yyval.tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); }
#line 4321 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 278:
#line 881 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); (yyval.tconstraints) = (yyvsp[-2].tconstraints); }
#line 4327 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 279:
#line 883 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(*(yyvsp[-1].string), *(yyvsp[0].mtypes)); }
#line 4333 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 280:
#line 884 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(EqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4339 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 281:
#line 885 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(NotEqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4345 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 282:
#line 886 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(FixIsoRecur::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4351 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 283:
#line 887 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4357 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 284:
#line 888 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4363 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 285:
#line 889 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4369 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 286:
#line 890 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4375 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 287:
#line 891 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4381 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 288:
#line 892 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(1), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4387 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 289:
#line 894 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4393 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 290:
#line 895 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4399 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 291:
#line 896 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4405 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 292:
#line 897 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4411 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 293:
#line 899 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4417 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 294:
#line 900 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4423 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 295:
#line 901 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4429 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 296:
#line 902 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4435 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 297:
#line 904 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), TString::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype))); }
#line 4441 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 298:
#line 905 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), *(yyvsp[-5].mtype),                *(yyvsp[-3].mtype))); }
#line 4447 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 299:
#line 906 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(AppendsToUnqualifier::constraintName(), list(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4453 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 300:
#line 908 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4459 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 301:
#line 909 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-1].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4465 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 302:
#line 911 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4471 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 303:
#line 912 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4477 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 304:
#line 914 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Func::make(tuplety(*(yyvsp[-2].mtypes)), *(yyvsp[0].mtype)))); }
#line 4483 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 305:
#line 915 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeTupleType(*(yyvsp[0].mtypes)))); }
#line 4489 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 306:
#line 916 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeSumType(*(yyvsp[0].mtypes)))); }
#line 4495 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 307:
#line 918 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(monoTypeByName(*(yyvsp[0].string)))); }
#line 4501 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 308:
#line 919 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(OpaquePtr::make(str::replace<char>(*(yyvsp[-1].string), ".", "::"), 0, false))); }
#line 4507 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 309:
#line 920 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("[]"))); }
#line 4513 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 310:
#line 921 "hexpr.y" /* yacc.c:1646  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(Array::make(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4519 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 311:
#line 922 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(FixedArray::make(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype)))); }
#line 4525 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 312:
#line 923 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("->"))); }
#line 4531 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 313:
#line 924 "hexpr.y" /* yacc.c:1646  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(clone(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4537 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 314:
#line 925 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeRecType(*(yyvsp[-1].mreclist)))); }
#line 4543 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 315:
#line 926 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeVarType(*(yyvsp[-1].mvarlist)))); }
#line 4549 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 316:
#line 927 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("unit"))); }
#line 4555 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 317:
#line 928 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(((yyvsp[0].intv) == 0) ? Prim::make("void") : TLong::make((yyvsp[0].intv)))); }
#line 4561 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 318:
#line 929 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr((yyvsp[0].boolv) ? TLong::make(1) : TLong::make(0))); }
#line 4567 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 319:
#line 930 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Exists::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4573 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 320:
#line 931 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(Prim::make("fileref"), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))))); }
#line 4579 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 321:
#line 932 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(Prim::make("fileref"), list(*(yyvsp[-2].mtype))))); }
#line 4585 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 322:
#line 933 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Recursive::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4591 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 323:
#line 934 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TString::make(str::unescape(str::trimq(*(yyvsp[0].string)))))); }
#line 4597 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 324:
#line 935 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp))))))); }
#line 4603 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 325:
#line 937 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TVar::make(*(yyvsp[0].string)))); }
#line 4609 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 326:
#line 938 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TLong::make((yyvsp[0].intv)))); }
#line 4615 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 327:
#line 940 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4621 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 328:
#line 941 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[-2].string); *(yyval.string) += "."; *(yyval.string) += *(yyvsp[0].string); }
#line 4627 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 329:
#line 943 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4633 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 330:
#line 944 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-1].mtypes)->insert((yyvsp[-1].mtypes)->begin(), *(yyvsp[-3].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4639 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 331:
#line 946 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4645 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 332:
#line 947 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-2].mtypes); }
#line 4651 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 333:
#line 949 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4657 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 334:
#line 950 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4663 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 335:
#line 952 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[-2].mtype)); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4669 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 336:
#line 953 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4675 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 337:
#line 955 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mreclist) = (yyvsp[-4].mreclist);                                 (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4681 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 338:
#line 956 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mreclist) = autorelease(new Record::Members()); (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4687 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 339:
#line 958 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = (yyvsp[-4].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4693 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 340:
#line 959 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = (yyvsp[-2].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4699 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 341:
#line 960 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4705 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 342:
#line 961 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4711 "hexpr.parse.C" /* yacc.c:1646  */
    break;


#line 4715 "hexpr.parse.C" /* yacc.c:1646  */
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
#line 965 "hexpr.y" /* yacc.c:1906  */

#pragma GCC diagnostic pop


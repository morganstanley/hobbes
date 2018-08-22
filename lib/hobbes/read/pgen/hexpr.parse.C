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
    TFLOAT = 276,
    TDOUBLE = 277,
    TIDENT = 278,
    TSTRING = 279,
    TREGEX = 280,
    TTIMEINTERVAL = 281,
    TTIME = 282,
    TDATETIME = 283,
    TTUPSECTION = 284,
    TCSTARROW = 285,
    TARROW = 286,
    TCOLON = 287,
    TEXISTS = 288,
    TASSIGN = 289,
    TPARROW = 290,
    TEQUALS = 291,
    TASSUMP = 292,
    TAPPEND = 293,
    TPLUS = 294,
    TMINUS = 295,
    TTIMES = 296,
    TDIVIDE = 297,
    TREM = 298,
    TDOT = 299,
    TEQUIV = 300,
    TEQ = 301,
    TCIEQ = 302,
    TNEQ = 303,
    TLT = 304,
    TLTE = 305,
    TGT = 306,
    TGTE = 307,
    TNOT = 308,
    TLET = 309,
    TCASE = 310,
    TDEFAULT = 311,
    TMATCH = 312,
    TMATCHES = 313,
    TPARSE = 314,
    TWITH = 315,
    TOF = 316,
    TAND = 317,
    TOR = 318,
    TIF = 319,
    TTHEN = 320,
    TELSE = 321,
    TIN = 322,
    TPACK = 323,
    TUNPACK = 324,
    TDO = 325,
    TRETURN = 326,
    TLPAREN = 327,
    TRPAREN = 328,
    TLBRACKET = 329,
    TRBRACKET = 330,
    TLBRACE = 331,
    TRBRACE = 332,
    TBAR = 333,
    TCOMMA = 334,
    TSEMICOLON = 335,
    TFN = 336,
    TFNL = 337,
    TCOMPOSE = 338,
    TUPTO = 339,
    TCARET = 340,
    TAT = 341,
    TDOLLAR = 342,
    TQUESTION = 343,
    TSQUOTE = 344,
    TEQUOTE = 345
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

#line 522 "hexpr.parse.C" /* yacc.c:355  */
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

#line 551 "hexpr.parse.C" /* yacc.c:358  */

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
#define YYFINAL  68
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2787

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  92
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  76
/* YYNRULES -- Number of rules.  */
#define YYNRULES  341
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  756

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   346

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
      85,    86,    87,    88,    89,    90,    91
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   489,   489,   490,   491,   492,   495,   496,   498,   499,
     500,   502,   503,   504,   505,   506,   508,   509,   510,   511,
     512,   513,   514,   515,   516,   517,   518,   519,   522,   525,
     528,   529,   532,   534,   537,   538,   539,   540,   541,   542,
     543,   544,   546,   547,   549,   551,   552,   554,   557,   558,
     559,   560,   562,   563,   565,   568,   570,   572,   573,   575,
     577,   579,   580,   581,   582,   583,   584,   585,   586,   587,
     588,   589,   590,   591,   592,   593,   594,   595,   596,   597,
     599,   600,   602,   603,   606,   607,   608,   609,   610,   611,
     612,   613,   614,   616,   617,   619,   620,   621,   622,   623,
     624,   625,   626,   627,   629,   630,   631,   632,   633,   635,
     636,   637,   638,   640,   643,   644,   647,   650,   653,   665,
     666,   669,   671,   672,   674,   676,   677,   679,   680,   683,
     684,   687,   688,   689,   690,   691,   692,   693,   694,   695,
     698,   699,   700,   701,   702,   705,   706,   707,   710,   713,
     714,   717,   718,   719,   720,   721,   722,   723,   724,   725,
     726,   727,   728,   729,   732,   735,   736,   737,   738,   739,
     740,   741,   742,   743,   744,   745,   746,   747,   748,   749,
     750,   751,   752,   755,   757,   758,   760,   762,   763,   765,
     767,   768,   770,   771,   773,   774,   775,   777,   778,   780,
     781,   783,   784,   786,   787,   790,   791,   793,   794,   795,
     796,   797,   798,   799,   800,   801,   802,   803,   804,   805,
     806,   807,   808,   809,   810,   811,   812,   813,   814,   815,
     817,   818,   819,   820,   821,   823,   825,   826,   828,   829,
     831,   832,   834,   836,   837,   838,   840,   841,   842,   843,
     844,   845,   846,   847,   848,   849,   850,   851,   852,   853,
     855,   856,   857,   858,   860,   861,   863,   864,   865,   867,
     868,   869,   871,   872,   875,   877,   878,   880,   881,   882,
     883,   884,   885,   886,   887,   888,   889,   891,   892,   893,
     894,   896,   897,   898,   899,   901,   902,   903,   905,   906,
     908,   909,   911,   912,   913,   915,   916,   917,   918,   919,
     920,   921,   922,   923,   924,   925,   926,   927,   928,   929,
     930,   931,   932,   934,   935,   937,   938,   940,   941,   943,
     944,   946,   947,   949,   950,   952,   953,   955,   956,   957,
     958,   960
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
  "\"longV\"", "\"floatV\"", "\"doubleV\"", "\"id\"", "\"stringV\"",
  "\"regexV\"", "\"timespanV\"", "\"timeV\"", "\"dateTimeV\"",
  "\"tupSection\"", "\"=>\"", "\"->\"", "\":\"", "\"exists\"", "\"<-\"",
  "\":=\"", "\"=\"", "\"::\"", "\"++\"", "\"+\"", "\"-\"", "\"*\"",
  "\"/\"", "\"%\"", "\".\"", "\"==\"", "\"===\"", "\"~\"", "\"!=\"",
  "\"<\"", "\"<=\"", "\">\"", "\">=\"", "\"!\"", "\"let\"", "\"case\"",
  "\"default\"", "\"match\"", "\"matches\"", "\"parse\"", "\"with\"",
  "\"of\"", "\"and\"", "\"or\"", "\"if\"", "\"then\"", "\"else\"",
  "\"in\"", "\"pack\"", "\"unpack\"", "\"do\"", "\"return\"", "\"(\"",
  "\")\"", "\"[\"", "\"]\"", "\"{\"", "\"}\"", "\"|\"", "\",\"", "\";\"",
  "\"\\\\\"", "\"fn\"", "\"o\"", "\"..\"", "\"^\"", "\"@\"", "\"$\"",
  "\"?\"", "\"'\"", "\"`\"", "\"=~\"", "$accept", "s", "module", "defs",
  "def", "importdef", "tydef", "vartybind", "vardef", "classdef",
  "fundeps", "fundep", "cmembers", "cmember", "instdef", "imembers",
  "imember", "names", "nameseq", "name", "opname", "idseq", "types",
  "l0expr", "l1expr", "l2expr", "l3expr", "l4expr", "l5expr",
  "letbindings", "letbinding", "dobindings", "dobinding", "l6expr",
  "prules", "prule", "prdefs", "prdef", "pbelems", "pbelem", "pvalue",
  "tsseq", "l6exprs", "patternexps", "patternexp", "patterns",
  "refutablep", "irrefutablep", "pattern", "patternseq", "patternseqn",
  "recpatfields", "recpatfield", "recfields", "recfieldname",
  "recfieldpath", "varfields", "varbind", "cargs", "qtype", "cst",
  "tpreds", "tpred", "l1mtargl", "ltmtype", "l0mtype", "l1mtype", "tyind",
  "cppid", "l0mtargl", "l0mtarglt", "mtuplist", "msumlist", "mreclist",
  "mvarlist", "id", YY_NULLPTR
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
     345,   346
};
# endif

#define YYPACT_NINF -572

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-572)))

#define YYTABLE_NINF -341

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     583,  1164,  2047,  2047,   118,    64,    64,    86,    86,    87,
      87,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  -572,  -572,  -572,   451,    39,  2047,  2461,
      36,  2461,    64,    71,  1430,  2047,   451,   466,  2047,  -572,
    1248,  -572,  -572,  -572,  -572,  -572,  -572,   190,  -572,   204,
     149,    14,   143,  2331,  2201,  2047,  1585,  2704,  2704,   382,
      47,   553,   554,   565,  -572,   225,   382,  -572,  -572,   251,
     267,  -572,  2696,    50,  -572,  -572,   112,  2424,   296,    86,
     312,  2487,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  -572,  -572,  -572,  -572,  2704,    64,   138,
    -572,   317,  -572,   295,   171,   272,    64,   171,   346,  2124,
     357,   374,  2266,   383,   438,   445,   451,   452,   464,   469,
     476,   486,   494,   511,   536,  1349,   549,   550,   555,  -572,
    -572,   557,   382,   122,   240,   475,   -23,   576,   590,     5,
     145,  -572,  2508,  2508,  2704,  2047,  1739,    14,  -572,  -572,
     451,  2047,   168,  -572,  -572,   327,   357,   374,  2266,   383,
     438,   445,   452,   464,   469,   486,   494,   511,   536,   549,
     550,   555,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  -572,  2704,  2704,    64,   524,   149,  2624,
    -572,  -572,  -572,  2639,  2047,  2047,  2047,  2201,  2201,  2331,
    2331,  2331,  2331,  2331,  2331,  2331,  2331,  2331,  2331,  2331,
    2396,  2396,  2396,  2047,  1248,    64,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  2508,  -572,  2508,  -572,  -572,  -572,    64,
      64,   981,   853,  2529,  2529,    64,  2047,   200,  -572,   307,
    2529,    64,    25,    86,  2696,    64,   981,    64,    64,   164,
    -572,     9,   393,   586,   329,  -572,  -572,   306,   563,   288,
    -572,   596,  2047,    74,  2201,   566,   567,   171,    12,  -572,
     599,  2461,  1508,   451,   337,  1662,  -572,   607,   612,  -572,
    -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  2047,
    2704,  1816,  -572,  -572,    57,  2047,  2047,  2047,  -572,  -572,
    1206,  -572,   620,  -572,  -572,  -572,   440,  2047,   106,  -572,
     382,  2047,   198,  2047,   447,   477,   427,   622,    65,  2047,
    -572,  2047,   573,   573,  -572,  -572,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,
    -572,   382,  1248,  -572,  -572,  -572,   616,   368,   589,  -572,
    2229,  -572,     8,  2487,  -572,   953,   981,    84,   491,   629,
      43,   362,   120,   619,   222,  -572,  2424,   456,  2529,  2529,
     451,  2529,  2529,  2529,  2294,  2529,   578,    86,   652,    64,
      64,  2487,   587,   635,   639,   659,  -572,  2529,  2529,  2529,
    2529,  -572,   600,  2704,  -572,    15,  2704,   382,  2047,  -572,
    -572,   538,  2704,   567,  -572,  -572,  -572,  -572,   292,  -572,
    -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,
    1508,  1893,   451,   539,   149,  -572,   596,  -572,  2047,  -572,
    -572,  2047,   382,   641,  -572,   308,  -572,   640,   382,   351,
     388,   981,   178,  2487,  -572,   416,  1970,  -572,   382,  2047,
     227,   420,  -572,   604,  -572,   608,  -572,    19,  2704,  2704,
    -572,   382,   382,  2529,  -572,  -572,  -572,  -572,  2529,   606,
    -572,  2529,  -572,    64,  2487,  2529,  2487,  -572,    64,  2487,
    2529,  -572,  -572,  2529,  2529,  2529,    34,    18,   108,   578,
     578,   578,  -572,   578,   578,    27,    86,   652,  -572,    21,
    -572,   404,  -572,  -572,   232,  2487,  2487,  2487,    86,   659,
    -572,   578,   578,   578,   578,  -572,  -572,  -572,  -572,  -572,
     382,   653,   498,  -572,   528,  2606,  -572,   609,  -572,    22,
    2461,   650,   128,   418,   434,  2047,  -572,  2047,  -572,  -572,
    -572,  -572,  -572,   425,   382,  2047,   230,  2047,  -572,  -572,
    -572,   610,   613,   578,   146,   556,   126,   657,  -572,    20,
     252,   615,   662,   618,    40,   578,    96,   110,   665,    35,
     666,  2529,  2529,  2529,  2529,  2529,   652,    64,  -572,  -572,
     652,    64,    64,  -572,   659,  -572,   462,  -572,  -572,   663,
    -572,    64,   644,   538,    64,  2047,  2047,  2047,  -572,  -572,
    -572,  2047,  -572,  -572,   669,   171,  1893,  1893,  -572,  -572,
    -572,   332,   382,  -572,   382,  2047,   244,   382,  -572,  -572,
     667,  -572,   675,  -572,   672,  2487,  2529,   673,   676,  2487,
     677,  2529,  2529,  2529,  2529,  2529,  2529,   578,   578,   578,
     578,   578,   652,    23,   652,  -572,    64,   659,  -572,  2487,
    2047,   679,  2047,  -572,   681,   382,    93,   382,  -572,   468,
     162,  -572,  2047,   382,  2047,   246,  2529,   637,  2529,  -572,
     182,  2529,  2529,  -572,  2529,   390,   247,   242,   161,   412,
     124,   652,  -572,   382,  2047,   382,  2047,  2047,  -572,  -572,
    -572,   461,   382,  2047,   311,   578,  -572,   578,   682,   578,
     578,   578,   683,  -572,  -572,  2529,  -572,  2529,   652,   382,
     382,   382,  -572,   382,  2047,   338,  2529,  2529,   298,   429,
     382,  2047,   385,   578,   578,  -572,  -572,   382,  2047,   395,
     382,  2047,   688,   382,  2047,   382
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,     8,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   151,   152,   153,   154,   155,   156,   157,   158,   159,
     341,   160,   197,   162,   163,   263,     0,     0,     0,     0,
       0,     0,     0,     0,   269,   269,   243,     0,     0,     2,
       7,     9,    11,    12,    13,    14,    15,     0,    28,   113,
     161,   148,   130,     0,     0,     0,   269,     0,     0,     4,
      92,    94,   103,   108,   112,   130,     5,   130,     1,     0,
      29,   325,     0,     0,    57,    59,     0,     0,     0,     0,
       0,     0,   254,   249,   253,   248,   247,   250,   251,   259,
     252,   255,   256,   257,   258,   262,   246,   237,     0,     0,
     123,     0,   230,     0,   200,     0,     0,   149,     0,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      66,     0,   270,     0,   270,     0,     0,     0,     0,     0,
       0,    10,     0,     0,     0,   269,     0,   147,   198,   261,
       0,     0,     0,   107,    86,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   207,   208,   209,   214,   210,   211,   212,   213,
     215,   219,   217,   218,   237,   237,     0,     0,   216,     0,
     235,   206,   229,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     8,     0,    74,    75,    76,    77,
      78,    79,    64,    68,    67,    65,    69,    70,    71,    72,
      61,    62,    73,     0,    58,     0,   316,   315,   321,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   275,     0,
     305,     0,    38,    55,    59,     0,     0,     0,     0,    48,
      82,   331,     0,   303,   304,   305,   239,     0,   236,     0,
     241,     0,     0,     0,     0,     0,     0,   199,     0,   185,
       0,     0,   237,   243,     0,     0,   126,     0,   130,   165,
     166,   167,   168,   169,   170,   173,   172,   171,   174,   175,
     178,   176,   177,   182,   179,   180,   181,    60,   164,     0,
       0,     0,   135,   145,     0,     0,     0,     0,   142,   183,
       0,    32,     0,   273,   121,   117,     0,     0,     0,   260,
      16,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     205,     0,    87,    88,    89,    90,    91,    97,    96,    95,
      98,    99,   100,   101,   102,   106,   104,   105,   109,   110,
     111,     3,     6,   326,    30,    31,     0,     0,     0,   314,
       0,   301,   331,     0,   307,     0,     0,     0,     0,   305,
       0,     0,   305,     0,     0,   274,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   277,   298,     0,     0,     0,
       0,     0,   301,     0,   340,     0,    83,     0,     0,     0,
       0,   231,     0,     0,   233,     0,     0,   114,     0,   122,
     124,     0,     0,   116,   202,   118,   184,   191,     0,   151,
     152,   153,   154,   155,   156,   157,   159,   160,   162,   163,
     237,   237,   243,     0,   161,   130,     0,   128,     0,   119,
     125,     0,   271,     0,   132,     0,   146,     0,   244,     0,
       0,     0,   331,     0,   129,     0,     0,   136,    17,     0,
       0,     0,   225,     0,   220,     0,   227,     0,     0,     0,
     222,    84,    85,     0,   306,   310,   311,   300,     0,     0,
     308,     0,   312,     0,     0,     0,     0,   313,     0,     0,
       0,   322,   276,     0,     0,     0,     0,     0,     0,   278,
     280,   279,   319,   318,   299,    34,     0,    40,    45,    39,
      42,     0,    80,    56,    49,     0,     0,     0,     0,    50,
      52,   333,   302,   332,   334,   232,   238,   234,   240,   242,
     115,     0,     0,   264,     0,     0,   201,   186,   188,     0,
       0,     0,     0,     0,     0,     0,   131,     0,   141,   140,
     272,   139,   138,     0,    18,     0,     0,     0,   226,   221,
     228,     0,     0,   317,     0,     0,     0,     0,   336,   331,
       0,     0,   338,   339,   331,   320,     0,     0,   305,     0,
     305,     0,     0,     0,     0,     0,     0,     0,    47,    46,
       0,     0,     0,    81,     0,   329,     0,   339,    54,     0,
      53,     0,   143,     0,     0,     0,     0,     0,   191,   196,
     195,     0,   190,   193,   194,   150,     0,     0,   142,   120,
     127,     0,   245,   137,    19,     0,     0,    93,   224,   223,
       0,   324,     0,   323,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   297,   290,   289,
     288,   287,    36,    35,    41,    43,    44,    51,   328,     0,
       0,     0,     0,   265,     0,   266,     0,   203,   187,     0,
       0,   133,     0,    20,     0,     0,     0,     0,     0,   335,
       0,     0,     0,   337,     0,   333,     0,     0,     0,     0,
       0,     0,   330,    33,     0,   144,     0,     0,   189,   192,
     194,     0,    21,     0,     0,   286,   309,   284,     0,   292,
     296,   295,     0,   283,   281,     0,   291,     0,    37,   268,
     267,   204,   134,    22,     0,     0,     0,     0,     0,     0,
      23,     0,     0,   285,   294,   282,   293,    24,     0,     0,
      25,     0,     0,    26,     0,    27
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -572,  -572,  -572,   512,   -27,  -572,  -572,   209,  -572,  -572,
     130,   129,  -571,  -497,  -572,   125,  -505,  -374,   588,    -1,
     479,   132,   330,    -2,   -37,   574,   -35,   406,    10,  -572,
     463,  -572,   450,   -21,  -572,   459,  -572,   123,  -572,  -572,
      58,   -53,  -572,  -572,   319,   -46,  -572,   -95,   -36,  -169,
    -572,  -167,  -384,  -572,   -17,   -47,  -572,   127,   -32,  -116,
     611,  -572,   358,  -572,   501,   196,   706,  -572,   505,  -572,
    -572,  -572,  -572,  -572,  -572,   396
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,    39,    40,    41,    42,    43,    44,   608,    45,
     519,   520,   517,   518,    46,   529,   530,   252,   253,    47,
     131,   521,   259,   132,    60,    61,    62,    63,    64,    99,
     100,   285,   286,    49,   278,   279,   547,   548,   549,   622,
     623,    50,   105,   423,   424,   189,   190,   101,   266,   267,
     268,   269,   270,   136,   137,    51,   542,   543,   133,   321,
     322,   247,   248,   395,   370,   323,   261,   642,    70,   262,
     606,   263,   264,   378,   381,    67
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      59,    66,   147,   135,   188,   188,    74,    74,   104,    95,
     107,    48,   193,   141,   287,   334,   335,   154,   153,   336,
     599,   191,   191,   515,   610,   662,   103,   324,   600,   664,
     701,   538,   398,   134,   596,    20,   140,   619,    20,  -327,
    -327,   317,    20,   149,   188,    20,   620,   407,   407,   488,
      48,  -327,   592,   155,   313,   593,   314,   147,   150,   407,
     147,   646,    20,    82,    83,    84,    85,    86,    87,    88,
     591,  -327,   234,    20,   655,   234,    89,   153,    74,   651,
      20,   197,   495,   318,   277,   496,   233,    20,   154,   425,
      90,   188,   537,   538,   394,   394,   570,    20,   621,    95,
     601,   479,   601,   399,   394,   597,   394,   284,   325,    20,
      20,    97,   106,   326,   198,    98,    91,   364,    68,   365,
     394,   394,    72,   153,   707,   491,   394,    92,    93,   394,
     728,   188,   188,   329,   456,    20,   188,   652,   466,    94,
     188,   418,   594,   480,   328,   595,    97,   109,   235,   330,
      98,   653,   499,   340,   609,   194,   195,   340,    72,    77,
     345,   346,   610,   727,   627,   599,    20,   599,   194,   195,
     394,   405,   355,   356,   357,   148,   196,   619,   236,   151,
     -59,   467,   394,   237,    72,    20,   620,    20,   238,   196,
     287,    20,   342,   343,   344,   308,   394,   239,  -340,  -340,
      25,   309,   725,   644,   331,   272,   628,   194,   195,  -327,
     394,   361,   394,   240,   387,    26,   388,   407,   273,   640,
     389,    20,   390,   391,    48,   392,   393,   142,   196,   444,
     147,   599,   394,    25,   469,   319,   256,   420,   242,   604,
     257,   143,   258,   145,   384,   146,   236,   394,    26,   245,
      20,   237,   234,    20,   246,    20,   238,   188,   214,   718,
     428,   213,   144,   565,   394,   239,   635,    20,   394,    20,
     417,   334,   335,   385,   453,   336,   145,   260,   146,   386,
     684,   240,   713,   284,   194,   195,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,   457,    22,    23,
      24,    25,   194,   195,   256,   196,   242,   452,   257,   455,
     258,   215,   501,   458,   459,   460,    26,   245,   310,   724,
     723,    25,   246,   196,   311,   465,   251,    28,   394,   468,
     647,   471,   276,   394,    20,   141,    26,   481,   394,   482,
      31,    32,   255,   387,    56,   388,    35,   734,    36,   389,
      37,   390,   391,   274,   392,   393,   275,   194,   195,   550,
     188,    20,    38,   188,   145,   414,   146,   415,   410,   188,
     194,   195,    48,   508,   741,   745,   545,   536,   196,   411,
     539,   147,   281,   556,   394,   412,   191,   444,   444,   194,
     195,   196,   333,   394,   194,   195,    74,    52,    65,   194,
     195,    69,    71,    75,    75,    79,    81,   681,    20,   135,
     196,   682,   215,   194,   195,   196,   540,   447,    20,   484,
     196,   748,    96,   102,   408,   188,   188,    20,   108,   558,
     289,   751,    96,   139,   196,   602,    52,   371,   371,   134,
     497,   498,   571,   572,   194,   195,   553,   290,   152,   554,
     194,   195,   402,   192,   192,   406,   292,    82,    83,    84,
      85,    86,    87,    88,   563,   196,   559,   564,   722,    75,
      89,   196,    75,   250,    20,   254,   394,   265,   194,   195,
     194,   195,   194,   195,    90,   138,   567,   194,   195,    20,
     726,   561,   188,   192,   271,   629,   194,   195,   394,   196,
     633,   196,   280,   196,   476,   288,   477,   746,   196,   340,
      91,   293,    96,   464,   630,   394,   402,   196,   294,   309,
     472,    92,    93,   194,   195,   295,   473,    74,   503,   625,
     194,   195,   504,    94,   505,   668,   732,   296,   265,   265,
     192,   669,   297,   337,   196,   708,    96,    20,   332,   298,
     312,   196,   474,   631,   309,   632,   475,   541,   551,   299,
     614,    20,    20,   634,   615,   637,   487,   300,   492,   489,
     493,   487,   371,   444,   444,   641,   612,   613,   147,    20,
     192,   192,   271,   338,   301,   192,     1,     2,     3,   192,
     571,   572,   207,   208,   209,    73,    76,   260,   199,   200,
     201,   202,   203,   204,   205,   206,   210,   211,   212,   302,
      52,   363,   315,   675,   676,   677,   358,   359,   360,   679,
      78,    80,   304,   305,   459,   460,   316,   409,   306,   265,
     307,   265,   416,   683,   427,   366,    71,   265,   265,   379,
     382,   383,   413,   451,   421,   422,   265,   397,  -230,    75,
     463,   401,   265,   403,   404,   265,   196,   402,   478,   560,
     483,   494,   485,   500,   394,   516,   525,   526,   703,   102,
     705,   527,   528,   535,   280,   555,   557,   568,   445,   446,
     711,   288,   712,   569,   575,   611,   626,   618,   638,   645,
     578,   639,   581,   648,   649,   583,   650,   654,   656,   670,
     672,   680,   729,   686,   730,   731,   192,   687,   688,   691,
      96,   733,   716,   692,   694,   704,   250,   706,   736,   737,
     406,   605,   578,   607,   754,   598,   362,   663,   470,   667,
     665,   524,   740,   400,   666,   450,   419,   426,   709,   747,
     673,   678,   546,   375,   502,   367,   750,     0,     0,   753,
       0,     0,   755,     0,     0,     0,     0,     0,    52,     0,
       0,     0,     0,     0,     0,     0,   265,     0,     0,   265,
       0,   265,   265,   347,   348,   349,   350,   351,   352,   353,
     354,     0,   250,   249,   265,   265,    96,   265,   265,   265,
     265,   265,     0,   254,     0,   522,   523,   265,     0,     0,
       0,     0,     0,   265,   265,   265,   265,     0,     0,   192,
       0,   271,   192,     0,     0,     0,     0,   544,   192,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   445,   445,   446,   552,
       0,   689,     0,     0,     0,   693,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   265,     0,   265,
       0,     0,     0,     0,     0,   702,   566,   236,     0,     0,
       0,     0,   237,   271,   192,   192,    20,   238,     0,   265,
       0,     0,     0,     0,   265,   373,   239,   265,     0,   577,
     265,   265,   265,     0,   582,   265,   265,     0,     0,   265,
     588,   590,   240,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,   603,     0,     0,
     265,   265,   265,   265,   254,   256,     0,   242,   374,   257,
       0,   258,     0,     0,     0,     0,     0,     0,   245,     0,
       0,   192,     0,   246,     0,   624,     0,   372,     0,   377,
     380,     0,     0,     0,     0,     0,   396,     0,     0,     0,
       0,     0,   636,     0,     0,     0,     0,   236,     0,     0,
       0,   643,   237,     0,     0,     0,    20,   238,     0,     0,
       0,     0,     0,     0,     0,     0,   239,   265,   265,   265,
     265,   265,     0,   522,     0,   236,     0,   522,   522,     0,
     237,     0,   240,     0,    20,   238,     0,   671,     0,   544,
     674,     0,   368,     0,   239,     0,     0,     0,     0,     0,
       0,     0,   445,   445,     0,   256,   462,   242,   490,   257,
     240,   258,   685,     0,     0,     0,     0,     0,   245,     0,
       0,   265,   265,   246,     0,   265,     0,   265,   265,   265,
     265,   265,   265,   256,   369,   242,     0,   257,     0,   258,
       0,     0,   603,     0,     0,   265,   245,     0,     0,     0,
       0,   246,     0,     0,     0,     0,   710,     0,     0,     0,
       0,   714,   265,     0,   265,     0,     0,   265,   265,     0,
     265,     0,   249,     0,   506,   507,     0,   509,   510,   511,
     513,   514,     0,     0,     0,     0,     0,     0,     0,     0,
     735,     0,     0,   531,   532,   533,   534,     0,     0,     0,
       0,   265,     0,   265,     0,     0,     0,     0,     0,     0,
       0,   742,   265,   265,     0,     0,     0,     0,   749,     0,
       0,     0,     0,     0,     0,   752,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   372,     0,     0,
       5,     0,     6,     7,     8,     9,    10,     0,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,   573,
      22,    23,    24,    25,   574,     0,     0,   576,     0,     0,
     579,   580,     0,     0,     0,   584,   585,     0,    26,   586,
     587,   589,     0,     0,     0,     0,     0,     0,    27,    28,
     236,    29,     0,    30,     0,   237,     0,     0,     0,    20,
     238,     0,    31,    32,    33,     0,    34,   368,    35,   239,
      36,     0,    37,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    38,   240,     6,     7,     8,     9,
      10,     0,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,     0,    22,    23,    24,    25,   461,   369,
     242,     0,   243,     0,   244,     0,     0,     0,     0,     0,
       0,   245,    26,     0,     0,     0,   246,   657,   658,   659,
     660,   661,    27,    28,     0,    29,     0,    30,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,     0,
      34,     0,    35,     0,    36,     0,    37,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    38,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   690,     0,     0,     0,     0,   695,   696,   697,
     698,   699,   700,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,     0,    22,    23,    24,    25,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    53,
       0,     0,   715,    26,   717,     0,     0,   719,   720,     0,
     721,     0,     0,    27,    28,     0,    29,     0,    30,     0,
       0,     0,     0,    55,     0,     0,     0,    31,    32,    33,
       0,    56,   303,    35,     0,    36,     0,    37,     0,     0,
       0,   738,     0,   739,     0,     0,     0,     0,     0,    38,
       0,     0,   743,   744,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,     0,    22,    23,    24,    25,
       0,     0,     0,     0,     0,     0,     0,     0,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,    27,    28,     0,    29,     0,    30,
       0,     0,   126,   127,    55,     0,     0,   128,    31,    32,
      33,     0,    56,     0,    35,     0,    36,     0,    37,     0,
       0,    57,    58,   129,     0,     0,     0,     0,     0,     0,
      38,   130,   429,   430,   431,   432,   433,   434,   435,    18,
     436,    20,   437,   181,    22,   438,   439,    25,     0,     0,
       0,     0,     0,     0,     0,     0,   156,   157,   158,   159,
     160,   161,    26,   162,   163,   164,   120,   165,   166,   167,
     168,   125,    27,    28,     0,    29,     0,    30,     0,     0,
     169,   170,    55,     0,     0,   171,    31,    32,    33,     0,
     440,     0,   441,     0,   442,     0,   443,     0,     0,    57,
      58,     0,     0,     0,     0,     0,     0,     0,    38,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
       0,    22,    23,    24,    25,     0,     0,     0,     0,     0,
       0,     0,     0,   156,   157,   158,   159,   160,   161,    26,
     162,   163,   164,   120,   165,   166,   167,   168,   125,    27,
      28,     0,    29,     0,    30,     0,     0,   169,   170,    55,
       0,     0,   171,    31,    32,    33,     0,    56,     0,    35,
       0,    36,     0,    37,     0,     0,    57,    58,     0,     0,
       0,     0,     0,     0,     0,    38,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,     0,    22,    23,
      24,    25,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    53,     0,     0,     0,    26,     0,     0,     0,
       0,     0,     0,     0,     0,    54,    27,    28,     0,    29,
       0,    30,     0,     0,     0,     0,    55,     0,     0,     0,
      31,    32,    33,   448,   282,     0,    35,     0,   283,   449,
      37,     0,     0,    57,    58,     0,     0,     0,     0,     0,
       0,     0,    38,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,     0,    22,    23,    24,    25,     0,
       0,   327,     0,     0,     0,     0,     0,     0,     0,    53,
       0,     0,     0,    26,     0,     0,     0,     0,     0,     0,
       0,     0,    54,    27,    28,     0,    29,     0,    30,     0,
       0,     0,     0,    55,     0,     0,     0,    31,    32,    33,
       0,    56,     0,    35,     0,    36,     0,    37,     0,     0,
      57,    58,     0,     0,     0,     0,     0,     0,     0,    38,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,     0,    22,    23,    24,    25,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    53,     0,     0,     0,
      26,     0,     0,     0,     0,     0,     0,     0,     0,    54,
      27,    28,     0,    29,     0,    30,     0,     0,     0,     0,
      55,     0,     0,     0,    31,    32,    33,     0,    56,     0,
      35,   454,    36,     0,    37,     0,     0,    57,    58,     0,
       0,     0,     0,     0,     0,     0,    38,   429,   430,   431,
     432,   433,   434,   435,    18,   436,    20,   437,   181,    22,
     438,   439,    25,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    53,     0,     0,     0,    26,     0,     0,
       0,     0,     0,     0,     0,     0,    54,    27,    28,     0,
      29,     0,    30,     0,     0,     0,     0,    55,     0,     0,
       0,    31,    32,    33,     0,   440,     0,   441,     0,   442,
       0,   443,     0,     0,    57,    58,     0,     0,     0,     0,
       0,     0,     0,    38,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,     0,    22,    23,    24,    25,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      53,     0,     0,     0,    26,     0,     0,     0,     0,     0,
       0,     0,     0,    54,    27,    28,     0,    29,     0,    30,
       0,     0,     0,     0,    55,     0,     0,     0,    31,    32,
      33,     0,    56,     0,    35,   562,    36,     0,    37,     0,
       0,    57,    58,     0,     0,     0,     0,     0,     0,     0,
      38,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,     0,    22,    23,    24,    25,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    53,     0,     0,
       0,    26,     0,     0,     0,     0,     0,     0,     0,     0,
      54,    27,    28,     0,    29,     0,    30,     0,     0,     0,
       0,    55,     0,     0,     0,    31,    32,    33,     0,    56,
       0,    35,     0,    36,     0,    37,     0,     0,    57,    58,
       0,     0,     0,     0,     0,     0,     0,    38,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,     0,
      22,    23,    24,    25,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    53,     0,     0,     0,    26,     0,
       0,     0,     0,     0,     0,     0,     0,    54,    27,    28,
       0,    29,     0,    30,     0,     0,     0,     0,    55,     0,
       0,     0,    31,    32,    33,     0,   282,     0,    35,     0,
     283,     0,    37,     0,     0,    57,    58,     0,     0,     0,
       0,     0,     0,     0,    38,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,     0,    22,    23,    24,
      25,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    53,     0,   236,     0,    26,     0,     0,   237,     0,
       0,     0,    20,   238,     0,    27,    28,     0,    29,     0,
      30,     0,   239,     0,     0,    55,     0,     0,     0,    31,
      32,    33,     0,    56,     0,    35,     0,    36,   240,    37,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    38,    22,    23,    24,    25,     0,     0,     0,     0,
       0,   256,   486,   242,     0,   257,    53,   258,   236,     0,
      26,     0,     0,   237,   245,     0,     0,    20,   238,   246,
      27,    28,     0,    29,     0,    30,     0,   239,     0,     0,
       0,     0,     0,     0,    31,    32,    33,     0,    56,   291,
      35,     0,    36,   240,    37,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    38,    22,    23,    24,
      25,     0,     0,     0,     0,     0,   376,     0,   242,     0,
     257,    53,   258,     0,     0,    26,     0,     0,     0,   245,
       0,     0,   512,     0,   246,    27,    28,     0,    29,     0,
      30,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,     0,    56,     0,    35,     0,    36,     0,    37,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    38,    22,    23,    24,    25,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   236,     0,
      26,     0,     0,   237,     0,     0,     0,    20,   238,     0,
      27,    28,     0,    29,     0,    30,     0,   239,     0,     0,
       0,     0,     0,     0,    31,    32,    33,     0,    56,     0,
      35,     0,    36,   240,    37,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    38,    22,    23,    24,
      25,     0,     0,     0,     0,     0,   241,     0,   242,     0,
     243,   236,   244,     0,     0,    26,   237,     0,     0,   245,
      20,   238,     0,     0,   246,     0,    28,     0,     0,     0,
     239,     0,   236,     0,     0,     0,     0,   237,     0,    31,
      32,    20,   238,    56,     0,    35,   240,    36,     0,    37,
       0,   239,     0,   236,     0,     0,     0,     0,   237,     0,
       0,    38,    20,   238,     0,     0,     0,   240,     0,   256,
       0,   242,   239,   257,     0,   258,     0,     0,     0,     0,
       0,     0,   245,     0,     0,     0,     0,   246,   240,     0,
     320,     0,   242,     0,   257,     0,   258,     0,     0,     0,
       0,     0,     0,   245,     0,     0,     0,     0,   246,     0,
       0,   376,     0,   242,     0,   257,     0,   258,     0,     0,
       0,     0,     0,   616,   245,     0,     0,     0,     0,   246,
     172,   173,   174,   175,   176,   177,   178,     0,   179,    20,
     180,   181,    22,   182,   183,     0,     0,   617,   172,   173,
     174,   175,   176,   177,   178,     0,   179,    20,   180,   181,
      22,   182,   183,   172,   173,   174,   175,   176,   177,   178,
       0,   179,    20,   180,   181,    22,   182,   183,   339,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   184,     0,
     185,     0,   186,   341,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   184,     0,   185,     0,
     186,     0,   187,     0,     0,     0,     0,     0,     0,     0,
       0,   184,     0,   185,     0,   186,     0,   187,   172,   173,
     174,   175,   176,   177,   178,     0,   179,    20,   180,   181,
      22,   182,   183,     0,   216,   217,   218,   219,   220,   221,
     222,   223,   224,   225,     0,   226,   227,   228,   229,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   230,   231,
       0,     0,     0,   232,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   184,     0,   185,   129,
     186,     0,   187,     0,     0,     0,     0,   130
};

static const yytype_int16 yycheck[] =
{
       2,     3,    49,    35,    57,    58,     7,     8,    29,    26,
      31,     1,    58,    40,   109,   184,   185,    54,    53,   186,
     517,    57,    58,   397,   529,   596,    28,   143,     7,   600,
       7,   415,     7,    35,     7,    23,    38,    15,    23,    31,
      31,    36,    23,    29,    97,    23,    24,    39,    39,    41,
      40,    31,    34,    55,    77,    37,    79,   104,    44,    39,
     107,    41,    23,     6,     7,     8,     9,    10,    11,    12,
      36,    31,    73,    23,    39,    76,    19,   112,    79,    39,
      23,    34,    39,    78,   105,    42,    36,    23,   125,    77,
      33,   144,    77,   477,    86,    86,    77,    23,    76,   116,
      79,    36,    79,    78,    86,    78,    86,   109,   144,    23,
      23,    72,    76,   145,    67,    76,    59,   233,     0,   235,
      86,    86,    72,   158,    31,    41,    86,    70,    71,    86,
     701,   184,   185,   150,    77,    23,   189,    41,    32,    82,
     193,    67,    34,    78,   146,    37,    72,    76,    36,   151,
      76,    41,    32,   189,   528,    62,    63,   193,    72,    72,
     197,   198,   667,    39,    36,   662,    23,   664,    62,    63,
      86,     7,   207,   208,   209,    26,    83,    15,    14,    36,
      37,    75,    86,    19,    72,    23,    24,    23,    24,    83,
     285,    23,   194,   195,   196,    73,    86,    33,    78,    79,
      29,    79,    41,    77,    36,    67,    78,    62,    63,    31,
      86,   213,    86,    49,    36,    44,    38,    39,    80,    73,
      42,    23,    44,    45,   214,    47,    48,    37,    83,   282,
     277,   728,    86,    29,    36,    90,    72,   274,    74,     7,
      76,    37,    78,    72,   246,    74,    14,    86,    44,    85,
      23,    19,   253,    23,    90,    23,    24,   310,     7,    77,
     281,    36,    58,    36,    86,    33,    36,    23,    86,    23,
     272,   440,   441,    73,   310,   442,    72,    81,    74,    79,
      36,    49,    36,   285,    62,    63,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,   314,    26,    27,
      28,    29,    62,    63,    72,    83,    74,   309,    76,   311,
      78,    44,    90,   315,   316,   317,    44,    85,    78,    77,
      73,    29,    90,    83,    84,   327,    30,    55,    86,   331,
      78,   333,    60,    86,    23,   362,    44,   339,    86,   341,
      68,    69,    30,    36,    72,    38,    74,    36,    76,    42,
      78,    44,    45,    36,    47,    48,    61,    62,    63,    67,
     413,    23,    90,   416,    72,    77,    74,    79,    39,   422,
      62,    63,   362,   390,    36,    77,   422,   413,    83,    73,
     416,   428,    36,    75,    86,    79,   422,   440,   441,    62,
      63,    83,    65,    86,    62,    63,   397,     1,     2,    62,
      63,     5,     6,     7,     8,     9,    10,    75,    23,   441,
      83,    79,    44,    62,    63,    83,   418,    80,    23,    51,
      83,    36,    26,    27,    31,   478,   479,    23,    32,    78,
      73,    36,    36,    37,    83,    31,    40,   241,   242,   441,
      78,    79,   478,   479,    62,    63,   448,    73,    52,   451,
      62,    63,   256,    57,    58,   259,    73,     6,     7,     8,
       9,    10,    11,    12,   466,    83,    78,   469,    78,    73,
      19,    83,    76,    77,    23,    79,    86,    81,    62,    63,
      62,    63,    62,    63,    33,    19,    66,    62,    63,    23,
      78,    75,   545,    97,    98,    77,    62,    63,    86,    83,
      75,    83,   106,    83,    77,   109,    79,    78,    83,   545,
      59,    73,   116,    73,    80,    86,   320,    83,    73,    79,
      73,    70,    71,    62,    63,    73,    79,   528,    72,   550,
      62,    63,    76,    82,    78,    73,    75,    73,   142,   143,
     144,    79,    73,    19,    83,    77,   150,    23,   152,    73,
      75,    83,    75,   555,    79,   557,    79,    19,    19,    73,
      32,    23,    23,   565,    36,   567,   370,    73,    77,   373,
      79,   375,   376,   626,   627,    19,    78,    79,   625,    23,
     184,   185,   186,   187,    73,   189,     3,     4,     5,   193,
     626,   627,    38,    39,    40,     7,     8,   401,    45,    46,
      47,    48,    49,    50,    51,    52,    41,    42,    43,    73,
     214,   215,    36,   615,   616,   617,   210,   211,   212,   621,
       9,    10,    73,    73,   626,   627,    36,    41,    73,   233,
      73,   235,    36,   635,    35,   239,   240,   241,   242,   243,
     244,   245,    79,    36,    78,    78,   250,   251,    36,   253,
      30,   255,   256,   257,   258,   259,    83,   461,    36,   463,
      44,    32,    73,    44,    86,    13,    79,    32,   670,   273,
     672,    32,    13,    73,   278,    34,    36,    73,   282,   283,
     682,   285,   684,    75,    78,    32,    36,    78,    78,    32,
     494,    78,   496,    78,    32,   499,    78,    32,    32,    36,
      56,    32,   704,    36,   706,   707,   310,    32,    36,    36,
     314,   713,    75,    37,    37,    36,   320,    36,    36,    36,
     524,   525,   526,   527,    36,   516,   214,   597,   332,   604,
     601,   401,   734,   254,   602,   285,   273,   278,   680,   741,
     613,   618,   423,   242,   386,   240,   748,    -1,    -1,   751,
      -1,    -1,   754,    -1,    -1,    -1,    -1,    -1,   362,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   370,    -1,    -1,   373,
      -1,   375,   376,   199,   200,   201,   202,   203,   204,   205,
     206,    -1,   386,    77,   388,   389,   390,   391,   392,   393,
     394,   395,    -1,   397,    -1,   399,   400,   401,    -1,    -1,
      -1,    -1,    -1,   407,   408,   409,   410,    -1,    -1,   413,
      -1,   415,   416,    -1,    -1,    -1,    -1,   421,   422,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   440,   441,   442,   443,
      -1,   645,    -1,    -1,    -1,   649,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   461,    -1,   463,
      -1,    -1,    -1,    -1,    -1,   669,   470,    14,    -1,    -1,
      -1,    -1,    19,   477,   478,   479,    23,    24,    -1,   483,
      -1,    -1,    -1,    -1,   488,    32,    33,   491,    -1,   493,
     494,   495,   496,    -1,   498,   499,   500,    -1,    -1,   503,
     504,   505,    49,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   516,    -1,    -1,    -1,    -1,   521,    -1,    -1,
     524,   525,   526,   527,   528,    72,    -1,    74,    75,    76,
      -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,
      -1,   545,    -1,    90,    -1,   549,    -1,   241,    -1,   243,
     244,    -1,    -1,    -1,    -1,    -1,   250,    -1,    -1,    -1,
      -1,    -1,   566,    -1,    -1,    -1,    -1,    14,    -1,    -1,
      -1,   575,    19,    -1,    -1,    -1,    23,    24,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    33,   591,   592,   593,
     594,   595,    -1,   597,    -1,    14,    -1,   601,   602,    -1,
      19,    -1,    49,    -1,    23,    24,    -1,   611,    -1,   613,
     614,    -1,    31,    -1,    33,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   626,   627,    -1,    72,   320,    74,    75,    76,
      49,    78,   636,    -1,    -1,    -1,    -1,    -1,    85,    -1,
      -1,   645,   646,    90,    -1,   649,    -1,   651,   652,   653,
     654,   655,   656,    72,    73,    74,    -1,    76,    -1,    78,
      -1,    -1,   666,    -1,    -1,   669,    85,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    -1,    -1,   680,    -1,    -1,    -1,
      -1,   685,   686,    -1,   688,    -1,    -1,   691,   692,    -1,
     694,    -1,   386,    -1,   388,   389,    -1,   391,   392,   393,
     394,   395,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     714,    -1,    -1,   407,   408,   409,   410,    -1,    -1,    -1,
      -1,   725,    -1,   727,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   735,   736,   737,    -1,    -1,    -1,    -1,   742,    -1,
      -1,    -1,    -1,    -1,    -1,   749,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   461,    -1,    -1,
       6,    -1,     8,     9,    10,    11,    12,    -1,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,   483,
      26,    27,    28,    29,   488,    -1,    -1,   491,    -1,    -1,
     494,   495,    -1,    -1,    -1,   499,   500,    -1,    44,   503,
     504,   505,    -1,    -1,    -1,    -1,    -1,    -1,    54,    55,
      14,    57,    -1,    59,    -1,    19,    -1,    -1,    -1,    23,
      24,    -1,    68,    69,    70,    -1,    72,    31,    74,    33,
      76,    -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    49,     8,     9,    10,    11,
      12,    -1,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    -1,    26,    27,    28,    29,    72,    73,
      74,    -1,    76,    -1,    78,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    44,    -1,    -1,    -1,    90,   591,   592,   593,
     594,   595,    54,    55,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    68,    69,    70,    -1,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   646,    -1,    -1,    -1,    -1,   651,   652,   653,
     654,   655,   656,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    -1,    26,    27,    28,    29,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,
      -1,    -1,   686,    44,   688,    -1,    -1,   691,   692,    -1,
     694,    -1,    -1,    54,    55,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    -1,    -1,    -1,    68,    69,    70,
      -1,    72,    73,    74,    -1,    76,    -1,    78,    -1,    -1,
      -1,   725,    -1,   727,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,   736,   737,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    -1,    26,    27,    28,    29,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    -1,    57,    -1,    59,
      -1,    -1,    62,    63,    64,    -1,    -1,    67,    68,    69,
      70,    -1,    72,    -1,    74,    -1,    76,    -1,    78,    -1,
      -1,    81,    82,    83,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    91,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    -1,    57,    -1,    59,    -1,    -1,
      62,    63,    64,    -1,    -1,    67,    68,    69,    70,    -1,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    -1,    81,
      82,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      -1,    26,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    -1,    57,    -1,    59,    -1,    -1,    62,    63,    64,
      -1,    -1,    67,    68,    69,    70,    -1,    72,    -1,    74,
      -1,    76,    -1,    78,    -1,    -1,    81,    82,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    -1,    26,    27,
      28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    53,    54,    55,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,
      68,    69,    70,    71,    72,    -1,    74,    -1,    76,    77,
      78,    -1,    -1,    81,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    -1,    26,    27,    28,    29,    -1,
      -1,    32,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,
      -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    53,    54,    55,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    -1,    -1,    -1,    68,    69,    70,
      -1,    72,    -1,    74,    -1,    76,    -1,    78,    -1,    -1,
      81,    82,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    -1,    26,    27,    28,    29,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    40,    -1,    -1,    -1,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,
      54,    55,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    -1,    -1,    -1,    68,    69,    70,    -1,    72,    -1,
      74,    75,    76,    -1,    78,    -1,    -1,    81,    82,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    40,    -1,    -1,    -1,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    53,    54,    55,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    -1,    -1,
      -1,    68,    69,    70,    -1,    72,    -1,    74,    -1,    76,
      -1,    78,    -1,    -1,    81,    82,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    -1,    26,    27,    28,    29,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      40,    -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    53,    54,    55,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    68,    69,
      70,    -1,    72,    -1,    74,    75,    76,    -1,    78,    -1,
      -1,    81,    82,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    -1,    26,    27,    28,    29,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,    -1,    -1,
      -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      53,    54,    55,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    -1,    -1,    -1,    68,    69,    70,    -1,    72,
      -1,    74,    -1,    76,    -1,    78,    -1,    -1,    81,    82,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    -1,
      26,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    40,    -1,    -1,    -1,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,    54,    55,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    -1,
      -1,    -1,    68,    69,    70,    -1,    72,    -1,    74,    -1,
      76,    -1,    78,    -1,    -1,    81,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    -1,    26,    27,    28,
      29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    40,    -1,    14,    -1,    44,    -1,    -1,    19,    -1,
      -1,    -1,    23,    24,    -1,    54,    55,    -1,    57,    -1,
      59,    -1,    33,    -1,    -1,    64,    -1,    -1,    -1,    68,
      69,    70,    -1,    72,    -1,    74,    -1,    76,    49,    78,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    90,    26,    27,    28,    29,    -1,    -1,    -1,    -1,
      -1,    72,    73,    74,    -1,    76,    40,    78,    14,    -1,
      44,    -1,    -1,    19,    85,    -1,    -1,    23,    24,    90,
      54,    55,    -1,    57,    -1,    59,    -1,    33,    -1,    -1,
      -1,    -1,    -1,    -1,    68,    69,    70,    -1,    72,    73,
      74,    -1,    76,    49,    78,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    90,    26,    27,    28,
      29,    -1,    -1,    -1,    -1,    -1,    72,    -1,    74,    -1,
      76,    40,    78,    -1,    -1,    44,    -1,    -1,    -1,    85,
      -1,    -1,    88,    -1,    90,    54,    55,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,
      69,    70,    -1,    72,    -1,    74,    -1,    76,    -1,    78,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    90,    26,    27,    28,    29,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    14,    -1,
      44,    -1,    -1,    19,    -1,    -1,    -1,    23,    24,    -1,
      54,    55,    -1,    57,    -1,    59,    -1,    33,    -1,    -1,
      -1,    -1,    -1,    -1,    68,    69,    70,    -1,    72,    -1,
      74,    -1,    76,    49,    78,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    90,    26,    27,    28,
      29,    -1,    -1,    -1,    -1,    -1,    72,    -1,    74,    -1,
      76,    14,    78,    -1,    -1,    44,    19,    -1,    -1,    85,
      23,    24,    -1,    -1,    90,    -1,    55,    -1,    -1,    -1,
      33,    -1,    14,    -1,    -1,    -1,    -1,    19,    -1,    68,
      69,    23,    24,    72,    -1,    74,    49,    76,    -1,    78,
      -1,    33,    -1,    14,    -1,    -1,    -1,    -1,    19,    -1,
      -1,    90,    23,    24,    -1,    -1,    -1,    49,    -1,    72,
      -1,    74,    33,    76,    -1,    78,    -1,    -1,    -1,    -1,
      -1,    -1,    85,    -1,    -1,    -1,    -1,    90,    49,    -1,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    72,    -1,    74,    -1,    76,    -1,    78,    -1,    -1,
      -1,    -1,    -1,     7,    85,    -1,    -1,    -1,    -1,    90,
      14,    15,    16,    17,    18,    19,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    -1,    -1,    31,    14,    15,
      16,    17,    18,    19,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    14,    15,    16,    17,    18,    19,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      74,    -1,    76,    44,    78,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    74,    -1,
      76,    -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    74,    -1,    76,    -1,    78,    14,    15,
      16,    17,    18,    19,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    -1,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    -1,    49,    50,    51,    52,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    62,    63,
      -1,    -1,    -1,    67,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    74,    83,
      76,    -1,    78,    -1,    -1,    -1,    -1,    91
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,    93,     6,     8,     9,    10,    11,
      12,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    26,    27,    28,    29,    44,    54,    55,    57,
      59,    68,    69,    70,    72,    74,    76,    78,    90,    94,
      95,    96,    97,    98,    99,   101,   106,   111,   120,   125,
     133,   147,   167,    40,    53,    64,    72,    81,    82,   115,
     116,   117,   118,   119,   120,   167,   115,   167,     0,   167,
     160,   167,    72,   110,   111,   167,   110,    72,   152,   167,
     152,   167,     6,     7,     8,     9,    10,    11,    12,    19,
      33,    59,    70,    71,    82,   146,   167,    72,    76,   121,
     122,   139,   167,   115,   125,   134,    76,   125,   167,    76,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    62,    63,    67,    83,
      91,   112,   115,   150,   115,   150,   145,   146,    19,   167,
     115,    96,    37,    37,    58,    72,    74,   147,    26,    29,
      44,    36,   167,   118,   116,   115,    38,    39,    40,    41,
      42,    43,    45,    46,    47,    49,    50,    51,    52,    62,
      63,    67,    14,    15,    16,    17,    18,    19,    20,    22,
      24,    25,    27,    28,    72,    74,    76,    78,   133,   137,
     138,   140,   167,   137,    62,    63,    83,    34,    67,    45,
      46,    47,    48,    49,    50,    51,    52,    38,    39,    40,
      41,    42,    43,    36,     7,    44,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    49,    50,    51,    52,
      62,    63,    67,    36,   111,    36,    14,    19,    24,    33,
      49,    72,    74,    76,    78,    85,    90,   153,   154,   158,
     167,    30,   109,   110,   167,    30,    72,    76,    78,   114,
     157,   158,   161,   163,   164,   167,   140,   141,   142,   143,
     144,   167,    67,    80,    36,    61,    60,   125,   126,   127,
     167,    36,    72,    76,   115,   123,   124,   139,   167,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    79,
      78,    84,    75,    77,    79,    36,    36,    36,    78,    90,
      72,   151,   152,   157,   151,   140,   150,    32,   115,   146,
     115,    36,   167,    65,   141,   141,   143,    19,   167,    44,
     140,    44,   115,   115,   115,   116,   116,   117,   117,   117,
     117,   117,   117,   117,   117,   118,   118,   118,   119,   119,
     119,   115,    95,   167,   151,   151,   167,   160,    31,    73,
     156,   157,   158,    32,    75,   156,    72,   158,   165,   167,
     158,   166,   167,   167,   115,    73,    79,    36,    38,    42,
      44,    45,    47,    48,    86,   155,   158,   167,     7,    78,
     112,   167,   157,   167,   167,     7,   157,    39,    31,    41,
      39,    73,    79,    79,    77,    79,    36,   115,    67,   122,
     116,    78,    78,   135,   136,    77,   127,    35,   125,    14,
      15,    16,    17,    18,    19,    20,    22,    24,    27,    28,
      72,    74,    76,    78,   133,   167,   167,    80,    71,    77,
     124,    36,   115,   140,    75,   115,    77,   146,   115,   115,
     115,    72,   158,    30,    73,   115,    32,    75,   115,    36,
     167,   115,    73,    79,    75,    79,    77,    79,    36,    36,
      78,   115,   115,    44,    51,    73,    73,   157,    41,   157,
      75,    41,    77,    79,    32,    39,    42,    78,    79,    32,
      44,    90,   154,    72,    76,    78,   158,   158,   146,   158,
     158,   158,    88,   158,   158,   109,    13,   104,   105,   102,
     103,   113,   167,   167,   114,    79,    32,    32,    13,   107,
     108,   158,   158,   158,   158,    73,   140,    77,   144,   140,
     115,    19,   148,   149,   167,   137,   136,   128,   129,   130,
      67,    19,   167,   115,   115,    34,    75,    36,    78,    78,
     157,    75,    75,   115,   115,    36,   167,    66,    73,    75,
      77,   140,   140,   158,   158,    78,   158,   167,   157,   158,
     158,   157,   167,   157,   158,   158,   158,   158,   167,   158,
     167,    36,    34,    37,    34,    37,     7,    78,    99,   105,
       7,    79,    31,   167,     7,   157,   162,   157,   100,   109,
     108,    32,    78,    79,    32,    36,     7,    31,    78,    15,
      24,    76,   131,   132,   167,   125,    36,    36,    78,    77,
      80,   115,   115,    75,   115,    36,   167,   115,    78,    78,
      73,    19,   159,   167,    77,    32,    41,    78,    78,    32,
      78,    39,    41,    41,    32,    39,    32,   158,   158,   158,
     158,   158,   104,   102,   104,   103,   113,   107,    73,    79,
      36,   167,    56,   149,   167,   115,   115,   115,   129,   115,
      32,    75,    79,   115,    36,   167,    36,    32,    36,   157,
     158,    36,    37,   157,    37,   158,   158,   158,   158,   158,
     158,     7,   157,   115,    36,   115,    36,    31,    77,   132,
     167,   115,   115,    36,   167,   158,    75,   158,    77,   158,
     158,   158,    78,    73,    77,    41,    78,    39,   104,   115,
     115,   115,    75,   115,    36,   167,    36,    36,   158,   158,
     115,    36,   167,   158,   158,    77,    78,   115,    36,   167,
     115,    36,   167,   115,    36,   115
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    92,    93,    93,    93,    93,    94,    94,    95,    95,
      95,    96,    96,    96,    96,    96,    96,    96,    96,    96,
      96,    96,    96,    96,    96,    96,    96,    96,    96,    97,
      98,    98,    99,   100,   101,   101,   101,   101,   101,   101,
     101,   101,   102,   102,   103,   104,   104,   105,   106,   106,
     106,   106,   107,   107,   108,   109,   109,   110,   110,   111,
     111,   112,   112,   112,   112,   112,   112,   112,   112,   112,
     112,   112,   112,   112,   112,   112,   112,   112,   112,   112,
     113,   113,   114,   114,   115,   115,   115,   115,   115,   115,
     115,   115,   115,   116,   116,   117,   117,   117,   117,   117,
     117,   117,   117,   117,   118,   118,   118,   118,   118,   119,
     119,   119,   119,   120,   120,   120,   120,   120,   120,   120,
     120,   120,   121,   121,   122,   123,   123,   124,   124,   125,
     125,   125,   125,   125,   125,   125,   125,   125,   125,   125,
     125,   125,   125,   125,   125,   125,   125,   125,   125,   125,
     125,   125,   125,   125,   125,   125,   125,   125,   125,   125,
     125,   125,   125,   125,   125,   125,   125,   125,   125,   125,
     125,   125,   125,   125,   125,   125,   125,   125,   125,   125,
     125,   125,   125,   125,   126,   126,   127,   128,   128,   129,
     130,   130,   131,   131,   132,   132,   132,   133,   133,   134,
     134,   135,   135,   136,   136,   137,   137,   138,   138,   138,
     138,   138,   138,   138,   138,   138,   138,   138,   138,   138,
     138,   138,   138,   138,   138,   138,   138,   138,   138,   138,
     139,   139,   139,   139,   139,   140,   141,   141,   142,   142,
     143,   143,   144,   145,   145,   145,   146,   146,   146,   146,
     146,   146,   146,   146,   146,   146,   146,   146,   146,   146,
     147,   147,   147,   147,   148,   148,   149,   149,   149,   150,
     150,   150,   151,   151,   152,   153,   153,   154,   154,   154,
     154,   154,   154,   154,   154,   154,   154,   154,   154,   154,
     154,   154,   154,   154,   154,   154,   154,   154,   155,   155,
     156,   156,   157,   157,   157,   158,   158,   158,   158,   158,
     158,   158,   158,   158,   158,   158,   158,   158,   158,   158,
     158,   158,   158,   159,   159,   160,   160,   161,   161,   162,
     162,   163,   163,   164,   164,   165,   165,   166,   166,   166,
     166,   167
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
       1,     1,     1,     1,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     2,     1,     3,     3,     1,     4,
       2,     0,     3,     1,     1,     1,     1,     1,     2,     2,
       1,     2,     1,     4,     6,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     4,     3,     5,     5,     3,     4,     3,     4,     1,
       1,     3,     4,     3,     4,     1,     1,     0,     3,     1,
       3,     1,     3,     0,     3,     5,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     2,     2,     1,     1,     3,     3,     5,     5,     0,
       1,     3,     3,     1,     3,     1,     3,     2,     3,     3,
       3,     7,     9,     7,     7,     9,     7,     5,     5,     5,
       5,     7,     7,     9,     9,     7,     7,     5,     1,     2,
       2,     1,     3,     1,     1,     1,     3,     2,     3,     7,
       3,     3,     3,     3,     2,     1,     1,     4,     3,     3,
       4,     1,     3,     1,     1,     1,     3,     1,     5,     1,
       3,     1,     3,     3,     3,     5,     3,     5,     3,     3,
       1,     1
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
#line 489 "hexpr.y" /* yacc.c:1646  */
    { yyParsedModule = (yyvsp[0].module);                     }
#line 2653 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 3:
#line 490 "hexpr.y" /* yacc.c:1646  */
    { yyParsedVar    = *(yyvsp[-2].string); yyParsedExpr = (yyvsp[0].exp); }
#line 2659 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 4:
#line 491 "hexpr.y" /* yacc.c:1646  */
    { yyParsedVar    = "";  yyParsedExpr = (yyvsp[0].exp); }
#line 2665 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 5:
#line 492 "hexpr.y" /* yacc.c:1646  */
    { yyParsedExpr   = (yyvsp[0].exp);                     }
#line 2671 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 6:
#line 495 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = new Module(*(yyvsp[-2].string), *(yyvsp[0].mdefs)); }
#line 2677 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 7:
#line 496 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = new Module(freshName(), *(yyvsp[0].mdefs)); }
#line 2683 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 8:
#line 498 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); }
#line 2689 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 9:
#line 499 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2695 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 10:
#line 500 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = (yyvsp[-1].mdefs);                            (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2701 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 11:
#line 502 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2707 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 12:
#line 503 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2713 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 13:
#line 504 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mvtydef); }
#line 2719 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 14:
#line 505 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2725 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 15:
#line 506 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2731 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 16:
#line 508 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2737 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 17:
#line 509 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 2743 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 18:
#line 510 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 2749 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 19:
#line 511 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 2755 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 20:
#line 512 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 2761 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 21:
#line 513 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 2767 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 22:
#line 514 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 2773 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 23:
#line 515 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-9]), (yylsp[0]))); }
#line 2779 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 24:
#line 516 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-10]), (yylsp[0]))); }
#line 2785 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 25:
#line 517 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-11]), (yylsp[0]))); }
#line 2791 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 26:
#line 518 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-12]), (yylsp[0]))); }
#line 2797 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 27:
#line 519 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-13]), (yylsp[0]))); }
#line 2803 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 28:
#line 522 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(freshName()), let(freshName(), ExprPtr((yyvsp[0].exp)), mktunit(m((yylsp[0]))), m((yylsp[0]))), m((yylsp[0]))); }
#line 2809 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 29:
#line 525 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MImport(yyModulePath, *(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 2815 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 30:
#line 528 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MTypeDef(MTypeDef::Transparent, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 2821 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 31:
#line 529 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MTypeDef(MTypeDef::Opaque, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 2827 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 32:
#line 532 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydef) = new MVarTypeDef(*(yyvsp[-2].string), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]), (yylsp[0]))); }
#line 2833 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 33:
#line 534 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdef) = new MVarDef(*(yyvsp[-2].strings), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2839 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 34:
#line 537 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-3].tconstraints), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2845 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 35:
#line 538 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-6]), (yylsp[0]))); wantIndent(false); }
#line 2851 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 36:
#line 539 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2857 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 37:
#line 540 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-7].tconstraints), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-8]), (yylsp[0])));            wantIndent(false); }
#line 2863 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 38:
#line 541 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2869 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 39:
#line 542 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2875 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 40:
#line 543 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-4]), (yylsp[0])));            wantIndent(false); }
#line 2881 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 41:
#line 544 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2887 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 42:
#line 546 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundeps) = autorelease(new CFunDepDefs()); (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2893 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 43:
#line 547 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundeps) = (yyvsp[-2].fundeps);                             (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2899 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 44:
#line 549 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundep) = autorelease(new CFunDepDef(*(yyvsp[-2].strings), *(yyvsp[0].strings))); }
#line 2905 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 45:
#line 551 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydefs) = autorelease(new MVarTypeDefs()); (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2911 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 46:
#line 552 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydefs) = (yyvsp[-1].mvtydefs);                              (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2917 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 47:
#line 554 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydef) = (yyvsp[0].mvtydef); }
#line 2923 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 48:
#line 557 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2929 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 49:
#line 558 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-3].tconstraints),           *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2935 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 50:
#line 559 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-4]), (yylsp[0])));        wantIndent(false); }
#line 2941 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 51:
#line 560 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-5].tconstraints),           *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-6]), (yylsp[0])));        wantIndent(false); }
#line 2947 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 52:
#line 562 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdefs) = autorelease(new MVarDefs()); (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2953 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 53:
#line 563 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdefs) = (yyvsp[-1].mvdefs);                          (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2959 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 54:
#line 565 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdef) = (yyvsp[0].mvdef); }
#line 2965 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 55:
#line 568 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[0].strings); }
#line 2971 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 56:
#line 570 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[-1].string)); (yyval.strings)->push_back(*(yyvsp[-2].string)); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 2977 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 57:
#line 572 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 2983 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 58:
#line 573 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 2989 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 59:
#line 575 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 2995 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 60:
#line 577 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[-1].string); }
#line 3001 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 61:
#line 579 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("and")); }
#line 3007 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 62:
#line 580 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("or")); }
#line 3013 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 63:
#line 581 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3019 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 64:
#line 582 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3025 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 65:
#line 583 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("~")); }
#line 3031 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 66:
#line 584 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("=~")); }
#line 3037 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 67:
#line 585 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("===")); }
#line 3043 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 68:
#line 586 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("==")); }
#line 3049 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 69:
#line 587 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("<")); }
#line 3055 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 70:
#line 588 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("<=")); }
#line 3061 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 71:
#line 589 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(">")); }
#line 3067 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 72:
#line 590 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(">=")); }
#line 3073 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 73:
#line 591 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("in")); }
#line 3079 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 74:
#line 592 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("append")); }
#line 3085 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 75:
#line 593 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("+")); }
#line 3091 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 76:
#line 594 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("-")); }
#line 3097 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 77:
#line 595 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("*")); }
#line 3103 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 78:
#line 596 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("/")); }
#line 3109 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 79:
#line 597 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("%")); }
#line 3115 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 80:
#line 599 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3121 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 81:
#line 600 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3127 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 82:
#line 602 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3133 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 83:
#line 603 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes);                           (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3139 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 84:
#line 606 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3145 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 85:
#line 607 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3151 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 86:
#line 608 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), (yyvsp[0].exp), m((yylsp[-1]),(yylsp[0]))); }
#line 3157 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 87:
#line 609 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("and",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3163 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 88:
#line 610 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("or",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3169 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 89:
#line 611 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("compose",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3175 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 90:
#line 612 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assign(ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3181 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 91:
#line 613 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("in",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3187 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 92:
#line 614 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3193 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 93:
#line 616 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP3(var("if",m((yylsp[-5]))), (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-5]), (yylsp[0]))); }
#line 3199 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 94:
#line 617 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3205 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 95:
#line 619 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("~",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3211 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 96:
#line 620 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("===",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3217 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 97:
#line 621 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3223 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 98:
#line 622 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))), m((yylsp[-2]),(yylsp[0]))); }
#line 3229 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 99:
#line 623 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("<",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3235 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 100:
#line 624 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("<=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3241 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 101:
#line 625 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var(">",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3247 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 102:
#line 626 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var(">=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3253 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 103:
#line 627 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3259 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 104:
#line 629 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("+",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3265 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 105:
#line 630 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("-",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3271 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 106:
#line 631 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("append",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3277 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 107:
#line 632 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("neg",m((yylsp[-1]))), ExprPtr((yyvsp[0].exp)), m((yylsp[-1]),(yylsp[0]))); }
#line 3283 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 108:
#line 633 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3289 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 109:
#line 635 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("*", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3295 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 110:
#line 636 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("/", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3301 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 111:
#line 637 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("%", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3307 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 112:
#line 638 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3313 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 113:
#line 640 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3319 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 114:
#line 643 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-2].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3325 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 115:
#line 644 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]),(yylsp[0])))->clone(); }
#line 3331 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 116:
#line 647 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileMatch(yyParseCC, *(yyvsp[-2].exps), normPatternRules(*(yyvsp[0].patternexps), m((yylsp[-3]),(yylsp[0]))), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3337 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 117:
#line 650 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileMatchTest(yyParseCC, ExprPtr((yyvsp[-2].exp)), PatternPtr((yyvsp[0].pattern)), m((yylsp[-2]),(yylsp[0])))->clone(); }
#line 3343 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 118:
#line 653 "hexpr.y" /* yacc.c:1646  */
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
#line 3358 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 119:
#line 665 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-1].letbindings), ExprPtr(new Unit(m((yylsp[-3]),(yylsp[0])))), m((yylsp[-3]),(yylsp[0]))); }
#line 3364 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 120:
#line 666 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[-1].exp)), m((yylsp[-5]),(yylsp[0]))); }
#line 3370 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 121:
#line 669 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assump(ExprPtr((yyvsp[-2].exp)), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]),(yylsp[0]))); }
#line 3376 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 122:
#line 671 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].letbindings)->push_back(*(yyvsp[0].letbinding)); (yyval.letbindings) = (yyvsp[-2].letbindings); }
#line 3382 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 123:
#line 672 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3388 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 124:
#line 674 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-2].pattern)), ExprPtr((yyvsp[0].exp)))); }
#line 3394 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 125:
#line 676 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = (yyvsp[-1].letbindings); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3400 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 126:
#line 677 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3406 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 127:
#line 679 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)))); }
#line 3412 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 128:
#line 680 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr(new MatchAny("_",m((yylsp[-1])))), ExprPtr((yyvsp[-1].exp)))); }
#line 3418 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 129:
#line 683 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].exps), m((yylsp[-3]), (yylsp[0]))); }
#line 3424 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 130:
#line 684 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = varCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3430 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 131:
#line 687 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("range", m((yylsp[-2]))), list(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]), (yylsp[0]))); }
#line 3436 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 132:
#line 688 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("iterateS", m((yylsp[-1]))), list(ExprPtr((yyvsp[-2].exp)), fn(str::strings(".x"), fncall(var("+", m((yylsp[-1]))), list(var(".x", m((yylsp[-1]))), ExprPtr(new Int(1, m((yylsp[-1]))))), m((yylsp[-1]))), m((yylsp[-1])))), m((yylsp[-3]), (yylsp[0]))); }
#line 3442 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 133:
#line 689 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileArrayComprehension(ExprPtr((yyvsp[-5].exp)), PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 3448 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 134:
#line 690 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileArrayComprehension(ExprPtr((yyvsp[-7].exp)), PatternPtr((yyvsp[-5].pattern)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 3454 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 135:
#line 691 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkArray(*(yyvsp[-1].exps), m((yylsp[-2]), (yylsp[0]))); }
#line 3460 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 136:
#line 692 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new AIndex(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3466 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 137:
#line 693 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("slice", m((yylsp[-2]))), list(ExprPtr((yyvsp[-5].exp)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-5]), (yylsp[0]))); }
#line 3472 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 138:
#line 694 "hexpr.y" /* yacc.c:1646  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-1]))), list(var(vn,m((yylsp[-4]))), ExprPtr((yyvsp[-2].exp)), fncall(var("size",m((yylsp[-1]))), list(var(vn,m((yylsp[-4])))),m((yylsp[-4])))),m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3478 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 139:
#line 695 "hexpr.y" /* yacc.c:1646  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-2]))), list(var(vn,m((yylsp[-4]))), fncall(var("size",m((yylsp[-2]))), list(var(vn,m((yylsp[-2])))),m((yylsp[-4]))), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3484 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 140:
#line 698 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-3].string), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3490 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 141:
#line 699 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(".f" + str::from((yyvsp[-3].intv)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3496 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 142:
#line 700 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-1].string), ExprPtr(new Unit(m((yylsp[-1])))), m((yylsp[-2]), (yylsp[0]))); }
#line 3502 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 143:
#line 701 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-4].exp)), *(yyvsp[-1].vfields), m((yylsp[-5]), (yylsp[0]))); }
#line 3508 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 144:
#line 702 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-6].exp)), *(yyvsp[-3].vfields), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3514 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 145:
#line 705 "hexpr.y" /* yacc.c:1646  */
    { if ((yyvsp[-1].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-1].rfields), m((yylsp[-2]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-2]), (yylsp[0]))); } }
#line 3520 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 146:
#line 706 "hexpr.y" /* yacc.c:1646  */
    { if ((yyvsp[-2].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-2].rfields), m((yylsp[-3]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-3]), (yylsp[0]))); } }
#line 3526 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 147:
#line 707 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makeProjSeq((yyvsp[-1].exp), *(yyvsp[0].strings), m((yylsp[-1]), (yylsp[0]))); }
#line 3532 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 148:
#line 710 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Fn(str::strings("x"), proj(var("x", m((yylsp[0]))), *(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3538 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 149:
#line 713 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Pack(ExprPtr((yyvsp[0].exp)), m((yylsp[-1]), (yylsp[0]))); }
#line 3544 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 150:
#line 714 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Unpack(*(yyvsp[-4].string), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3550 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 151:
#line 717 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Bool((yyvsp[0].boolv), m((yylsp[0]))); }
#line 3556 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 152:
#line 718 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3562 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 153:
#line 719 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3568 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 154:
#line 720 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3574 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 155:
#line 721 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Short((yyvsp[0].shortv), m((yylsp[0]))); }
#line 3580 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 156:
#line 722 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Int((yyvsp[0].intv), m((yylsp[0]))); }
#line 3586 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 157:
#line 723 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Long((yyvsp[0].longv), m((yylsp[0]))); }
#line 3592 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 158:
#line 724 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Float((yyvsp[0].floatv), m((yylsp[0]))); }
#line 3598 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 159:
#line 725 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Double((yyvsp[0].doublev), m((yylsp[0]))); }
#line 3604 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 160:
#line 726 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3610 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 161:
#line 727 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0])))->clone(); }
#line 3616 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 162:
#line 728 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3622 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 163:
#line 729 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3628 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 164:
#line 732 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = pickNestedExp((yyvsp[-1].exps), m((yylsp[-2]),(yylsp[0]))); }
#line 3634 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 165:
#line 735 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("append", m((yylsp[-1]))); }
#line 3640 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 166:
#line 736 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("+",      m((yylsp[-1]))); }
#line 3646 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 167:
#line 737 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("-",      m((yylsp[-1]))); }
#line 3652 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 168:
#line 738 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("*",      m((yylsp[-1]))); }
#line 3658 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 169:
#line 739 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("/",      m((yylsp[-1]))); }
#line 3664 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 170:
#line 740 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("%",      m((yylsp[-1]))); }
#line 3670 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 171:
#line 741 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("~",      m((yylsp[-1]))); }
#line 3676 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 172:
#line 742 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("===",    m((yylsp[-1]))); }
#line 3682 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 173:
#line 743 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("==",     m((yylsp[-1]))); }
#line 3688 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 174:
#line 744 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("!=",     m((yylsp[-1]))); }
#line 3694 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 175:
#line 745 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("<",      m((yylsp[-1]))); }
#line 3700 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 176:
#line 746 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var(">",      m((yylsp[-1]))); }
#line 3706 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 177:
#line 747 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var(">=",     m((yylsp[-1]))); }
#line 3712 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 178:
#line 748 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("<=",     m((yylsp[-1]))); }
#line 3718 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 179:
#line 749 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("and",    m((yylsp[-1]))); }
#line 3724 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 180:
#line 750 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("or",     m((yylsp[-1]))); }
#line 3730 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 181:
#line 751 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("in",     m((yylsp[-1]))); }
#line 3736 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 182:
#line 752 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("not",    m((yylsp[-1]))); }
#line 3742 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 183:
#line 755 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assump(fncall(var("unsafeCast", m((yylsp[-1]))), list(mktunit(m((yylsp[-1])))), m((yylsp[-1]))), qualtype(tapp(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp)))))), m((yylsp[-1]))); }
#line 3748 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 184:
#line 757 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prules) = (yyvsp[-1].prules); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3754 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 185:
#line 758 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prules) = autorelease(new Grammar()); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3760 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 186:
#line 760 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prule) = autorelease(new Grammar::value_type(*(yyvsp[-2].string), *(yyvsp[0].prdefs))); }
#line 3766 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 187:
#line 762 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdefs) = (yyvsp[-2].prdefs); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3772 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 188:
#line 763 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdefs) = autorelease(new GrammarRules()); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3778 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 189:
#line 765 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdef) = autorelease(new GrammarRule(*(yyvsp[-3].pbelems), ExprPtr((yyvsp[-1].exp)))); }
#line 3784 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 190:
#line 767 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelems) = (yyvsp[-1].pbelems); (yyval.pbelems)->push_back(*(yyvsp[0].pbelem)); }
#line 3790 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 191:
#line 768 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelems) = autorelease(new BoundGrammarValues()); }
#line 3796 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 192:
#line 770 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue(*(yyvsp[-2].string), GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3802 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 193:
#line 771 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue("_", GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3808 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 194:
#line 773 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GSymRef(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3814 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 195:
#line 774 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GStr(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3820 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 196:
#line 775 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GStr(std::string(1, str::readCharDef(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3826 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 197:
#line 777 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3832 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 198:
#line 778 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3838 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 199:
#line 780 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = (yyvsp[-1].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3844 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 200:
#line 781 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3850 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 201:
#line 783 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexps) = (yyvsp[-1].patternexps); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3856 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 202:
#line 784 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexps) = autorelease(new PatternRows()); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3862 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 203:
#line 786 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)))); }
#line 3868 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 204:
#line 787 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-4].patterns), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)))); }
#line 3874 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 205:
#line 790 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[-1].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3880 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 206:
#line 791 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = autorelease(new Patterns()); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3886 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 207:
#line 793 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Bool((yyvsp[0].boolv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3892 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 208:
#line 794 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3898 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 209:
#line 795 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3904 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 210:
#line 796 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Short((yyvsp[0].shortv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3910 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 211:
#line 797 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int((yyvsp[0].intv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3916 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 212:
#line 798 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Long((yyvsp[0].longv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3922 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 213:
#line 799 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Double((yyvsp[0].doublev), m((yylsp[0])))), m((yylsp[0]))); }
#line 3928 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 214:
#line 800 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = mkpatarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3934 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 215:
#line 801 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = mkpatarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3940 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 216:
#line 802 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkTimespanPrim(*(yyvsp[0].strings), m((yylsp[0]))), mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3946 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 217:
#line 803 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 3952 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 218:
#line 804 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkDateTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 3958 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 219:
#line 805 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRegex(std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0]))); }
#line 3964 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 220:
#line 806 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 3970 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 221:
#line 807 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 3976 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 222:
#line 808 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-1].string), PatternPtr(new MatchLiteral(PrimitivePtr(new Unit(m((yylsp[-1])))), m((yylsp[-1])))), m((yylsp[-2]),(yylsp[0]))); }
#line 3982 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 223:
#line 809 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-3].string), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 3988 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 224:
#line 810 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(".f" + str::from((yyvsp[-3].intv)), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 3994 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 225:
#line 811 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4000 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 226:
#line 812 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4006 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 227:
#line 813 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4012 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 228:
#line 814 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4018 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 229:
#line 815 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = patVarCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4024 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 230:
#line 817 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchAny(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4030 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 231:
#line 818 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4036 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 232:
#line 819 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4042 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 233:
#line 820 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4048 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 234:
#line 821 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4054 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 235:
#line 823 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = (yyvsp[0].pattern); }
#line 4060 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 236:
#line 825 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[0].patterns); }
#line 4066 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 237:
#line 826 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = new Patterns(); }
#line 4072 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 238:
#line 828 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[-2].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4078 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 239:
#line 829 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = new Patterns(); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4084 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 240:
#line 831 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfields) = (yyvsp[-2].recpatfields); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4090 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 241:
#line 832 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfields) = new MatchRecord::Fields(); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4096 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 242:
#line 834 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfield) = new MatchRecord::Field(*(yyvsp[-2].string), PatternPtr((yyvsp[0].pattern))); }
#line 4102 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 243:
#line 836 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); }
#line 4108 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 244:
#line 837 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4114 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 245:
#line 838 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = (yyvsp[-4].rfields);                                     (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4120 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 246:
#line 840 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4126 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 247:
#line 841 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("data")); }
#line 4132 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 248:
#line 842 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("type")); }
#line 4138 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 249:
#line 843 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("where")); }
#line 4144 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 250:
#line 844 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("class")); wantIndent(false); }
#line 4150 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 251:
#line 845 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("instance")); wantIndent(false); }
#line 4156 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 252:
#line 846 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("exists")); }
#line 4162 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 253:
#line 847 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("import")); }
#line 4168 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 254:
#line 848 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("module")); }
#line 4174 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 255:
#line 849 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("parse")); }
#line 4180 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 256:
#line 850 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("do")); }
#line 4186 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 257:
#line 851 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("return")); }
#line 4192 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 258:
#line 852 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("fn")); }
#line 4198 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 259:
#line 853 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(".f" + str::from((yyvsp[0].intv)))); }
#line 4204 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 260:
#line 855 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-2].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4210 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 261:
#line 856 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings); str::seq x = tupSectionFields(*(yyvsp[0].string)); (yyval.strings)->insert((yyval.strings)->end(), x.begin(), x.end()); }
#line 4216 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 262:
#line 857 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4222 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 263:
#line 858 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); *(yyval.strings) = tupSectionFields(*(yyvsp[0].string)); }
#line 4228 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 264:
#line 860 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vfields) = autorelease(new Case::Bindings()); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4234 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 265:
#line 861 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vfields) = (yyvsp[-2].vfields); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4240 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 266:
#line 863 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-2].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4246 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 267:
#line 864 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-4].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4252 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 268:
#line 865 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(".f" + str::from((yyvsp[-4].intv)), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4258 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 269:
#line 867 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); }
#line 4264 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 270:
#line 868 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4270 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 271:
#line 869 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].exps)->push_back(ExprPtr((yyvsp[0].exp))); (yyval.exps) = (yyvsp[-2].exps); }
#line 4276 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 272:
#line 871 "hexpr.y" /* yacc.c:1646  */
    { (yyval.qualtype) = new QualType(*(yyvsp[-2].tconstraints), *(yyvsp[0].mtype)); }
#line 4282 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 273:
#line 872 "hexpr.y" /* yacc.c:1646  */
    { (yyval.qualtype) = new QualType(Constraints(), *(yyvsp[0].mtype)); }
#line 4288 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 274:
#line 875 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraints) = (yyvsp[-1].tconstraints); }
#line 4294 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 275:
#line 877 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraints) = autorelease(new Constraints()); (yyval.tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); }
#line 4300 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 276:
#line 878 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); (yyval.tconstraints) = (yyvsp[-2].tconstraints); }
#line 4306 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 277:
#line 880 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(*(yyvsp[-1].string), *(yyvsp[0].mtypes)); }
#line 4312 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 278:
#line 881 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(EqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4318 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 279:
#line 882 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(NotEqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4324 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 280:
#line 883 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(FixIsoRecur::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4330 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 281:
#line 884 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4336 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 282:
#line 885 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4342 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 283:
#line 886 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4348 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 284:
#line 887 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4354 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 285:
#line 888 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4360 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 286:
#line 889 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(1), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4366 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 287:
#line 891 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4372 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 288:
#line 892 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4378 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 289:
#line 893 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4384 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 290:
#line 894 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4390 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 291:
#line 896 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4396 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 292:
#line 897 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4402 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 293:
#line 898 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4408 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 294:
#line 899 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4414 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 295:
#line 901 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), TString::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype))); }
#line 4420 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 296:
#line 902 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), *(yyvsp[-5].mtype),                *(yyvsp[-3].mtype))); }
#line 4426 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 297:
#line 903 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(AppendsToUnqualifier::constraintName(), list(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4432 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 298:
#line 905 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4438 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 299:
#line 906 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-1].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4444 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 300:
#line 908 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4450 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 301:
#line 909 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4456 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 302:
#line 911 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Func::make(tuplety(*(yyvsp[-2].mtypes)), *(yyvsp[0].mtype)))); }
#line 4462 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 303:
#line 912 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeTupleType(*(yyvsp[0].mtypes)))); }
#line 4468 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 304:
#line 913 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeSumType(*(yyvsp[0].mtypes)))); }
#line 4474 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 305:
#line 915 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(monoTypeByName(*(yyvsp[0].string)))); }
#line 4480 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 306:
#line 916 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(OpaquePtr::make(str::replace<char>(*(yyvsp[-1].string), ".", "::"), 0, false))); }
#line 4486 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 307:
#line 917 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("[]"))); }
#line 4492 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 308:
#line 918 "hexpr.y" /* yacc.c:1646  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(Array::make(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4498 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 309:
#line 919 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(FixedArray::make(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype)))); }
#line 4504 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 310:
#line 920 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("->"))); }
#line 4510 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 311:
#line 921 "hexpr.y" /* yacc.c:1646  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(clone(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4516 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 312:
#line 922 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeRecType(*(yyvsp[-1].mreclist)))); }
#line 4522 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 313:
#line 923 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeVarType(*(yyvsp[-1].mvarlist)))); }
#line 4528 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 314:
#line 924 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("unit"))); }
#line 4534 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 315:
#line 925 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(((yyvsp[0].intv) == 0) ? Prim::make("void") : TLong::make((yyvsp[0].intv)))); }
#line 4540 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 316:
#line 926 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr((yyvsp[0].boolv) ? TLong::make(1) : TLong::make(0))); }
#line 4546 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 317:
#line 927 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Exists::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4552 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 318:
#line 928 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(Prim::make("fileref"), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))))); }
#line 4558 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 319:
#line 929 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(Prim::make("fileref"), list(*(yyvsp[-2].mtype))))); }
#line 4564 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 320:
#line 930 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Recursive::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4570 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 321:
#line 931 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TString::make(str::unescape(str::trimq(*(yyvsp[0].string)))))); }
#line 4576 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 322:
#line 932 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp))))))); }
#line 4582 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 323:
#line 934 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TVar::make(*(yyvsp[0].string)))); }
#line 4588 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 324:
#line 935 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TLong::make((yyvsp[0].intv)))); }
#line 4594 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 325:
#line 937 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4600 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 326:
#line 938 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[-2].string); *(yyval.string) += "."; *(yyval.string) += *(yyvsp[0].string); }
#line 4606 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 327:
#line 940 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4612 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 328:
#line 941 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-1].mtypes)->insert((yyvsp[-1].mtypes)->begin(), *(yyvsp[-3].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4618 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 329:
#line 943 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4624 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 330:
#line 944 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-2].mtypes); }
#line 4630 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 331:
#line 946 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4636 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 332:
#line 947 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4642 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 333:
#line 949 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[-2].mtype)); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4648 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 334:
#line 950 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4654 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 335:
#line 952 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mreclist) = (yyvsp[-4].mreclist);                                 (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4660 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 336:
#line 953 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mreclist) = autorelease(new Record::Members()); (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4666 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 337:
#line 955 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = (yyvsp[-4].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4672 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 338:
#line 956 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = (yyvsp[-2].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4678 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 339:
#line 957 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4684 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 340:
#line 958 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4690 "hexpr.parse.C" /* yacc.c:1646  */
    break;


#line 4694 "hexpr.parse.C" /* yacc.c:1646  */
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
#line 962 "hexpr.y" /* yacc.c:1906  */

#pragma GCC diagnostic pop


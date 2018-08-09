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
    TCOMPOSE = 337,
    TUPTO = 338,
    TCARET = 339,
    TAT = 340,
    TDOLLAR = 341,
    TQUESTION = 342,
    TSQUOTE = 343,
    TEQUOTE = 344
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

#line 521 "hexpr.parse.C" /* yacc.c:355  */
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

#line 550 "hexpr.parse.C" /* yacc.c:358  */

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
#define YYFINAL  67
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2748

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  91
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  76
/* YYNRULES -- Number of rules.  */
#define YYNRULES  339
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  751

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   345

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
      85,    86,    87,    88,    89,    90
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   488,   488,   489,   490,   491,   494,   495,   497,   498,
     499,   501,   502,   503,   504,   505,   507,   508,   509,   510,
     511,   512,   513,   514,   515,   516,   517,   518,   521,   524,
     527,   528,   531,   533,   536,   537,   538,   539,   540,   541,
     542,   543,   545,   546,   548,   550,   551,   553,   556,   557,
     558,   559,   561,   562,   564,   567,   569,   571,   572,   574,
     576,   578,   579,   580,   581,   582,   583,   584,   585,   586,
     587,   588,   589,   590,   591,   592,   593,   594,   595,   596,
     598,   599,   601,   602,   605,   606,   607,   608,   609,   610,
     611,   612,   614,   615,   617,   618,   619,   620,   621,   622,
     623,   624,   625,   627,   628,   629,   630,   631,   633,   634,
     635,   636,   638,   641,   642,   645,   648,   651,   663,   664,
     667,   669,   670,   672,   674,   675,   677,   678,   681,   682,
     685,   686,   687,   688,   689,   690,   691,   692,   693,   696,
     697,   698,   699,   700,   703,   704,   705,   708,   711,   712,
     715,   716,   717,   718,   719,   720,   721,   722,   723,   724,
     725,   726,   727,   730,   733,   734,   735,   736,   737,   738,
     739,   740,   741,   742,   743,   744,   745,   746,   747,   748,
     749,   750,   753,   755,   756,   758,   760,   761,   763,   765,
     766,   768,   769,   771,   772,   773,   775,   776,   778,   779,
     781,   782,   784,   785,   788,   789,   791,   792,   793,   794,
     795,   796,   797,   798,   799,   800,   801,   802,   803,   804,
     805,   806,   807,   808,   809,   810,   811,   812,   813,   815,
     816,   817,   818,   819,   821,   823,   824,   826,   827,   829,
     830,   832,   834,   835,   836,   838,   839,   840,   841,   842,
     843,   844,   845,   846,   847,   848,   849,   850,   852,   853,
     854,   855,   857,   858,   860,   861,   862,   864,   865,   866,
     868,   869,   872,   874,   875,   877,   878,   879,   880,   881,
     882,   883,   884,   885,   886,   888,   889,   890,   891,   893,
     894,   895,   896,   898,   899,   900,   902,   903,   905,   906,
     908,   909,   910,   912,   913,   914,   915,   916,   917,   918,
     919,   920,   921,   922,   923,   924,   925,   926,   927,   928,
     929,   931,   932,   934,   935,   937,   938,   940,   941,   943,
     944,   946,   947,   949,   950,   952,   953,   954,   955,   957
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
  "\"\\\\\"", "\"o\"", "\"..\"", "\"^\"", "\"@\"", "\"$\"", "\"?\"",
  "\"'\"", "\"`\"", "\"=~\"", "$accept", "s", "module", "defs", "def",
  "importdef", "tydef", "vartybind", "vardef", "classdef", "fundeps",
  "fundep", "cmembers", "cmember", "instdef", "imembers", "imember",
  "names", "nameseq", "name", "opname", "idseq", "types", "l0expr",
  "l1expr", "l2expr", "l3expr", "l4expr", "l5expr", "letbindings",
  "letbinding", "dobindings", "dobinding", "l6expr", "prules", "prule",
  "prdefs", "prdef", "pbelems", "pbelem", "pvalue", "tsseq", "l6exprs",
  "patternexps", "patternexp", "patterns", "refutablep", "irrefutablep",
  "pattern", "patternseq", "patternseqn", "recpatfields", "recpatfield",
  "recfields", "recfieldname", "recfieldpath", "varfields", "varbind",
  "cargs", "qtype", "cst", "tpreds", "tpred", "l1mtargl", "ltmtype",
  "l0mtype", "l1mtype", "tyind", "cppid", "l0mtargl", "l0mtarglt",
  "mtuplist", "msumlist", "mreclist", "mvarlist", "id", YY_NULLPTR
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
     345
};
# endif

#define YYPACT_NINF -569

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-569)))

#define YYTABLE_NINF -339

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     558,  1220,  2019,  2019,    63,    95,    95,    38,    38,    85,
      85,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,  -569,  -569,  -569,  1200,   103,  2019,   664,
      74,   664,    95,   106,  1486,  2019,  1200,    11,  2019,  -569,
    1304,  -569,  -569,  -569,  -569,  -569,  -569,   181,  -569,   166,
     208,   178,   249,  2399,  2247,  2019,  1639,  1072,   393,   177,
     548,   547,   560,  -569,   237,   393,  -569,  -569,   256,   254,
    -569,   775,   133,  -569,  -569,   140,  2491,   293,    38,   314,
    2538,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,  -569,  -569,  1072,    95,   247,  -569,   324,
    -569,   270,   231,    23,    95,   231,   336,  2095,   368,   440,
    2323,   445,   458,   483,  1200,   498,   531,   534,   540,   542,
     549,   550,   552,  2171,   554,   556,   564,  -569,  -569,   565,
     393,   -19,   257,   141,   461,   373,   555,    86,   219,  -569,
    2558,  2558,  1072,  2019,  1715,   178,  -569,  -569,  1200,  2019,
     255,  -569,  -569,   328,   368,   440,  2323,   445,   458,   483,
     498,   531,   534,   542,   549,   550,   552,   554,   556,   564,
    -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,
    -569,  -569,  1072,  1072,    95,   459,   208,  2670,  -569,  -569,
    -569,  2019,  2019,  2019,  2247,  2247,  2399,  2399,  2399,  2399,
    2399,  2399,  2399,  2399,  2399,  2399,  2399,  2475,  2475,  2475,
    2019,  1304,    95,  -569,  -569,  -569,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,
    2558,  -569,  2558,  -569,  -569,  -569,    95,    95,   481,   876,
    2576,  2576,    95,  2019,   107,  -569,   331,  2576,    95,    18,
      38,   775,    95,   481,    95,    95,   190,  -569,    42,   366,
     580,   600,  -569,  -569,   123,   561,   487,  -569,   607,  2019,
      98,  2247,   567,   582,   231,    -8,  -569,   624,   664,  1563,
    1200,   274,  1405,  -569,   625,   626,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,  -569,  -569,  -569,  2019,  1072,  1791,  -569,
    -569,   129,  2019,  2019,  2019,  -569,  -569,   987,  -569,   636,
    -569,  -569,  -569,   359,  2019,    27,  -569,   393,  2019,   315,
    2019,   388,   453,   490,   631,   179,  2019,  -569,   586,   586,
    -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,  -569,  -569,  -569,  -569,   393,  1304,  -569,
    -569,  -569,   645,   273,   597,  -569,  1033,  -569,    31,  2538,
    -569,  1104,   481,    44,   503,   662,   116,   473,    99,   651,
     227,  -569,  2491,   310,  2576,  2576,  1200,  2576,  2576,  2576,
    1262,  2576,   611,    38,   685,    95,    95,  2538,   623,   671,
     672,   694,  -569,  2576,  2576,  2576,  2576,  -569,   637,  1072,
    -569,    34,  1072,   393,  2019,  -569,  -569,   510,  1072,   582,
    -569,  -569,  -569,  -569,   154,  -569,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,  -569,  -569,  -569,  1563,  1867,  1200,   518,
     208,  -569,   607,  -569,  2019,  -569,  -569,  2019,   393,   675,
    -569,   343,  -569,   676,   393,   351,   361,   481,   211,  2538,
    -569,   402,  1943,  -569,   393,  2019,   319,   391,  -569,   640,
    -569,   639,  -569,    43,  1072,  1072,  -569,   393,  2576,  -569,
    -569,  -569,  -569,  2576,   638,  -569,  2576,  -569,    95,  2538,
    2576,  2538,  -569,    95,  2538,  2576,  -569,  -569,  2576,  2576,
    2576,    83,   109,   250,   611,   611,   611,  -569,   611,   611,
      28,    38,   685,  -569,    21,  -569,   284,  -569,  -569,   269,
    2538,  2538,  2538,    38,   694,  -569,   611,   611,   611,   611,
    -569,  -569,  -569,  -569,  -569,   393,   683,   496,  -569,   511,
    2652,  -569,   642,  -569,    41,   664,   681,   199,   417,   301,
    2019,  -569,  2019,  -569,  -569,  -569,  -569,  -569,   434,   393,
    2019,   323,  2019,  -569,  -569,  -569,   643,   644,   611,   163,
     526,   236,   686,  -569,    45,   398,   646,   693,   648,    49,
     611,   144,   146,   696,   -10,   697,  2576,  2576,  2576,  2576,
    2576,   685,    95,  -569,  -569,   685,    95,    95,  -569,   694,
    -569,   444,  -569,  -569,   695,  -569,    95,   674,   510,    95,
    2019,  2019,  2019,  -569,  -569,  -569,  2019,  -569,  -569,   703,
     231,  1867,  1867,  -569,  -569,  -569,   286,   393,  -569,   393,
    2019,   371,   393,  -569,  -569,   701,  -569,   707,  -569,   709,
    2538,  2576,   710,   706,  2538,   711,  2576,  2576,  2576,  2576,
    2576,  2576,   611,   611,   611,   611,   611,   685,    24,   685,
    -569,    95,   694,  -569,  2538,  2019,   713,  2019,  -569,   714,
     393,    62,   393,  -569,   429,   228,  -569,  2019,   393,  2019,
     375,  2576,   679,  2576,  -569,   307,  2576,  2576,  -569,  2576,
     403,   277,   367,   176,   425,    48,   685,  -569,   393,  2019,
     393,  2019,  2019,  -569,  -569,  -569,   464,   393,  2019,   392,
     611,  -569,   611,   715,   611,   611,   611,   719,  -569,  -569,
    2576,  -569,  2576,   685,   393,   393,   393,  -569,   393,  2019,
     404,  2576,  2576,   408,   437,   393,  2019,   435,   611,   611,
    -569,  -569,   393,  2019,   484,   393,  2019,   720,   393,  2019,
     393
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,     8,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   150,   151,   152,   153,   154,   155,   156,   157,   158,
     339,   159,   196,   161,   162,   261,     0,     0,     0,     0,
       0,     0,     0,     0,   267,   267,   242,     0,     0,     2,
       7,     9,    11,    12,    13,    14,    15,     0,    28,   112,
     160,   147,   129,     0,     0,     0,   267,     0,     4,    91,
      93,   102,   107,   111,   129,     5,   129,     1,     0,    29,
     323,     0,     0,    57,    59,     0,     0,     0,     0,     0,
       0,   253,   248,   252,   247,   246,   249,   250,   257,   251,
     254,   255,   256,   260,   245,   236,     0,     0,   122,     0,
     229,     0,   199,     0,     0,   148,     0,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    66,     0,
     268,     0,   268,     0,     0,     0,     0,     0,     0,    10,
       0,     0,     0,   267,     0,   146,   197,   259,     0,     0,
       0,   106,    85,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     206,   207,   208,   213,   209,   210,   211,   212,   214,   218,
     216,   217,   236,   236,     0,     0,   215,     0,   234,   205,
     228,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     8,     0,    74,    75,    76,    77,    78,    79,    64,
      68,    67,    65,    69,    70,    71,    72,    61,    62,    73,
       0,    58,     0,   314,   313,   319,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   273,     0,   303,     0,    38,
      55,    59,     0,     0,     0,     0,    48,    82,   329,     0,
     301,   302,   303,   238,     0,   235,     0,   240,     0,     0,
       0,     0,     0,     0,   198,     0,   184,     0,     0,   236,
     242,     0,     0,   125,     0,   129,   164,   165,   166,   167,
     168,   169,   172,   171,   170,   173,   174,   177,   175,   176,
     181,   178,   179,   180,    60,   163,     0,     0,     0,   134,
     144,     0,     0,     0,     0,   141,   182,     0,    32,     0,
     271,   120,   116,     0,     0,     0,   258,    16,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   204,    86,    87,
      88,    89,    90,    96,    95,    94,    97,    98,    99,   100,
     101,   105,   103,   104,   108,   109,   110,     3,     6,   324,
      30,    31,     0,     0,     0,   312,     0,   299,   329,     0,
     305,     0,     0,     0,     0,   303,     0,     0,   303,     0,
       0,   272,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   275,   296,     0,     0,     0,     0,     0,   299,     0,
     338,     0,    83,     0,     0,     0,     0,   230,     0,     0,
     232,     0,     0,   113,     0,   121,   123,     0,     0,   115,
     201,   117,   183,   190,     0,   150,   151,   152,   153,   154,
     155,   156,   158,   159,   161,   162,   236,   236,   242,     0,
     160,   129,     0,   127,     0,   118,   124,     0,   269,     0,
     131,     0,   145,     0,   243,     0,     0,     0,   329,     0,
     128,     0,     0,   135,    17,     0,     0,     0,   224,     0,
     219,     0,   226,     0,     0,     0,   221,    84,     0,   304,
     308,   309,   298,     0,     0,   306,     0,   310,     0,     0,
       0,     0,   311,     0,     0,     0,   320,   274,     0,     0,
       0,     0,     0,     0,   276,   278,   277,   317,   316,   297,
      34,     0,    40,    45,    39,    42,     0,    80,    56,    49,
       0,     0,     0,     0,    50,    52,   331,   300,   330,   332,
     231,   237,   233,   239,   241,   114,     0,     0,   262,     0,
       0,   200,   185,   187,     0,     0,     0,     0,     0,     0,
       0,   130,     0,   140,   139,   270,   138,   137,     0,    18,
       0,     0,     0,   225,   220,   227,     0,     0,   315,     0,
       0,     0,     0,   334,   329,     0,     0,   336,   337,   329,
     318,     0,     0,   303,     0,   303,     0,     0,     0,     0,
       0,     0,     0,    47,    46,     0,     0,     0,    81,     0,
     327,     0,   337,    54,     0,    53,     0,   142,     0,     0,
       0,     0,     0,   190,   195,   194,     0,   189,   192,   193,
     149,     0,     0,   141,   119,   126,     0,   244,   136,    19,
       0,     0,    92,   223,   222,     0,   322,     0,   321,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   295,   288,   287,   286,   285,    36,    35,    41,
      43,    44,    51,   326,     0,     0,     0,     0,   263,     0,
     264,     0,   202,   186,     0,     0,   132,     0,    20,     0,
       0,     0,     0,     0,   333,     0,     0,     0,   335,     0,
     331,     0,     0,     0,     0,     0,     0,   328,    33,     0,
     143,     0,     0,   188,   191,   193,     0,    21,     0,     0,
     284,   307,   282,     0,   290,   294,   293,     0,   281,   279,
       0,   289,     0,    37,   266,   265,   203,   133,    22,     0,
       0,     0,     0,     0,     0,    23,     0,     0,   283,   292,
     280,   291,    24,     0,     0,    25,     0,     0,    26,     0,
      27
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -569,  -569,  -569,   546,   -28,  -569,  -569,   248,  -569,  -569,
     169,   162,  -568,  -498,  -569,   165,  -502,  -372,   581,    -4,
     516,   171,   365,    -2,   -41,   455,   -33,   409,     8,  -569,
     499,  -569,   488,   -24,  -569,   497,  -569,   158,  -569,  -569,
     100,    50,  -569,  -569,   354,   356,  -569,   -89,   -38,  -166,
    -569,  -173,  -379,  -569,   -16,   -47,  -569,   180,   -29,  -117,
     602,  -569,   395,  -569,   553,   -72,   767,  -569,   557,  -569,
    -569,  -569,  -569,  -569,  -569,   394
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,    39,    40,    41,    42,    43,    44,   603,    45,
     514,   515,   512,   513,    46,   524,   525,   249,   250,    47,
     129,   516,   256,   130,    59,    60,    61,    62,    63,    97,
      98,   282,   283,    49,   275,   276,   542,   543,   544,   617,
     618,    50,   103,   419,   420,   187,   188,    99,   263,   264,
     265,   266,   267,   134,   135,    51,   537,   538,   131,   318,
     319,   244,   245,   391,   366,   320,   258,   637,    69,   259,
     601,   260,   261,   374,   377,    66
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      58,    65,   145,    73,    73,   102,   133,   105,   257,    48,
      93,   333,   139,   152,   594,    20,   331,   332,   284,   189,
     151,   510,   605,   657,   321,   394,   101,   659,   595,   650,
     136,   696,   533,   132,    20,   591,   138,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    48,    22,
      23,    24,    25,   153,   305,   145,   614,    20,   145,   462,
     306,    20,  -325,    67,    20,   615,    20,    26,   231,   421,
     403,   231,   483,  -325,    73,   390,  -325,   151,    28,   274,
    -325,   403,   152,   273,   403,   486,   641,   722,   646,   191,
     192,    31,    32,   702,   533,    56,   395,    35,    93,    36,
     596,    37,   463,   596,   322,   281,   592,   186,    20,   193,
      71,   532,    38,   360,   323,   361,   390,   616,    20,   586,
     565,    20,   314,   151,   191,   192,    20,   390,   723,   390,
     390,   494,   326,   390,   390,    81,    82,    83,    84,    85,
      86,    87,   325,   587,   193,   186,   588,   327,    88,   337,
     104,   604,    20,   341,   342,   490,    20,    76,   491,   594,
     605,   594,    89,    20,   315,   414,   367,   367,   390,   230,
      95,   351,   352,   353,    96,    95,   232,  -338,  -338,    96,
     381,   398,   107,    25,   402,   647,   382,   648,    90,   338,
     339,   340,   186,   284,   390,    25,   407,   401,    26,    91,
      92,   390,   408,   141,   233,    71,   452,   147,   357,   234,
      26,   194,    71,    20,   235,   475,   309,   720,   140,    48,
     306,   545,   148,   236,   142,   594,   143,   145,   144,   390,
     416,   390,   186,   186,   146,   622,   635,   186,   143,   237,
     144,   380,  -325,   614,   195,   398,   231,   383,   390,   384,
     403,    20,   615,   385,   424,   386,   387,   476,   388,   389,
      25,   390,   253,   211,   239,   333,   254,   413,   255,   449,
     331,   332,    20,   210,   242,    26,   599,   623,    20,   243,
     281,   191,   192,   233,   589,   149,   -59,   590,   234,   191,
     192,   328,    20,   235,   482,   453,   390,   484,   212,   482,
     367,   193,   236,   143,   448,   144,   451,    20,   316,   193,
     454,   455,   456,   639,   269,   597,   496,   212,   237,   191,
     192,   390,   461,   248,   479,   257,   464,   270,   467,   440,
     139,   272,   191,   192,   477,   307,   191,   192,    20,   193,
     308,   253,    20,   239,   252,   254,    20,   255,   191,   192,
     718,   465,   193,   242,   443,   560,   193,   186,   243,   630,
     271,   676,   390,   191,   192,   677,    48,   383,   193,   384,
     503,   531,   278,   385,   534,   386,   387,   145,   388,   389,
     189,   625,   498,   193,   713,   398,   499,   555,   500,    73,
     191,   192,   390,   330,    20,    52,    64,   404,    20,    68,
      70,    74,    74,    78,    80,   191,   192,   679,   133,   312,
     193,   708,   535,   191,   192,    20,   390,   573,   551,   576,
      94,   100,   578,   191,   192,   193,   106,    20,   729,   553,
      94,   137,   460,   193,    52,   132,   566,   567,   306,   554,
     736,   286,   548,   193,   719,   549,   150,   402,   600,   573,
     602,   190,   390,   191,   192,   191,   192,   562,    20,   186,
     558,   468,   186,   559,   191,   192,    74,   469,   186,    74,
     247,   743,   251,   193,   262,   193,   642,   556,   334,   191,
     192,   717,    20,   390,   193,   740,   440,   440,   390,   190,
     268,   191,   192,   390,   624,   233,   191,   192,   277,   193,
     234,   285,   337,   721,    20,   235,   703,    20,    94,   628,
     390,   193,   364,   287,   236,   741,   193,   663,   289,    73,
     746,   620,   390,   664,   186,   186,   191,   192,   470,   536,
     237,   290,   471,    20,   262,   262,   190,   546,   310,   727,
     311,    20,    94,   609,   329,   636,   193,   610,   626,    20,
     627,   492,   493,   253,   365,   239,   291,   254,   629,   255,
     632,     1,     2,     3,   410,   242,   411,   472,   684,   473,
     243,   292,   688,   145,   607,   608,   190,   190,   268,   335,
     487,   190,   488,   566,   567,   204,   205,   206,    72,    75,
     186,   313,   697,   196,   197,   198,   199,   200,   201,   202,
     203,   207,   208,   209,   293,    52,   359,   294,   670,   671,
     672,    77,    79,   295,   674,   296,   354,   355,   356,   455,
     456,   405,   297,   298,   262,   299,   262,   301,   678,   302,
     362,    70,   262,   262,   375,   378,   379,   303,   304,   406,
     409,   262,   393,   412,    74,   417,   397,   262,   399,   400,
     262,   343,   344,   345,   346,   347,   348,   349,   350,   423,
     418,   447,  -229,   698,   100,   700,   459,   474,   193,   277,
     480,   440,   440,   441,   442,   706,   285,   707,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,   478,
      22,    23,    24,    25,   489,   495,   390,   724,   511,   725,
     726,   190,   520,   521,   522,    94,   728,   523,    26,   550,
     530,   247,   552,   563,   564,   606,   570,   621,   640,    28,
     613,   633,   634,   466,   643,   644,   645,   735,   649,   651,
     667,   665,    31,    32,   742,   675,    56,   681,    35,   682,
      36,   745,    37,   687,   748,   683,   686,   750,   689,   699,
     701,   731,    52,    38,   711,   732,   749,   358,   660,   593,
     262,   658,   519,   262,   662,   262,   262,   396,   661,   415,
     446,   673,   422,   541,   540,   704,   247,   497,   262,   262,
      94,   262,   262,   262,   262,   262,     0,   251,   668,   517,
     518,   262,   371,     0,   363,     0,     0,   262,   262,   262,
     262,     0,     0,   190,     0,   268,   190,     0,     0,     0,
       0,   539,   190,   213,   214,   215,   216,   217,   218,   219,
     220,   221,   222,     0,   223,   224,   225,   226,     0,     0,
     441,   441,   442,   547,     0,     0,     0,   227,   228,     0,
       0,     0,   229,   246,     0,     0,     0,     0,     0,     0,
       0,   262,     0,   262,     0,     0,     0,   127,     0,     0,
     561,     0,     0,     0,     0,   128,     0,   268,   190,   190,
       0,     0,   262,     0,     0,     0,     0,   262,     0,     0,
     262,     0,   572,   262,   262,   262,     0,   577,   262,   262,
     233,     0,   262,   583,   585,   234,     0,     0,     0,    20,
     235,     0,     0,     0,     0,    74,     0,     0,   369,   236,
     598,     0,     0,   262,   262,   262,   262,   251,     0,     0,
       0,     0,     0,     0,     0,   237,     0,     0,     0,     0,
       0,     0,     0,     0,   190,     0,     0,     0,   619,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   253,     0,
     239,   370,   254,     0,   255,   631,     0,     0,     0,     0,
     242,     0,     0,     0,   638,   243,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     262,   262,   262,   262,   262,     0,   517,     0,     0,     0,
     517,   517,     0,     0,     0,     0,     0,     0,     0,     0,
     666,   233,   539,   669,     0,   368,   234,   373,   376,     0,
      20,   235,     0,     0,   392,   441,   441,     0,   364,     0,
     236,     0,     0,     0,     0,   680,     0,     0,     0,     0,
       0,     0,     0,     0,   262,   262,   237,     0,   262,     0,
     262,   262,   262,   262,   262,   262,     0,   233,     0,     0,
       0,     0,   234,     0,     0,   598,    20,   235,   262,   457,
     365,   239,     0,   240,     0,   241,   236,     0,     0,   705,
       0,   242,     0,     0,   709,   262,   243,   262,     0,     0,
     262,   262,   237,   262,   458,     0,   170,   171,   172,   173,
     174,   175,   176,     0,   177,    20,   178,   179,    22,   180,
     181,     0,     0,   730,     0,   253,   481,   239,     0,   254,
       0,   255,     0,     0,   262,     0,   262,   242,   233,     0,
       0,     0,   243,   234,   737,   262,   262,    20,   235,     0,
       0,   744,     0,     0,     0,     0,     0,   236,   747,     0,
       0,     0,     0,     0,   182,     0,   183,     0,   184,   246,
     185,   501,   502,   237,   504,   505,   506,   508,   509,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     526,   527,   528,   529,     0,     0,   253,     0,   239,   485,
     254,     0,   255,     0,     0,     0,     0,     0,   242,     0,
       0,     0,     0,   243,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    81,    82,    83,    84,
      85,    86,    87,     0,     0,     0,     0,     0,     0,    88,
       0,     0,     0,    20,   368,     0,     5,     0,     6,     7,
       8,     9,    10,    89,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,   568,    22,    23,    24,    25,
     569,     0,     0,   571,     0,     0,   574,   575,     0,    90,
       0,   579,   580,     0,    26,   581,   582,   584,     0,     0,
      91,    92,     0,     0,    27,    28,   233,    29,     0,    30,
       0,   234,     0,     0,     0,    20,   235,     0,    31,    32,
      33,     0,    34,     0,    35,   236,    36,     0,    37,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    38,
       0,   237,     6,     7,     8,     9,    10,     0,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,     0,
      22,    23,    24,    25,   372,     0,   239,     0,   254,     0,
     255,     0,     0,     0,     0,     0,   242,     0,    26,   507,
       0,   243,     0,   652,   653,   654,   655,   656,    27,    28,
       0,    29,     0,    30,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,     0,    34,     0,    35,     0,
      36,     0,    37,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    38,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   685,     0,
       0,     0,     0,   690,   691,   692,   693,   694,   695,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
       0,    22,    23,    24,    25,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    53,     0,     0,   710,    26,
     712,     0,     0,   714,   715,     0,   716,     0,    54,    27,
      28,     0,    29,     0,    30,     0,     0,     0,     0,    55,
       0,     0,     0,    31,    32,    33,   444,   279,     0,    35,
       0,   280,   445,    37,     0,     0,    57,   733,     0,   734,
       0,     0,     0,     0,    38,     0,     0,     0,   738,   739,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,     0,    22,    23,    24,    25,     0,     0,     0,     0,
       0,     0,     0,     0,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,   122,   123,
      27,    28,     0,    29,     0,    30,     0,     0,   124,   125,
      55,     0,     0,   126,    31,    32,    33,     0,    56,     0,
      35,     0,    36,     0,    37,     0,     0,    57,   127,     0,
       0,     0,     0,     0,     0,    38,   128,   425,   426,   427,
     428,   429,   430,   431,    18,   432,    20,   433,   179,    22,
     434,   435,    25,     0,     0,     0,     0,     0,     0,     0,
       0,   154,   155,   156,   157,   158,   159,    26,   160,   161,
     162,   118,   163,   164,   165,   166,   123,    27,    28,     0,
      29,     0,    30,     0,     0,   167,   168,    55,     0,     0,
     169,    31,    32,    33,     0,   436,     0,   437,     0,   438,
       0,   439,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,    38,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,     0,    22,    23,    24,    25,     0,
       0,     0,     0,     0,     0,     0,     0,   154,   155,   156,
     157,   158,   159,    26,   160,   161,   162,   118,   163,   164,
     165,   166,   123,    27,    28,     0,    29,     0,    30,     0,
       0,   167,   168,    55,     0,     0,   169,    31,    32,    33,
       0,    56,     0,    35,     0,    36,     0,    37,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,    38,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
       0,    22,    23,    24,    25,     0,     0,   324,     0,     0,
       0,     0,     0,     0,     0,    53,     0,     0,     0,    26,
       0,     0,     0,     0,     0,     0,     0,     0,    54,    27,
      28,     0,    29,     0,    30,     0,     0,     0,     0,    55,
       0,     0,     0,    31,    32,    33,     0,    56,     0,    35,
       0,    36,     0,    37,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,    38,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,     0,    22,    23,    24,
      25,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    53,     0,     0,     0,    26,     0,     0,     0,     0,
       0,     0,     0,     0,    54,    27,    28,     0,    29,     0,
      30,     0,     0,     0,     0,    55,     0,     0,     0,    31,
      32,    33,     0,    56,     0,    35,   450,    36,     0,    37,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
      38,   425,   426,   427,   428,   429,   430,   431,    18,   432,
      20,   433,   179,    22,   434,   435,    25,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    53,     0,     0,
       0,    26,     0,     0,     0,     0,     0,     0,     0,     0,
      54,    27,    28,     0,    29,     0,    30,     0,     0,     0,
       0,    55,     0,     0,     0,    31,    32,    33,     0,   436,
       0,   437,     0,   438,     0,   439,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,    38,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,     0,    22,
      23,    24,    25,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    53,     0,     0,     0,    26,     0,     0,
       0,     0,     0,     0,     0,     0,    54,    27,    28,     0,
      29,     0,    30,     0,     0,     0,     0,    55,     0,     0,
       0,    31,    32,    33,     0,    56,     0,    35,   557,    36,
       0,    37,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,    38,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,     0,    22,    23,    24,    25,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    53,
       0,     0,     0,    26,     0,     0,     0,     0,     0,     0,
       0,     0,    54,    27,    28,     0,    29,     0,    30,     0,
       0,     0,     0,    55,     0,     0,     0,    31,    32,    33,
       0,    56,     0,    35,     0,    36,     0,    37,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,    38,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
       0,    22,    23,    24,    25,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    53,     0,     0,     0,    26,
       0,     0,     0,     0,     0,     0,     0,     0,    54,    27,
      28,     0,    29,     0,    30,     0,     0,     0,     0,    55,
       0,     0,     0,    31,    32,    33,     0,   279,     0,    35,
       0,   280,     0,    37,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,    38,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,     0,    22,    23,    24,
      25,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    53,     0,     0,     0,    26,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    27,    28,     0,    29,     0,
      30,     0,     0,     0,     0,    55,     0,     0,     0,    31,
      32,    33,     0,    56,   300,    35,     0,    36,     0,    37,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      38,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,     0,    22,    23,    24,    25,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    53,     0,     0,
       0,    26,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    27,    28,     0,    29,     0,    30,     0,     0,     0,
       0,    55,     0,     0,     0,    31,    32,    33,     0,    56,
       0,    35,     0,    36,     0,    37,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    38,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,     0,    22,
      23,    24,    25,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    53,     0,     0,     0,    26,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    27,    28,     0,
      29,     0,    30,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,     0,    56,   288,    35,     0,    36,
       0,    37,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    38,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,     0,    22,    23,    24,    25,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    53,
       0,     0,     0,    26,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    27,    28,     0,    29,     0,    30,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
       0,    56,     0,    35,     0,    36,     0,    37,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    38,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
       0,    22,    23,    24,    25,   233,     0,     0,     0,     0,
     234,     0,     0,     0,    20,   235,     0,     0,     0,    26,
       0,     0,     0,     0,   236,     0,     0,     0,     0,    27,
      28,     0,    29,     0,    30,     0,     0,     0,     0,     0,
     237,     0,     0,    31,    32,    33,     0,    56,     0,    35,
       0,    36,   233,    37,     0,     0,     0,   234,     0,     0,
       0,    20,   235,   238,    38,   239,     0,   240,     0,   241,
       0,   236,   233,     0,     0,   242,     0,   234,     0,     0,
     243,    20,   235,     0,     0,     0,     0,   237,     0,     0,
     233,   236,     0,     0,     0,   234,     0,     0,     0,    20,
     235,     0,     0,     0,     0,     0,     0,   237,     0,   236,
     253,     0,   239,     0,   254,     0,   255,     0,     0,     0,
       0,     0,   242,     0,     0,   237,     0,   243,     0,     0,
     317,     0,   239,     0,   254,     0,   255,     0,     0,     0,
       0,     0,   242,     0,     0,     0,     0,   243,   372,     0,
     239,     0,   254,     0,   255,     0,     0,     0,     0,   611,
     242,     0,     0,     0,     0,   243,   170,   171,   172,   173,
     174,   175,   176,     0,   177,    20,   178,   179,    22,   180,
     181,     0,     0,   612,   170,   171,   172,   173,   174,   175,
     176,     0,   177,    20,   178,   179,    22,   180,   181,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   336,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   182,     0,   183,     0,   184,     0,
     185,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   182,     0,   183,     0,   184,     0,   185
};

static const yytype_int16 yycheck[] =
{
       2,     3,    49,     7,     8,    29,    35,    31,    80,     1,
      26,   184,    40,    54,   512,    23,   182,   183,   107,    57,
      53,   393,   524,   591,   141,     7,    28,   595,     7,    39,
      19,     7,   411,    35,    23,     7,    38,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    40,    26,
      27,    28,    29,    55,    73,   102,    15,    23,   105,    32,
      79,    23,    31,     0,    23,    24,    23,    44,    72,    77,
      39,    75,    41,    31,    78,    85,    31,   110,    55,   103,
      31,    39,   123,    60,    39,    41,    41,    39,    39,    62,
      63,    68,    69,    31,   473,    72,    78,    74,   114,    76,
      79,    78,    75,    79,   142,   107,    78,    57,    23,    82,
      72,    77,    89,   230,   143,   232,    85,    76,    23,    36,
      77,    23,    36,   156,    62,    63,    23,    85,   696,    85,
      85,    32,   148,    85,    85,     6,     7,     8,     9,    10,
      11,    12,   144,    34,    82,    95,    37,   149,    19,   187,
      76,   523,    23,   194,   195,    39,    23,    72,    42,   657,
     662,   659,    33,    23,    78,    67,   238,   239,    85,    36,
      72,   204,   205,   206,    76,    72,    36,    78,    79,    76,
      73,   253,    76,    29,   256,    41,    79,    41,    59,   191,
     192,   193,   142,   282,    85,    29,    73,     7,    44,    70,
      71,    85,    79,    37,    14,    72,    77,    29,   210,    19,
      44,    34,    72,    23,    24,    36,    75,    41,    37,   211,
      79,    67,    44,    33,    58,   723,    72,   274,    74,    85,
     271,    85,   182,   183,    26,    36,    73,   187,    72,    49,
      74,   243,    31,    15,    67,   317,   250,    36,    85,    38,
      39,    23,    24,    42,   278,    44,    45,    78,    47,    48,
      29,    85,    72,     7,    74,   438,    76,   269,    78,   307,
     436,   437,    23,    36,    84,    44,     7,    78,    23,    89,
     282,    62,    63,    14,    34,    36,    37,    37,    19,    62,
      63,    36,    23,    24,   366,   311,    85,   369,    44,   371,
     372,    82,    33,    72,   306,    74,   308,    23,    89,    82,
     312,   313,   314,    77,    67,    31,    89,    44,    49,    62,
      63,    85,   324,    30,    51,   397,   328,    80,   330,   279,
     358,    61,    62,    63,   336,    78,    62,    63,    23,    82,
      83,    72,    23,    74,    30,    76,    23,    78,    62,    63,
      73,    36,    82,    84,    80,    36,    82,   307,    89,    36,
      36,    75,    85,    62,    63,    79,   358,    36,    82,    38,
     386,   409,    36,    42,   412,    44,    45,   424,    47,    48,
     418,    80,    72,    82,    77,   457,    76,   459,    78,   393,
      62,    63,    85,    65,    23,     1,     2,    31,    23,     5,
       6,     7,     8,     9,    10,    62,    63,    36,   437,    36,
      82,    36,   414,    62,    63,    23,    85,   489,    75,   491,
      26,    27,   494,    62,    63,    82,    32,    23,    36,    78,
      36,    37,    73,    82,    40,   437,   474,   475,    79,    78,
      36,    73,   444,    82,    77,   447,    52,   519,   520,   521,
     522,    57,    85,    62,    63,    62,    63,    66,    23,   409,
     462,    73,   412,   465,    62,    63,    72,    79,   418,    75,
      76,    36,    78,    82,    80,    82,    78,    75,    19,    62,
      63,    78,    23,    85,    82,    77,   436,   437,    85,    95,
      96,    62,    63,    85,    77,    14,    62,    63,   104,    82,
      19,   107,   540,    78,    23,    24,    77,    23,   114,    75,
      85,    82,    31,    73,    33,    78,    82,    73,    73,   523,
      36,   545,    85,    79,   474,   475,    62,    63,    75,    19,
      49,    73,    79,    23,   140,   141,   142,    19,    77,    75,
      79,    23,   148,    32,   150,    19,    82,    36,   550,    23,
     552,    78,    79,    72,    73,    74,    73,    76,   560,    78,
     562,     3,     4,     5,    77,    84,    79,    77,   640,    79,
      89,    73,   644,   620,    78,    79,   182,   183,   184,   185,
      77,   187,    79,   621,   622,    38,    39,    40,     7,     8,
     540,    36,   664,    45,    46,    47,    48,    49,    50,    51,
      52,    41,    42,    43,    73,   211,   212,    73,   610,   611,
     612,     9,    10,    73,   616,    73,   207,   208,   209,   621,
     622,    41,    73,    73,   230,    73,   232,    73,   630,    73,
     236,   237,   238,   239,   240,   241,   242,    73,    73,    39,
      79,   247,   248,    36,   250,    78,   252,   253,   254,   255,
     256,   196,   197,   198,   199,   200,   201,   202,   203,    35,
      78,    36,    36,   665,   270,   667,    30,    36,    82,   275,
      73,   621,   622,   279,   280,   677,   282,   679,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    44,
      26,    27,    28,    29,    32,    44,    85,   699,    13,   701,
     702,   307,    79,    32,    32,   311,   708,    13,    44,    34,
      73,   317,    36,    73,    75,    32,    78,    36,    32,    55,
      78,    78,    78,   329,    78,    32,    78,   729,    32,    32,
      56,    36,    68,    69,   736,    32,    72,    36,    74,    32,
      76,   743,    78,    37,   746,    36,    36,   749,    37,    36,
      36,    36,   358,    89,    75,    36,    36,   211,   596,   511,
     366,   592,   397,   369,   599,   371,   372,   251,   597,   270,
     282,   613,   275,   419,   418,   675,   382,   382,   384,   385,
     386,   387,   388,   389,   390,   391,    -1,   393,   608,   395,
     396,   397,   239,    -1,   237,    -1,    -1,   403,   404,   405,
     406,    -1,    -1,   409,    -1,   411,   412,    -1,    -1,    -1,
      -1,   417,   418,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    -1,    49,    50,    51,    52,    -1,    -1,
     436,   437,   438,   439,    -1,    -1,    -1,    62,    63,    -1,
      -1,    -1,    67,    76,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   457,    -1,   459,    -1,    -1,    -1,    82,    -1,    -1,
     466,    -1,    -1,    -1,    -1,    90,    -1,   473,   474,   475,
      -1,    -1,   478,    -1,    -1,    -1,    -1,   483,    -1,    -1,
     486,    -1,   488,   489,   490,   491,    -1,   493,   494,   495,
      14,    -1,   498,   499,   500,    19,    -1,    -1,    -1,    23,
      24,    -1,    -1,    -1,    -1,   511,    -1,    -1,    32,    33,
     516,    -1,    -1,   519,   520,   521,   522,   523,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   540,    -1,    -1,    -1,   544,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      74,    75,    76,    -1,    78,   561,    -1,    -1,    -1,    -1,
      84,    -1,    -1,    -1,   570,    89,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     586,   587,   588,   589,   590,    -1,   592,    -1,    -1,    -1,
     596,   597,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     606,    14,   608,   609,    -1,   238,    19,   240,   241,    -1,
      23,    24,    -1,    -1,   247,   621,   622,    -1,    31,    -1,
      33,    -1,    -1,    -1,    -1,   631,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   640,   641,    49,    -1,   644,    -1,
     646,   647,   648,   649,   650,   651,    -1,    14,    -1,    -1,
      -1,    -1,    19,    -1,    -1,   661,    23,    24,   664,    72,
      73,    74,    -1,    76,    -1,    78,    33,    -1,    -1,   675,
      -1,    84,    -1,    -1,   680,   681,    89,   683,    -1,    -1,
     686,   687,    49,   689,   317,    -1,    14,    15,    16,    17,
      18,    19,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    -1,    -1,   709,    -1,    72,    73,    74,    -1,    76,
      -1,    78,    -1,    -1,   720,    -1,   722,    84,    14,    -1,
      -1,    -1,    89,    19,   730,   731,   732,    23,    24,    -1,
      -1,   737,    -1,    -1,    -1,    -1,    -1,    33,   744,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    74,    -1,    76,   382,
      78,   384,   385,    49,   387,   388,   389,   390,   391,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     403,   404,   405,   406,    -1,    -1,    72,    -1,    74,    75,
      76,    -1,    78,    -1,    -1,    -1,    -1,    -1,    84,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      -1,    -1,    -1,    23,   457,    -1,     6,    -1,     8,     9,
      10,    11,    12,    33,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,   478,    26,    27,    28,    29,
     483,    -1,    -1,   486,    -1,    -1,   489,   490,    -1,    59,
      -1,   494,   495,    -1,    44,   498,   499,   500,    -1,    -1,
      70,    71,    -1,    -1,    54,    55,    14,    57,    -1,    59,
      -1,    19,    -1,    -1,    -1,    23,    24,    -1,    68,    69,
      70,    -1,    72,    -1,    74,    33,    76,    -1,    78,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      -1,    49,     8,     9,    10,    11,    12,    -1,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    -1,
      26,    27,    28,    29,    72,    -1,    74,    -1,    76,    -1,
      78,    -1,    -1,    -1,    -1,    -1,    84,    -1,    44,    87,
      -1,    89,    -1,   586,   587,   588,   589,   590,    54,    55,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    68,    69,    70,    -1,    72,    -1,    74,    -1,
      76,    -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   641,    -1,
      -1,    -1,    -1,   646,   647,   648,   649,   650,   651,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      -1,    26,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    40,    -1,    -1,   681,    44,
     683,    -1,    -1,   686,   687,    -1,   689,    -1,    53,    54,
      55,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      -1,    -1,    -1,    68,    69,    70,    71,    72,    -1,    74,
      -1,    76,    77,    78,    -1,    -1,    81,   720,    -1,   722,
      -1,    -1,    -1,    -1,    89,    -1,    -1,    -1,   731,   732,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    -1,    26,    27,    28,    29,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    -1,    57,    -1,    59,    -1,    -1,    62,    63,
      64,    -1,    -1,    67,    68,    69,    70,    -1,    72,    -1,
      74,    -1,    76,    -1,    78,    -1,    -1,    81,    82,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    90,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    -1,
      57,    -1,    59,    -1,    -1,    62,    63,    64,    -1,    -1,
      67,    68,    69,    70,    -1,    72,    -1,    74,    -1,    76,
      -1,    78,    -1,    -1,    81,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    -1,    26,    27,    28,    29,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    -1,    57,    -1,    59,    -1,
      -1,    62,    63,    64,    -1,    -1,    67,    68,    69,    70,
      -1,    72,    -1,    74,    -1,    76,    -1,    78,    -1,    -1,
      81,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      -1,    26,    27,    28,    29,    -1,    -1,    32,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    40,    -1,    -1,    -1,    44,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,    54,
      55,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      -1,    -1,    -1,    68,    69,    70,    -1,    72,    -1,    74,
      -1,    76,    -1,    78,    -1,    -1,    81,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    -1,    26,    27,    28,
      29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    53,    54,    55,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    68,
      69,    70,    -1,    72,    -1,    74,    75,    76,    -1,    78,
      -1,    -1,    81,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,    -1,    -1,
      -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      53,    54,    55,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    -1,    -1,    -1,    68,    69,    70,    -1,    72,
      -1,    74,    -1,    76,    -1,    78,    -1,    -1,    81,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    -1,    26,
      27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    40,    -1,    -1,    -1,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    53,    54,    55,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    -1,    -1,
      -1,    68,    69,    70,    -1,    72,    -1,    74,    75,    76,
      -1,    78,    -1,    -1,    81,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    -1,    26,    27,    28,    29,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,
      -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    53,    54,    55,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    64,    -1,    -1,    -1,    68,    69,    70,
      -1,    72,    -1,    74,    -1,    76,    -1,    78,    -1,    -1,
      81,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      -1,    26,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    40,    -1,    -1,    -1,    44,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,    54,
      55,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,
      -1,    -1,    -1,    68,    69,    70,    -1,    72,    -1,    74,
      -1,    76,    -1,    78,    -1,    -1,    81,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    -1,    26,    27,    28,
      29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    54,    55,    -1,    57,    -1,
      59,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    68,
      69,    70,    -1,    72,    73,    74,    -1,    76,    -1,    78,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    -1,    26,    27,    28,    29,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,    -1,    -1,
      -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    54,    55,    -1,    57,    -1,    59,    -1,    -1,    -1,
      -1,    64,    -1,    -1,    -1,    68,    69,    70,    -1,    72,
      -1,    74,    -1,    76,    -1,    78,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    -1,    26,
      27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    40,    -1,    -1,    -1,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    54,    55,    -1,
      57,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    68,    69,    70,    -1,    72,    73,    74,    -1,    76,
      -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    89,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    -1,    26,    27,    28,    29,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,
      -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    54,    55,    -1,    57,    -1,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,    69,    70,
      -1,    72,    -1,    74,    -1,    76,    -1,    78,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      -1,    26,    27,    28,    29,    14,    -1,    -1,    -1,    -1,
      19,    -1,    -1,    -1,    23,    24,    -1,    -1,    -1,    44,
      -1,    -1,    -1,    -1,    33,    -1,    -1,    -1,    -1,    54,
      55,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    68,    69,    70,    -1,    72,    -1,    74,
      -1,    76,    14,    78,    -1,    -1,    -1,    19,    -1,    -1,
      -1,    23,    24,    72,    89,    74,    -1,    76,    -1,    78,
      -1,    33,    14,    -1,    -1,    84,    -1,    19,    -1,    -1,
      89,    23,    24,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      14,    33,    -1,    -1,    -1,    19,    -1,    -1,    -1,    23,
      24,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    33,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    -1,    -1,
      -1,    -1,    84,    -1,    -1,    49,    -1,    89,    -1,    -1,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    -1,    -1,
      -1,    -1,    84,    -1,    -1,    -1,    -1,    89,    72,    -1,
      74,    -1,    76,    -1,    78,    -1,    -1,    -1,    -1,     7,
      84,    -1,    -1,    -1,    -1,    89,    14,    15,    16,    17,
      18,    19,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    -1,    -1,    31,    14,    15,    16,    17,    18,    19,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    74,    -1,    76,    -1,
      78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    74,    -1,    76,    -1,    78
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,    92,     6,     8,     9,    10,    11,
      12,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    26,    27,    28,    29,    44,    54,    55,    57,
      59,    68,    69,    70,    72,    74,    76,    78,    89,    93,
      94,    95,    96,    97,    98,   100,   105,   110,   119,   124,
     132,   146,   166,    40,    53,    64,    72,    81,   114,   115,
     116,   117,   118,   119,   166,   114,   166,     0,   166,   159,
     166,    72,   109,   110,   166,   109,    72,   151,   166,   151,
     166,     6,     7,     8,     9,    10,    11,    12,    19,    33,
      59,    70,    71,   145,   166,    72,    76,   120,   121,   138,
     166,   114,   124,   133,    76,   124,   166,    76,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    62,    63,    67,    82,    90,   111,
     114,   149,   114,   149,   144,   145,    19,   166,   114,    95,
      37,    37,    58,    72,    74,   146,    26,    29,    44,    36,
     166,   117,   115,   114,    38,    39,    40,    41,    42,    43,
      45,    46,    47,    49,    50,    51,    52,    62,    63,    67,
      14,    15,    16,    17,    18,    19,    20,    22,    24,    25,
      27,    28,    72,    74,    76,    78,   132,   136,   137,   139,
     166,    62,    63,    82,    34,    67,    45,    46,    47,    48,
      49,    50,    51,    52,    38,    39,    40,    41,    42,    43,
      36,     7,    44,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    49,    50,    51,    52,    62,    63,    67,
      36,   110,    36,    14,    19,    24,    33,    49,    72,    74,
      76,    78,    84,    89,   152,   153,   157,   166,    30,   108,
     109,   166,    30,    72,    76,    78,   113,   156,   157,   160,
     162,   163,   166,   139,   140,   141,   142,   143,   166,    67,
      80,    36,    61,    60,   124,   125,   126,   166,    36,    72,
      76,   114,   122,   123,   138,   166,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    79,    78,    83,    75,
      77,    79,    36,    36,    36,    78,    89,    72,   150,   151,
     156,   150,   139,   149,    32,   114,   145,   114,    36,   166,
      65,   140,   140,   142,    19,   166,    44,   139,   114,   114,
     114,   115,   115,   116,   116,   116,   116,   116,   116,   116,
     116,   117,   117,   117,   118,   118,   118,   114,    94,   166,
     150,   150,   166,   159,    31,    73,   155,   156,   157,    32,
      75,   155,    72,   157,   164,   166,   157,   165,   166,   166,
     114,    73,    79,    36,    38,    42,    44,    45,    47,    48,
      85,   154,   157,   166,     7,    78,   111,   166,   156,   166,
     166,     7,   156,    39,    31,    41,    39,    73,    79,    79,
      77,    79,    36,   114,    67,   121,   115,    78,    78,   134,
     135,    77,   126,    35,   124,    14,    15,    16,    17,    18,
      19,    20,    22,    24,    27,    28,    72,    74,    76,    78,
     132,   166,   166,    80,    71,    77,   123,    36,   114,   139,
      75,   114,    77,   145,   114,   114,   114,    72,   157,    30,
      73,   114,    32,    75,   114,    36,   166,   114,    73,    79,
      75,    79,    77,    79,    36,    36,    78,   114,    44,    51,
      73,    73,   156,    41,   156,    75,    41,    77,    79,    32,
      39,    42,    78,    79,    32,    44,    89,   153,    72,    76,
      78,   157,   157,   145,   157,   157,   157,    87,   157,   157,
     108,    13,   103,   104,   101,   102,   112,   166,   166,   113,
      79,    32,    32,    13,   106,   107,   157,   157,   157,   157,
      73,   139,    77,   143,   139,   114,    19,   147,   148,   166,
     136,   135,   127,   128,   129,    67,    19,   166,   114,   114,
      34,    75,    36,    78,    78,   156,    75,    75,   114,   114,
      36,   166,    66,    73,    75,    77,   139,   139,   157,   157,
      78,   157,   166,   156,   157,   157,   156,   166,   156,   157,
     157,   157,   157,   166,   157,   166,    36,    34,    37,    34,
      37,     7,    78,    98,   104,     7,    79,    31,   166,     7,
     156,   161,   156,    99,   108,   107,    32,    78,    79,    32,
      36,     7,    31,    78,    15,    24,    76,   130,   131,   166,
     124,    36,    36,    78,    77,    80,   114,   114,    75,   114,
      36,   166,   114,    78,    78,    73,    19,   158,   166,    77,
      32,    41,    78,    78,    32,    78,    39,    41,    41,    32,
      39,    32,   157,   157,   157,   157,   157,   103,   101,   103,
     102,   112,   106,    73,    79,    36,   166,    56,   148,   166,
     114,   114,   114,   128,   114,    32,    75,    79,   114,    36,
     166,    36,    32,    36,   156,   157,    36,    37,   156,    37,
     157,   157,   157,   157,   157,   157,     7,   156,   114,    36,
     114,    36,    31,    77,   131,   166,   114,   114,    36,   166,
     157,    75,   157,    77,   157,   157,   157,    78,    73,    77,
      41,    78,    39,   103,   114,   114,   114,    75,   114,    36,
     166,    36,    36,   157,   157,   114,    36,   166,   157,   157,
      77,    78,   114,    36,   166,   114,    36,   166,   114,    36,
     114
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    91,    92,    92,    92,    92,    93,    93,    94,    94,
      94,    95,    95,    95,    95,    95,    95,    95,    95,    95,
      95,    95,    95,    95,    95,    95,    95,    95,    95,    96,
      97,    97,    98,    99,   100,   100,   100,   100,   100,   100,
     100,   100,   101,   101,   102,   103,   103,   104,   105,   105,
     105,   105,   106,   106,   107,   108,   108,   109,   109,   110,
     110,   111,   111,   111,   111,   111,   111,   111,   111,   111,
     111,   111,   111,   111,   111,   111,   111,   111,   111,   111,
     112,   112,   113,   113,   114,   114,   114,   114,   114,   114,
     114,   114,   115,   115,   116,   116,   116,   116,   116,   116,
     116,   116,   116,   117,   117,   117,   117,   117,   118,   118,
     118,   118,   119,   119,   119,   119,   119,   119,   119,   119,
     119,   120,   120,   121,   122,   122,   123,   123,   124,   124,
     124,   124,   124,   124,   124,   124,   124,   124,   124,   124,
     124,   124,   124,   124,   124,   124,   124,   124,   124,   124,
     124,   124,   124,   124,   124,   124,   124,   124,   124,   124,
     124,   124,   124,   124,   124,   124,   124,   124,   124,   124,
     124,   124,   124,   124,   124,   124,   124,   124,   124,   124,
     124,   124,   124,   125,   125,   126,   127,   127,   128,   129,
     129,   130,   130,   131,   131,   131,   132,   132,   133,   133,
     134,   134,   135,   135,   136,   136,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   138,
     138,   138,   138,   138,   139,   140,   140,   141,   141,   142,
     142,   143,   144,   144,   144,   145,   145,   145,   145,   145,
     145,   145,   145,   145,   145,   145,   145,   145,   146,   146,
     146,   146,   147,   147,   148,   148,   148,   149,   149,   149,
     150,   150,   151,   152,   152,   153,   153,   153,   153,   153,
     153,   153,   153,   153,   153,   153,   153,   153,   153,   153,
     153,   153,   153,   153,   153,   153,   154,   154,   155,   155,
     156,   156,   156,   157,   157,   157,   157,   157,   157,   157,
     157,   157,   157,   157,   157,   157,   157,   157,   157,   157,
     157,   158,   158,   159,   159,   160,   160,   161,   161,   162,
     162,   163,   163,   164,   164,   165,   165,   165,   165,   166
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
       1,     2,     1,     2,     4,     2,     3,     3,     3,     3,
       3,     1,     6,     1,     3,     3,     3,     3,     3,     3,
       3,     3,     1,     3,     3,     3,     2,     1,     3,     3,
       3,     1,     1,     4,     5,     4,     3,     4,     4,     6,
       3,     3,     1,     3,     2,     1,     4,     2,     4,     1,
       5,     4,     7,     9,     3,     4,     6,     5,     5,     5,
       5,     3,     6,     8,     3,     4,     2,     1,     2,     6,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     2,     1,     3,     3,     1,     4,     2,
       0,     3,     1,     1,     1,     1,     1,     2,     2,     1,
       2,     1,     4,     6,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       4,     3,     5,     5,     3,     4,     3,     4,     1,     1,
       3,     4,     3,     4,     1,     1,     0,     3,     1,     3,
       1,     3,     0,     3,     5,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     2,
       2,     1,     1,     3,     3,     5,     5,     0,     1,     3,
       3,     1,     3,     1,     3,     2,     3,     3,     3,     7,
       9,     7,     7,     9,     7,     5,     5,     5,     5,     7,
       7,     9,     9,     7,     7,     5,     1,     2,     2,     1,
       3,     1,     1,     1,     3,     2,     3,     7,     3,     3,
       3,     3,     2,     1,     1,     4,     3,     3,     4,     1,
       3,     1,     1,     1,     3,     1,     5,     1,     3,     1,
       3,     3,     3,     5,     3,     5,     3,     3,     1,     1
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
#line 488 "hexpr.y" /* yacc.c:1646  */
    { yyParsedModule = (yyvsp[0].module);                     }
#line 2640 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 3:
#line 489 "hexpr.y" /* yacc.c:1646  */
    { yyParsedVar    = *(yyvsp[-2].string); yyParsedExpr = (yyvsp[0].exp); }
#line 2646 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 4:
#line 490 "hexpr.y" /* yacc.c:1646  */
    { yyParsedVar    = "";  yyParsedExpr = (yyvsp[0].exp); }
#line 2652 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 5:
#line 491 "hexpr.y" /* yacc.c:1646  */
    { yyParsedExpr   = (yyvsp[0].exp);                     }
#line 2658 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 6:
#line 494 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = new Module(*(yyvsp[-2].string), *(yyvsp[0].mdefs)); }
#line 2664 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 7:
#line 495 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = new Module(freshName(), *(yyvsp[0].mdefs)); }
#line 2670 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 8:
#line 497 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); }
#line 2676 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 9:
#line 498 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2682 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 10:
#line 499 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = (yyvsp[-1].mdefs);                            (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2688 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 11:
#line 501 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2694 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 12:
#line 502 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2700 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 13:
#line 503 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mvtydef); }
#line 2706 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 14:
#line 504 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2712 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 15:
#line 505 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2718 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 16:
#line 507 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2724 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 17:
#line 508 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 2730 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 18:
#line 509 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 2736 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 19:
#line 510 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 2742 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 20:
#line 511 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 2748 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 21:
#line 512 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 2754 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 22:
#line 513 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 2760 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 23:
#line 514 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-9]), (yylsp[0]))); }
#line 2766 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 24:
#line 515 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-10]), (yylsp[0]))); }
#line 2772 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 25:
#line 516 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-11]), (yylsp[0]))); }
#line 2778 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 26:
#line 517 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-12]), (yylsp[0]))); }
#line 2784 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 27:
#line 518 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-13]), (yylsp[0]))); }
#line 2790 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 28:
#line 521 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(freshName()), let(freshName(), ExprPtr((yyvsp[0].exp)), mktunit(m((yylsp[0]))), m((yylsp[0]))), m((yylsp[0]))); }
#line 2796 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 29:
#line 524 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MImport(yyModulePath, *(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 2802 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 30:
#line 527 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MTypeDef(MTypeDef::Transparent, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 2808 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 31:
#line 528 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MTypeDef(MTypeDef::Opaque, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 2814 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 32:
#line 531 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydef) = new MVarTypeDef(*(yyvsp[-2].string), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]), (yylsp[0]))); }
#line 2820 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 33:
#line 533 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdef) = new MVarDef(*(yyvsp[-2].strings), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2826 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 34:
#line 536 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-3].tconstraints), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2832 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 35:
#line 537 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-6]), (yylsp[0]))); wantIndent(false); }
#line 2838 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 36:
#line 538 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2844 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 37:
#line 539 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-7].tconstraints), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-8]), (yylsp[0])));            wantIndent(false); }
#line 2850 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 38:
#line 540 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2856 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 39:
#line 541 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2862 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 40:
#line 542 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-4]), (yylsp[0])));            wantIndent(false); }
#line 2868 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 41:
#line 543 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2874 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 42:
#line 545 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundeps) = autorelease(new CFunDepDefs()); (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2880 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 43:
#line 546 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundeps) = (yyvsp[-2].fundeps);                             (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2886 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 44:
#line 548 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundep) = autorelease(new CFunDepDef(*(yyvsp[-2].strings), *(yyvsp[0].strings))); }
#line 2892 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 45:
#line 550 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydefs) = autorelease(new MVarTypeDefs()); (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2898 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 46:
#line 551 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydefs) = (yyvsp[-1].mvtydefs);                              (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2904 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 47:
#line 553 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydef) = (yyvsp[0].mvtydef); }
#line 2910 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 48:
#line 556 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2916 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 49:
#line 557 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-3].tconstraints),           *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2922 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 50:
#line 558 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-4]), (yylsp[0])));        wantIndent(false); }
#line 2928 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 51:
#line 559 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-5].tconstraints),           *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-6]), (yylsp[0])));        wantIndent(false); }
#line 2934 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 52:
#line 561 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdefs) = autorelease(new MVarDefs()); (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2940 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 53:
#line 562 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdefs) = (yyvsp[-1].mvdefs);                          (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2946 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 54:
#line 564 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdef) = (yyvsp[0].mvdef); }
#line 2952 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 55:
#line 567 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[0].strings); }
#line 2958 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 56:
#line 569 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[-1].string)); (yyval.strings)->push_back(*(yyvsp[-2].string)); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 2964 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 57:
#line 571 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 2970 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 58:
#line 572 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 2976 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 59:
#line 574 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 2982 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 60:
#line 576 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[-1].string); }
#line 2988 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 61:
#line 578 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("and")); }
#line 2994 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 62:
#line 579 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("or")); }
#line 3000 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 63:
#line 580 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3006 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 64:
#line 581 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3012 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 65:
#line 582 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("~")); }
#line 3018 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 66:
#line 583 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("=~")); }
#line 3024 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 67:
#line 584 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("===")); }
#line 3030 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 68:
#line 585 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("==")); }
#line 3036 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 69:
#line 586 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("<")); }
#line 3042 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 70:
#line 587 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("<=")); }
#line 3048 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 71:
#line 588 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(">")); }
#line 3054 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 72:
#line 589 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(">=")); }
#line 3060 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 73:
#line 590 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("in")); }
#line 3066 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 74:
#line 591 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("append")); }
#line 3072 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 75:
#line 592 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("+")); }
#line 3078 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 76:
#line 593 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("-")); }
#line 3084 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 77:
#line 594 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("*")); }
#line 3090 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 78:
#line 595 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("/")); }
#line 3096 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 79:
#line 596 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("%")); }
#line 3102 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 80:
#line 598 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3108 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 81:
#line 599 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3114 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 82:
#line 601 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3120 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 83:
#line 602 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes);                           (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3126 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 84:
#line 605 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3132 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 85:
#line 606 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), (yyvsp[0].exp), m((yylsp[-1]),(yylsp[0]))); }
#line 3138 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 86:
#line 607 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("and",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3144 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 87:
#line 608 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("or",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3150 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 88:
#line 609 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("compose",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3156 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 89:
#line 610 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assign(ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3162 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 90:
#line 611 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("in",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3168 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 91:
#line 612 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3174 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 92:
#line 614 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP3(var("if",m((yylsp[-5]))), (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-5]), (yylsp[0]))); }
#line 3180 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 93:
#line 615 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3186 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 94:
#line 617 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("~",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3192 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 95:
#line 618 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("===",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3198 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 96:
#line 619 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3204 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 97:
#line 620 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))), m((yylsp[-2]),(yylsp[0]))); }
#line 3210 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 98:
#line 621 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("<",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3216 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 99:
#line 622 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("<=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3222 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 100:
#line 623 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var(">",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3228 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 101:
#line 624 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var(">=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3234 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 102:
#line 625 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3240 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 103:
#line 627 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("+",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3246 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 104:
#line 628 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("-",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3252 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 105:
#line 629 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("append",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3258 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 106:
#line 630 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("neg",m((yylsp[-1]))), ExprPtr((yyvsp[0].exp)), m((yylsp[-1]),(yylsp[0]))); }
#line 3264 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 107:
#line 631 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3270 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 108:
#line 633 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("*", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3276 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 109:
#line 634 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("/", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3282 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 110:
#line 635 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("%", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3288 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 111:
#line 636 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3294 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 112:
#line 638 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3300 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 113:
#line 641 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-2].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3306 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 114:
#line 642 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]),(yylsp[0])))->clone(); }
#line 3312 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 115:
#line 645 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileMatch(yyParseCC, *(yyvsp[-2].exps), normPatternRules(*(yyvsp[0].patternexps), m((yylsp[-3]),(yylsp[0]))), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3318 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 116:
#line 648 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileMatchTest(yyParseCC, ExprPtr((yyvsp[-2].exp)), PatternPtr((yyvsp[0].pattern)), m((yylsp[-2]),(yylsp[0])))->clone(); }
#line 3324 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 117:
#line 651 "hexpr.y" /* yacc.c:1646  */
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
#line 3339 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 118:
#line 663 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-1].letbindings), ExprPtr(new Unit(m((yylsp[-3]),(yylsp[0])))), m((yylsp[-3]),(yylsp[0]))); }
#line 3345 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 119:
#line 664 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[-1].exp)), m((yylsp[-5]),(yylsp[0]))); }
#line 3351 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 120:
#line 667 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assump(ExprPtr((yyvsp[-2].exp)), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]),(yylsp[0]))); }
#line 3357 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 121:
#line 669 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].letbindings)->push_back(*(yyvsp[0].letbinding)); (yyval.letbindings) = (yyvsp[-2].letbindings); }
#line 3363 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 122:
#line 670 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3369 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 123:
#line 672 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-2].pattern)), ExprPtr((yyvsp[0].exp)))); }
#line 3375 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 124:
#line 674 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = (yyvsp[-1].letbindings); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3381 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 125:
#line 675 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3387 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 126:
#line 677 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)))); }
#line 3393 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 127:
#line 678 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr(new MatchAny("_",m((yylsp[-1])))), ExprPtr((yyvsp[-1].exp)))); }
#line 3399 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 128:
#line 681 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].exps), m((yylsp[-3]), (yylsp[0]))); }
#line 3405 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 129:
#line 682 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = varCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3411 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 130:
#line 685 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("range", m((yylsp[-2]))), list(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]), (yylsp[0]))); }
#line 3417 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 131:
#line 686 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("iterateS", m((yylsp[-1]))), list(ExprPtr((yyvsp[-2].exp)), fn(str::strings(".x"), fncall(var("+", m((yylsp[-1]))), list(var(".x", m((yylsp[-1]))), ExprPtr(new Int(1, m((yylsp[-1]))))), m((yylsp[-1]))), m((yylsp[-1])))), m((yylsp[-3]), (yylsp[0]))); }
#line 3423 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 132:
#line 687 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileArrayComprehension(ExprPtr((yyvsp[-5].exp)), PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 3429 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 133:
#line 688 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileArrayComprehension(ExprPtr((yyvsp[-7].exp)), PatternPtr((yyvsp[-5].pattern)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 3435 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 134:
#line 689 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkArray(*(yyvsp[-1].exps), m((yylsp[-2]), (yylsp[0]))); }
#line 3441 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 135:
#line 690 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new AIndex(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3447 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 136:
#line 691 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("slice", m((yylsp[-2]))), list(ExprPtr((yyvsp[-5].exp)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-5]), (yylsp[0]))); }
#line 3453 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 137:
#line 692 "hexpr.y" /* yacc.c:1646  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-1]))), list(var(vn,m((yylsp[-4]))), ExprPtr((yyvsp[-2].exp)), fncall(var("size",m((yylsp[-1]))), list(var(vn,m((yylsp[-4])))),m((yylsp[-4])))),m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3459 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 138:
#line 693 "hexpr.y" /* yacc.c:1646  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-2]))), list(var(vn,m((yylsp[-4]))), fncall(var("size",m((yylsp[-2]))), list(var(vn,m((yylsp[-2])))),m((yylsp[-4]))), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3465 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 139:
#line 696 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-3].string), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3471 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 140:
#line 697 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(".f" + str::from((yyvsp[-3].intv)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3477 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 141:
#line 698 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-1].string), ExprPtr(new Unit(m((yylsp[-1])))), m((yylsp[-2]), (yylsp[0]))); }
#line 3483 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 142:
#line 699 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-4].exp)), *(yyvsp[-1].vfields), m((yylsp[-5]), (yylsp[0]))); }
#line 3489 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 143:
#line 700 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-6].exp)), *(yyvsp[-3].vfields), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3495 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 144:
#line 703 "hexpr.y" /* yacc.c:1646  */
    { if ((yyvsp[-1].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-1].rfields), m((yylsp[-2]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-2]), (yylsp[0]))); } }
#line 3501 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 145:
#line 704 "hexpr.y" /* yacc.c:1646  */
    { if ((yyvsp[-2].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-2].rfields), m((yylsp[-3]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-3]), (yylsp[0]))); } }
#line 3507 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 146:
#line 705 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makeProjSeq((yyvsp[-1].exp), *(yyvsp[0].strings), m((yylsp[-1]), (yylsp[0]))); }
#line 3513 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 147:
#line 708 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Fn(str::strings("x"), proj(var("x", m((yylsp[0]))), *(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3519 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 148:
#line 711 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Pack(ExprPtr((yyvsp[0].exp)), m((yylsp[-1]), (yylsp[0]))); }
#line 3525 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 149:
#line 712 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Unpack(*(yyvsp[-4].string), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3531 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 150:
#line 715 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Bool((yyvsp[0].boolv), m((yylsp[0]))); }
#line 3537 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 151:
#line 716 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3543 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 152:
#line 717 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3549 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 153:
#line 718 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3555 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 154:
#line 719 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Short((yyvsp[0].shortv), m((yylsp[0]))); }
#line 3561 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 155:
#line 720 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Int((yyvsp[0].intv), m((yylsp[0]))); }
#line 3567 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 156:
#line 721 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Long((yyvsp[0].longv), m((yylsp[0]))); }
#line 3573 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 157:
#line 722 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Float((yyvsp[0].floatv), m((yylsp[0]))); }
#line 3579 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 158:
#line 723 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Double((yyvsp[0].doublev), m((yylsp[0]))); }
#line 3585 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 159:
#line 724 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3591 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 160:
#line 725 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0])))->clone(); }
#line 3597 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 161:
#line 726 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3603 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 162:
#line 727 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3609 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 163:
#line 730 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = pickNestedExp((yyvsp[-1].exps), m((yylsp[-2]),(yylsp[0]))); }
#line 3615 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 164:
#line 733 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("append", m((yylsp[-1]))); }
#line 3621 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 165:
#line 734 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("+",      m((yylsp[-1]))); }
#line 3627 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 166:
#line 735 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("-",      m((yylsp[-1]))); }
#line 3633 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 167:
#line 736 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("*",      m((yylsp[-1]))); }
#line 3639 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 168:
#line 737 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("/",      m((yylsp[-1]))); }
#line 3645 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 169:
#line 738 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("%",      m((yylsp[-1]))); }
#line 3651 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 170:
#line 739 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("~",      m((yylsp[-1]))); }
#line 3657 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 171:
#line 740 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("===",    m((yylsp[-1]))); }
#line 3663 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 172:
#line 741 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("==",     m((yylsp[-1]))); }
#line 3669 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 173:
#line 742 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("!=",     m((yylsp[-1]))); }
#line 3675 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 174:
#line 743 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("<",      m((yylsp[-1]))); }
#line 3681 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 175:
#line 744 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var(">",      m((yylsp[-1]))); }
#line 3687 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 176:
#line 745 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var(">=",     m((yylsp[-1]))); }
#line 3693 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 177:
#line 746 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("<=",     m((yylsp[-1]))); }
#line 3699 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 178:
#line 747 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("and",    m((yylsp[-1]))); }
#line 3705 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 179:
#line 748 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("or",     m((yylsp[-1]))); }
#line 3711 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 180:
#line 749 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("in",     m((yylsp[-1]))); }
#line 3717 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 181:
#line 750 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("not",    m((yylsp[-1]))); }
#line 3723 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 182:
#line 753 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assump(fncall(var("unsafeCast", m((yylsp[-1]))), list(mktunit(m((yylsp[-1])))), m((yylsp[-1]))), qualtype(tapp(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp)))))), m((yylsp[-1]))); }
#line 3729 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 183:
#line 755 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prules) = (yyvsp[-1].prules); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3735 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 184:
#line 756 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prules) = autorelease(new Grammar()); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3741 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 185:
#line 758 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prule) = autorelease(new Grammar::value_type(*(yyvsp[-2].string), *(yyvsp[0].prdefs))); }
#line 3747 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 186:
#line 760 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdefs) = (yyvsp[-2].prdefs); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3753 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 187:
#line 761 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdefs) = autorelease(new GrammarRules()); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3759 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 188:
#line 763 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdef) = autorelease(new GrammarRule(*(yyvsp[-3].pbelems), ExprPtr((yyvsp[-1].exp)))); }
#line 3765 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 189:
#line 765 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelems) = (yyvsp[-1].pbelems); (yyval.pbelems)->push_back(*(yyvsp[0].pbelem)); }
#line 3771 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 190:
#line 766 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelems) = autorelease(new BoundGrammarValues()); }
#line 3777 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 191:
#line 768 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue(*(yyvsp[-2].string), GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3783 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 192:
#line 769 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue("_", GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3789 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 193:
#line 771 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GSymRef(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3795 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 194:
#line 772 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GStr(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3801 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 195:
#line 773 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GStr(std::string(1, str::readCharDef(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3807 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 196:
#line 775 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3813 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 197:
#line 776 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3819 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 198:
#line 778 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = (yyvsp[-1].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3825 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 199:
#line 779 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3831 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 200:
#line 781 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexps) = (yyvsp[-1].patternexps); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3837 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 201:
#line 782 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexps) = autorelease(new PatternRows()); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3843 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 202:
#line 784 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)))); }
#line 3849 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 203:
#line 785 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-4].patterns), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)))); }
#line 3855 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 204:
#line 788 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[-1].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3861 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 205:
#line 789 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = autorelease(new Patterns()); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3867 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 206:
#line 791 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Bool((yyvsp[0].boolv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3873 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 207:
#line 792 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3879 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 208:
#line 793 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3885 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 209:
#line 794 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Short((yyvsp[0].shortv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3891 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 210:
#line 795 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int((yyvsp[0].intv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3897 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 211:
#line 796 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Long((yyvsp[0].longv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3903 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 212:
#line 797 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Double((yyvsp[0].doublev), m((yylsp[0])))), m((yylsp[0]))); }
#line 3909 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 213:
#line 798 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = mkpatarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3915 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 214:
#line 799 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = mkpatarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3921 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 215:
#line 800 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkTimespanPrim(*(yyvsp[0].strings), m((yylsp[0]))), mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3927 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 216:
#line 801 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 3933 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 217:
#line 802 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkDateTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 3939 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 218:
#line 803 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRegex(std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0]))); }
#line 3945 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 219:
#line 804 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 3951 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 220:
#line 805 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 3957 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 221:
#line 806 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-1].string), PatternPtr(new MatchLiteral(PrimitivePtr(new Unit(m((yylsp[-1])))), m((yylsp[-1])))), m((yylsp[-2]),(yylsp[0]))); }
#line 3963 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 222:
#line 807 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-3].string), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 3969 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 223:
#line 808 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(".f" + str::from((yyvsp[-3].intv)), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 3975 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 224:
#line 809 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 3981 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 225:
#line 810 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 3987 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 226:
#line 811 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 3993 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 227:
#line 812 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 3999 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 228:
#line 813 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = patVarCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4005 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 229:
#line 815 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchAny(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4011 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 230:
#line 816 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4017 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 231:
#line 817 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4023 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 232:
#line 818 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4029 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 233:
#line 819 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4035 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 234:
#line 821 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = (yyvsp[0].pattern); }
#line 4041 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 235:
#line 823 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[0].patterns); }
#line 4047 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 236:
#line 824 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = new Patterns(); }
#line 4053 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 237:
#line 826 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[-2].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4059 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 238:
#line 827 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = new Patterns(); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4065 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 239:
#line 829 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfields) = (yyvsp[-2].recpatfields); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4071 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 240:
#line 830 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfields) = new MatchRecord::Fields(); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4077 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 241:
#line 832 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfield) = new MatchRecord::Field(*(yyvsp[-2].string), PatternPtr((yyvsp[0].pattern))); }
#line 4083 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 242:
#line 834 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); }
#line 4089 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 243:
#line 835 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4095 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 244:
#line 836 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = (yyvsp[-4].rfields);                                     (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4101 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 245:
#line 838 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4107 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 246:
#line 839 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("data")); }
#line 4113 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 247:
#line 840 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("type")); }
#line 4119 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 248:
#line 841 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("where")); }
#line 4125 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 249:
#line 842 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("class")); wantIndent(false); }
#line 4131 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 250:
#line 843 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("instance")); wantIndent(false); }
#line 4137 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 251:
#line 844 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("exists")); }
#line 4143 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 252:
#line 845 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("import")); }
#line 4149 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 253:
#line 846 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("module")); }
#line 4155 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 254:
#line 847 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("parse")); }
#line 4161 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 255:
#line 848 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("do")); }
#line 4167 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 256:
#line 849 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("return")); }
#line 4173 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 257:
#line 850 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(".f" + str::from((yyvsp[0].intv)))); }
#line 4179 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 258:
#line 852 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-2].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4185 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 259:
#line 853 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings); str::seq x = tupSectionFields(*(yyvsp[0].string)); (yyval.strings)->insert((yyval.strings)->end(), x.begin(), x.end()); }
#line 4191 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 260:
#line 854 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4197 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 261:
#line 855 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); *(yyval.strings) = tupSectionFields(*(yyvsp[0].string)); }
#line 4203 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 262:
#line 857 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vfields) = autorelease(new Case::Bindings()); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4209 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 263:
#line 858 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vfields) = (yyvsp[-2].vfields); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4215 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 264:
#line 860 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-2].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4221 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 265:
#line 861 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-4].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4227 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 266:
#line 862 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(".f" + str::from((yyvsp[-4].intv)), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4233 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 267:
#line 864 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); }
#line 4239 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 268:
#line 865 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4245 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 269:
#line 866 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].exps)->push_back(ExprPtr((yyvsp[0].exp))); (yyval.exps) = (yyvsp[-2].exps); }
#line 4251 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 270:
#line 868 "hexpr.y" /* yacc.c:1646  */
    { (yyval.qualtype) = new QualType(*(yyvsp[-2].tconstraints), *(yyvsp[0].mtype)); }
#line 4257 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 271:
#line 869 "hexpr.y" /* yacc.c:1646  */
    { (yyval.qualtype) = new QualType(Constraints(), *(yyvsp[0].mtype)); }
#line 4263 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 272:
#line 872 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraints) = (yyvsp[-1].tconstraints); }
#line 4269 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 273:
#line 874 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraints) = autorelease(new Constraints()); (yyval.tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); }
#line 4275 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 274:
#line 875 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); (yyval.tconstraints) = (yyvsp[-2].tconstraints); }
#line 4281 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 275:
#line 877 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(*(yyvsp[-1].string), *(yyvsp[0].mtypes)); }
#line 4287 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 276:
#line 878 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(EqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4293 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 277:
#line 879 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(NotEqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4299 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 278:
#line 880 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(FixIsoRecur::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4305 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 279:
#line 881 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4311 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 280:
#line 882 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4317 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 281:
#line 883 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4323 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 282:
#line 884 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4329 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 283:
#line 885 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4335 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 284:
#line 886 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(1), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4341 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 285:
#line 888 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4347 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 286:
#line 889 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4353 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 287:
#line 890 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4359 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 288:
#line 891 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4365 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 289:
#line 893 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4371 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 290:
#line 894 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4377 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 291:
#line 895 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4383 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 292:
#line 896 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4389 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 293:
#line 898 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), TString::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype))); }
#line 4395 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 294:
#line 899 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), *(yyvsp[-5].mtype),                *(yyvsp[-3].mtype))); }
#line 4401 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 295:
#line 900 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(AppendsToUnqualifier::constraintName(), list(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4407 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 296:
#line 902 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4413 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 297:
#line 903 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-1].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4419 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 298:
#line 905 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4425 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 299:
#line 906 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4431 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 300:
#line 908 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Func::make(tuplety(*(yyvsp[-2].mtypes)), *(yyvsp[0].mtype)))); }
#line 4437 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 301:
#line 909 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeTupleType(*(yyvsp[0].mtypes)))); }
#line 4443 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 302:
#line 910 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeSumType(*(yyvsp[0].mtypes)))); }
#line 4449 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 303:
#line 912 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(monoTypeByName(*(yyvsp[0].string)))); }
#line 4455 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 304:
#line 913 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(OpaquePtr::make(str::replace<char>(*(yyvsp[-1].string), ".", "::"), 0, false))); }
#line 4461 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 305:
#line 914 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("[]"))); }
#line 4467 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 306:
#line 915 "hexpr.y" /* yacc.c:1646  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(Array::make(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4473 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 307:
#line 916 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(FixedArray::make(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype)))); }
#line 4479 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 308:
#line 917 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("->"))); }
#line 4485 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 309:
#line 918 "hexpr.y" /* yacc.c:1646  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(clone(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4491 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 310:
#line 919 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeRecType(*(yyvsp[-1].mreclist)))); }
#line 4497 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 311:
#line 920 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeVarType(*(yyvsp[-1].mvarlist)))); }
#line 4503 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 312:
#line 921 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("unit"))); }
#line 4509 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 313:
#line 922 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(((yyvsp[0].intv) == 0) ? Prim::make("void") : TLong::make((yyvsp[0].intv)))); }
#line 4515 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 314:
#line 923 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr((yyvsp[0].boolv) ? TLong::make(1) : TLong::make(0))); }
#line 4521 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 315:
#line 924 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Exists::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4527 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 316:
#line 925 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(Prim::make("fileref"), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))))); }
#line 4533 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 317:
#line 926 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(Prim::make("fileref"), list(*(yyvsp[-2].mtype))))); }
#line 4539 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 318:
#line 927 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Recursive::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4545 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 319:
#line 928 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TString::make(str::unescape(str::trimq(*(yyvsp[0].string)))))); }
#line 4551 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 320:
#line 929 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp))))))); }
#line 4557 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 321:
#line 931 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TVar::make(*(yyvsp[0].string)))); }
#line 4563 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 322:
#line 932 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TLong::make((yyvsp[0].intv)))); }
#line 4569 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 323:
#line 934 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4575 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 324:
#line 935 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[-2].string); *(yyval.string) += "."; *(yyval.string) += *(yyvsp[0].string); }
#line 4581 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 325:
#line 937 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4587 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 326:
#line 938 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-1].mtypes)->insert((yyvsp[-1].mtypes)->begin(), *(yyvsp[-3].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4593 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 327:
#line 940 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4599 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 328:
#line 941 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-2].mtypes); }
#line 4605 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 329:
#line 943 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4611 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 330:
#line 944 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4617 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 331:
#line 946 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[-2].mtype)); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4623 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 332:
#line 947 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4629 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 333:
#line 949 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mreclist) = (yyvsp[-4].mreclist);                                 (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4635 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 334:
#line 950 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mreclist) = autorelease(new Record::Members()); (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4641 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 335:
#line 952 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = (yyvsp[-4].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4647 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 336:
#line 953 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = (yyvsp[-2].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4653 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 337:
#line 954 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4659 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 338:
#line 955 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4665 "hexpr.parse.C" /* yacc.c:1646  */
    break;


#line 4669 "hexpr.parse.C" /* yacc.c:1646  */
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
#line 959 "hexpr.y" /* yacc.c:1906  */

#pragma GCC diagnostic pop


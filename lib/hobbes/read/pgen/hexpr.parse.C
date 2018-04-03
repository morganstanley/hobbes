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

MonoTypePtr forceMonotype(const QualTypePtr& qt, const LexicalAnnotation& la) {
  MonoTypeUnifier u(yyParseCC->typeEnv());
  Definitions ds;
  while (refine(yyParseCC->typeEnv(), qt->constraints(), &u, &ds)) {
    yyParseCC->drainUnqualifyDefs(ds);
    ds.clear();
  }
  yyParseCC->drainUnqualifyDefs(ds);
  ds.clear();

  // make sure that the output type exists and is realizable
  if (hobbes::satisfied(yyParseCC->typeEnv(), qt->constraints(), &ds)) {
    yyParseCC->drainUnqualifyDefs(ds);
    return u.substitute(qt->monoType());
  } else {
    throw annotated_error(la, "Cannot resolve qualifications in type");
  }
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
    for (int i = 0; i < exprs->size(); ++i) {
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
    for (int i = 0; i < pats->size(); ++i) {
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


#line 353 "hexpr.parse.C" /* yacc.c:339  */

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
#line 293 "hexpr.y" /* yacc.c:355  */

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

#line 540 "hexpr.parse.C" /* yacc.c:355  */
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

#line 569 "hexpr.parse.C" /* yacc.c:358  */

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
#define YYLAST   2709

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
       0,   506,   506,   507,   508,   509,   512,   513,   515,   516,
     517,   519,   520,   521,   522,   523,   525,   526,   527,   528,
     529,   530,   531,   532,   533,   534,   535,   536,   539,   542,
     545,   546,   549,   551,   554,   555,   556,   557,   558,   559,
     560,   561,   563,   564,   566,   568,   569,   571,   574,   575,
     576,   577,   579,   580,   582,   585,   587,   589,   590,   592,
     594,   596,   597,   598,   599,   600,   601,   602,   603,   604,
     605,   606,   607,   608,   609,   610,   611,   612,   613,   614,
     616,   617,   619,   620,   623,   624,   625,   626,   627,   628,
     630,   631,   632,   633,   634,   635,   636,   637,   638,   639,
     641,   642,   643,   644,   645,   647,   648,   649,   650,   652,
     653,   656,   657,   660,   663,   666,   678,   679,   682,   684,
     685,   687,   689,   690,   692,   693,   696,   697,   699,   700,
     703,   704,   705,   706,   707,   708,   709,   710,   711,   714,
     715,   716,   717,   718,   721,   722,   723,   726,   729,   730,
     733,   734,   735,   736,   737,   738,   739,   740,   741,   742,
     743,   744,   745,   748,   751,   752,   753,   754,   755,   756,
     757,   758,   759,   760,   761,   762,   763,   764,   765,   766,
     767,   768,   771,   773,   774,   776,   778,   779,   781,   783,
     784,   786,   787,   789,   790,   791,   793,   794,   796,   797,
     799,   800,   802,   803,   806,   807,   809,   810,   811,   812,
     813,   814,   815,   816,   817,   818,   819,   820,   821,   822,
     823,   824,   825,   826,   827,   828,   829,   830,   831,   833,
     834,   835,   836,   837,   839,   841,   842,   844,   845,   847,
     848,   850,   852,   853,   854,   856,   857,   858,   859,   860,
     861,   862,   863,   864,   865,   866,   867,   868,   870,   871,
     872,   873,   875,   876,   878,   879,   880,   882,   883,   884,
     886,   887,   890,   892,   893,   895,   896,   897,   898,   899,
     900,   901,   902,   903,   904,   906,   907,   908,   909,   911,
     912,   913,   914,   916,   917,   918,   920,   921,   923,   924,
     926,   927,   928,   930,   931,   932,   933,   934,   935,   936,
     937,   938,   939,   940,   941,   942,   943,   944,   945,   946,
     947,   949,   950,   952,   953,   955,   956,   958,   959,   961,
     962,   964,   965,   967,   968,   970,   971,   972,   973,   975
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
  "l1expr", "l2expr", "l3expr", "l4expr", "letbindings", "letbinding",
  "dobindings", "dobinding", "l5expr", "l6expr", "prules", "prule",
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
     422,   458,  2068,  2068,    40,   130,   130,    55,    55,    57,
      57,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,  -569,  -569,  -569,   672,  2068,   334,   130,
    1380,  2068,   672,   324,  2068,  -569,   824,  -569,  -569,  -569,
    -569,  -569,  -569,   232,  -569,   203,    59,   123,   208,  2372,
    2372,    42,   334,   164,  2448,   266,  1688,  2631,   455,   268,
     502,   552,  -569,    98,   356,   455,  -569,  -569,   338,   240,
    -569,   973,    16,  -569,  -569,   189,   716,   389,    55,   423,
    1190,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,  -569,  -569,   277,   227,   425,   398,   416,
    2220,   489,   515,   516,   672,   525,   526,   529,   539,   550,
     551,   556,   557,  2296,   559,   560,   561,  -569,  -569,   562,
     455,   378,   248,   480,   339,   589,   600,   138,   243,  -569,
    1534,   334,  2068,  1764,   123,  -569,  -569,   672,  2068,   122,
    -569,   268,  2631,   130,   166,  -569,   601,  -569,   227,  2524,
     130,   573,  2144,   398,   416,  2220,   489,   515,   516,   525,
     526,   529,   550,   551,   556,   557,   559,   560,   561,  -569,
    -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,
    -569,  2631,  2631,   130,   437,    59,  2616,  -569,  -569,  -569,
    2068,  2068,  2068,  2372,  2372,  2372,  2372,  2372,  2372,  2372,
    2372,  2372,  2372,  2372,  2372,  2448,  2448,  2448,  1534,  2631,
    2068,   824,   130,  -569,  -569,  -569,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,
    1534,  -569,  1534,  -569,  -569,  -569,   130,   130,   582,   474,
    2540,  2540,   130,  2068,   426,  -569,   139,  2540,   130,    23,
      55,   973,   130,   582,   130,   130,    48,  -569,     2,   608,
     599,   602,  -569,   566,   334,  -569,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,  -569,  -569,  2068,  2631,  1840,  -569,  -569,
     195,  2068,  2068,  2068,  -569,  -569,   850,  -569,   612,  -569,
     227,   449,  2068,   148,  -569,   455,  2068,   212,  -569,   465,
     569,   442,  -569,   609,  2068,    41,  2372,   572,   227,    15,
    -569,   617,  2448,  1612,   672,   317,  1492,  -569,   621,   623,
     466,   481,   505,   625,   142,  2068,  -569,   580,   580,  -569,
     586,   586,   586,   586,   586,   586,   586,   586,   586,  -569,
    -569,  -569,  -569,  -569,  -569,  -569,  -569,   455,   824,  -569,
    -569,  -569,   620,     7,   594,  -569,   713,  -569,     6,  1190,
    -569,  1097,   582,   132,   541,   636,    91,    -4,   -10,   629,
     245,  -569,   716,   453,  2540,  2540,   672,  2540,  2540,  2540,
    1271,  2540,   584,    55,   661,   130,   130,  1190,   597,   645,
     653,   673,  -569,  2540,  2540,  2540,  2540,   542,   190,   455,
     654,  -569,   323,  -569,   651,   455,   259,   372,   582,   730,
    1190,  -569,   367,  1916,  -569,   455,  2068,   216,  -569,   616,
    2631,  -569,    27,  2631,  -569,  2068,  -569,   502,  2631,   572,
    -569,  -569,  -569,  -569,   624,  -569,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,  -569,  -569,  -569,  1612,  1992,   672,   545,
      59,  -569,   609,  -569,  2068,  -569,  -569,  2068,  -569,   619,
    -569,   618,  -569,    33,  2631,  2631,  -569,   455,  2540,  -569,
    -569,  -569,  -569,  2540,   622,  -569,  2540,  -569,   130,  1190,
    2540,  1190,  -569,   130,  1190,  2540,  -569,  -569,  2540,  2540,
    2540,    62,    49,   226,   584,   584,   584,  -569,   584,   584,
      24,    55,   661,  -569,    17,  -569,   219,  -569,  -569,    70,
    1190,  1190,  1190,    55,   673,  -569,   584,   584,   584,   584,
     662,   298,  -569,   535,   334,  2068,  -569,  2068,  -569,  -569,
    -569,  -569,  -569,   377,   455,  2068,   273,  -569,  -569,  -569,
    -569,  -569,  -569,  1570,  -569,   628,  -569,    19,  2448,   660,
     177,   381,   432,  -569,  -569,  -569,   631,   633,   584,   -25,
     564,   193,   666,  -569,    22,    77,   634,   671,   637,     5,
     584,   144,   168,   675,    31,   684,  2540,  2540,  2540,  2540,
    2540,   661,   130,  -569,  -569,   661,   130,   130,  -569,   673,
    -569,   478,  -569,  -569,   681,  -569,   130,   663,   542,   130,
    2068,   207,   309,   455,  -569,   455,  2068,   346,  2068,  2068,
    -569,  -569,  -569,  2068,  -569,  -569,   686,  -569,  1992,  1992,
    -569,  -569,  -569,  -569,  -569,   688,  -569,   690,  -569,   689,
    1190,  2540,   692,   683,  1190,   696,  2540,  2540,  2540,  2540,
    2540,  2540,   584,   584,   584,   584,   584,   661,    21,   661,
    -569,   130,   673,  -569,  1190,  2068,   693,  2068,  -569,   702,
     455,  -569,  2068,   455,  2068,   347,    78,  -569,  -569,   438,
     279,  2540,   670,  2540,  -569,   251,  2540,  2540,  -569,  2540,
     296,   201,   288,   174,   322,    86,   661,  -569,   455,  2068,
     455,  2068,   429,   455,  2068,   386,  2068,  -569,  -569,  -569,
     584,  -569,   584,   712,   584,   584,   584,   714,  -569,  -569,
    2540,  -569,  2540,   661,   455,   455,  -569,   455,  2068,   405,
    -569,  2540,  2540,   290,   326,   455,  2068,   460,   584,   584,
    -569,  -569,   455,  2068,   467,   455,  2068,   715,   455,  2068,
     455
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,     8,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   150,   151,   152,   153,   154,   155,   156,   157,   158,
     339,   159,   196,   161,   162,   261,     0,     0,     0,     0,
     267,   267,   242,     0,     0,     2,     7,     9,    11,    12,
      13,    14,    15,     0,    28,   127,   160,   147,   129,     0,
       0,     0,     0,     0,     0,     0,   267,     0,     4,    89,
      99,   104,   108,   110,   129,     5,   129,     1,     0,    29,
     323,     0,     0,    57,    59,     0,     0,     0,     0,     0,
       0,   253,   248,   252,   247,   246,   249,   250,   257,   251,
     254,   255,   256,   260,   245,     0,   148,     0,     0,     0,
       0,     0,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    66,     0,
     268,     0,   268,     0,     0,     0,     0,     0,     0,    10,
       0,     0,   267,     0,   146,   197,   259,     0,     0,     0,
     103,    85,   236,     0,     0,   120,     0,   229,   199,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   206,
     207,   208,   213,   209,   210,   211,   212,   214,   218,   216,
     217,   236,   236,     0,     0,   215,     0,   234,   205,   228,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     8,     0,    74,    75,    76,    77,    78,    79,    64,
      68,    67,    65,    69,    70,    71,    72,    61,    62,    73,
       0,    58,     0,   314,   313,   319,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   273,     0,   303,     0,    38,
      55,    59,     0,     0,     0,     0,    48,    82,   329,     0,
     301,   302,   303,     0,     0,   164,   165,   166,   167,   168,
     169,   172,   171,   170,   173,   174,   177,   175,   176,   181,
     178,   179,   180,    60,   163,     0,     0,     0,   134,   144,
       0,     0,     0,     0,   141,   182,     0,    32,     0,   271,
     126,     0,     0,     0,   258,    16,     0,     0,   238,     0,
     235,     0,   240,     0,     0,     0,     0,     0,   198,     0,
     184,     0,     0,   236,   242,     0,     0,   123,     0,   129,
       0,     0,     0,     0,     0,     0,   204,    86,    87,    88,
      92,    91,    90,    93,    94,    95,    96,    97,    98,   102,
     100,   101,   105,   106,   107,   118,   114,     3,     6,   324,
      30,    31,     0,     0,     0,   312,     0,   299,   329,     0,
     305,     0,     0,     0,     0,   303,     0,     0,   303,     0,
       0,   272,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   275,   296,     0,     0,     0,     0,     0,   299,     0,
     338,     0,    83,     0,     0,     0,     0,     0,     0,   269,
       0,   131,     0,   145,     0,   243,     0,     0,     0,   329,
       0,   128,     0,     0,   135,    17,     0,     0,   230,     0,
       0,   232,     0,     0,   111,     0,   119,   121,     0,   113,
     201,   115,   183,   190,     0,   150,   151,   152,   153,   154,
     155,   156,   158,   159,   161,   162,   236,   236,   242,     0,
     160,   129,     0,   125,     0,   116,   122,     0,   224,     0,
     219,     0,   226,     0,     0,     0,   221,    84,     0,   304,
     308,   309,   298,     0,     0,   306,     0,   310,     0,     0,
       0,     0,   311,     0,     0,     0,   320,   274,     0,     0,
       0,     0,     0,     0,   276,   278,   277,   317,   316,   297,
      34,     0,    40,    45,    39,    42,     0,    80,    56,    49,
       0,     0,     0,     0,    50,    52,   331,   300,   330,   332,
       0,     0,   262,     0,     0,     0,   130,     0,   140,   139,
     270,   138,   137,     0,    18,     0,     0,   231,   237,   233,
     239,   241,   112,     0,   200,   185,   187,     0,     0,     0,
       0,     0,     0,   225,   220,   227,     0,     0,   315,     0,
       0,     0,     0,   334,   329,     0,     0,   336,   337,   329,
     318,     0,     0,   303,     0,   303,     0,     0,     0,     0,
       0,     0,     0,    47,    46,     0,     0,     0,    81,     0,
     327,     0,   337,    54,     0,    53,     0,   142,     0,     0,
       0,   149,     0,   244,   136,    19,     0,     0,     0,     0,
     190,   195,   194,     0,   189,   192,   193,   109,     0,     0,
     141,   117,   124,   223,   222,     0,   322,     0,   321,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   295,   288,   287,   286,   285,    36,    35,    41,
      43,    44,    51,   326,     0,     0,     0,     0,   263,     0,
     264,   132,     0,    20,     0,     0,     0,   202,   186,     0,
       0,     0,     0,     0,   333,     0,     0,     0,   335,     0,
     331,     0,     0,     0,     0,     0,     0,   328,    33,     0,
     143,     0,     0,    21,     0,     0,     0,   188,   191,   193,
     284,   307,   282,     0,   290,   294,   293,     0,   281,   279,
       0,   289,     0,    37,   266,   265,   133,    22,     0,     0,
     203,     0,     0,     0,     0,    23,     0,     0,   283,   292,
     280,   291,    24,     0,     0,    25,     0,     0,    26,     0,
      27
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -569,  -569,  -569,   547,   -29,  -569,  -569,   249,  -569,  -569,
     167,   160,  -568,  -496,  -569,   165,  -505,  -372,   546,     1,
     512,   170,   373,    -2,    -1,   -34,   404,   -49,  -569,   456,
    -569,   447,    10,   -26,  -569,   457,  -569,   161,  -569,  -569,
     104,   -43,  -569,  -569,   354,   357,  -569,  -135,   -37,  -169,
    -569,  -165,  -206,  -569,   -22,   -42,  -569,   188,   -21,  -173,
     565,  -569,   417,  -569,   567,   -74,   893,  -569,   577,  -569,
    -569,  -569,  -569,  -569,  -569,   571
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,    35,    36,    37,    38,    39,    40,   603,    41,
     514,   515,   512,   513,    42,   524,   525,   249,   250,    43,
     119,   516,   256,   120,    59,    60,    61,    62,   144,   145,
     326,   327,    63,    45,   319,   320,   555,   556,   557,   624,
     625,    46,   149,   439,   440,   186,   187,   146,   308,   309,
     310,   311,   312,   124,   125,    47,   531,   532,   121,   297,
     298,   244,   245,   391,   366,   299,   258,   637,    69,   259,
     601,   260,   261,   374,   377,    66
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      58,    65,    96,   134,    93,   151,   257,   129,    73,    73,
     123,    44,   330,   331,   185,   140,   594,   328,   332,   605,
     188,   510,   494,   657,   595,    95,   148,   659,   696,   122,
     394,   591,   128,  -325,   621,   355,  -325,  -325,    20,    20,
      67,   403,    20,   622,   646,   403,    44,   483,   635,   141,
      20,   212,   230,  -325,   134,   401,    20,   360,   479,   361,
     390,   403,   233,   641,    20,    20,   140,   234,  -338,  -338,
     650,    20,   235,   231,   492,   493,   231,   599,    20,    73,
      20,   236,    93,   587,   233,   135,   588,   390,    71,   234,
     390,   390,   441,    20,   235,   623,   596,   237,   586,   185,
     596,   395,   592,   236,   549,   300,   134,   390,   435,   706,
     565,   301,   141,   142,   142,   304,   390,   143,   143,   237,
     253,   140,   239,   318,   254,   722,   255,    71,   723,    76,
     490,   303,   242,   491,   390,   208,   305,   243,   185,   185,
     190,   191,   253,   185,   239,    20,   254,   390,   255,   336,
     325,   604,   136,    20,   242,   642,   209,   605,   306,   243,
     192,   594,   390,   594,   367,   367,   185,   137,   349,   350,
     351,   390,   356,   486,   293,   383,   390,   384,   475,   398,
     423,   385,   402,   386,   387,   647,   388,   389,   337,   338,
     339,   328,   340,   341,   342,   343,   344,   345,   346,   347,
     348,    81,    82,    83,    84,    85,    86,    87,   357,   648,
     190,   191,    20,   629,    88,   720,   294,   390,    20,    25,
     476,    44,   398,   424,   390,   232,   550,   594,    89,   390,
     192,    20,    25,   314,    26,    20,    25,   131,   408,    20,
     150,   380,    20,   185,   138,   -59,   315,    26,   426,   410,
     597,   231,   545,   390,    90,   630,    25,   534,   134,   390,
     589,    71,   132,   590,   133,    91,    92,   550,   414,   130,
     639,    26,   413,   444,   718,   132,   134,   133,   390,   132,
     460,   133,   437,   409,   212,   412,   390,   330,   331,   415,
     416,   417,   482,   332,   621,   484,    20,   482,   367,   132,
     422,   133,    20,   622,   425,   190,   191,   190,   191,   616,
     190,   191,   434,   193,   194,   195,   196,   197,   198,   199,
     200,   190,   191,   257,   325,   192,   286,   192,   713,   129,
     192,   287,   295,   477,   496,   201,   390,   538,   263,   190,
     191,   192,   152,   126,   398,   211,   540,    20,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,   192,
      22,    23,    24,    25,   503,   719,   134,   740,    44,    20,
      20,   190,   191,   390,   717,   390,   607,   608,    26,   190,
     191,   390,   674,   704,   671,   190,   191,   185,   672,    27,
     185,   192,   210,   548,    73,   185,   551,   463,   536,   192,
     721,   188,    28,    29,   741,   192,    56,   390,    31,    20,
      32,   390,    33,   460,   460,   573,   289,   576,   290,   248,
     578,   543,   728,    34,   544,     1,     2,     3,    20,   190,
     191,   185,   185,   552,   190,   191,   123,   566,   567,   190,
     191,   736,   541,   190,   191,   402,   600,   573,   602,   192,
     539,   284,   614,   252,   192,   122,   333,   285,   631,   192,
      20,   264,   561,   192,     5,   562,     6,     7,     8,     9,
      10,   265,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    20,    22,    23,    24,    25,   233,   266,
      20,   190,   191,   234,   190,   191,   743,    20,   235,   381,
     190,   191,    26,   746,   726,   382,   369,   236,   611,   627,
     185,   192,   632,    27,   192,   707,   336,   190,   191,   431,
     192,   432,   421,   237,    73,   498,    28,    29,   285,   499,
      30,   500,    31,   612,    32,   613,    33,   192,   428,   468,
     202,   203,   204,   615,   429,   469,   253,    34,   239,   370,
     254,   663,   255,    72,    75,   288,   470,   664,   242,   285,
     471,   530,   268,   243,   559,    20,   684,   609,    20,   134,
     688,   610,    48,    64,    77,    79,    68,    70,    74,    74,
      78,    80,   472,   636,   473,   460,   460,    20,   269,   270,
     697,   566,   567,   205,   206,   207,   233,    94,   271,   272,
      97,   234,   273,    94,   127,    20,   235,    48,   670,   352,
     353,   354,   274,   364,   673,   236,   676,   677,   487,   139,
     488,   679,   147,   275,   276,   291,   416,   417,   189,   277,
     278,   237,   280,   281,   282,   283,   292,   316,   322,   404,
     405,   406,   420,    74,   407,   433,    74,   247,   430,   251,
     438,   262,   443,   201,   253,   365,   239,   467,   254,  -229,
     255,   474,   192,   698,   478,   700,   242,   480,   489,   390,
     702,   243,   703,   495,   511,    94,   520,   521,    81,    82,
      83,    84,    85,    86,    87,   522,   523,   537,   535,   547,
     558,    88,   563,   564,   606,    20,   628,   724,   640,   725,
     570,   262,   727,   644,   730,    89,   620,   649,    94,   633,
     307,   634,   643,   189,   313,   645,   651,   665,   680,   667,
     687,   321,   682,   329,   681,   683,   735,   233,   686,   699,
     233,    90,   234,   689,   742,   234,    20,   235,   701,    20,
     235,   745,    91,    92,   748,   711,   236,   750,   731,   236,
     732,   749,   189,   189,   313,   334,   660,   189,   358,   658,
     593,  -325,   237,   396,   662,   237,   383,   661,   384,   403,
     519,   436,   385,   466,   386,   387,   442,   388,   389,   262,
     189,   678,    48,   359,   708,   253,   481,   239,   238,   254,
     239,   255,   240,   554,   241,   553,   668,   242,     0,   497,
     242,   262,   243,   262,     0,   243,   371,   362,    70,   262,
     262,   375,   378,   379,   363,   390,     0,     0,   262,   393,
       0,    74,     0,   397,   262,   399,   400,   262,     0,     0,
       0,     0,     6,     7,     8,     9,    10,     0,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,     0,
      22,    23,    24,    25,     0,     0,     0,   189,     0,     0,
       0,    94,     0,     0,   233,     0,     0,   247,    26,   234,
       0,     0,     0,    20,   235,     0,     0,     0,   427,    27,
       0,   364,     0,   236,     0,     0,   147,     0,     0,     0,
     321,     0,    28,    29,   461,   462,    30,   329,    31,   237,
      32,     0,    33,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    34,     0,     0,     0,     0,     0,     0,
       0,     0,   418,   365,   239,     0,   240,     0,   241,    48,
       0,     0,     0,     0,   242,     0,     0,   262,     0,   243,
     262,     0,   262,   262,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   247,     0,   262,   262,    94,   262,   262,
     262,   262,   262,     0,   251,     0,   517,   518,   262,   246,
       0,     0,     0,     0,   262,   262,   262,   262,   533,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   262,
       0,   262,     0,     0,     0,     0,     0,     0,   546,     0,
       0,   189,     0,   313,   189,     0,     0,     0,     0,   189,
       0,   213,   214,   215,   216,   217,   218,   219,   220,   221,
     222,     0,   223,   224,   225,   226,     0,   461,   461,   462,
     560,     0,     0,     0,     0,   227,   228,     0,     0,     0,
     229,     0,     0,     0,   313,   189,   189,     0,     0,   262,
       0,     0,     0,     0,   262,   117,     0,   262,     0,   572,
     262,   262,   262,   118,   577,   262,   262,     0,     0,   262,
     583,   585,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,   598,     0,     0,
     262,   262,   262,   262,   251,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   233,     0,     0,     0,     0,   234,   617,     0,     0,
      20,   235,     0,     0,   189,     0,     0,     0,   626,     0,
     236,   368,     0,   373,   376,     0,     0,     0,     0,     0,
     392,   638,     0,     0,     0,     0,   237,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   262,   262,   262,
     262,   262,     0,   517,     0,     0,     0,   517,   517,   253,
       0,   239,   485,   254,     0,   255,     0,   666,     0,   533,
     669,   242,     0,     0,     0,     0,   243,     0,   675,   419,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   461,
     461,     0,     0,     0,   233,     0,     0,     0,     0,   234,
       0,   262,   262,    20,   235,   262,     0,   262,   262,   262,
     262,   262,   262,   236,     0,     0,     0,     0,     0,     0,
       0,     0,   598,     0,     0,   262,     0,     0,     0,   237,
       0,     0,     0,     0,     0,     0,   705,     0,     0,     0,
       0,   709,   262,     0,   262,     0,     0,   262,   262,     0,
     262,     0,   253,     0,   239,     0,   254,     0,   255,     0,
       0,     0,     0,     0,   242,   246,   729,   501,   502,   243,
     504,   505,   506,   508,   509,   233,     0,     0,     0,     0,
     234,   262,     0,   262,    20,   235,   526,   527,   528,   529,
     737,     0,   262,   262,   236,     0,     0,     0,   744,     0,
       0,   368,     0,     0,     0,   747,     0,     0,     0,     0,
     237,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   372,     0,   239,     0,   254,     0,   255,
       0,     0,     0,     0,     0,   242,     0,     0,   507,     0,
     243,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   568,     0,     0,     0,     0,   569,     0,     0,   571,
       0,     0,   574,   575,     0,     0,     0,   579,   580,     0,
       0,   581,   582,   584,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,     0,    22,    23,    24,    25,
       0,     0,     0,     0,     0,     0,     0,     0,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,    51,    27,     0,    52,     0,    53,
       0,     0,   114,   115,    54,     0,     0,   116,    28,    29,
      55,     0,    56,     0,    31,     0,    32,     0,    33,     0,
       0,    57,   117,     0,     0,     0,     0,     0,     0,    34,
     118,     0,     0,     0,     0,     0,     0,     0,     0,   652,
     653,   654,   655,   656,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,     0,    22,    23,
      24,    25,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,     0,   685,     0,    26,     0,     0,   690,
     691,   692,   693,   694,   695,    50,    51,    27,   233,    52,
       0,    53,     0,   234,     0,     0,    54,    20,   235,     0,
      28,    29,    55,   464,   323,     0,    31,   236,   324,   465,
      33,     0,     0,    57,   710,     0,   712,   618,     0,   714,
     715,    34,   716,   237,   169,   170,   171,   172,   173,   174,
     175,     0,   176,    20,   177,   178,    22,   179,   180,     0,
       0,   619,     0,     0,     0,     0,   296,     0,   239,     0,
     254,     0,   255,   733,     0,   734,     0,     0,   242,     0,
       0,     0,     0,   243,   738,   739,   445,   446,   447,   448,
     449,   450,   451,    18,   452,    20,   453,   178,    22,   454,
     455,    25,   181,     0,   182,     0,   183,     0,   184,     0,
     153,   154,   155,   156,   157,   158,    26,   159,   160,   161,
     108,   162,   163,   164,   165,   113,    51,    27,     0,    52,
       0,    53,     0,     0,   166,   167,    54,     0,     0,   168,
      28,    29,    55,     0,   456,     0,   457,     0,   458,     0,
     459,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,    34,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,     0,    22,    23,    24,    25,     0,     0,
       0,     0,     0,     0,     0,     0,   153,   154,   155,   156,
     157,   158,    26,   159,   160,   161,   108,   162,   163,   164,
     165,   113,    51,    27,     0,    52,     0,    53,     0,     0,
     166,   167,    54,     0,     0,   168,    28,    29,    55,     0,
      56,     0,    31,     0,    32,     0,    33,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,    34,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,     0,
      22,    23,    24,    25,     0,     0,   302,     0,     0,     0,
       0,     0,     0,     0,    49,     0,     0,     0,    26,     0,
       0,     0,     0,     0,     0,     0,     0,    50,    51,    27,
       0,    52,     0,    53,     0,     0,     0,     0,    54,     0,
       0,     0,    28,    29,    55,     0,    56,     0,    31,     0,
      32,     0,    33,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,    34,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,     0,    22,    23,    24,    25,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,     0,     0,     0,    26,     0,     0,     0,     0,     0,
       0,     0,     0,    50,    51,    27,     0,    52,     0,    53,
       0,     0,     0,     0,    54,     0,     0,     0,    28,    29,
      55,     0,    56,     0,    31,   411,    32,     0,    33,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,    34,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,     0,    22,    23,    24,    25,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,     0,     0,     0,
      26,     0,     0,     0,     0,     0,     0,     0,     0,    50,
      51,    27,     0,    52,     0,    53,     0,     0,     0,     0,
      54,     0,     0,     0,    28,    29,    55,     0,    56,     0,
      31,   542,    32,     0,    33,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,    34,   445,   446,   447,   448,
     449,   450,   451,    18,   452,    20,   453,   178,    22,   454,
     455,    25,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,     0,     0,     0,    26,     0,     0,     0,
       0,     0,     0,     0,     0,    50,    51,    27,     0,    52,
       0,    53,     0,     0,     0,     0,    54,     0,     0,     0,
      28,    29,    55,     0,   456,     0,   457,     0,   458,     0,
     459,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,    34,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,     0,    22,    23,    24,    25,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,     0,
       0,     0,    26,     0,     0,     0,     0,     0,     0,     0,
       0,    50,    51,    27,     0,    52,     0,    53,     0,     0,
       0,     0,    54,     0,     0,     0,    28,    29,    55,     0,
      56,     0,    31,     0,    32,     0,    33,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,    34,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,     0,
      22,    23,    24,    25,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,     0,     0,     0,    26,     0,
       0,     0,     0,     0,     0,     0,     0,    50,    51,    27,
       0,    52,     0,    53,     0,     0,     0,     0,    54,     0,
       0,     0,    28,    29,    55,     0,   323,     0,    31,     0,
     324,     0,    33,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,    34,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,     0,    22,    23,    24,    25,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,     0,     0,     0,    26,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    51,    27,     0,    52,     0,    53,
       0,     0,     0,     0,    54,     0,     0,     0,    28,    29,
      55,     0,    56,   267,    31,     0,    32,     0,    33,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    34,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,     0,    22,    23,    24,    25,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,     0,     0,     0,
      26,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      51,    27,     0,    52,     0,    53,     0,     0,     0,     0,
      54,     0,     0,     0,    28,    29,    55,     0,    56,   279,
      31,     0,    32,     0,    33,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    34,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,     0,    22,    23,
      24,    25,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,     0,     0,     0,    26,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    51,    27,     0,    52,
       0,    53,     0,     0,     0,     0,    54,     0,     0,     0,
      28,    29,    55,     0,    56,     0,    31,     0,    32,     0,
      33,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    34,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,     0,    22,    23,    24,    25,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    26,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    51,    27,     0,    52,     0,    53,     0,     0,
       0,     0,    54,     0,     0,     0,    28,    29,    55,     0,
      56,     0,    31,     0,    32,     0,    33,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    34,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,     0,
      22,    23,    24,    25,   233,     0,     0,     0,     0,   234,
       0,     0,     0,    20,   235,     0,     0,     0,    26,     0,
       0,     0,     0,   236,     0,     0,     0,     0,     0,    27,
       0,     0,     0,     0,   317,     0,     0,     0,     0,   237,
       0,     0,    28,    29,     0,     0,    56,     0,    31,     0,
      32,     0,    33,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   372,    34,   239,     0,   254,     0,   255,     0,
       0,     0,     0,     0,   242,     0,     0,     0,     0,   243,
     169,   170,   171,   172,   173,   174,   175,     0,   176,    20,
     177,   178,    22,   179,   180,   169,   170,   171,   172,   173,
     174,   175,     0,   176,    20,   177,   178,    22,   179,   180,
     335,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,     0,
     182,     0,   183,     0,   184,     0,     0,     0,     0,     0,
       0,     0,     0,   181,     0,   182,     0,   183,     0,   184
};

static const yytype_int16 yycheck[] =
{
       2,     3,    28,    45,    26,    54,    80,    36,     7,     8,
      31,     1,   181,   182,    57,    49,   512,   152,   183,   524,
      57,   393,    32,   591,     7,    27,    52,   595,     7,    31,
       7,     7,    34,    31,    15,   208,    31,    31,    23,    23,
       0,    39,    23,    24,    39,    39,    36,    41,    73,    50,
      23,    44,    36,    31,    96,     7,    23,   230,    51,   232,
      85,    39,    14,    41,    23,    23,   100,    19,    78,    79,
      39,    23,    24,    72,    78,    79,    75,     7,    23,    78,
      23,    33,   104,    34,    14,    26,    37,    85,    72,    19,
      85,    85,    77,    23,    24,    76,    79,    49,    36,   142,
      79,    78,    78,    33,    77,   131,   148,    85,    67,    31,
      77,   132,   113,    72,    72,   137,    85,    76,    76,    49,
      72,   155,    74,   149,    76,    39,    78,    72,   696,    72,
      39,   133,    84,    42,    85,    37,   138,    89,   181,   182,
      62,    63,    72,   186,    74,    23,    76,    85,    78,   186,
     152,   523,    29,    23,    84,    78,    58,   662,    36,    89,
      82,   657,    85,   659,   238,   239,   209,    44,   202,   203,
     204,    85,   209,    41,    36,    36,    85,    38,    36,   253,
      32,    42,   256,    44,    45,    41,    47,    48,   190,   191,
     192,   326,   193,   194,   195,   196,   197,   198,   199,   200,
     201,     6,     7,     8,     9,    10,    11,    12,   210,    41,
      62,    63,    23,    36,    19,    41,    78,    85,    23,    29,
      78,   211,   296,    75,    85,    36,   432,   723,    33,    85,
      82,    23,    29,    67,    44,    23,    29,    34,   264,    23,
      76,   243,    23,   286,    36,    37,    80,    44,    36,   286,
      31,   250,    36,    85,    59,    78,    29,    67,   300,    85,
      34,    72,    72,    37,    74,    70,    71,   473,   290,    37,
      77,    44,    77,   322,    73,    72,   318,    74,    85,    72,
     323,    74,   316,   285,    44,   287,    85,   456,   457,   291,
     292,   293,   366,   458,    15,   369,    23,   371,   372,    72,
     302,    74,    23,    24,   306,    62,    63,    62,    63,    36,
      62,    63,   314,    45,    46,    47,    48,    49,    50,    51,
      52,    62,    63,   397,   326,    82,    78,    82,    77,   358,
      82,    83,    89,   335,    89,    67,    85,    78,    61,    62,
      63,    82,    76,    19,   418,     7,   420,    23,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    82,
      26,    27,    28,    29,   386,    77,   408,    77,   358,    23,
      23,    62,    63,    85,    78,    85,    78,    79,    44,    62,
      63,    85,    36,    36,    75,    62,    63,   430,    79,    55,
     433,    82,    36,   430,   393,   438,   433,    80,    75,    82,
      78,   438,    68,    69,    78,    82,    72,    85,    74,    23,
      76,    85,    78,   456,   457,   489,    77,   491,    79,    30,
     494,   423,    36,    89,   426,     3,     4,     5,    23,    62,
      63,   474,   475,   435,    62,    63,   457,   474,   475,    62,
      63,    36,    75,    62,    63,   519,   520,   521,   522,    82,
      78,    73,    75,    30,    82,   457,    19,    79,    77,    82,
      23,    36,   464,    82,     6,   467,     8,     9,    10,    11,
      12,    73,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    23,    26,    27,    28,    29,    14,    73,
      23,    62,    63,    19,    62,    63,    36,    23,    24,    73,
      62,    63,    44,    36,    75,    79,    32,    33,   534,   558,
     553,    82,    80,    55,    82,    77,   553,    62,    63,    77,
      82,    79,    73,    49,   523,    72,    68,    69,    79,    76,
      72,    78,    74,   535,    76,   537,    78,    82,    73,    73,
      38,    39,    40,   545,    79,    79,    72,    89,    74,    75,
      76,    73,    78,     7,     8,    75,    75,    79,    84,    79,
      79,    19,    73,    89,    19,    23,   640,    32,    23,   611,
     644,    36,     1,     2,     9,    10,     5,     6,     7,     8,
       9,    10,    77,    19,    79,   628,   629,    23,    73,    73,
     664,   628,   629,    41,    42,    43,    14,    26,    73,    73,
      29,    19,    73,    32,    33,    23,    24,    36,   610,   205,
     206,   207,    73,    31,   616,    33,   618,   619,    77,    48,
      79,   623,    51,    73,    73,    36,   628,   629,    57,    73,
      73,    49,    73,    73,    73,    73,    36,    36,    65,    31,
      41,    39,    30,    72,    78,    36,    75,    76,    79,    78,
      78,    80,    35,    67,    72,    73,    74,    36,    76,    36,
      78,    36,    82,   665,    44,   667,    84,    73,    32,    85,
     672,    89,   674,    44,    13,   104,    79,    32,     6,     7,
       8,     9,    10,    11,    12,    32,    13,    36,    34,    73,
      66,    19,    73,    75,    32,    23,    36,   699,    32,   701,
      78,   130,   704,    32,   706,    33,    78,    32,   137,    78,
     139,    78,    78,   142,   143,    78,    32,    36,    32,    56,
      37,   150,    32,   152,    36,    36,   728,    14,    36,    36,
      14,    59,    19,    37,   736,    19,    23,    24,    36,    23,
      24,   743,    70,    71,   746,    75,    33,   749,    36,    33,
      36,    36,   181,   182,   183,   184,   596,   186,   211,   592,
     511,    31,    49,   251,   599,    49,    36,   597,    38,    39,
     397,   315,    42,   326,    44,    45,   319,    47,    48,   208,
     209,   620,   211,   212,   680,    72,    73,    74,    72,    76,
      74,    78,    76,   439,    78,   438,   608,    84,    -1,   382,
      84,   230,    89,   232,    -1,    89,   239,   236,   237,   238,
     239,   240,   241,   242,   237,    85,    -1,    -1,   247,   248,
      -1,   250,    -1,   252,   253,   254,   255,   256,    -1,    -1,
      -1,    -1,     8,     9,    10,    11,    12,    -1,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    -1,
      26,    27,    28,    29,    -1,    -1,    -1,   286,    -1,    -1,
      -1,   290,    -1,    -1,    14,    -1,    -1,   296,    44,    19,
      -1,    -1,    -1,    23,    24,    -1,    -1,    -1,   307,    55,
      -1,    31,    -1,    33,    -1,    -1,   315,    -1,    -1,    -1,
     319,    -1,    68,    69,   323,   324,    72,   326,    74,    49,
      76,    -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    -1,    76,    -1,    78,   358,
      -1,    -1,    -1,    -1,    84,    -1,    -1,   366,    -1,    89,
     369,    -1,   371,   372,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   382,    -1,   384,   385,   386,   387,   388,
     389,   390,   391,    -1,   393,    -1,   395,   396,   397,    76,
      -1,    -1,    -1,    -1,   403,   404,   405,   406,   407,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   418,
      -1,   420,    -1,    -1,    -1,    -1,    -1,    -1,   427,    -1,
      -1,   430,    -1,   432,   433,    -1,    -1,    -1,    -1,   438,
      -1,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    -1,    49,    50,    51,    52,    -1,   456,   457,   458,
     459,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      67,    -1,    -1,    -1,   473,   474,   475,    -1,    -1,   478,
      -1,    -1,    -1,    -1,   483,    82,    -1,   486,    -1,   488,
     489,   490,   491,    90,   493,   494,   495,    -1,    -1,   498,
     499,   500,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   511,    -1,    -1,    -1,    -1,   516,    -1,    -1,
     519,   520,   521,   522,   523,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    14,    -1,    -1,    -1,    -1,    19,   546,    -1,    -1,
      23,    24,    -1,    -1,   553,    -1,    -1,    -1,   557,    -1,
      33,   238,    -1,   240,   241,    -1,    -1,    -1,    -1,    -1,
     247,   570,    -1,    -1,    -1,    -1,    49,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   586,   587,   588,
     589,   590,    -1,   592,    -1,    -1,    -1,   596,   597,    72,
      -1,    74,    75,    76,    -1,    78,    -1,   606,    -1,   608,
     609,    84,    -1,    -1,    -1,    -1,    89,    -1,   617,   296,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   628,
     629,    -1,    -1,    -1,    14,    -1,    -1,    -1,    -1,    19,
      -1,   640,   641,    23,    24,   644,    -1,   646,   647,   648,
     649,   650,   651,    33,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   661,    -1,    -1,   664,    -1,    -1,    -1,    49,
      -1,    -1,    -1,    -1,    -1,    -1,   675,    -1,    -1,    -1,
      -1,   680,   681,    -1,   683,    -1,    -1,   686,   687,    -1,
     689,    -1,    72,    -1,    74,    -1,    76,    -1,    78,    -1,
      -1,    -1,    -1,    -1,    84,   382,   705,   384,   385,    89,
     387,   388,   389,   390,   391,    14,    -1,    -1,    -1,    -1,
      19,   720,    -1,   722,    23,    24,   403,   404,   405,   406,
     729,    -1,   731,   732,    33,    -1,    -1,    -1,   737,    -1,
      -1,   418,    -1,    -1,    -1,   744,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    74,    -1,    76,    -1,    78,
      -1,    -1,    -1,    -1,    -1,    84,    -1,    -1,    87,    -1,
      89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   478,    -1,    -1,    -1,    -1,   483,    -1,    -1,   486,
      -1,    -1,   489,   490,    -1,    -1,    -1,   494,   495,    -1,
      -1,   498,   499,   500,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    -1,    26,    27,    28,    29,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    -1,    57,    -1,    59,
      -1,    -1,    62,    63,    64,    -1,    -1,    67,    68,    69,
      70,    -1,    72,    -1,    74,    -1,    76,    -1,    78,    -1,
      -1,    81,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   586,
     587,   588,   589,   590,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    -1,    26,    27,
      28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    40,    -1,   641,    -1,    44,    -1,    -1,   646,
     647,   648,   649,   650,   651,    53,    54,    55,    14,    57,
      -1,    59,    -1,    19,    -1,    -1,    64,    23,    24,    -1,
      68,    69,    70,    71,    72,    -1,    74,    33,    76,    77,
      78,    -1,    -1,    81,   681,    -1,   683,     7,    -1,   686,
     687,    89,   689,    49,    14,    15,    16,    17,    18,    19,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    -1,
      -1,    31,    -1,    -1,    -1,    -1,    72,    -1,    74,    -1,
      76,    -1,    78,   720,    -1,   722,    -1,    -1,    84,    -1,
      -1,    -1,    -1,    89,   731,   732,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    72,    -1,    74,    -1,    76,    -1,    78,    -1,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    -1,    57,
      -1,    59,    -1,    -1,    62,    63,    64,    -1,    -1,    67,
      68,    69,    70,    -1,    72,    -1,    74,    -1,    76,    -1,
      78,    -1,    -1,    81,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    -1,    26,    27,    28,    29,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    -1,    57,    -1,    59,    -1,    -1,
      62,    63,    64,    -1,    -1,    67,    68,    69,    70,    -1,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    -1,    81,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    -1,
      26,    27,    28,    29,    -1,    -1,    32,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    40,    -1,    -1,    -1,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,    54,    55,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    -1,
      -1,    -1,    68,    69,    70,    -1,    72,    -1,    74,    -1,
      76,    -1,    78,    -1,    -1,    81,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    -1,    26,    27,    28,    29,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      40,    -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    53,    54,    55,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    68,    69,
      70,    -1,    72,    -1,    74,    75,    76,    -1,    78,    -1,
      -1,    81,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    -1,    26,    27,    28,    29,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    40,    -1,    -1,    -1,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,
      54,    55,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    -1,    -1,    -1,    68,    69,    70,    -1,    72,    -1,
      74,    75,    76,    -1,    78,    -1,    -1,    81,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    53,    54,    55,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,
      68,    69,    70,    -1,    72,    -1,    74,    -1,    76,    -1,
      78,    -1,    -1,    81,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    -1,    26,    27,    28,    29,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,    -1,
      -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    53,    54,    55,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    -1,    -1,    -1,    68,    69,    70,    -1,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    -1,    81,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    -1,
      26,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    40,    -1,    -1,    -1,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,    54,    55,
      -1,    57,    -1,    59,    -1,    -1,    -1,    -1,    64,    -1,
      -1,    -1,    68,    69,    70,    -1,    72,    -1,    74,    -1,
      76,    -1,    78,    -1,    -1,    81,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    -1,    26,    27,    28,    29,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      40,    -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    54,    55,    -1,    57,    -1,    59,
      -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    68,    69,
      70,    -1,    72,    73,    74,    -1,    76,    -1,    78,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    -1,    26,    27,    28,    29,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    40,    -1,    -1,    -1,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      54,    55,    -1,    57,    -1,    59,    -1,    -1,    -1,    -1,
      64,    -1,    -1,    -1,    68,    69,    70,    -1,    72,    73,
      74,    -1,    76,    -1,    78,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    -1,    26,    27,
      28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    40,    -1,    -1,    -1,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    54,    55,    -1,    57,
      -1,    59,    -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,
      68,    69,    70,    -1,    72,    -1,    74,    -1,    76,    -1,
      78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    89,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    -1,    26,    27,    28,    29,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    54,    55,    -1,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    -1,    -1,    -1,    68,    69,    70,    -1,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    -1,
      26,    27,    28,    29,    14,    -1,    -1,    -1,    -1,    19,
      -1,    -1,    -1,    23,    24,    -1,    -1,    -1,    44,    -1,
      -1,    -1,    -1,    33,    -1,    -1,    -1,    -1,    -1,    55,
      -1,    -1,    -1,    -1,    60,    -1,    -1,    -1,    -1,    49,
      -1,    -1,    68,    69,    -1,    -1,    72,    -1,    74,    -1,
      76,    -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    89,    74,    -1,    76,    -1,    78,    -1,
      -1,    -1,    -1,    -1,    84,    -1,    -1,    -1,    -1,    89,
      14,    15,    16,    17,    18,    19,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    14,    15,    16,    17,    18,
      19,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      74,    -1,    76,    -1,    78,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    74,    -1,    76,    -1,    78
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,    92,     6,     8,     9,    10,    11,
      12,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    26,    27,    28,    29,    44,    55,    68,    69,
      72,    74,    76,    78,    89,    93,    94,    95,    96,    97,
      98,   100,   105,   110,   123,   124,   132,   146,   166,    40,
      53,    54,    57,    59,    64,    70,    72,    81,   114,   115,
     116,   117,   118,   123,   166,   114,   166,     0,   166,   159,
     166,    72,   109,   110,   166,   109,    72,   151,   166,   151,
     166,     6,     7,     8,     9,    10,    11,    12,    19,    33,
      59,    70,    71,   145,   166,   114,   124,   166,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    62,    63,    67,    82,    90,   111,
     114,   149,   114,   149,   144,   145,    19,   166,   114,    95,
      37,    34,    72,    74,   146,    26,    29,    44,    36,   166,
     116,   115,    72,    76,   119,   120,   138,   166,   124,   133,
      76,   118,    76,    38,    39,    40,    41,    42,    43,    45,
      46,    47,    49,    50,    51,    52,    62,    63,    67,    14,
      15,    16,    17,    18,    19,    20,    22,    24,    25,    27,
      28,    72,    74,    76,    78,   132,   136,   137,   139,   166,
      62,    63,    82,    45,    46,    47,    48,    49,    50,    51,
      52,    67,    38,    39,    40,    41,    42,    43,    37,    58,
      36,     7,    44,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    49,    50,    51,    52,    62,    63,    67,
      36,   110,    36,    14,    19,    24,    33,    49,    72,    74,
      76,    78,    84,    89,   152,   153,   157,   166,    30,   108,
     109,   166,    30,    72,    76,    78,   113,   156,   157,   160,
     162,   163,   166,    61,    36,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    79,    78,    83,    75,    77,
      79,    36,    36,    36,    78,    89,    72,   150,   151,   156,
     124,   149,    32,   114,   145,   114,    36,   166,   139,   140,
     141,   142,   143,   166,    67,    80,    36,    60,   124,   125,
     126,   166,    65,    72,    76,   114,   121,   122,   138,   166,
     140,   140,   142,    19,   166,    44,   139,   114,   114,   114,
     115,   115,   115,   115,   115,   115,   115,   115,   115,   116,
     116,   116,   117,   117,   117,   150,   139,   114,    94,   166,
     150,   150,   166,   159,    31,    73,   155,   156,   157,    32,
      75,   155,    72,   157,   164,   166,   157,   165,   166,   166,
     114,    73,    79,    36,    38,    42,    44,    45,    47,    48,
      85,   154,   157,   166,     7,    78,   111,   166,   156,   166,
     166,     7,   156,    39,    31,    41,    39,    78,   124,   114,
     139,    75,   114,    77,   145,   114,   114,   114,    72,   157,
      30,    73,   114,    32,    75,   114,    36,   166,    73,    79,
      79,    77,    79,    36,   114,    67,   120,   116,    78,   134,
     135,    77,   126,    35,   118,    14,    15,    16,    17,    18,
      19,    20,    22,    24,    27,    28,    72,    74,    76,    78,
     132,   166,   166,    80,    71,    77,   122,    36,    73,    79,
      75,    79,    77,    79,    36,    36,    78,   114,    44,    51,
      73,    73,   156,    41,   156,    75,    41,    77,    79,    32,
      39,    42,    78,    79,    32,    44,    89,   153,    72,    76,
      78,   157,   157,   145,   157,   157,   157,    87,   157,   157,
     108,    13,   103,   104,   101,   102,   112,   166,   166,   113,
      79,    32,    32,    13,   106,   107,   157,   157,   157,   157,
      19,   147,   148,   166,    67,    34,    75,    36,    78,    78,
     156,    75,    75,   114,   114,    36,   166,    73,   139,    77,
     143,   139,   114,   136,   135,   127,   128,   129,    66,    19,
     166,   114,   114,    73,    75,    77,   139,   139,   157,   157,
      78,   157,   166,   156,   157,   157,   156,   166,   156,   157,
     157,   157,   157,   166,   157,   166,    36,    34,    37,    34,
      37,     7,    78,    98,   104,     7,    79,    31,   166,     7,
     156,   161,   156,    99,   108,   107,    32,    78,    79,    32,
      36,   124,   114,   114,    75,   114,    36,   166,     7,    31,
      78,    15,    24,    76,   130,   131,   166,   118,    36,    36,
      78,    77,    80,    78,    78,    73,    19,   158,   166,    77,
      32,    41,    78,    78,    32,    78,    39,    41,    41,    32,
      39,    32,   157,   157,   157,   157,   157,   103,   101,   103,
     102,   112,   106,    73,    79,    36,   166,    56,   148,   166,
     114,    75,    79,   114,    36,   166,   114,   114,   128,   114,
      32,    36,    32,    36,   156,   157,    36,    37,   156,    37,
     157,   157,   157,   157,   157,   157,     7,   156,   114,    36,
     114,    36,   114,   114,    36,   166,    31,    77,   131,   166,
     157,    75,   157,    77,   157,   157,   157,    78,    73,    77,
      41,    78,    39,   103,   114,   114,    75,   114,    36,   166,
     114,    36,    36,   157,   157,   114,    36,   166,   157,   157,
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
     115,   115,   115,   115,   115,   115,   115,   115,   115,   115,
     116,   116,   116,   116,   116,   117,   117,   117,   117,   118,
     118,   118,   118,   118,   118,   118,   118,   118,   118,   119,
     119,   120,   121,   121,   122,   122,   123,   123,   124,   124,
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
       1,     2,     1,     2,     4,     2,     3,     3,     3,     1,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     1,
       3,     3,     3,     2,     1,     3,     3,     3,     1,     6,
       1,     4,     5,     4,     3,     4,     4,     6,     3,     3,
       1,     3,     2,     1,     4,     2,     3,     1,     4,     1,
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
#line 506 "hexpr.y" /* yacc.c:1646  */
    { yyParsedModule = (yyvsp[0].module);                     }
#line 2651 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 3:
#line 507 "hexpr.y" /* yacc.c:1646  */
    { yyParsedVar    = *(yyvsp[-2].string); yyParsedExpr = (yyvsp[0].exp); }
#line 2657 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 4:
#line 508 "hexpr.y" /* yacc.c:1646  */
    { yyParsedVar    = "";  yyParsedExpr = (yyvsp[0].exp); }
#line 2663 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 5:
#line 509 "hexpr.y" /* yacc.c:1646  */
    { yyParsedExpr   = (yyvsp[0].exp);                     }
#line 2669 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 6:
#line 512 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = new Module(*(yyvsp[-2].string), *(yyvsp[0].mdefs)); }
#line 2675 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 7:
#line 513 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = new Module(freshName(), *(yyvsp[0].mdefs)); }
#line 2681 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 8:
#line 515 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); }
#line 2687 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 9:
#line 516 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2693 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 10:
#line 517 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = (yyvsp[-1].mdefs);                            (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2699 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 11:
#line 519 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2705 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 12:
#line 520 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2711 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 13:
#line 521 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mvtydef); }
#line 2717 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 14:
#line 522 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2723 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 15:
#line 523 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2729 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 16:
#line 525 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2735 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 17:
#line 526 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 2741 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 18:
#line 527 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 2747 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 19:
#line 528 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 2753 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 20:
#line 529 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 2759 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 21:
#line 530 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 2765 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 22:
#line 531 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 2771 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 23:
#line 532 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-9]), (yylsp[0]))); }
#line 2777 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 24:
#line 533 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-10]), (yylsp[0]))); }
#line 2783 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 25:
#line 534 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-11]), (yylsp[0]))); }
#line 2789 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 26:
#line 535 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-12]), (yylsp[0]))); }
#line 2795 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 27:
#line 536 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-13]), (yylsp[0]))); }
#line 2801 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 28:
#line 539 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(freshName()), let(freshName(), ExprPtr((yyvsp[0].exp)), mktunit(m((yylsp[0]))), m((yylsp[0]))), m((yylsp[0]))); }
#line 2807 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 29:
#line 542 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MImport(yyModulePath, *(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 2813 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 30:
#line 545 "hexpr.y" /* yacc.c:1646  */
    { MTypeDef* td = new MTypeDef(MTypeDef::Transparent, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), forceMonotype(QualTypePtr((yyvsp[0].qualtype)), m((yylsp[0]))), m((yylsp[-3]), (yylsp[0]))); yyParseCC->defineTypeAlias(td->name(), td->arguments(), td->type()); (yyval.mdef) = td; }
#line 2819 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 31:
#line 546 "hexpr.y" /* yacc.c:1646  */
    { MTypeDef* td = new MTypeDef(MTypeDef::Opaque, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), forceMonotype(QualTypePtr((yyvsp[0].qualtype)), m((yylsp[0]))), m((yylsp[-3]), (yylsp[0]))); yyParseCC->defineNamedType(td->name(), td->arguments(), td->type()); (yyval.mdef) = td; }
#line 2825 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 32:
#line 549 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydef) = new MVarTypeDef(*(yyvsp[-2].string), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]), (yylsp[0]))); }
#line 2831 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 33:
#line 551 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdef) = new MVarDef(*(yyvsp[-2].strings), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2837 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 34:
#line 554 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-3].tconstraints), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2843 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 35:
#line 555 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-6]), (yylsp[0]))); wantIndent(false); }
#line 2849 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 36:
#line 556 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2855 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 37:
#line 557 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-7].tconstraints), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-8]), (yylsp[0])));            wantIndent(false); }
#line 2861 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 38:
#line 558 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2867 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 39:
#line 559 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2873 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 40:
#line 560 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-4]), (yylsp[0])));            wantIndent(false); }
#line 2879 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 41:
#line 561 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2885 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 42:
#line 563 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundeps) = autorelease(new CFunDepDefs()); (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2891 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 43:
#line 564 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundeps) = (yyvsp[-2].fundeps);                             (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2897 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 44:
#line 566 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundep) = autorelease(new CFunDepDef(*(yyvsp[-2].strings), *(yyvsp[0].strings))); }
#line 2903 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 45:
#line 568 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydefs) = autorelease(new MVarTypeDefs()); (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2909 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 46:
#line 569 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydefs) = (yyvsp[-1].mvtydefs);                              (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2915 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 47:
#line 571 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydef) = (yyvsp[0].mvtydef); }
#line 2921 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 48:
#line 574 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2927 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 49:
#line 575 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-3].tconstraints),           *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2933 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 50:
#line 576 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-4]), (yylsp[0])));        wantIndent(false); }
#line 2939 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 51:
#line 577 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-5].tconstraints),           *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-6]), (yylsp[0])));        wantIndent(false); }
#line 2945 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 52:
#line 579 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdefs) = autorelease(new MVarDefs()); (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2951 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 53:
#line 580 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdefs) = (yyvsp[-1].mvdefs);                          (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2957 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 54:
#line 582 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdef) = (yyvsp[0].mvdef); }
#line 2963 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 55:
#line 585 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[0].strings); }
#line 2969 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 56:
#line 587 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[-1].string)); (yyval.strings)->push_back(*(yyvsp[-2].string)); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 2975 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 57:
#line 589 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 2981 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 58:
#line 590 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 2987 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 59:
#line 592 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 2993 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 60:
#line 594 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[-1].string); }
#line 2999 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 61:
#line 596 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("and")); }
#line 3005 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 62:
#line 597 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("or")); }
#line 3011 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 63:
#line 598 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3017 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 64:
#line 599 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3023 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 65:
#line 600 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("~")); }
#line 3029 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 66:
#line 601 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("=~")); }
#line 3035 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 67:
#line 602 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("===")); }
#line 3041 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 68:
#line 603 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("==")); }
#line 3047 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 69:
#line 604 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("<")); }
#line 3053 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 70:
#line 605 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("<=")); }
#line 3059 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 71:
#line 606 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(">")); }
#line 3065 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 72:
#line 607 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(">=")); }
#line 3071 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 73:
#line 608 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("in")); }
#line 3077 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 74:
#line 609 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("append")); }
#line 3083 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 75:
#line 610 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("+")); }
#line 3089 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 76:
#line 611 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("-")); }
#line 3095 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 77:
#line 612 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("*")); }
#line 3101 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 78:
#line 613 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("/")); }
#line 3107 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 79:
#line 614 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("%")); }
#line 3113 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 80:
#line 616 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3119 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 81:
#line 617 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3125 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 82:
#line 619 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3131 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 83:
#line 620 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes);                           (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3137 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 84:
#line 623 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3143 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 85:
#line 624 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), (yyvsp[0].exp), m((yylsp[-1]),(yylsp[0]))); }
#line 3149 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 86:
#line 625 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("and",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3155 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 87:
#line 626 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("or",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3161 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 88:
#line 627 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("compose",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3167 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 89:
#line 628 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3173 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 90:
#line 630 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("~",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3179 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 91:
#line 631 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("===",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3185 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 92:
#line 632 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3191 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 93:
#line 633 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))), m((yylsp[-2]),(yylsp[0]))); }
#line 3197 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 94:
#line 634 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("<",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3203 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 95:
#line 635 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("<=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3209 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 96:
#line 636 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var(">",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3215 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 97:
#line 637 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var(">=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3221 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 98:
#line 638 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("in",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3227 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 99:
#line 639 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3233 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 100:
#line 641 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("+",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3239 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 101:
#line 642 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("-",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3245 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 102:
#line 643 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("append",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3251 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 103:
#line 644 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("neg",m((yylsp[-1]))), ExprPtr((yyvsp[0].exp)), m((yylsp[-1]),(yylsp[0]))); }
#line 3257 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 104:
#line 645 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3263 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 105:
#line 647 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("*", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3269 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 106:
#line 648 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("/", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3275 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 107:
#line 649 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("%", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3281 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 108:
#line 650 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3287 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 109:
#line 652 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP3(var("if",m((yylsp[-5]))), (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-5]), (yylsp[0]))); }
#line 3293 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 110:
#line 653 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3299 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 111:
#line 656 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-2].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3305 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 112:
#line 657 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]),(yylsp[0])))->clone(); }
#line 3311 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 113:
#line 660 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileMatch(yyParseCC, *(yyvsp[-2].exps), normPatternRules(*(yyvsp[0].patternexps), m((yylsp[-3]),(yylsp[0]))), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3317 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 114:
#line 663 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileMatchTest(yyParseCC, ExprPtr((yyvsp[-2].exp)), PatternPtr((yyvsp[0].pattern)), m((yylsp[-2]),(yylsp[0])))->clone(); }
#line 3323 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 115:
#line 666 "hexpr.y" /* yacc.c:1646  */
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
#line 3338 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 116:
#line 678 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-1].letbindings), ExprPtr(new Unit(m((yylsp[-3]),(yylsp[0])))), m((yylsp[-3]),(yylsp[0]))); }
#line 3344 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 117:
#line 679 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[-1].exp)), m((yylsp[-5]),(yylsp[0]))); }
#line 3350 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 118:
#line 682 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assump(ExprPtr((yyvsp[-2].exp)), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]),(yylsp[0]))); }
#line 3356 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 119:
#line 684 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].letbindings)->push_back(*(yyvsp[0].letbinding)); (yyval.letbindings) = (yyvsp[-2].letbindings); }
#line 3362 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 120:
#line 685 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3368 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 121:
#line 687 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-2].pattern)), ExprPtr((yyvsp[0].exp)))); }
#line 3374 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 122:
#line 689 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = (yyvsp[-1].letbindings); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3380 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 123:
#line 690 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3386 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 124:
#line 692 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)))); }
#line 3392 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 125:
#line 693 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr(new MatchAny("_",m((yylsp[-1])))), ExprPtr((yyvsp[-1].exp)))); }
#line 3398 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 126:
#line 696 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assign(ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3404 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 127:
#line 697 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3410 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 128:
#line 699 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].exps), m((yylsp[-3]), (yylsp[0]))); }
#line 3416 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 129:
#line 700 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = varCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3422 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 130:
#line 703 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("range", m((yylsp[-2]))), list(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]), (yylsp[0]))); }
#line 3428 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 131:
#line 704 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("iterateS", m((yylsp[-1]))), list(ExprPtr((yyvsp[-2].exp)), fn(str::strings(".x"), fncall(var("+", m((yylsp[-1]))), list(var(".x", m((yylsp[-1]))), ExprPtr(new Int(1, m((yylsp[-1]))))), m((yylsp[-1]))), m((yylsp[-1])))), m((yylsp[-3]), (yylsp[0]))); }
#line 3434 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 132:
#line 705 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileArrayComprehension(ExprPtr((yyvsp[-5].exp)), PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 3440 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 133:
#line 706 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileArrayComprehension(ExprPtr((yyvsp[-7].exp)), PatternPtr((yyvsp[-5].pattern)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 3446 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 134:
#line 707 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkArray(*(yyvsp[-1].exps), m((yylsp[-2]), (yylsp[0]))); }
#line 3452 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 135:
#line 708 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new AIndex(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3458 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 136:
#line 709 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("slice", m((yylsp[-2]))), list(ExprPtr((yyvsp[-5].exp)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-5]), (yylsp[0]))); }
#line 3464 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 137:
#line 710 "hexpr.y" /* yacc.c:1646  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-1]))), list(var(vn,m((yylsp[-4]))), ExprPtr((yyvsp[-2].exp)), fncall(var("size",m((yylsp[-1]))), list(var(vn,m((yylsp[-4])))),m((yylsp[-4])))),m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3470 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 138:
#line 711 "hexpr.y" /* yacc.c:1646  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-2]))), list(var(vn,m((yylsp[-4]))), fncall(var("size",m((yylsp[-2]))), list(var(vn,m((yylsp[-2])))),m((yylsp[-4]))), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3476 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 139:
#line 714 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-3].string), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3482 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 140:
#line 715 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(".f" + str::from((yyvsp[-3].intv)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3488 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 141:
#line 716 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-1].string), ExprPtr(new Unit(m((yylsp[-1])))), m((yylsp[-2]), (yylsp[0]))); }
#line 3494 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 142:
#line 717 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-4].exp)), *(yyvsp[-1].vfields), m((yylsp[-5]), (yylsp[0]))); }
#line 3500 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 143:
#line 718 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-6].exp)), *(yyvsp[-3].vfields), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3506 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 144:
#line 721 "hexpr.y" /* yacc.c:1646  */
    { if ((yyvsp[-1].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-1].rfields), m((yylsp[-2]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-2]), (yylsp[0]))); } }
#line 3512 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 145:
#line 722 "hexpr.y" /* yacc.c:1646  */
    { if ((yyvsp[-2].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-2].rfields), m((yylsp[-3]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-3]), (yylsp[0]))); } }
#line 3518 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 146:
#line 723 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makeProjSeq((yyvsp[-1].exp), *(yyvsp[0].strings), m((yylsp[-1]), (yylsp[0]))); }
#line 3524 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 147:
#line 726 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Fn(str::strings("x"), proj(var("x", m((yylsp[0]))), *(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3530 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 148:
#line 729 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Pack(ExprPtr((yyvsp[0].exp)), m((yylsp[-1]), (yylsp[0]))); }
#line 3536 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 149:
#line 730 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Unpack(*(yyvsp[-4].string), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3542 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 150:
#line 733 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Bool((yyvsp[0].boolv), m((yylsp[0]))); }
#line 3548 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 151:
#line 734 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3554 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 152:
#line 735 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3560 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 153:
#line 736 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3566 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 154:
#line 737 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Short((yyvsp[0].shortv), m((yylsp[0]))); }
#line 3572 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 155:
#line 738 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Int((yyvsp[0].intv), m((yylsp[0]))); }
#line 3578 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 156:
#line 739 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Long((yyvsp[0].longv), m((yylsp[0]))); }
#line 3584 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 157:
#line 740 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Float((yyvsp[0].floatv), m((yylsp[0]))); }
#line 3590 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 158:
#line 741 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Double((yyvsp[0].doublev), m((yylsp[0]))); }
#line 3596 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 159:
#line 742 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3602 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 160:
#line 743 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0])))->clone(); }
#line 3608 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 161:
#line 744 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3614 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 162:
#line 745 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3620 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 163:
#line 748 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = pickNestedExp((yyvsp[-1].exps), m((yylsp[-2]),(yylsp[0]))); }
#line 3626 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 164:
#line 751 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("append", m((yylsp[-1]))); }
#line 3632 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 165:
#line 752 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("+",      m((yylsp[-1]))); }
#line 3638 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 166:
#line 753 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("-",      m((yylsp[-1]))); }
#line 3644 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 167:
#line 754 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("*",      m((yylsp[-1]))); }
#line 3650 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 168:
#line 755 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("/",      m((yylsp[-1]))); }
#line 3656 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 169:
#line 756 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("%",      m((yylsp[-1]))); }
#line 3662 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 170:
#line 757 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("~",      m((yylsp[-1]))); }
#line 3668 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 171:
#line 758 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("===",    m((yylsp[-1]))); }
#line 3674 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 172:
#line 759 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("==",     m((yylsp[-1]))); }
#line 3680 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 173:
#line 760 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("!=",     m((yylsp[-1]))); }
#line 3686 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 174:
#line 761 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("<",      m((yylsp[-1]))); }
#line 3692 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 175:
#line 762 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var(">",      m((yylsp[-1]))); }
#line 3698 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 176:
#line 763 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var(">=",     m((yylsp[-1]))); }
#line 3704 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 177:
#line 764 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("<=",     m((yylsp[-1]))); }
#line 3710 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 178:
#line 765 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("and",    m((yylsp[-1]))); }
#line 3716 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 179:
#line 766 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("or",     m((yylsp[-1]))); }
#line 3722 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 180:
#line 767 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("in",     m((yylsp[-1]))); }
#line 3728 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 181:
#line 768 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("not",    m((yylsp[-1]))); }
#line 3734 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 182:
#line 771 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assump(fncall(var("unsafeCast", m((yylsp[-1]))), list(mktunit(m((yylsp[-1])))), m((yylsp[-1]))), qualtype(tapp(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp)))))), m((yylsp[-1]))); }
#line 3740 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 183:
#line 773 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prules) = (yyvsp[-1].prules); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3746 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 184:
#line 774 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prules) = autorelease(new Grammar()); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3752 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 185:
#line 776 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prule) = autorelease(new Grammar::value_type(*(yyvsp[-2].string), *(yyvsp[0].prdefs))); }
#line 3758 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 186:
#line 778 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdefs) = (yyvsp[-2].prdefs); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3764 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 187:
#line 779 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdefs) = autorelease(new GrammarRules()); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3770 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 188:
#line 781 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdef) = autorelease(new GrammarRule(*(yyvsp[-3].pbelems), ExprPtr((yyvsp[-1].exp)))); }
#line 3776 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 189:
#line 783 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelems) = (yyvsp[-1].pbelems); (yyval.pbelems)->push_back(*(yyvsp[0].pbelem)); }
#line 3782 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 190:
#line 784 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelems) = autorelease(new BoundGrammarValues()); }
#line 3788 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 191:
#line 786 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue(*(yyvsp[-2].string), GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3794 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 192:
#line 787 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue("_", GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3800 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 193:
#line 789 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GSymRef(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3806 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 194:
#line 790 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GStr(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3812 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 195:
#line 791 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GStr(std::string(1, str::readCharDef(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3818 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 196:
#line 793 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3824 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 197:
#line 794 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3830 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 198:
#line 796 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = (yyvsp[-1].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3836 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 199:
#line 797 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3842 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 200:
#line 799 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexps) = (yyvsp[-1].patternexps); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3848 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 201:
#line 800 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexps) = autorelease(new PatternRows()); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3854 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 202:
#line 802 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)))); }
#line 3860 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 203:
#line 803 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-4].patterns), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)))); }
#line 3866 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 204:
#line 806 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[-1].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3872 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 205:
#line 807 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = autorelease(new Patterns()); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3878 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 206:
#line 809 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Bool((yyvsp[0].boolv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3884 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 207:
#line 810 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3890 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 208:
#line 811 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3896 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 209:
#line 812 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Short((yyvsp[0].shortv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3902 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 210:
#line 813 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int((yyvsp[0].intv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3908 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 211:
#line 814 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Long((yyvsp[0].longv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3914 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 212:
#line 815 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Double((yyvsp[0].doublev), m((yylsp[0])))), m((yylsp[0]))); }
#line 3920 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 213:
#line 816 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = mkpatarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3926 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 214:
#line 817 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = mkpatarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3932 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 215:
#line 818 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkTimespanPrim(*(yyvsp[0].strings), m((yylsp[0]))), mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3938 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 216:
#line 819 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 3944 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 217:
#line 820 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkDateTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 3950 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 218:
#line 821 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRegex(std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0]))); }
#line 3956 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 219:
#line 822 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 3962 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 220:
#line 823 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 3968 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 221:
#line 824 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-1].string), PatternPtr(new MatchLiteral(PrimitivePtr(new Unit(m((yylsp[-1])))), m((yylsp[-1])))), m((yylsp[-2]),(yylsp[0]))); }
#line 3974 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 222:
#line 825 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-3].string), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 3980 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 223:
#line 826 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(".f" + str::from((yyvsp[-3].intv)), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 3986 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 224:
#line 827 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 3992 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 225:
#line 828 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 3998 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 226:
#line 829 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4004 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 227:
#line 830 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4010 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 228:
#line 831 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = patVarCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4016 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 229:
#line 833 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchAny(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4022 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 230:
#line 834 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4028 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 231:
#line 835 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4034 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 232:
#line 836 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4040 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 233:
#line 837 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4046 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 234:
#line 839 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = (yyvsp[0].pattern); }
#line 4052 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 235:
#line 841 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[0].patterns); }
#line 4058 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 236:
#line 842 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = new Patterns(); }
#line 4064 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 237:
#line 844 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[-2].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4070 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 238:
#line 845 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = new Patterns(); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4076 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 239:
#line 847 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfields) = (yyvsp[-2].recpatfields); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4082 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 240:
#line 848 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfields) = new MatchRecord::Fields(); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4088 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 241:
#line 850 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfield) = new MatchRecord::Field(*(yyvsp[-2].string), PatternPtr((yyvsp[0].pattern))); }
#line 4094 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 242:
#line 852 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); }
#line 4100 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 243:
#line 853 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4106 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 244:
#line 854 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = (yyvsp[-4].rfields);                                     (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4112 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 245:
#line 856 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4118 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 246:
#line 857 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("data")); }
#line 4124 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 247:
#line 858 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("type")); }
#line 4130 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 248:
#line 859 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("where")); }
#line 4136 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 249:
#line 860 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("class")); wantIndent(false); }
#line 4142 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 250:
#line 861 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("instance")); wantIndent(false); }
#line 4148 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 251:
#line 862 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("exists")); }
#line 4154 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 252:
#line 863 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("import")); }
#line 4160 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 253:
#line 864 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("module")); }
#line 4166 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 254:
#line 865 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("parse")); }
#line 4172 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 255:
#line 866 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("do")); }
#line 4178 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 256:
#line 867 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("return")); }
#line 4184 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 257:
#line 868 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(".f" + str::from((yyvsp[0].intv)))); }
#line 4190 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 258:
#line 870 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-2].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4196 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 259:
#line 871 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings); str::seq x = tupSectionFields(*(yyvsp[0].string)); (yyval.strings)->insert((yyval.strings)->end(), x.begin(), x.end()); }
#line 4202 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 260:
#line 872 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4208 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 261:
#line 873 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); *(yyval.strings) = tupSectionFields(*(yyvsp[0].string)); }
#line 4214 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 262:
#line 875 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vfields) = autorelease(new Case::Bindings()); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4220 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 263:
#line 876 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vfields) = (yyvsp[-2].vfields); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4226 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 264:
#line 878 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-2].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4232 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 265:
#line 879 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-4].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4238 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 266:
#line 880 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(".f" + str::from((yyvsp[-4].intv)), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4244 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 267:
#line 882 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); }
#line 4250 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 268:
#line 883 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4256 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 269:
#line 884 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].exps)->push_back(ExprPtr((yyvsp[0].exp))); (yyval.exps) = (yyvsp[-2].exps); }
#line 4262 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 270:
#line 886 "hexpr.y" /* yacc.c:1646  */
    { (yyval.qualtype) = new QualType(*(yyvsp[-2].tconstraints), *(yyvsp[0].mtype)); }
#line 4268 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 271:
#line 887 "hexpr.y" /* yacc.c:1646  */
    { (yyval.qualtype) = new QualType(Constraints(), *(yyvsp[0].mtype)); }
#line 4274 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 272:
#line 890 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraints) = (yyvsp[-1].tconstraints); }
#line 4280 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 273:
#line 892 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraints) = autorelease(new Constraints()); (yyval.tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); }
#line 4286 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 274:
#line 893 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); (yyval.tconstraints) = (yyvsp[-2].tconstraints); }
#line 4292 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 275:
#line 895 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(*(yyvsp[-1].string), *(yyvsp[0].mtypes)); }
#line 4298 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 276:
#line 896 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(EqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4304 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 277:
#line 897 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(NotEqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4310 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 278:
#line 898 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(FixIsoRecur::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4316 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 279:
#line 899 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4322 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 280:
#line 900 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4328 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 281:
#line 901 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4334 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 282:
#line 902 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4340 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 283:
#line 903 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4346 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 284:
#line 904 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(1), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4352 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 285:
#line 906 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4358 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 286:
#line 907 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4364 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 287:
#line 908 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4370 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 288:
#line 909 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4376 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 289:
#line 911 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4382 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 290:
#line 912 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4388 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 291:
#line 913 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4394 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 292:
#line 914 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4400 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 293:
#line 916 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), TString::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype))); }
#line 4406 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 294:
#line 917 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), *(yyvsp[-5].mtype),                *(yyvsp[-3].mtype))); }
#line 4412 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 295:
#line 918 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(AppendsToUnqualifier::constraintName(), list(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4418 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 296:
#line 920 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4424 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 297:
#line 921 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-1].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4430 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 298:
#line 923 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4436 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 299:
#line 924 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4442 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 300:
#line 926 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Func::make(tuplety(*(yyvsp[-2].mtypes)), *(yyvsp[0].mtype)))); }
#line 4448 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 301:
#line 927 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeTupleType(*(yyvsp[0].mtypes)))); }
#line 4454 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 302:
#line 928 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeSumType(*(yyvsp[0].mtypes)))); }
#line 4460 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 303:
#line 930 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(monoTypeByName(*(yyvsp[0].string)))); }
#line 4466 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 304:
#line 931 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(OpaquePtr::make(str::replace<char>(*(yyvsp[-1].string), ".", "::"), 0, false))); }
#line 4472 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 305:
#line 932 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("[]"))); }
#line 4478 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 306:
#line 933 "hexpr.y" /* yacc.c:1646  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(Array::make(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4484 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 307:
#line 934 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(FixedArray::make(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype)))); }
#line 4490 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 308:
#line 935 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("->"))); }
#line 4496 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 309:
#line 936 "hexpr.y" /* yacc.c:1646  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(clone(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4502 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 310:
#line 937 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeRecType(*(yyvsp[-1].mreclist)))); }
#line 4508 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 311:
#line 938 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeVarType(*(yyvsp[-1].mvarlist)))); }
#line 4514 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 312:
#line 939 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("unit"))); }
#line 4520 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 313:
#line 940 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(((yyvsp[0].intv) == 0) ? Prim::make("void") : TLong::make((yyvsp[0].intv)))); }
#line 4526 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 314:
#line 941 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr((yyvsp[0].boolv) ? TLong::make(1) : TLong::make(0))); }
#line 4532 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 315:
#line 942 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Exists::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4538 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 316:
#line 943 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(Prim::make("fileref"), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))))); }
#line 4544 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 317:
#line 944 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(Prim::make("fileref"), list(*(yyvsp[-2].mtype))))); }
#line 4550 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 318:
#line 945 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Recursive::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4556 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 319:
#line 946 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TString::make(str::unescape(str::trimq(*(yyvsp[0].string)))))); }
#line 4562 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 320:
#line 947 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp))))))); }
#line 4568 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 321:
#line 949 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TVar::make(*(yyvsp[0].string)))); }
#line 4574 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 322:
#line 950 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TLong::make((yyvsp[0].intv)))); }
#line 4580 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 323:
#line 952 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4586 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 324:
#line 953 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[-2].string); *(yyval.string) += "."; *(yyval.string) += *(yyvsp[0].string); }
#line 4592 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 325:
#line 955 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4598 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 326:
#line 956 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-1].mtypes)->insert((yyvsp[-1].mtypes)->begin(), *(yyvsp[-3].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4604 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 327:
#line 958 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4610 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 328:
#line 959 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-2].mtypes); }
#line 4616 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 329:
#line 961 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4622 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 330:
#line 962 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4628 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 331:
#line 964 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[-2].mtype)); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4634 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 332:
#line 965 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4640 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 333:
#line 967 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mreclist) = (yyvsp[-4].mreclist);                                 (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4646 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 334:
#line 968 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mreclist) = autorelease(new Record::Members()); (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4652 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 335:
#line 970 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = (yyvsp[-4].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4658 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 336:
#line 971 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = (yyvsp[-2].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4664 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 337:
#line 972 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4670 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 338:
#line 973 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4676 "hexpr.parse.C" /* yacc.c:1646  */
    break;


#line 4680 "hexpr.parse.C" /* yacc.c:1646  */
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
#line 977 "hexpr.y" /* yacc.c:1906  */



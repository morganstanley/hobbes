/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

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
#define YYBISON_VERSION "3.0.4"

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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"

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
#line 25 "hexpr.y" /* yacc.c:339  */

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

Expr* mkAIndex(const ExprPtr& arr, const ExprPtr& idx, const LexicalAnnotation& la) {
  return new AIndex(arr, fncall(var("arrayIndexFrom", la), list(idx), la), la);
}

Expr* maybeArraySliceWithTime(const ExprPtr& e, const std::string& t, const LexicalAnnotation& la) {
  auto cs = str::csplit(t, ":");
  if (cs.size() == 2) {
    auto i = str::to<int>(cs[0]);
    auto f = str::to<int>(cs[1]);
    return new App(var("slice", la), list(e, constant(i, la), constant(f, la)), la);
  } else {
    return mkAIndex(e, ExprPtr(mkTimeExpr(t, la)), la);
  }
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


#line 322 "hexpr.parse.C" /* yacc.c:339  */

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

union YYSTYPE
{
#line 262 "hexpr.y" /* yacc.c:355  */

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

#line 515 "hexpr.parse.C" /* yacc.c:355  */
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

/* Copy the second part of user declarations.  */

#line 546 "hexpr.parse.C" /* yacc.c:358  */

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
#define YYFINAL  72
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2879

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  94
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  80
/* YYNRULES -- Number of rules.  */
#define YYNRULES  353
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  774

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
       0,   484,   484,   485,   486,   487,   490,   491,   492,   494,
     495,   496,   498,   499,   500,   501,   502,   504,   505,   506,
     507,   508,   509,   510,   511,   512,   513,   514,   515,   518,
     521,   524,   525,   528,   530,   533,   534,   535,   536,   537,
     538,   539,   540,   542,   543,   545,   547,   548,   550,   553,
     554,   555,   556,   558,   559,   561,   564,   566,   568,   569,
     571,   573,   575,   576,   577,   578,   579,   580,   581,   582,
     583,   584,   585,   586,   587,   588,   589,   590,   591,   592,
     593,   595,   596,   598,   599,   602,   603,   604,   605,   607,
     608,   609,   610,   611,   612,   614,   615,   617,   618,   619,
     620,   621,   622,   623,   624,   625,   627,   628,   629,   630,
     631,   633,   634,   635,   636,   638,   641,   642,   645,   648,
     651,   663,   664,   667,   669,   670,   672,   674,   675,   677,
     678,   680,   681,   683,   684,   686,   687,   690,   691,   694,
     695,   696,   697,   698,   699,   700,   701,   702,   705,   706,
     707,   708,   709,   712,   713,   714,   717,   720,   723,   724,
     727,   728,   729,   730,   731,   732,   733,   734,   735,   736,
     737,   738,   739,   740,   743,   746,   747,   748,   749,   750,
     751,   752,   753,   754,   755,   756,   757,   758,   759,   760,
     761,   762,   763,   766,   768,   769,   771,   773,   774,   776,
     778,   779,   781,   782,   784,   785,   786,   788,   789,   791,
     792,   794,   795,   797,   798,   801,   802,   804,   805,   806,
     807,   808,   809,   810,   811,   812,   813,   814,   815,   816,
     817,   818,   819,   820,   821,   822,   823,   824,   825,   826,
     827,   829,   830,   831,   832,   833,   835,   837,   838,   840,
     841,   843,   844,   846,   848,   849,   850,   852,   853,   854,
     855,   856,   857,   858,   859,   860,   861,   862,   863,   864,
     865,   866,   868,   869,   870,   871,   873,   874,   876,   877,
     878,   880,   881,   882,   884,   885,   888,   890,   891,   893,
     894,   895,   896,   897,   898,   899,   900,   901,   902,   904,
     905,   906,   907,   909,   910,   911,   912,   914,   915,   916,
     918,   919,   921,   922,   924,   925,   926,   928,   929,   930,
     931,   932,   933,   934,   935,   936,   937,   938,   939,   940,
     941,   942,   943,   944,   945,   947,   948,   950,   951,   953,
     954,   956,   957,   959,   960,   962,   963,   965,   966,   968,
     969,   970,   971,   973
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
  "opname", "idseq", "types", "l0expr", "lhexpr", "l1expr", "l2expr",
  "l3expr", "l4expr", "l5expr", "letbindings", "letbinding", "dobindings",
  "dobinding", "cselconds", "cselection", "cselections", "l6expr",
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
     345,   346,   347,   348
};
# endif

#define YYPACT_NINF -585

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-585)))

#define YYTABLE_NINF -353

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     400,  1379,  2152,  2152,    50,   119,   119,   119,    37,    37,
      44,    44,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,    33,
      51,  2152,  2776,    -6,  2776,   119,    -1,  1605,  2152,    33,
     250,  2152,  -585,   819,  -585,  -585,  -585,  -585,  -585,  -585,
     157,  -585,   265,   232,    18,   281,  2620,  2464,  2152,  1684,
    1229,  1229,  -585,   134,   151,   466,   425,   426,  -585,   290,
    -585,  -585,  -585,  1379,   279,   303,  -585,  1013,   153,  -585,
    -585,   183,  2034,   388,    37,   394,  2112,  -585,  -585,  -585,
    -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  -585,  -585,  1229,   119,    -3,  -585,   331,  -585,   384,
     163,   336,   119,   163,   421,  2230,   363,   413,  2542,   414,
     423,   424,    33,   427,   428,   432,   433,   435,   436,   446,
     447,  1484,   457,   458,   461,  -585,  -585,   462,  -585,   -24,
     -13,   367,   295,   452,   454,   192,   404,  -585,  2190,  2190,
    1229,  2152,  1918,    18,  -585,  -585,    33,  2152,   228,  -585,
    -585,   434,   363,   413,  2542,   414,   423,   424,   427,   428,
     432,   435,   436,   446,   447,   457,   458,   461,  -585,  -585,
    -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  1229,  1229,   119,   398,   232,   953,  -585,  -585,  -585,
    2799,  2386,  2386,  2386,  2386,  2464,  2620,  2620,  2620,  2620,
    2620,  2620,  2620,  2620,  2620,  2620,  2620,  2698,  2698,  2698,
    2152,  -585,   819,   119,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  2190,  -585,  2190,  -585,  -585,  -585,   119,   119,   208,
     417,  2268,  2268,   119,  2152,    67,  -585,   118,  2268,   119,
      30,    37,  1013,   119,   208,   119,   119,   101,  -585,    42,
     516,   507,   511,  -585,  -585,   320,   472,   360,  -585,   517,
    2152,   136,  2464,   474,   477,   163,    10,  -585,   519,  2776,
    1762,    33,   478,  1840,  -585,   520,   521,  -585,  -585,  -585,
    -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  -585,  -585,  -585,  -585,  -585,  -585,  2152,  1229,  1996,
    -585,  -585,   734,  2152,  2152,  2152,  -585,  -585,   627,  -585,
     529,  -585,  -585,  -585,   327,   487,  2152,   167,  -585,  -585,
    2152,   278,  2152,   338,   372,   396,   527,   217,  2152,  -585,
    2152,   229,   481,   481,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,   819,  -585,  -585,  -585,   524,     2,   492,  -585,  1133,
    -585,    98,  2112,  -585,  1276,   208,   137,   401,   540,   188,
      79,   135,   531,   483,  -585,  2034,   337,  2268,  2268,    33,
    2268,  2268,  2268,  1956,  2268,   490,    37,   566,   119,   119,
    2112,   501,   549,   550,   572,  -585,  2268,  2268,  2268,  2268,
    -585,   512,  1229,  -585,    12,  1229,  -585,  2152,  -585,  -585,
     402,  1229,   477,  -585,  -585,  -585,  -585,   216,  -585,  -585,
    -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,  1762,  2308,    33,   409,   232,  -585,   517,  -585,  2152,
    -585,  -585,  2152,  -585,  -585,   270,   554,  -585,   515,  -585,
     559,  -585,   518,   522,   208,    52,  2112,  -585,  -585,   523,
    2074,  -585,  -585,  2152,   288,   535,  -585,   526,  -585,   528,
    -585,    29,  1229,  1229,  -585,  -585,  -585,  2268,  -585,  -585,
    -585,  -585,  2268,   530,  -585,  2268,  -585,   119,  2112,  2268,
    2112,  -585,   119,  2112,  2268,  -585,  -585,  2268,  2268,  2268,
      40,   115,   355,   490,   490,   490,  -585,   490,   490,    39,
      37,   566,  -585,    23,  -585,   276,  -585,  -585,   187,  2112,
    2112,  2112,    37,   572,  -585,   490,   490,   490,   490,  -585,
    -585,  -585,  -585,  -585,  -585,   565,   375,  -585,   420,  1563,
    -585,   532,  -585,    45,  2776,   569,   230,   525,   534,  -585,
    1229,  2152,  -585,  2152,  -585,  -585,  -585,  -585,  -585,   541,
    -585,  2152,   293,  2152,  -585,  -585,  -585,   542,   543,   490,
     249,   415,   -14,   574,  -585,   104,   247,   545,   586,   546,
      47,   490,   154,   168,   587,    97,   593,  2268,  2268,  2268,
    2268,  2268,   566,   119,  -585,  -585,   566,   119,   119,  -585,
     572,  -585,   349,  -585,  -585,   590,  -585,   119,   575,   402,
     119,  2152,  2152,  2152,  -585,  -585,  -585,  2152,  -585,  -585,
     598,   163,  2308,  2308,  -585,  -585,  -585,  -585,   553,  -585,
    -585,  -585,  2152,   304,  -585,  -585,  -585,   600,  -585,   605,
    -585,   606,  2112,  2268,   608,   604,  2112,   610,  2268,  2268,
    2268,  2268,  2268,  2268,   490,   490,   490,   490,   490,   566,
      26,   566,  -585,   119,   572,  -585,  2112,  2152,   613,  2152,
    -585,   616,  -585,   623,  -585,  -585,   578,   273,  2386,  -585,
    2152,   305,  2268,   581,  2268,  -585,    55,  2268,  2268,  -585,
    2268,   291,   257,   138,   181,   308,   148,   566,  -585,  -585,
    2152,  -585,  2152,  2152,  -585,  -585,  -585,   229,   580,  -585,
    2152,   343,   490,  -585,   490,   621,   490,   490,   490,   625,
    -585,  -585,  2268,  -585,  2268,   566,  -585,  -585,  -585,  2386,
    -585,  2152,   345,  2268,  2268,   170,   312,   229,  -585,  2152,
     348,   490,   490,  -585,  -585,  -585,  2152,   352,  -585,  2152,
     626,  -585,  2152,  -585
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,     9,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   160,   161,   162,   163,   164,   165,   166,   167,
     168,   169,   353,   170,   157,   207,   172,   173,   275,     0,
       0,     0,     0,     0,     0,     0,     0,   281,   281,   254,
       0,     0,     2,     8,    10,    12,    13,    14,    15,    16,
       0,    29,   115,   171,   156,   138,     0,     0,     0,   281,
       0,     0,     4,    88,    94,    96,   105,   110,   114,   138,
       5,   138,     1,     9,     0,    30,   337,     0,     0,    58,
      60,     0,     0,     0,     0,     0,     0,   265,   260,   264,
     259,   258,   261,   262,   270,   271,   263,   266,   267,   268,
     269,   274,   257,   248,     0,     0,   125,     0,   241,     0,
     210,     0,     0,   158,     0,     0,     0,     0,     0,     0,
       0,     0,    65,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    64,    67,     0,   282,     0,
     282,     0,     0,     0,     0,     0,     0,    11,     0,     0,
       0,   281,     0,   155,   208,   273,     0,     0,     0,   109,
      89,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   217,   218,
     219,   225,   220,   221,   222,   223,   224,   226,   230,   228,
     229,   248,   248,     0,     0,   227,     0,   246,   216,   240,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     6,     9,     0,    75,    76,    77,    78,    79,    80,
      65,    69,    68,    66,    70,    71,    72,    73,    62,    63,
      74,     0,    59,     0,   328,   327,   333,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   287,     0,   317,     0,
      39,    56,    60,     0,     0,     0,     0,    49,    83,   343,
       0,   315,   316,   317,   250,     0,   247,     0,   252,     0,
       0,     0,     0,     0,     0,   209,     0,   195,     0,     0,
     248,   254,     0,     0,   128,     0,   138,   175,   176,   177,
     178,   179,   180,   183,   182,   181,   184,   185,   188,   186,
     187,   192,   189,   190,   191,    61,   174,     0,     0,     0,
     142,   153,     0,     0,     0,     0,   150,   193,     0,    33,
       0,   285,   123,   119,     0,   172,     0,     0,   272,    17,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   215,
       0,    87,    90,    91,    92,    93,    99,    98,    97,   100,
     101,   102,   103,   104,   108,   106,   107,   111,   112,   113,
       3,     7,   338,    31,    32,     0,     0,     0,   326,     0,
     313,   343,     0,   319,     0,     0,     0,     0,   317,     0,
       0,   317,     0,     0,   286,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   289,   310,     0,     0,     0,     0,
       0,   313,     0,   352,     0,    84,     0,     0,     0,     0,
     242,     0,     0,   244,     0,     0,   116,     0,   124,   126,
       0,     0,   118,   212,   120,   194,   201,     0,   160,   161,
     162,   163,   164,   165,   166,   167,   169,   170,   157,   172,
     173,   248,   248,   254,     0,   171,   138,     0,   130,     0,
     121,   127,     0,   283,   136,     0,     0,   140,     0,   154,
       0,   255,     0,     0,     0,   343,     0,   137,   143,     0,
       0,   144,    18,     0,     0,     0,   236,     0,   231,     0,
     238,     0,     0,     0,   233,    85,    86,     0,   318,   322,
     323,   312,     0,     0,   320,     0,   324,     0,     0,     0,
       0,   325,     0,     0,     0,   334,   288,     0,     0,     0,
       0,     0,     0,   290,   292,   291,   331,   330,   311,    35,
       0,    41,    46,    40,    43,     0,    81,    57,    50,     0,
       0,     0,     0,    51,    53,   345,   314,   344,   346,   243,
     249,   245,   251,   253,   117,     0,     0,   276,     0,     0,
     211,   196,   198,     0,     0,     0,     0,     0,     0,   141,
       0,     0,   139,     0,   149,   148,   284,   147,   146,     0,
      19,     0,     0,     0,   237,   232,   239,     0,     0,   329,
       0,     0,     0,     0,   348,   343,     0,     0,   350,   351,
     343,   332,     0,     0,   317,     0,   317,     0,     0,     0,
       0,     0,     0,     0,    48,    47,     0,     0,     0,    82,
       0,   341,     0,   351,    55,     0,    54,     0,   151,     0,
       0,     0,     0,     0,   201,   206,   205,     0,   200,   203,
     204,   159,     0,     0,   150,   122,   129,   135,   134,   256,
     145,    20,     0,     0,    95,   235,   234,     0,   336,     0,
     335,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   309,   302,   301,   300,   299,    37,
      36,    42,    44,    45,    52,   340,     0,     0,     0,     0,
     277,     0,   278,     0,   213,   197,     0,     0,     0,    21,
       0,     0,     0,     0,     0,   347,     0,     0,     0,   349,
       0,   345,     0,     0,     0,     0,     0,     0,   342,    34,
       0,   152,     0,     0,   199,   202,   204,   132,   133,    22,
       0,     0,   298,   321,   296,     0,   304,   308,   307,     0,
     295,   293,     0,   303,     0,    38,   280,   279,   214,     0,
      23,     0,     0,     0,     0,     0,     0,   131,    24,     0,
       0,   297,   306,   294,   305,    25,     0,     0,    26,     0,
       0,    27,     0,    28
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -585,  -585,   592,   444,   -35,  -585,  -585,   139,  -585,  -585,
      57,    56,  -584,  -505,  -585,    48,  -518,  -379,   475,     3,
     405,    53,   262,    -2,  -198,   -34,   318,   -32,   255,     9,
    -585,   393,  -585,   382,  -585,   106,  -585,   -16,  -585,   391,
    -585,    46,  -585,  -585,   -18,    54,  -585,  -585,   252,   -46,
    -585,   -93,   -47,  -171,  -585,  -176,  -391,  -585,   -10,   -50,
    -585,    59,   -31,  -119,   476,  -585,   297,  -585,   440,   -77,
     865,  -585,   438,  -585,  -585,  -585,  -585,  -585,  -585,   533
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,    42,    43,    44,    45,    46,    47,   624,    48,
     533,   534,   531,   532,    49,   543,   544,   260,   261,    50,
     137,   535,   267,   138,    63,    64,    65,    66,    67,    68,
     105,   106,   293,   294,   728,   464,   465,    52,   286,   287,
     561,   562,   563,   638,   639,    53,   111,   432,   433,   196,
     197,   107,   274,   275,   276,   277,   278,   142,   143,    54,
     556,   557,   139,   329,   330,   255,   256,   404,   379,   331,
     269,   659,    75,   270,   622,   271,   272,   387,   390,    71
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      62,    70,   153,   351,   352,   353,   354,   141,   147,   268,
      51,    79,    79,   198,   198,   200,   110,   345,   113,   101,
     343,   344,   295,   160,   159,   626,   615,   529,   679,   109,
     332,   616,   681,   552,   717,    22,   140,    22,   407,   146,
      87,    88,    89,    90,    91,    92,    93,   612,   223,   155,
      72,   316,    51,    94,    22,   498,   161,   317,    22,    95,
     153,   635,    22,   153,   156,   661,   280,   318,    96,    22,
      22,   636,   112,   319,   403,  -339,    22,   115,   607,   281,
    -339,   242,    51,   416,   242,  -339,   159,    79,   668,   434,
     396,   551,   397,   416,    97,   285,   398,   160,   399,   400,
     552,   401,   402,   333,   617,    98,    99,   617,   586,   414,
     408,    77,   101,   292,   195,   195,   244,   100,    82,   613,
     334,   245,   373,   637,   374,   103,    22,   246,   403,   104,
     403,  -339,   159,   745,   735,   403,   247,  -339,   672,   416,
     403,   502,   394,   403,    22,   416,   338,   663,   395,   349,
     337,   608,   248,   349,   609,   339,   396,   195,   397,   511,
     512,    22,   398,   625,   399,   400,   626,   401,   402,   513,
     201,   355,   380,   380,   615,   264,   615,   250,    22,   265,
     505,   266,   364,   365,   366,   403,   403,   411,   253,   744,
     415,   241,   403,   254,    28,   620,   148,   669,   202,   203,
     295,   480,   244,   403,   195,   427,   403,   245,    22,    29,
     103,   670,    22,   246,   104,  -352,  -352,   741,   370,   204,
     205,   243,   247,   244,   742,   403,   403,    77,   245,   509,
     325,    51,   510,    22,   246,   153,   403,   151,   248,   152,
     615,   377,   403,   247,   481,   195,   195,    28,   429,   763,
     195,   411,   393,    22,   195,   493,   403,    77,   403,   248,
     154,   264,    29,   250,   242,   265,   340,   266,   643,   403,
     144,   466,   326,   437,   253,    22,   403,   345,   426,   254,
     343,   344,   264,   378,   250,   564,   265,   222,   266,   635,
     151,   292,   152,   202,   203,   253,    28,   494,    22,   636,
     254,    22,   501,    22,   149,   503,    22,   501,   380,   618,
     644,    29,   470,    22,   204,   463,   483,   468,    22,   157,
     -60,   471,   472,   473,   657,   150,   581,   664,   220,    22,
      22,   652,   740,   268,   479,   403,   147,   403,   482,   151,
     485,   152,   700,   730,   455,   403,   495,   569,   496,   223,
     570,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    22,   282,
      22,   739,   195,    22,   321,   550,   322,    22,   553,   403,
      51,   751,    29,   759,   198,   559,   766,   153,   743,   522,
     769,   610,   764,    31,   611,   420,   403,   411,   284,   576,
     403,   421,   477,     1,     2,     3,    34,    35,   317,    79,
      59,   517,    38,   486,    39,   518,    40,   519,   346,   487,
     259,   141,   555,    22,   685,   554,   263,    22,    41,   565,
     686,   594,   244,   597,    22,   658,   599,   245,   297,   423,
      22,   424,    22,   246,   320,   587,   588,   283,   317,   488,
     140,   382,   247,   489,   630,   628,   629,   567,   631,   289,
     568,   415,   621,   594,   623,   214,   215,   216,   248,   217,
     218,   219,   367,   368,   369,   490,   195,   491,   579,   195,
     506,   580,   507,    78,    81,   195,    83,    85,   298,   300,
     323,   264,   324,   250,   383,   265,   327,   266,   301,   302,
     727,   342,   303,   304,   253,   455,   455,   305,   306,   254,
     307,   308,   349,   206,   207,   208,   209,   210,   211,   212,
     213,   309,   310,   466,   356,   357,   358,   359,   360,   361,
     362,   363,   312,   313,    55,    69,   314,   315,    73,    74,
      76,    80,    80,    84,    86,    79,   195,   195,   641,   417,
     418,   757,   419,   422,   430,   425,   436,   431,   462,  -241,
     458,   476,   102,   108,   478,   492,   204,   499,   114,   648,
     497,   649,   102,   145,   508,   515,    55,   514,   403,   651,
     530,   654,   539,   540,   541,   705,   542,   549,   158,   709,
     571,   153,   572,   199,   199,   587,   588,   573,   574,   627,
     577,   584,   575,   583,   645,   585,    55,   642,   662,   718,
     591,    80,   634,   195,    80,   258,   646,   262,   650,   273,
     666,   671,   655,   656,   195,   665,   667,   673,   687,   692,
     693,   694,   697,   689,   698,   696,   199,   279,   702,   703,
     472,   473,   244,   708,   704,   288,   707,   245,   296,   710,
     699,   720,    22,   246,   722,   102,   723,   724,   733,   753,
     377,   749,   247,   754,   772,   221,   371,   409,   684,   614,
     680,   683,   538,   682,   428,   461,   647,   435,   248,   725,
     695,   273,   273,   199,   560,   719,   376,   721,   690,   102,
     384,   341,   516,     0,     0,     0,   455,   455,   729,     0,
       0,   474,   378,   250,     0,   251,     0,   252,     0,     0,
       0,     0,     0,     0,   253,     0,     0,     0,   746,   254,
     747,   748,     0,     0,   199,   199,   279,   347,   750,   199,
       0,     0,     0,   199,     0,     0,     0,     0,     0,     0,
       0,    87,    88,    89,    90,    91,    92,    93,     0,   758,
       0,     0,     0,     0,    94,    55,   372,   765,     0,    22,
      95,     0,     0,     0,   768,     0,     0,   771,     0,    96,
     773,     0,     0,     0,   273,     0,   273,     0,     0,     0,
     375,    76,   273,   273,   388,   391,   392,     0,     0,     0,
       0,   273,   406,     0,    80,    97,   410,   273,   412,   413,
     273,     0,     0,     0,     0,     0,    98,    99,     0,     0,
       0,     0,     0,   469,   108,     0,     0,     0,   100,   288,
       0,     0,     0,   456,   457,     0,   296,     0,     7,     8,
       9,    10,    11,     0,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,   199,     0,     0,     0,   102,     0,     0,     0,     0,
       0,   258,     0,     0,     0,    29,     0,     0,     0,     0,
       0,     0,     0,     0,   484,    30,    31,     0,    32,     0,
      33,     0,     0,     0,     0,     0,     0,     0,     0,    34,
      35,    36,     0,    37,     0,    38,     0,    39,     0,    40,
       0,     0,     0,     0,    55,     0,     0,     0,     0,     0,
       0,    41,   273,     0,     0,   273,     0,   273,   273,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   258,     0,
     273,   273,   102,   273,   273,   273,   273,   273,     0,   262,
       0,   536,   537,   273,     0,     0,     0,   257,     0,   273,
     273,   273,   273,     0,     0,   199,     0,   279,   199,     0,
       0,     0,     0,   558,   199,     0,     0,     0,   178,   179,
     180,   181,   182,   183,   184,   185,     0,   186,    22,   187,
     188,    25,   189,   190,   456,   456,   457,   566,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   348,
       0,     0,     0,     0,     0,     0,     0,   273,     0,   273,
       0,     0,     0,     0,     0,     0,     0,   582,     0,     0,
       0,     0,     0,     0,   279,   199,   199,   191,     0,   192,
     273,   193,     0,   194,     0,   273,     0,     0,   273,     0,
     593,   273,   273,   273,     0,   598,   273,   273,     0,     0,
     273,   604,   606,   224,   225,   226,   227,   228,   229,   230,
     231,   232,   233,    80,   234,   235,   236,   237,   619,     0,
       0,   273,   273,   273,   273,   262,     0,   238,   239,     0,
       0,     0,   240,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   199,     0,     0,     0,   640,     0,   135,     0,
       0,     0,     0,   199,     0,     0,   136,     0,     0,     0,
       0,     0,     0,     0,   381,   653,   386,   389,     0,     0,
       0,     0,     0,   405,   660,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     273,   273,   273,   273,   273,     0,   536,     0,   244,     0,
     536,   536,     0,   245,     0,     0,     0,     0,    22,   246,
     688,     0,   558,   691,     0,     0,     0,     0,   247,     0,
       0,     0,     0,     0,     0,   456,   456,     0,     0,     0,
       0,     0,     0,     0,   248,     0,   701,     0,     0,     0,
       0,     0,     0,   475,     0,   273,   273,     0,     0,   273,
       0,   273,   273,   273,   273,   273,   273,   264,   500,   250,
       0,   265,     0,   266,     0,     0,   619,     0,     0,   273,
     253,     0,     0,     0,     0,   254,     0,     0,     0,     0,
     726,     0,     0,     0,   731,   273,     0,   273,     0,     0,
     273,   273,     0,   273,   178,   179,   180,   181,   182,   183,
     184,   185,     0,   186,    22,   187,   188,    25,   189,   190,
     257,     0,   520,   521,   752,   523,   524,   525,   527,   528,
       0,     0,     0,     0,     0,   273,     0,   273,     0,     0,
       0,   545,   546,   547,   548,   760,   273,   273,     0,     0,
       0,   244,     0,   767,     0,     0,   245,     0,     0,     0,
     770,    22,   246,   191,     0,   192,     0,   193,     0,   194,
       0,   247,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   248,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   381,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     264,     0,   250,   504,   265,     0,   266,     0,     0,     0,
       0,     0,   589,   253,     0,     0,     0,   590,   254,     0,
     592,     0,     0,   595,   596,     0,     0,     0,   600,   601,
       0,     0,   602,   603,   605,     5,     6,     0,     7,     8,
       9,    10,    11,     0,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    29,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,     0,    32,     0,
      33,     0,     0,     0,     0,     0,     0,     0,     0,    34,
      35,    36,     0,    37,     0,    38,     0,    39,     0,    40,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    41,   674,   675,   676,   677,   678,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    56,     0,   706,     0,
      29,     0,     0,   711,   712,   713,   714,   715,   716,     0,
      30,    31,     0,    32,     0,    33,     0,     0,     0,     0,
      58,     0,     0,     0,    34,    35,    36,     0,    59,   311,
      38,     0,    39,     0,    40,     0,     0,   732,     0,   734,
       0,   632,   736,   737,     0,   738,    41,     0,   178,   179,
     180,   181,   182,   183,   184,   185,     0,   186,    22,   187,
     188,    25,   189,   190,     0,     0,   633,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   755,     0,   756,
       0,     0,     0,     0,     0,     0,     0,     0,   761,   762,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,   191,     0,   192,
       0,   193,     0,   194,     0,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,    30,    31,     0,    32,     0,    33,     0,     0,   132,
     133,    58,     0,     0,   134,    34,    35,    36,     0,    59,
       0,    38,     0,    39,     0,    40,     0,     0,    60,    61,
     135,     0,     0,     0,     0,     0,     0,    41,   136,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,     0,     0,
       0,     0,     0,     0,   162,   163,   164,   165,   166,   167,
      29,   168,   169,   170,   126,   171,   172,   173,   174,   131,
      30,    31,     0,    32,     0,    33,     0,     0,   175,   176,
      58,     0,     0,   177,    34,    35,    36,     0,    59,     0,
      38,     0,    39,     0,    40,     0,     0,    60,    61,     0,
       0,     0,     0,     0,     0,     0,    41,   438,   439,   440,
     441,   442,   443,   444,   445,    20,   446,    22,   447,   448,
      25,   449,   450,    28,     0,     0,     0,     0,     0,     0,
       0,     0,   162,   163,   164,   165,   166,   167,    29,   168,
     169,   170,   126,   171,   172,   173,   174,   131,    30,    31,
       0,    32,     0,    33,     0,     0,   175,   176,    58,     0,
       0,   177,    34,    35,    36,     0,   451,     0,   452,     0,
     453,     0,   454,     0,     0,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    41,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    56,     0,     0,     0,    29,     0,     0,     0,
       0,     0,     0,     0,     0,    57,    30,    31,     0,    32,
       0,    33,     0,     0,     0,     0,    58,     0,     0,     0,
      34,    35,    36,   459,   290,     0,    38,     0,   291,   460,
      40,     0,     0,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    41,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,   335,    27,    28,
       0,     0,   336,     0,     0,     0,     0,     0,     0,     0,
      56,     0,     0,     0,    29,     0,     0,     0,     0,     0,
       0,   244,     0,    57,    30,    31,   245,    32,     0,    33,
       0,    22,   246,     0,    58,     0,     0,     0,    34,    35,
      36,   247,    59,     0,    38,     0,    39,     0,    40,     0,
       0,    60,    61,     0,     0,     0,     0,   248,     0,     0,
      41,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,     0,     0,
     385,     0,   250,     0,   265,     0,   266,     0,    56,     0,
       0,     0,    29,   253,     0,     0,   526,     0,   254,   244,
       0,    57,    30,    31,   245,    32,     0,    33,     0,    22,
     246,     0,    58,     0,     0,     0,    34,    35,    36,   247,
      59,     0,    38,   467,    39,     0,    40,     0,     0,    60,
      61,     0,     0,     0,     0,   248,     0,     0,    41,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,   249,     0,
     250,     0,   251,     0,   252,     0,    56,     0,     0,     0,
      29,   253,     0,     0,     0,     0,   254,   244,     0,    57,
      30,    31,   245,    32,     0,    33,     0,    22,   246,     0,
      58,     0,     0,     0,    34,    35,    36,   247,    59,     0,
      38,   578,    39,     0,    40,     0,     0,    60,    61,     0,
       0,     0,     0,   248,     0,     0,    41,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,   264,     0,   250,     0,
     265,     0,   266,     0,    56,     0,     0,     0,    29,   253,
       0,     0,     0,     0,   254,   244,     0,    57,    30,    31,
     245,    32,     0,    33,     0,    22,   246,     0,    58,     0,
       0,     0,    34,    35,    36,   247,    59,     0,    38,     0,
      39,     0,    40,     0,     0,    60,    61,     0,     0,     0,
       0,   248,     0,     0,    41,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,     0,     0,   328,     0,   250,     0,   265,     0,
     266,     0,    56,     0,     0,     0,    29,   253,     0,     0,
       0,     0,   254,   244,     0,    57,    30,    31,   245,    32,
       0,    33,     0,    22,   246,     0,    58,     0,     0,     0,
      34,    35,    36,   247,   290,     0,    38,     0,   291,     0,
      40,     0,     0,    60,    61,     0,     0,     0,     0,   248,
       0,     0,    41,   438,   439,   440,   441,   442,   443,   444,
     445,    20,   446,    22,   447,   448,    25,   449,   450,    28,
       0,     0,   385,     0,   250,     0,   265,     0,   266,     0,
      56,     0,     0,     0,    29,   253,     0,     0,     0,     0,
     254,     0,     0,    57,    30,    31,     0,    32,     0,    33,
       0,     0,     0,     0,    58,     0,     0,     0,    34,    35,
      36,     0,   451,     0,   452,     0,   453,     0,   454,     0,
       0,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      41,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    56,     0,
       0,     0,    29,     0,     0,     0,     0,     0,     0,     0,
       0,    57,    30,    31,     0,    32,     0,    33,     0,     0,
       0,     0,    58,     0,     0,     0,    34,    35,    36,     0,
      59,     0,    38,     0,    39,     0,    40,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    41,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    56,     0,     0,     0,
      29,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,     0,    32,     0,    33,     0,     0,     0,     0,
      58,     0,     0,     0,    34,    35,    36,     0,    59,     0,
      38,     0,    39,     0,    40,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    41,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    56,     0,     0,     0,    29,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
       0,    32,     0,    33,     0,     0,     0,     0,     0,     0,
       0,     0,    34,    35,    36,     0,    59,   299,    38,     0,
      39,     0,    40,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    41,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    56,     0,     0,     0,    29,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    30,    31,     0,    32,
       0,    33,     0,     0,     0,     0,     0,     0,     0,     0,
      34,    35,    36,     0,    59,     0,    38,     0,    39,     0,
      40,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    41,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    29,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,     0,    32,     0,    33,
       0,     0,     0,     0,     0,     0,     0,     0,    34,    35,
      36,     0,    59,     0,    38,     0,    39,     0,    40,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      41,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,     0,     0,
       0,     0,     0,     0,   178,   179,   180,   181,   182,   183,
     184,   185,    29,   186,    22,   187,   188,    25,   189,   190,
       0,     0,     0,    31,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   350,    34,    35,     0,     0,
      59,     0,    38,     0,    39,     0,    40,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    41,     0,
       0,     0,     0,   191,     0,   192,     0,   193,     0,   194
};

static const yytype_int16 yycheck[] =
{
       2,     3,    52,   201,   202,   203,   204,    38,    43,    86,
       1,     8,     9,    60,    61,    61,    32,   193,    34,    29,
     191,   192,   115,    57,    56,   543,   531,   406,   612,    31,
     149,     8,   616,   424,     8,    25,    38,    25,     8,    41,
       7,     8,     9,    10,    11,    12,    13,     8,    46,    31,
       0,    75,    43,    20,    25,    53,    58,    81,    25,    26,
     110,    16,    25,   113,    46,    79,    69,    80,    35,    25,
      25,    26,    78,    86,    88,    33,    25,    78,    38,    82,
      33,    78,    73,    41,    81,    33,   118,    84,    41,    79,
      38,    79,    40,    41,    61,   111,    44,   131,    46,    47,
     491,    49,    50,   150,    81,    72,    73,    81,    79,     8,
      80,    74,   122,   115,    60,    61,    15,    84,    74,    80,
     151,    20,   241,    78,   243,    74,    25,    26,    88,    78,
      88,    33,   164,   717,    79,    88,    35,    33,    41,    41,
      88,    43,    75,    88,    25,    41,   156,    43,    81,   196,
     152,    36,    51,   200,    39,   157,    38,   103,    40,    80,
      81,    25,    44,   542,    46,    47,   684,    49,    50,    34,
      36,   205,   249,   250,   679,    74,   681,    76,    25,    78,
      43,    80,   214,   215,   216,    88,    88,   264,    87,    41,
     267,    38,    88,    92,    31,     8,    39,    43,    64,    65,
     293,    34,    15,    88,   150,    69,    88,    20,    25,    46,
      74,    43,    25,    26,    78,    80,    81,    79,   220,    85,
      69,    38,    35,    15,    43,    88,    88,    74,    20,    41,
      38,   222,    44,    25,    26,   285,    88,    74,    51,    76,
     745,    33,    88,    35,    77,   191,   192,    31,   282,    79,
     196,   328,   254,    25,   200,    38,    88,    74,    88,    51,
      28,    74,    46,    76,   261,    78,    38,    80,    38,    88,
      20,   318,    80,   289,    87,    25,    88,   453,   280,    92,
     451,   452,    74,    75,    76,    69,    78,     8,    80,    16,
      74,   293,    76,    64,    65,    87,    31,    80,    25,    26,
      92,    25,   379,    25,    39,   382,    25,   384,   385,    33,
      80,    46,   322,    25,    85,   317,    38,   319,    25,    38,
      39,   323,   324,   325,    75,    60,    38,    80,    38,    25,
      25,    38,    75,   410,   336,    88,   371,    88,   340,    74,
     342,    76,    38,    38,   290,    88,   348,    77,   350,    46,
      80,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    25,    38,
      25,    80,   318,    25,    79,   422,    81,    25,   425,    88,
     371,    38,    46,    38,   431,   431,    38,   437,    80,   399,
      38,    36,    80,    57,    39,    75,    88,   474,    62,   476,
      88,    81,    75,     3,     4,     5,    70,    71,    81,   406,
      74,    74,    76,    75,    78,    78,    80,    80,    20,    81,
      32,   452,    20,    25,    75,   427,    32,    25,    92,    20,
      81,   508,    15,   510,    25,    20,   513,    20,    75,    79,
      25,    81,    25,    26,    77,   492,   493,    63,    81,    77,
     452,    34,    35,    81,    34,    80,    81,   459,    38,    38,
     462,   538,   539,   540,   541,    40,    41,    42,    51,    43,
      44,    45,   217,   218,   219,    79,   422,    81,   480,   425,
      79,   483,    81,     8,     9,   431,    10,    11,    75,    75,
      38,    74,    38,    76,    77,    78,    92,    80,    75,    75,
     698,    67,    75,    75,    87,   451,   452,    75,    75,    92,
      75,    75,   559,    47,    48,    49,    50,    51,    52,    53,
      54,    75,    75,   570,   206,   207,   208,   209,   210,   211,
     212,   213,    75,    75,     1,     2,    75,    75,     5,     6,
       7,     8,     9,    10,    11,   542,   492,   493,   564,    33,
      43,   749,    41,    81,    80,    38,    37,    80,    38,    38,
      82,    32,    29,    30,    77,    38,    85,    75,    35,   571,
      46,   573,    39,    40,    34,    92,    43,    46,    88,   581,
      14,   583,    81,    34,    34,   662,    14,    75,    55,   666,
      36,   641,    77,    60,    61,   642,   643,    38,    80,    34,
      77,    75,    80,    68,    79,    77,    73,    38,    34,   686,
      80,    78,    80,   559,    81,    82,    82,    84,    77,    86,
      34,    34,    80,    80,   570,    80,    80,    34,    38,   631,
     632,   633,    34,    58,    81,   637,   103,   104,    38,    34,
     642,   643,    15,    39,    38,   112,    38,    20,   115,    39,
     652,    38,    25,    26,    38,   122,    33,    79,    77,    38,
      33,    81,    35,    38,    38,    73,   222,   262,   620,   530,
     613,   618,   410,   617,   281,   293,   570,   286,    51,   697,
     634,   148,   149,   150,   432,   687,   248,   689,   629,   156,
     250,   158,   395,    -1,    -1,    -1,   642,   643,   700,    -1,
      -1,    74,    75,    76,    -1,    78,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,   720,    92,
     722,   723,    -1,    -1,   191,   192,   193,   194,   730,   196,
      -1,    -1,    -1,   200,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     7,     8,     9,    10,    11,    12,    13,    -1,   751,
      -1,    -1,    -1,    -1,    20,   222,   223,   759,    -1,    25,
      26,    -1,    -1,    -1,   766,    -1,    -1,   769,    -1,    35,
     772,    -1,    -1,    -1,   241,    -1,   243,    -1,    -1,    -1,
     247,   248,   249,   250,   251,   252,   253,    -1,    -1,    -1,
      -1,   258,   259,    -1,   261,    61,   263,   264,   265,   266,
     267,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    79,   281,    -1,    -1,    -1,    84,   286,
      -1,    -1,    -1,   290,   291,    -1,   293,    -1,     9,    10,
      11,    12,    13,    -1,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,   318,    -1,    -1,    -1,   322,    -1,    -1,    -1,    -1,
      -1,   328,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   341,    56,    57,    -1,    59,    -1,
      61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,
      -1,    -1,    -1,    -1,   371,    -1,    -1,    -1,    -1,    -1,
      -1,    92,   379,    -1,    -1,   382,    -1,   384,   385,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   395,    -1,
     397,   398,   399,   400,   401,   402,   403,   404,    -1,   406,
      -1,   408,   409,   410,    -1,    -1,    -1,    82,    -1,   416,
     417,   418,   419,    -1,    -1,   422,    -1,   424,   425,    -1,
      -1,    -1,    -1,   430,   431,    -1,    -1,    -1,    15,    16,
      17,    18,    19,    20,    21,    22,    -1,    24,    25,    26,
      27,    28,    29,    30,   451,   452,   453,   454,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    46,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   474,    -1,   476,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,    -1,
      -1,    -1,    -1,    -1,   491,   492,   493,    74,    -1,    76,
     497,    78,    -1,    80,    -1,   502,    -1,    -1,   505,    -1,
     507,   508,   509,   510,    -1,   512,   513,   514,    -1,    -1,
     517,   518,   519,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,   530,    51,    52,    53,    54,   535,    -1,
      -1,   538,   539,   540,   541,   542,    -1,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   559,    -1,    -1,    -1,   563,    -1,    85,    -1,
      -1,    -1,    -1,   570,    -1,    -1,    93,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   249,   582,   251,   252,    -1,    -1,
      -1,    -1,    -1,   258,   591,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     607,   608,   609,   610,   611,    -1,   613,    -1,    15,    -1,
     617,   618,    -1,    20,    -1,    -1,    -1,    -1,    25,    26,
     627,    -1,   629,   630,    -1,    -1,    -1,    -1,    35,    -1,
      -1,    -1,    -1,    -1,    -1,   642,   643,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    51,    -1,   653,    -1,    -1,    -1,
      -1,    -1,    -1,   328,    -1,   662,   663,    -1,    -1,   666,
      -1,   668,   669,   670,   671,   672,   673,    74,    75,    76,
      -1,    78,    -1,    80,    -1,    -1,   683,    -1,    -1,   686,
      87,    -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,
     697,    -1,    -1,    -1,   701,   702,    -1,   704,    -1,    -1,
     707,   708,    -1,   710,    15,    16,    17,    18,    19,    20,
      21,    22,    -1,    24,    25,    26,    27,    28,    29,    30,
     395,    -1,   397,   398,   731,   400,   401,   402,   403,   404,
      -1,    -1,    -1,    -1,    -1,   742,    -1,   744,    -1,    -1,
      -1,   416,   417,   418,   419,   752,   753,   754,    -1,    -1,
      -1,    15,    -1,   760,    -1,    -1,    20,    -1,    -1,    -1,
     767,    25,    26,    74,    -1,    76,    -1,    78,    -1,    80,
      -1,    35,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   474,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    76,    77,    78,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   497,    87,    -1,    -1,    -1,   502,    92,    -1,
     505,    -1,    -1,   508,   509,    -1,    -1,    -1,   513,   514,
      -1,    -1,   517,   518,   519,     6,     7,    -1,     9,    10,
      11,    12,    13,    -1,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    56,    57,    -1,    59,    -1,
      61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    92,   607,   608,   609,   610,   611,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,   663,    -1,
      46,    -1,    -1,   668,   669,   670,   671,   672,   673,    -1,
      56,    57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    70,    71,    72,    -1,    74,    75,
      76,    -1,    78,    -1,    80,    -1,    -1,   702,    -1,   704,
      -1,     8,   707,   708,    -1,   710,    92,    -1,    15,    16,
      17,    18,    19,    20,    21,    22,    -1,    24,    25,    26,
      27,    28,    29,    30,    -1,    -1,    33,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   742,    -1,   744,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   753,   754,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    74,    -1,    76,
      -1,    78,    -1,    80,    -1,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    -1,    61,    -1,    -1,    64,
      65,    66,    -1,    -1,    69,    70,    71,    72,    -1,    74,
      -1,    76,    -1,    78,    -1,    80,    -1,    -1,    83,    84,
      85,    -1,    -1,    -1,    -1,    -1,    -1,    92,    93,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    -1,    59,    -1,    61,    -1,    -1,    64,    65,
      66,    -1,    -1,    69,    70,    71,    72,    -1,    74,    -1,
      76,    -1,    78,    -1,    80,    -1,    -1,    83,    84,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    15,    16,    17,
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
      -1,    -1,    42,    -1,    -1,    -1,    46,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    55,    56,    57,    -1,    59,
      -1,    61,    -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    -1,    76,    -1,    78,    79,
      80,    -1,    -1,    83,    84,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      42,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,
      -1,    15,    -1,    55,    56,    57,    20,    59,    -1,    61,
      -1,    25,    26,    -1,    66,    -1,    -1,    -1,    70,    71,
      72,    35,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      -1,    83,    84,    -1,    -1,    -1,    -1,    51,    -1,    -1,
      92,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    -1,    -1,
      74,    -1,    76,    -1,    78,    -1,    80,    -1,    42,    -1,
      -1,    -1,    46,    87,    -1,    -1,    90,    -1,    92,    15,
      -1,    55,    56,    57,    20,    59,    -1,    61,    -1,    25,
      26,    -1,    66,    -1,    -1,    -1,    70,    71,    72,    35,
      74,    -1,    76,    77,    78,    -1,    80,    -1,    -1,    83,
      84,    -1,    -1,    -1,    -1,    51,    -1,    -1,    92,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    74,    -1,
      76,    -1,    78,    -1,    80,    -1,    42,    -1,    -1,    -1,
      46,    87,    -1,    -1,    -1,    -1,    92,    15,    -1,    55,
      56,    57,    20,    59,    -1,    61,    -1,    25,    26,    -1,
      66,    -1,    -1,    -1,    70,    71,    72,    35,    74,    -1,
      76,    77,    78,    -1,    80,    -1,    -1,    83,    84,    -1,
      -1,    -1,    -1,    51,    -1,    -1,    92,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    -1,    -1,    74,    -1,    76,    -1,
      78,    -1,    80,    -1,    42,    -1,    -1,    -1,    46,    87,
      -1,    -1,    -1,    -1,    92,    15,    -1,    55,    56,    57,
      20,    59,    -1,    61,    -1,    25,    26,    -1,    66,    -1,
      -1,    -1,    70,    71,    72,    35,    74,    -1,    76,    -1,
      78,    -1,    80,    -1,    -1,    83,    84,    -1,    -1,    -1,
      -1,    51,    -1,    -1,    92,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    -1,    74,    -1,    76,    -1,    78,    -1,
      80,    -1,    42,    -1,    -1,    -1,    46,    87,    -1,    -1,
      -1,    -1,    92,    15,    -1,    55,    56,    57,    20,    59,
      -1,    61,    -1,    25,    26,    -1,    66,    -1,    -1,    -1,
      70,    71,    72,    35,    74,    -1,    76,    -1,    78,    -1,
      80,    -1,    -1,    83,    84,    -1,    -1,    -1,    -1,    51,
      -1,    -1,    92,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      42,    -1,    -1,    -1,    46,    87,    -1,    -1,    -1,    -1,
      92,    -1,    -1,    55,    56,    57,    -1,    59,    -1,    61,
      -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,    70,    71,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      -1,    83,    84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      92,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,
      -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    55,    56,    57,    -1,    59,    -1,    61,    -1,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    70,    71,    72,    -1,
      74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      56,    57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    70,    71,    72,    -1,    74,    -1,
      76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    56,    57,
      -1,    59,    -1,    61,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    -1,    74,    75,    76,    -1,
      78,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    42,    -1,    -1,    -1,    46,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    56,    57,    -1,    59,
      -1,    61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    -1,    74,    -1,    76,    -1,    78,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    56,    57,    -1,    59,    -1,    61,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      92,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    -1,    -1,
      -1,    -1,    -1,    -1,    15,    16,    17,    18,    19,    20,
      21,    22,    46,    24,    25,    26,    27,    28,    29,    30,
      -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    70,    71,    -1,    -1,
      74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    -1,    -1,    74,    -1,    76,    -1,    78,    -1,    80
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
     113,   123,   131,   139,   153,   173,    42,    55,    66,    74,
      83,    84,   117,   118,   119,   120,   121,   122,   123,   173,
     117,   173,     0,   173,   173,   166,   173,    74,   112,   113,
     173,   112,    74,   158,   173,   158,   173,     7,     8,     9,
      10,    11,    12,    13,    20,    26,    35,    61,    72,    73,
      84,   152,   173,    74,    78,   124,   125,   145,   173,   117,
     131,   140,    78,   131,   173,    78,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    64,    65,    69,    85,    93,   114,   117,   156,
     117,   156,   151,   152,    20,   173,   117,    98,    39,    39,
      60,    74,    76,   153,    28,    31,    46,    38,   173,   121,
     119,   117,    40,    41,    42,    43,    44,    45,    47,    48,
      49,    51,    52,    53,    54,    64,    65,    69,    15,    16,
      17,    18,    19,    20,    21,    22,    24,    26,    27,    29,
      30,    74,    76,    78,    80,   139,   143,   144,   146,   173,
     143,    36,    64,    65,    85,    69,    47,    48,    49,    50,
      51,    52,    53,    54,    40,    41,    42,    43,    44,    45,
      38,    96,     8,    46,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    51,    52,    53,    54,    64,    65,
      69,    38,   113,    38,    15,    20,    26,    35,    51,    74,
      76,    78,    80,    87,    92,   159,   160,   164,   173,    32,
     111,   112,   173,    32,    74,    78,    80,   116,   163,   164,
     167,   169,   170,   173,   146,   147,   148,   149,   150,   173,
      69,    82,    38,    63,    62,   131,   132,   133,   173,    38,
      74,    78,   117,   126,   127,   145,   173,    75,    75,    75,
      75,    75,    75,    75,    75,    75,    75,    75,    75,    75,
      75,    75,    75,    75,    75,    75,    75,    81,    80,    86,
      77,    79,    81,    38,    38,    38,    80,    92,    74,   157,
     158,   163,   157,   146,   156,    29,    34,   117,   152,   117,
      38,   173,    67,   147,   147,   149,    20,   173,    46,   146,
      46,   118,   118,   118,   118,   119,   120,   120,   120,   120,
     120,   120,   120,   120,   121,   121,   121,   122,   122,   122,
     117,    97,   173,   157,   157,   173,   166,    33,    75,   162,
     163,   164,    34,    77,   162,    74,   164,   171,   173,   164,
     172,   173,   173,   117,    75,    81,    38,    40,    44,    46,
      47,    49,    50,    88,   161,   164,   173,     8,    80,   114,
     173,   163,   173,   173,     8,   163,    41,    33,    43,    41,
      75,    81,    81,    79,    81,    38,   117,    69,   125,   119,
      80,    80,   141,   142,    79,   133,    37,   131,    15,    16,
      17,    18,    19,    20,    21,    22,    24,    26,    27,    29,
      30,    74,    76,    78,    80,   139,   173,   173,    82,    73,
      79,   127,    38,   117,   129,   130,   146,    77,   117,    79,
     152,   117,   117,   117,    74,   164,    32,    75,    77,   117,
      34,    77,   117,    38,   173,   117,    75,    81,    77,    81,
      79,    81,    38,    38,    80,   117,   117,    46,    53,    75,
      75,   163,    43,   163,    77,    43,    79,    81,    34,    41,
      44,    80,    81,    34,    46,    92,   160,    74,    78,    80,
     164,   164,   152,   164,   164,   164,    90,   164,   164,   111,
      14,   106,   107,   104,   105,   115,   173,   173,   116,    81,
      34,    34,    14,   109,   110,   164,   164,   164,   164,    75,
     146,    79,   150,   146,   117,    20,   154,   155,   173,   143,
     142,   134,   135,   136,    69,    20,   173,   117,   117,    77,
      80,    36,    77,    38,    80,    80,   163,    77,    77,   117,
     117,    38,   173,    68,    75,    77,    79,   146,   146,   164,
     164,    80,   164,   173,   163,   164,   164,   163,   173,   163,
     164,   164,   164,   164,   173,   164,   173,    38,    36,    39,
      36,    39,     8,    80,   101,   107,     8,    81,    33,   173,
       8,   163,   168,   163,   102,   111,   110,    34,    80,    81,
      34,    38,     8,    33,    80,    16,    26,    78,   137,   138,
     173,   131,    38,    38,    80,    79,    82,   129,   117,   117,
      77,   117,    38,   173,   117,    80,    80,    75,    20,   165,
     173,    79,    34,    43,    80,    80,    34,    80,    41,    43,
      43,    34,    41,    34,   164,   164,   164,   164,   164,   106,
     104,   106,   105,   115,   109,    75,    81,    38,   173,    58,
     155,   173,   117,   117,   117,   135,   117,    34,    81,   117,
      38,   173,    38,    34,    38,   163,   164,    38,    39,   163,
      39,   164,   164,   164,   164,   164,   164,     8,   163,   117,
      38,   117,    38,    33,    79,   138,   173,   118,   128,   117,
      38,   173,   164,    77,   164,    79,   164,   164,   164,    80,
      75,    79,    43,    80,    41,   106,   117,   117,   117,    81,
     117,    38,   173,    38,    38,   164,   164,   118,   117,    38,
     173,   164,   164,    79,    80,   117,    38,   173,   117,    38,
     173,   117,    38,   117
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
     114,   115,   115,   116,   116,   117,   117,   117,   117,   118,
     118,   118,   118,   118,   118,   119,   119,   120,   120,   120,
     120,   120,   120,   120,   120,   120,   121,   121,   121,   121,
     121,   122,   122,   122,   122,   123,   123,   123,   123,   123,
     123,   123,   123,   123,   124,   124,   125,   126,   126,   127,
     127,   128,   128,   129,   129,   130,   130,   131,   131,   131,
     131,   131,   131,   131,   131,   131,   131,   131,   131,   131,
     131,   131,   131,   131,   131,   131,   131,   131,   131,   131,
     131,   131,   131,   131,   131,   131,   131,   131,   131,   131,
     131,   131,   131,   131,   131,   131,   131,   131,   131,   131,
     131,   131,   131,   131,   131,   131,   131,   131,   131,   131,
     131,   131,   131,   131,   132,   132,   133,   134,   134,   135,
     136,   136,   137,   137,   138,   138,   138,   139,   139,   140,
     140,   141,   141,   142,   142,   143,   143,   144,   144,   144,
     144,   144,   144,   144,   144,   144,   144,   144,   144,   144,
     144,   144,   144,   144,   144,   144,   144,   144,   144,   144,
     144,   145,   145,   145,   145,   145,   146,   147,   147,   148,
     148,   149,   149,   150,   151,   151,   151,   152,   152,   152,
     152,   152,   152,   152,   152,   152,   152,   152,   152,   152,
     152,   152,   153,   153,   153,   153,   154,   154,   155,   155,
     155,   156,   156,   156,   157,   157,   158,   159,   159,   160,
     160,   160,   160,   160,   160,   160,   160,   160,   160,   160,
     160,   160,   160,   160,   160,   160,   160,   160,   160,   160,
     161,   161,   162,   162,   163,   163,   163,   164,   164,   164,
     164,   164,   164,   164,   164,   164,   164,   164,   164,   164,
     164,   164,   164,   164,   164,   165,   165,   166,   166,   167,
     167,   168,   168,   169,   169,   170,   170,   171,   171,   172,
     172,   172,   172,   173
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
       1,     1,     2,     1,     2,     4,     4,     3,     1,     2,
       3,     3,     3,     3,     1,     6,     1,     3,     3,     3,
       3,     3,     3,     3,     3,     1,     3,     3,     3,     2,
       1,     3,     3,     3,     1,     1,     4,     5,     4,     3,
       4,     4,     6,     3,     3,     1,     3,     2,     1,     4,
       2,     3,     1,     5,     3,     3,     1,     4,     1,     5,
       4,     5,     3,     4,     4,     6,     5,     5,     5,     5,
       3,     6,     8,     3,     4,     2,     1,     1,     2,     6,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     2,     1,     3,     3,     1,     4,
       2,     0,     3,     1,     1,     1,     1,     1,     2,     2,
       1,     2,     1,     4,     6,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     4,     3,     5,     5,     3,     4,     3,     4,
       1,     1,     3,     4,     3,     4,     1,     1,     0,     3,
       1,     3,     1,     3,     0,     3,     5,     1,     1,     1,
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
#line 484 "hexpr.y" /* yacc.c:1646  */
    { yyParsedModule = (yyvsp[0].module);                     }
#line 2676 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 3:
#line 485 "hexpr.y" /* yacc.c:1646  */
    { yyParsedVar    = *(yyvsp[-2].string); yyParsedExpr = (yyvsp[0].exp); }
#line 2682 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 4:
#line 486 "hexpr.y" /* yacc.c:1646  */
    { yyParsedVar    = "";  yyParsedExpr = (yyvsp[0].exp); }
#line 2688 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 5:
#line 487 "hexpr.y" /* yacc.c:1646  */
    { yyParsedExpr   = (yyvsp[0].exp);                     }
#line 2694 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 6:
#line 490 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = (yyvsp[0].module); (yyval.module)->setOption(*(yyvsp[-1].string), m((yylsp[-2]))); }
#line 2700 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 7:
#line 491 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = new Module(*(yyvsp[-2].string), *(yyvsp[0].mdefs)); }
#line 2706 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 8:
#line 492 "hexpr.y" /* yacc.c:1646  */
    { (yyval.module) = new Module(freshName(), *(yyvsp[0].mdefs)); }
#line 2712 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 9:
#line 494 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); }
#line 2718 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 10:
#line 495 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2724 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 11:
#line 496 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdefs) = (yyvsp[-1].mdefs);                            (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2730 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 12:
#line 498 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2736 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 13:
#line 499 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2742 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 14:
#line 500 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mvtydef); }
#line 2748 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 15:
#line 501 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2754 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 16:
#line 502 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2760 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 17:
#line 504 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2766 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 18:
#line 505 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 2772 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 19:
#line 506 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 2778 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 20:
#line 507 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 2784 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 21:
#line 508 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 2790 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 22:
#line 509 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 2796 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 23:
#line 510 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 2802 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 24:
#line 511 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-9]), (yylsp[0]))); }
#line 2808 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 25:
#line 512 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-10]), (yylsp[0]))); }
#line 2814 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 26:
#line 513 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-11]), (yylsp[0]))); }
#line 2820 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 27:
#line 514 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-12]), (yylsp[0]))); }
#line 2826 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 28:
#line 515 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-13]), (yylsp[0]))); }
#line 2832 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 29:
#line 518 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MVarDef(list(freshName()), let(freshName(), ExprPtr((yyvsp[0].exp)), mktunit(m((yylsp[0]))), m((yylsp[0]))), m((yylsp[0]))); }
#line 2838 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 30:
#line 521 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MImport(yyModulePath, *(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 2844 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 31:
#line 524 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MTypeDef(MTypeDef::Transparent, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 2850 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 32:
#line 525 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new MTypeDef(MTypeDef::Opaque, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 2856 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 33:
#line 528 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydef) = new MVarTypeDef(*(yyvsp[-2].string), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]), (yylsp[0]))); }
#line 2862 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 34:
#line 530 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdef) = new MVarDef(*(yyvsp[-2].strings), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2868 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 35:
#line 533 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-3].tconstraints), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2874 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 36:
#line 534 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-6]), (yylsp[0]))); wantIndent(false); }
#line 2880 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 37:
#line 535 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2886 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 38:
#line 536 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-7].tconstraints), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-8]), (yylsp[0])));            wantIndent(false); }
#line 2892 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 39:
#line 537 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2898 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 40:
#line 538 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2904 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 41:
#line 539 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-4]), (yylsp[0])));            wantIndent(false); }
#line 2910 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 42:
#line 540 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2916 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 43:
#line 542 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundeps) = autorelease(new CFunDepDefs()); (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2922 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 44:
#line 543 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundeps) = (yyvsp[-2].fundeps);                             (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2928 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 45:
#line 545 "hexpr.y" /* yacc.c:1646  */
    { (yyval.fundep) = autorelease(new CFunDepDef(*(yyvsp[-2].strings), *(yyvsp[0].strings))); }
#line 2934 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 46:
#line 547 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydefs) = autorelease(new MVarTypeDefs()); (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2940 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 47:
#line 548 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydefs) = (yyvsp[-1].mvtydefs);                              (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2946 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 48:
#line 550 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvtydef) = (yyvsp[0].mvtydef); }
#line 2952 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 49:
#line 553 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2958 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 50:
#line 554 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-3].tconstraints),           *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2964 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 51:
#line 555 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-4]), (yylsp[0])));        wantIndent(false); }
#line 2970 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 52:
#line 556 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-5].tconstraints),           *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-6]), (yylsp[0])));        wantIndent(false); }
#line 2976 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 53:
#line 558 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdefs) = autorelease(new MVarDefs()); (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2982 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 54:
#line 559 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdefs) = (yyvsp[-1].mvdefs);                          (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2988 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 55:
#line 561 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvdef) = (yyvsp[0].mvdef); }
#line 2994 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 56:
#line 564 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[0].strings); }
#line 3000 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 57:
#line 566 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[-1].string)); (yyval.strings)->push_back(*(yyvsp[-2].string)); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3006 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 58:
#line 568 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3012 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 59:
#line 569 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3018 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 60:
#line 571 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 3024 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 61:
#line 573 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[-1].string); }
#line 3030 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 62:
#line 575 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("and")); }
#line 3036 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 63:
#line 576 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("or")); }
#line 3042 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 64:
#line 577 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3048 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 65:
#line 578 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3054 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 66:
#line 579 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("~")); }
#line 3060 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 67:
#line 580 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("=~")); }
#line 3066 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 68:
#line 581 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("===")); }
#line 3072 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 69:
#line 582 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("==")); }
#line 3078 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 70:
#line 583 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("<")); }
#line 3084 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 71:
#line 584 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("<=")); }
#line 3090 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 72:
#line 585 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(">")); }
#line 3096 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 73:
#line 586 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(">=")); }
#line 3102 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 74:
#line 587 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("in")); }
#line 3108 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 75:
#line 588 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("append")); }
#line 3114 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 76:
#line 589 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("+")); }
#line 3120 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 77:
#line 590 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("-")); }
#line 3126 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 78:
#line 591 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("*")); }
#line 3132 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 79:
#line 592 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("/")); }
#line 3138 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 80:
#line 593 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("%")); }
#line 3144 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 81:
#line 595 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3150 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 82:
#line 596 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3156 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 83:
#line 598 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3162 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 84:
#line 599 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes);                           (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3168 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 85:
#line 602 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3174 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 86:
#line 603 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3180 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 87:
#line 604 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assign(ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3186 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 88:
#line 605 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3192 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 89:
#line 607 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), (yyvsp[0].exp), m((yylsp[-1]),(yylsp[0]))); }
#line 3198 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 90:
#line 608 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("and",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3204 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 91:
#line 609 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("or",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3210 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 92:
#line 610 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("compose",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3216 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 93:
#line 611 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("in",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3222 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 94:
#line 612 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3228 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 95:
#line 614 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP3(var("if",m((yylsp[-5]))), (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-5]), (yylsp[0]))); }
#line 3234 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 96:
#line 615 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3240 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 97:
#line 617 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("~",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3246 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 98:
#line 618 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("===",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3252 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 99:
#line 619 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3258 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 100:
#line 620 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))), m((yylsp[-2]),(yylsp[0]))); }
#line 3264 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 101:
#line 621 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("<",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3270 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 102:
#line 622 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("<=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3276 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 103:
#line 623 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var(">",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3282 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 104:
#line 624 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var(">=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3288 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 105:
#line 625 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3294 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 106:
#line 627 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("+",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3300 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 107:
#line 628 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("-",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3306 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 108:
#line 629 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("append",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3312 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 109:
#line 630 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP1(var("neg",m((yylsp[-1]))), ExprPtr((yyvsp[0].exp)), m((yylsp[-1]),(yylsp[0]))); }
#line 3318 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 110:
#line 631 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3324 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 111:
#line 633 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("*", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3330 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 112:
#line 634 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("/", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3336 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 113:
#line 635 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = TAPP2(var("%", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3342 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 114:
#line 636 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3348 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 115:
#line 638 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3354 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 116:
#line 641 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-2].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3360 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 117:
#line 642 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]),(yylsp[0])))->clone(); }
#line 3366 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 118:
#line 645 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileMatch(yyParseCC, *(yyvsp[-2].exps), normPatternRules(*(yyvsp[0].patternexps), m((yylsp[-3]),(yylsp[0]))), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3372 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 119:
#line 648 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileMatchTest(yyParseCC, ExprPtr((yyvsp[-2].exp)), PatternPtr((yyvsp[0].pattern)), m((yylsp[-2]),(yylsp[0])))->clone(); }
#line 3378 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 120:
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
#line 3393 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 121:
#line 663 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-1].letbindings), ExprPtr(new Unit(m((yylsp[-3]),(yylsp[0])))), m((yylsp[-3]),(yylsp[0]))); }
#line 3399 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 122:
#line 664 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[-1].exp)), m((yylsp[-5]),(yylsp[0]))); }
#line 3405 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 123:
#line 667 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assump(ExprPtr((yyvsp[-2].exp)), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]),(yylsp[0]))); }
#line 3411 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 124:
#line 669 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].letbindings)->push_back(*(yyvsp[0].letbinding)); (yyval.letbindings) = (yyvsp[-2].letbindings); }
#line 3417 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 125:
#line 670 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3423 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 126:
#line 672 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-2].pattern)), ExprPtr((yyvsp[0].exp)))); }
#line 3429 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 127:
#line 674 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = (yyvsp[-1].letbindings); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3435 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 128:
#line 675 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3441 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 129:
#line 677 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)))); }
#line 3447 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 130:
#line 678 "hexpr.y" /* yacc.c:1646  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr(new MatchAny("_",m((yylsp[-1])))), ExprPtr((yyvsp[-1].exp)))); }
#line 3453 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 131:
#line 680 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = (yyvsp[-2].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3459 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 132:
#line 681 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3465 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 133:
#line 683 "hexpr.y" /* yacc.c:1646  */
    { (yyval.cselection) = new CSelection(); (yyval.cselection)->pat = PatternPtr((yyvsp[-4].pattern)); (yyval.cselection)->seq = ExprPtr((yyvsp[-2].exp)); (yyval.cselection)->conds = *(yyvsp[0].exps); }
#line 3471 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 134:
#line 684 "hexpr.y" /* yacc.c:1646  */
    { (yyval.cselection) = new CSelection(); (yyval.cselection)->pat = PatternPtr((yyvsp[-2].pattern)); (yyval.cselection)->seq = ExprPtr((yyvsp[0].exp)); }
#line 3477 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 135:
#line 686 "hexpr.y" /* yacc.c:1646  */
    { (yyval.cselections) = (yyvsp[-2].cselections); (yyval.cselections)->push_back(CSelectionPtr((yyvsp[0].cselection))); }
#line 3483 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 136:
#line 687 "hexpr.y" /* yacc.c:1646  */
    { (yyval.cselections) = autorelease(new CSelections()); (yyval.cselections)->push_back(CSelectionPtr((yyvsp[0].cselection))); }
#line 3489 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 137:
#line 690 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].exps), m((yylsp[-3]), (yylsp[0]))); }
#line 3495 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 138:
#line 691 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = varCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3501 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 139:
#line 694 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("range", m((yylsp[-2]))), list(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]), (yylsp[0]))); }
#line 3507 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 140:
#line 695 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("iterateS", m((yylsp[-1]))), list(ExprPtr((yyvsp[-2].exp)), fn(str::strings(".x"), fncall(var("+", m((yylsp[-1]))), list(var(".x", m((yylsp[-1]))), ExprPtr(new Int(1, m((yylsp[-1]))))), m((yylsp[-1]))), m((yylsp[-1])))), m((yylsp[-3]), (yylsp[0]))); }
#line 3513 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 141:
#line 696 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = desugarComprehension(yyParseCC, ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].cselections), m((yylsp[-4]), (yylsp[0]))); }
#line 3519 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 142:
#line 697 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkArray(*(yyvsp[-1].exps), m((yylsp[-2]), (yylsp[0]))); }
#line 3525 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 143:
#line 698 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = maybeArraySliceWithTime(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].string), m((yylsp[-3]), (yylsp[0]))); }
#line 3531 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 144:
#line 699 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkAIndex(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3537 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 145:
#line 700 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new App(var("slice", m((yylsp[-2]))), list(ExprPtr((yyvsp[-5].exp)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-5]), (yylsp[0]))); }
#line 3543 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 146:
#line 701 "hexpr.y" /* yacc.c:1646  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-1]))), list(var(vn,m((yylsp[-4]))), ExprPtr((yyvsp[-2].exp)), fncall(var("size",m((yylsp[-1]))), list(var(vn,m((yylsp[-4])))),m((yylsp[-4])))),m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3549 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 147:
#line 702 "hexpr.y" /* yacc.c:1646  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-2]))), list(var(vn,m((yylsp[-4]))), fncall(var("size",m((yylsp[-2]))), list(var(vn,m((yylsp[-2])))),m((yylsp[-4]))), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3555 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 148:
#line 705 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-3].string), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3561 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 149:
#line 706 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(".f" + str::from((yyvsp[-3].intv)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3567 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 150:
#line 707 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-1].string), ExprPtr(new Unit(m((yylsp[-1])))), m((yylsp[-2]), (yylsp[0]))); }
#line 3573 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 151:
#line 708 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-4].exp)), *(yyvsp[-1].vfields), m((yylsp[-5]), (yylsp[0]))); }
#line 3579 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 152:
#line 709 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-6].exp)), *(yyvsp[-3].vfields), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3585 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 153:
#line 712 "hexpr.y" /* yacc.c:1646  */
    { if ((yyvsp[-1].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-1].rfields), m((yylsp[-2]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-2]), (yylsp[0]))); } }
#line 3591 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 154:
#line 713 "hexpr.y" /* yacc.c:1646  */
    { if ((yyvsp[-2].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-2].rfields), m((yylsp[-3]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-3]), (yylsp[0]))); } }
#line 3597 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 155:
#line 714 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = makeProjSeq((yyvsp[-1].exp), *(yyvsp[0].strings), m((yylsp[-1]), (yylsp[0]))); }
#line 3603 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 156:
#line 717 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Fn(str::strings("x"), proj(var("x", m((yylsp[0]))), *(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3609 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 157:
#line 720 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = compileRegexFn(yyParseCC, std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0])))->clone(); }
#line 3615 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 158:
#line 723 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Pack(ExprPtr((yyvsp[0].exp)), m((yylsp[-1]), (yylsp[0]))); }
#line 3621 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 159:
#line 724 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Unpack(*(yyvsp[-4].string), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3627 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 160:
#line 727 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Bool((yyvsp[0].boolv), m((yylsp[0]))); }
#line 3633 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 161:
#line 728 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3639 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 162:
#line 729 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3645 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 163:
#line 730 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3651 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 164:
#line 731 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Short((yyvsp[0].shortv), m((yylsp[0]))); }
#line 3657 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 165:
#line 732 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Int((yyvsp[0].intv), m((yylsp[0]))); }
#line 3663 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 166:
#line 733 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Long((yyvsp[0].longv), m((yylsp[0]))); }
#line 3669 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 167:
#line 734 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Int128((yyvsp[0].int128v), m((yylsp[0]))); }
#line 3675 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 168:
#line 735 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Float((yyvsp[0].floatv), m((yylsp[0]))); }
#line 3681 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 169:
#line 736 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Double((yyvsp[0].doublev), m((yylsp[0]))); }
#line 3687 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 170:
#line 737 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3693 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 171:
#line 738 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0])))->clone(); }
#line 3699 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 172:
#line 739 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3705 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 173:
#line 740 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3711 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 174:
#line 743 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = pickNestedExp((yyvsp[-1].exps), m((yylsp[-2]),(yylsp[0]))); }
#line 3717 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 175:
#line 746 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("append", m((yylsp[-1]))); }
#line 3723 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 176:
#line 747 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("+",      m((yylsp[-1]))); }
#line 3729 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 177:
#line 748 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("-",      m((yylsp[-1]))); }
#line 3735 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 178:
#line 749 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("*",      m((yylsp[-1]))); }
#line 3741 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 179:
#line 750 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("/",      m((yylsp[-1]))); }
#line 3747 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 180:
#line 751 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("%",      m((yylsp[-1]))); }
#line 3753 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 181:
#line 752 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("~",      m((yylsp[-1]))); }
#line 3759 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 182:
#line 753 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("===",    m((yylsp[-1]))); }
#line 3765 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 183:
#line 754 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("==",     m((yylsp[-1]))); }
#line 3771 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 184:
#line 755 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("!=",     m((yylsp[-1]))); }
#line 3777 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 185:
#line 756 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("<",      m((yylsp[-1]))); }
#line 3783 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 186:
#line 757 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var(">",      m((yylsp[-1]))); }
#line 3789 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 187:
#line 758 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var(">=",     m((yylsp[-1]))); }
#line 3795 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 188:
#line 759 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("<=",     m((yylsp[-1]))); }
#line 3801 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 189:
#line 760 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("and",    m((yylsp[-1]))); }
#line 3807 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 190:
#line 761 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("or",     m((yylsp[-1]))); }
#line 3813 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 191:
#line 762 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("in",     m((yylsp[-1]))); }
#line 3819 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 192:
#line 763 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Var("not",    m((yylsp[-1]))); }
#line 3825 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 193:
#line 766 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exp) = new Assump(fncall(var("unsafeCast", m((yylsp[-1]))), list(mktunit(m((yylsp[-1])))), m((yylsp[-1]))), qualtype(tapp(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp)))))), m((yylsp[-1]))); }
#line 3831 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 194:
#line 768 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prules) = (yyvsp[-1].prules); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3837 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 195:
#line 769 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prules) = autorelease(new Grammar()); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3843 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 196:
#line 771 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prule) = autorelease(new Grammar::value_type(*(yyvsp[-2].string), *(yyvsp[0].prdefs))); }
#line 3849 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 197:
#line 773 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdefs) = (yyvsp[-2].prdefs); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3855 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 198:
#line 774 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdefs) = autorelease(new GrammarRules()); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3861 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 199:
#line 776 "hexpr.y" /* yacc.c:1646  */
    { (yyval.prdef) = autorelease(new GrammarRule(*(yyvsp[-3].pbelems), ExprPtr((yyvsp[-1].exp)))); }
#line 3867 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 200:
#line 778 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelems) = (yyvsp[-1].pbelems); (yyval.pbelems)->push_back(*(yyvsp[0].pbelem)); }
#line 3873 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 201:
#line 779 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelems) = autorelease(new BoundGrammarValues()); }
#line 3879 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 202:
#line 781 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue(*(yyvsp[-2].string), GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3885 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 203:
#line 782 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue("_", GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3891 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 204:
#line 784 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GSymRef(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3897 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 205:
#line 785 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GStr(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3903 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 206:
#line 786 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pvalue) = new GStr(std::string(1, str::readCharDef(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3909 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 207:
#line 788 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3915 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 208:
#line 789 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3921 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 209:
#line 791 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = (yyvsp[-1].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3927 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 210:
#line 792 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3933 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 211:
#line 794 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexps) = (yyvsp[-1].patternexps); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3939 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 212:
#line 795 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexps) = autorelease(new PatternRows()); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3945 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 213:
#line 797 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)))); }
#line 3951 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 214:
#line 798 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-4].patterns), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)))); }
#line 3957 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 215:
#line 801 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[-1].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3963 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 216:
#line 802 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = autorelease(new Patterns()); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3969 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 217:
#line 804 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Bool((yyvsp[0].boolv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3975 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 218:
#line 805 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3981 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 219:
#line 806 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3987 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 220:
#line 807 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Short((yyvsp[0].shortv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3993 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 221:
#line 808 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int((yyvsp[0].intv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3999 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 222:
#line 809 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Long((yyvsp[0].longv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4005 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 223:
#line 810 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int128((yyvsp[0].int128v), m((yylsp[0])))), m((yylsp[0]))); }
#line 4011 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 224:
#line 811 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Double((yyvsp[0].doublev), m((yylsp[0])))), m((yylsp[0]))); }
#line 4017 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 225:
#line 812 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = mkpatarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 4023 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 226:
#line 813 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = mkpatarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4029 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 227:
#line 814 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkTimespanPrim(*(yyvsp[0].strings), m((yylsp[0]))), mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 4035 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 228:
#line 815 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4041 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 229:
#line 816 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchLiteral(mkDateTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4047 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 230:
#line 817 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRegex(std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0]))); }
#line 4053 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 231:
#line 818 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4059 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 232:
#line 819 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4065 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 233:
#line 820 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-1].string), PatternPtr(new MatchLiteral(PrimitivePtr(new Unit(m((yylsp[-1])))), m((yylsp[-1])))), m((yylsp[-2]),(yylsp[0]))); }
#line 4071 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 234:
#line 821 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-3].string), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4077 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 235:
#line 822 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchVariant(".f" + str::from((yyvsp[-3].intv)), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4083 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 236:
#line 823 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4089 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 237:
#line 824 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4095 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 238:
#line 825 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4101 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 239:
#line 826 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4107 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 240:
#line 827 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = patVarCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4113 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 241:
#line 829 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchAny(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4119 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 242:
#line 830 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4125 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 243:
#line 831 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4131 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 244:
#line 832 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4137 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 245:
#line 833 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4143 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 246:
#line 835 "hexpr.y" /* yacc.c:1646  */
    { (yyval.pattern) = (yyvsp[0].pattern); }
#line 4149 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 247:
#line 837 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[0].patterns); }
#line 4155 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 248:
#line 838 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = new Patterns(); }
#line 4161 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 249:
#line 840 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = (yyvsp[-2].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4167 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 250:
#line 841 "hexpr.y" /* yacc.c:1646  */
    { (yyval.patterns) = new Patterns(); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4173 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 251:
#line 843 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfields) = (yyvsp[-2].recpatfields); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4179 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 252:
#line 844 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfields) = new MatchRecord::Fields(); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4185 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 253:
#line 846 "hexpr.y" /* yacc.c:1646  */
    { (yyval.recpatfield) = new MatchRecord::Field(*(yyvsp[-2].string), PatternPtr((yyvsp[0].pattern))); }
#line 4191 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 254:
#line 848 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); }
#line 4197 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 255:
#line 849 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4203 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 256:
#line 850 "hexpr.y" /* yacc.c:1646  */
    { (yyval.rfields) = (yyvsp[-4].rfields);                                     (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4209 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 257:
#line 852 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4215 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 258:
#line 853 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("data")); }
#line 4221 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 259:
#line 854 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("type")); }
#line 4227 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 260:
#line 855 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("where")); }
#line 4233 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 261:
#line 856 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("class")); wantIndent(false); }
#line 4239 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 262:
#line 857 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("instance")); wantIndent(false); }
#line 4245 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 263:
#line 858 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("exists")); }
#line 4251 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 264:
#line 859 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("import")); }
#line 4257 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 265:
#line 860 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("module")); }
#line 4263 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 266:
#line 861 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("parse")); }
#line 4269 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 267:
#line 862 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("do")); }
#line 4275 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 268:
#line 863 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("return")); }
#line 4281 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 269:
#line 864 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string("fn")); }
#line 4287 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 270:
#line 865 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(".f" + str::from((yyvsp[0].intv)))); }
#line 4293 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 271:
#line 866 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = autorelease(new std::string(str::unescape(str::trimq(*(yyvsp[0].string))))); }
#line 4299 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 272:
#line 868 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-2].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4305 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 273:
#line 869 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = (yyvsp[-1].strings); str::seq x = tupSectionFields(*(yyvsp[0].string)); (yyval.strings)->insert((yyval.strings)->end(), x.begin(), x.end()); }
#line 4311 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 274:
#line 870 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4317 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 275:
#line 871 "hexpr.y" /* yacc.c:1646  */
    { (yyval.strings) = autorelease(new str::seq()); *(yyval.strings) = tupSectionFields(*(yyvsp[0].string)); }
#line 4323 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 276:
#line 873 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vfields) = autorelease(new Case::Bindings()); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4329 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 277:
#line 874 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vfields) = (yyvsp[-2].vfields); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4335 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 278:
#line 876 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-2].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4341 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 279:
#line 877 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-4].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4347 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 280:
#line 878 "hexpr.y" /* yacc.c:1646  */
    { (yyval.vbind) = autorelease(new Case::Binding(".f" + str::from((yyvsp[-4].intv)), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4353 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 281:
#line 880 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); }
#line 4359 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 282:
#line 881 "hexpr.y" /* yacc.c:1646  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4365 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 283:
#line 882 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].exps)->push_back(ExprPtr((yyvsp[0].exp))); (yyval.exps) = (yyvsp[-2].exps); }
#line 4371 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 284:
#line 884 "hexpr.y" /* yacc.c:1646  */
    { (yyval.qualtype) = new QualType(*(yyvsp[-2].tconstraints), *(yyvsp[0].mtype)); }
#line 4377 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 285:
#line 885 "hexpr.y" /* yacc.c:1646  */
    { (yyval.qualtype) = new QualType(Constraints(), *(yyvsp[0].mtype)); }
#line 4383 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 286:
#line 888 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraints) = (yyvsp[-1].tconstraints); }
#line 4389 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 287:
#line 890 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraints) = autorelease(new Constraints()); (yyval.tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); }
#line 4395 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 288:
#line 891 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); (yyval.tconstraints) = (yyvsp[-2].tconstraints); }
#line 4401 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 289:
#line 893 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(*(yyvsp[-1].string), *(yyvsp[0].mtypes)); }
#line 4407 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 290:
#line 894 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(EqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4413 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 291:
#line 895 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(NotEqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4419 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 292:
#line 896 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(FixIsoRecur::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4425 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 293:
#line 897 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4431 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 294:
#line 898 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4437 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 295:
#line 899 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4443 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 296:
#line 900 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4449 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 297:
#line 901 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4455 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 298:
#line 902 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(1), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4461 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 299:
#line 904 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4467 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 300:
#line 905 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4473 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 301:
#line 906 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4479 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 302:
#line 907 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4485 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 303:
#line 909 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4491 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 304:
#line 910 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4497 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 305:
#line 911 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4503 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 306:
#line 912 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4509 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 307:
#line 914 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), TString::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype))); }
#line 4515 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 308:
#line 915 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), *(yyvsp[-5].mtype),                *(yyvsp[-3].mtype))); }
#line 4521 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 309:
#line 916 "hexpr.y" /* yacc.c:1646  */
    { (yyval.tconstraint) = new Constraint(AppendsToUnqualifier::constraintName(), list(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4527 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 310:
#line 918 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4533 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 311:
#line 919 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-1].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4539 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 312:
#line 921 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4545 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 313:
#line 922 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4551 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 314:
#line 924 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Func::make(tuplety(*(yyvsp[-2].mtypes)), *(yyvsp[0].mtype)))); }
#line 4557 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 315:
#line 925 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeTupleType(*(yyvsp[0].mtypes)))); }
#line 4563 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 316:
#line 926 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeSumType(*(yyvsp[0].mtypes)))); }
#line 4569 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 317:
#line 928 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(monoTypeByName(*(yyvsp[0].string)))); }
#line 4575 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 318:
#line 929 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(OpaquePtr::make(str::replace<char>(*(yyvsp[-1].string), ".", "::"), 0, false))); }
#line 4581 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 319:
#line 930 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("[]"))); }
#line 4587 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 320:
#line 931 "hexpr.y" /* yacc.c:1646  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(Array::make(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4593 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 321:
#line 932 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(FixedArray::make(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype)))); }
#line 4599 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 322:
#line 933 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("->"))); }
#line 4605 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 323:
#line 934 "hexpr.y" /* yacc.c:1646  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(clone(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4611 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 324:
#line 935 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeRecType(*(yyvsp[-1].mreclist)))); }
#line 4617 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 325:
#line 936 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeVarType(*(yyvsp[-1].mvarlist)))); }
#line 4623 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 326:
#line 937 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("unit"))); }
#line 4629 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 327:
#line 938 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(((yyvsp[0].intv) == 0) ? Prim::make("void") : TLong::make((yyvsp[0].intv)))); }
#line 4635 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 328:
#line 939 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr((yyvsp[0].boolv) ? TLong::make(1) : TLong::make(0))); }
#line 4641 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 329:
#line 940 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Exists::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4647 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 330:
#line 941 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype), *(yyvsp[0].mtype)))); }
#line 4653 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 331:
#line 942 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype)))); }
#line 4659 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 332:
#line 943 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Recursive::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4665 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 333:
#line 944 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TString::make(str::unescape(str::trimq(*(yyvsp[0].string)))))); }
#line 4671 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 334:
#line 945 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp))))))); }
#line 4677 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 335:
#line 947 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TVar::make(*(yyvsp[0].string)))); }
#line 4683 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 336:
#line 948 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TLong::make((yyvsp[0].intv)))); }
#line 4689 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 337:
#line 950 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4695 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 338:
#line 951 "hexpr.y" /* yacc.c:1646  */
    { (yyval.string) = (yyvsp[-2].string); *(yyval.string) += "."; *(yyval.string) += *(yyvsp[0].string); }
#line 4701 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 339:
#line 953 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4707 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 340:
#line 954 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-1].mtypes)->insert((yyvsp[-1].mtypes)->begin(), *(yyvsp[-3].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4713 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 341:
#line 956 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4719 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 342:
#line 957 "hexpr.y" /* yacc.c:1646  */
    { (yyvsp[-2].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-2].mtypes); }
#line 4725 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 343:
#line 959 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4731 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 344:
#line 960 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4737 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 345:
#line 962 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[-2].mtype)); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4743 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 346:
#line 963 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4749 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 347:
#line 965 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mreclist) = (yyvsp[-4].mreclist);                                 (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4755 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 348:
#line 966 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mreclist) = autorelease(new Record::Members()); (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4761 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 349:
#line 968 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = (yyvsp[-4].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4767 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 350:
#line 969 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = (yyvsp[-2].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4773 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 351:
#line 970 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4779 "hexpr.parse.C" /* yacc.c:1646  */
    break;

  case 352:
#line 971 "hexpr.y" /* yacc.c:1646  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4785 "hexpr.parse.C" /* yacc.c:1646  */
    break;


#line 4789 "hexpr.parse.C" /* yacc.c:1646  */
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
#line 975 "hexpr.y" /* yacc.c:1906  */

#pragma GCC diagnostic pop


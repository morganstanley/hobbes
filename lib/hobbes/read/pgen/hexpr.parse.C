/* A Bison parser, made by GNU Bison 3.3.2.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.3.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "hexpr.y" /* yacc.c:337  */

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
#line 25 "hexpr.y" /* yacc.c:337  */

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


#line 326 "hexpr.parse.C" /* yacc.c:337  */
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
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
#line 262 "hexpr.y" /* yacc.c:352  */

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

#line 522 "hexpr.parse.C" /* yacc.c:352  */
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
typedef unsigned short yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short yytype_int16;
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
#  define YYSIZE_T unsigned
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

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
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
#define YYLAST   2927

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  94
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  80
/* YYNRULES -- Number of rules.  */
#define YYNRULES  352
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  773

#define YYUNDEFTOK  2
#define YYMAXUTOK   348

/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  ((unsigned) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
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
     865,   867,   868,   869,   870,   872,   873,   875,   876,   877,
     879,   880,   881,   883,   884,   887,   889,   890,   892,   893,
     894,   895,   896,   897,   898,   899,   900,   901,   903,   904,
     905,   906,   908,   909,   910,   911,   913,   914,   915,   917,
     918,   920,   921,   923,   924,   925,   927,   928,   929,   930,
     931,   932,   933,   934,   935,   936,   937,   938,   939,   940,
     941,   942,   943,   944,   946,   947,   949,   950,   952,   953,
     955,   956,   958,   959,   961,   962,   964,   965,   967,   968,
     969,   970,   972
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

#define YYPACT_NINF -582

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-582)))

#define YYTABLE_NINF -352

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     378,  1427,  2200,  2200,    47,    53,    53,    53,    41,    41,
      89,    89,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,   432,
      28,  2200,  2824,   -24,  2824,    53,   157,  1653,  2200,   432,
     251,  2200,  -582,   877,  -582,  -582,  -582,  -582,  -582,  -582,
      59,  -582,   213,   129,   185,    33,  2668,  2512,  2200,  1732,
    1516,  1516,  -582,    82,   118,   562,   377,   382,  -582,   199,
    -582,  -582,  -582,  1427,   309,   247,  -582,  2742,    43,  -582,
    -582,    78,  1321,   328,    41,   373,  2004,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  1516,    53,    97,  -582,   371,  -582,   352,   208,
    2746,    53,   208,   400,  2278,   275,   383,  2590,   389,   390,
     391,   432,   393,   394,   395,   396,   397,   398,   401,   403,
    1532,   406,   407,   408,  -582,  -582,   410,  -582,   274,   255,
      79,   144,   448,   449,    88,   356,  -582,  2082,  2082,  1516,
    2200,  1966,   185,  -582,  -582,   432,  2200,   215,  -582,  -582,
     421,   275,   383,  2590,   389,   390,   391,   393,   394,   395,
     397,   398,   401,   403,   406,   407,   408,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    1516,  1516,    53,   299,   129,  1011,  -582,  -582,  -582,  2847,
    2434,  2434,  2434,  2434,  2512,  2668,  2668,  2668,  2668,  2668,
    2668,  2668,  2668,  2668,  2668,  2668,   503,   503,   503,  2200,
    -582,   877,    53,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    2082,  -582,  2082,  -582,  -582,  -582,    53,    53,   464,   786,
    2160,  2160,    53,  2200,   283,  -582,   180,  2160,    53,    27,
      41,  2742,    53,   464,    53,    53,    49,  -582,    50,   458,
     451,   454,  -582,  -582,   292,   411,   322,  -582,   460,  2200,
       8,  2512,   416,   420,   208,    20,  -582,   465,  2824,  1810,
     432,   419,  1888,  -582,   468,   470,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  2200,  1516,  2044,  -582,
    -582,   318,  2200,  2200,  2200,  -582,  -582,   636,  -582,   477,
    -582,  -582,  -582,   293,   433,  2200,   119,  -582,  -582,  2200,
     218,  2200,   295,   183,   327,   473,   152,  2200,  -582,  2200,
     184,   427,   427,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
     877,  -582,  -582,  -582,   467,   109,   442,  -582,   336,  -582,
       5,  2004,  -582,  1092,   464,    54,   350,   501,    93,   312,
     130,   491,   455,  -582,  1321,   285,  2160,  2160,   432,  2160,
    2160,  2160,  1191,  2160,   462,    41,   529,    53,    53,  2004,
     472,   512,   514,   540,  -582,  2160,  2160,  2160,  2160,  -582,
     480,  1516,  -582,    42,  1516,  -582,  2200,  -582,  -582,   332,
    1516,   420,  -582,  -582,  -582,  -582,   145,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    1810,  2356,   432,   360,   129,  -582,   460,  -582,  2200,  -582,
    -582,  2200,  -582,  -582,   214,   521,  -582,   484,  -582,   525,
    -582,   485,   486,   464,   159,  2004,  -582,  -582,   490,  2122,
    -582,  -582,  2200,   220,   504,  -582,   494,  -582,   499,  -582,
      45,  1516,  1516,  -582,  -582,  -582,  2160,  -582,  -582,  -582,
    -582,  2160,   502,  -582,  2160,  -582,    53,  2004,  2160,  2004,
    -582,    53,  2004,  2160,  -582,  -582,  2160,  2160,  2160,     2,
      13,    92,   462,   462,   462,  -582,   462,   462,    29,    41,
     529,  -582,    23,  -582,   125,  -582,  -582,   187,  2004,  2004,
    2004,    41,   540,  -582,   462,   462,   462,   462,  -582,  -582,
    -582,  -582,  -582,  -582,   537,   341,  -582,   361,  1611,  -582,
     505,  -582,    35,  2824,   551,   156,   513,   509,  -582,  1516,
    2200,  -582,  2200,  -582,  -582,  -582,  -582,  -582,   519,  -582,
    2200,   250,  2200,  -582,  -582,  -582,   526,   527,   462,   210,
     369,    51,   570,  -582,    22,   100,   538,   571,   539,    55,
     462,   105,   116,   583,     1,   586,  2160,  2160,  2160,  2160,
    2160,   529,    53,  -582,  -582,   529,    53,    53,  -582,   540,
    -582,   297,  -582,  -582,   585,  -582,    53,   568,   332,    53,
    2200,  2200,  2200,  -582,  -582,  -582,  2200,  -582,  -582,   599,
     208,  2356,  2356,  -582,  -582,  -582,  -582,   555,  -582,  -582,
    -582,  2200,   258,  -582,  -582,  -582,   600,  -582,   603,  -582,
     604,  2004,  2160,   605,   602,  2004,   606,  2160,  2160,  2160,
    2160,  2160,  2160,   462,   462,   462,   462,   462,   529,    24,
     529,  -582,    53,   540,  -582,  2004,  2200,   608,  2200,  -582,
     610,  -582,   611,  -582,  -582,   575,   287,  2434,  -582,  2200,
     261,  2160,   573,  2160,  -582,   154,  2160,  2160,  -582,  2160,
     254,   217,   178,   127,   260,   110,   529,  -582,  -582,  2200,
    -582,  2200,  2200,  -582,  -582,  -582,   184,   574,  -582,  2200,
     270,   462,  -582,   462,   619,   462,   462,   462,   620,  -582,
    -582,  2160,  -582,  2160,   529,  -582,  -582,  -582,  2434,  -582,
    2200,   272,  2160,  2160,   230,   266,   184,  -582,  2200,   277,
     462,   462,  -582,  -582,  -582,  2200,   298,  -582,  2200,   621,
    -582,  2200,  -582
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,     9,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   160,   161,   162,   163,   164,   165,   166,   167,
     168,   169,   352,   170,   157,   207,   172,   173,   274,     0,
       0,     0,     0,     0,     0,     0,     0,   280,   280,   254,
       0,     0,     2,     8,    10,    12,    13,    14,    15,    16,
       0,    29,   115,   171,   156,   138,     0,     0,     0,   280,
       0,     0,     4,    88,    94,    96,   105,   110,   114,   138,
       5,   138,     1,     9,     0,    30,   336,     0,     0,    58,
      60,     0,     0,     0,     0,     0,     0,   265,   260,   264,
     259,   258,   261,   262,   270,   263,   266,   267,   268,   269,
     273,   257,   248,     0,     0,   125,     0,   241,     0,   210,
       0,     0,   158,     0,     0,     0,     0,     0,     0,     0,
       0,    65,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    64,    67,     0,   281,     0,   281,
       0,     0,     0,     0,     0,     0,    11,     0,     0,     0,
     280,     0,   155,   208,   272,     0,     0,     0,   109,    89,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   217,   218,   219,
     225,   220,   221,   222,   223,   224,   226,   230,   228,   229,
     248,   248,     0,     0,   227,     0,   246,   216,   240,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       6,     9,     0,    75,    76,    77,    78,    79,    80,    65,
      69,    68,    66,    70,    71,    72,    73,    62,    63,    74,
       0,    59,     0,   327,   326,   332,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   286,     0,   316,     0,    39,
      56,    60,     0,     0,     0,     0,    49,    83,   342,     0,
     314,   315,   316,   250,     0,   247,     0,   252,     0,     0,
       0,     0,     0,     0,   209,     0,   195,     0,     0,   248,
     254,     0,     0,   128,     0,   138,   175,   176,   177,   178,
     179,   180,   183,   182,   181,   184,   185,   188,   186,   187,
     192,   189,   190,   191,    61,   174,     0,     0,     0,   142,
     153,     0,     0,     0,     0,   150,   193,     0,    33,     0,
     284,   123,   119,     0,   172,     0,     0,   271,    17,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   215,     0,
      87,    90,    91,    92,    93,    99,    98,    97,   100,   101,
     102,   103,   104,   108,   106,   107,   111,   112,   113,     3,
       7,   337,    31,    32,     0,     0,     0,   325,     0,   312,
     342,     0,   318,     0,     0,     0,     0,   316,     0,     0,
     316,     0,     0,   285,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   288,   309,     0,     0,     0,     0,     0,
     312,     0,   351,     0,    84,     0,     0,     0,     0,   242,
       0,     0,   244,     0,     0,   116,     0,   124,   126,     0,
       0,   118,   212,   120,   194,   201,     0,   160,   161,   162,
     163,   164,   165,   166,   167,   169,   170,   157,   172,   173,
     248,   248,   254,     0,   171,   138,     0,   130,     0,   121,
     127,     0,   282,   136,     0,     0,   140,     0,   154,     0,
     255,     0,     0,     0,   342,     0,   137,   143,     0,     0,
     144,    18,     0,     0,     0,   236,     0,   231,     0,   238,
       0,     0,     0,   233,    85,    86,     0,   317,   321,   322,
     311,     0,     0,   319,     0,   323,     0,     0,     0,     0,
     324,     0,     0,     0,   333,   287,     0,     0,     0,     0,
       0,     0,   289,   291,   290,   330,   329,   310,    35,     0,
      41,    46,    40,    43,     0,    81,    57,    50,     0,     0,
       0,     0,    51,    53,   344,   313,   343,   345,   243,   249,
     245,   251,   253,   117,     0,     0,   275,     0,     0,   211,
     196,   198,     0,     0,     0,     0,     0,     0,   141,     0,
       0,   139,     0,   149,   148,   283,   147,   146,     0,    19,
       0,     0,     0,   237,   232,   239,     0,     0,   328,     0,
       0,     0,     0,   347,   342,     0,     0,   349,   350,   342,
     331,     0,     0,   316,     0,   316,     0,     0,     0,     0,
       0,     0,     0,    48,    47,     0,     0,     0,    82,     0,
     340,     0,   350,    55,     0,    54,     0,   151,     0,     0,
       0,     0,     0,   201,   206,   205,     0,   200,   203,   204,
     159,     0,     0,   150,   122,   129,   135,   134,   256,   145,
      20,     0,     0,    95,   235,   234,     0,   335,     0,   334,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   308,   301,   300,   299,   298,    37,    36,
      42,    44,    45,    52,   339,     0,     0,     0,     0,   276,
       0,   277,     0,   213,   197,     0,     0,     0,    21,     0,
       0,     0,     0,     0,   346,     0,     0,     0,   348,     0,
     344,     0,     0,     0,     0,     0,     0,   341,    34,     0,
     152,     0,     0,   199,   202,   204,   132,   133,    22,     0,
       0,   297,   320,   295,     0,   303,   307,   306,     0,   294,
     292,     0,   302,     0,    38,   279,   278,   214,     0,    23,
       0,     0,     0,     0,     0,     0,   131,    24,     0,     0,
     296,   305,   293,   304,    25,     0,     0,    26,     0,     0,
      27,     0,    28
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -582,  -582,   587,   443,   -26,  -582,  -582,   134,  -582,  -582,
      56,    61,  -581,  -503,  -582,    48,  -518,  -380,   438,    -5,
     405,    58,   263,    -2,  -190,   -35,   524,   -30,   237,     7,
    -582,   399,  -582,   388,  -582,   112,  -582,   -18,  -582,   404,
    -582,    52,  -582,  -582,   -14,   -17,  -582,  -582,   252,   -46,
    -582,   -91,   -55,  -170,  -582,  -174,  -382,  -582,   -10,   -50,
    -582,    60,   -31,  -120,   440,  -582,   296,  -582,   444,   -77,
     914,  -582,   445,  -582,  -582,  -582,  -582,  -582,  -582,   592
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,    42,    43,    44,    45,    46,    47,   623,    48,
     532,   533,   530,   531,    49,   542,   543,   259,   260,    50,
     136,   534,   266,   137,    63,    64,    65,    66,    67,    68,
     104,   105,   292,   293,   727,   463,   464,    52,   285,   286,
     560,   561,   562,   637,   638,    53,   110,   431,   432,   195,
     196,   106,   273,   274,   275,   276,   277,   141,   142,    54,
     555,   556,   138,   328,   329,   254,   255,   403,   378,   330,
     268,   658,    75,   269,   621,   270,   271,   386,   389,    71
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      62,    70,   152,    79,    79,   197,   197,   140,    51,   267,
     350,   351,   352,   353,   109,   199,   112,   146,   344,   100,
     342,   343,   159,   294,   625,   528,   158,   614,   331,   108,
     678,   615,   716,    22,   680,   406,   139,   611,  -338,   145,
     606,   551,   671,   194,   194,    22,   415,    72,   501,   607,
      51,   634,   608,    22,   111,  -338,   160,   413,    22,   152,
      22,   635,   152,   415,   243,   662,    22,    22,    22,   244,
      22,   156,   -60,   241,    22,   245,   241,   426,    22,    79,
      51,   240,   102,  -338,   246,   194,   103,   158,  -338,   402,
     402,   415,   284,   402,   332,   159,   667,   504,   147,   433,
     247,   402,   102,    22,   616,   616,   103,   407,   551,   612,
     402,   100,   291,   636,    22,    77,   242,    77,   200,   333,
     372,   550,   373,   263,   585,   249,   324,   264,   609,   265,
     660,   610,   194,   158,   508,   744,   252,   509,   402,   402,
     348,   253,   402,   402,   348,   337,   201,   202,   668,   336,
      22,   743,    77,   479,   338,   222,   319,   153,   617,   669,
     316,   624,   497,    82,   512,   625,   279,   203,   325,   354,
     741,   379,   379,   194,   194,   614,    28,   614,   194,   280,
     663,   402,   194,   363,   364,   365,   410,   204,   402,   414,
     492,    29,  -338,   402,   642,   619,   480,   395,   402,   396,
     415,   294,   243,   397,   402,   398,   399,   244,   400,   401,
    -351,  -351,    22,   245,   563,   402,   154,   369,   395,   150,
     396,   151,   246,   320,   397,   321,   398,   399,    51,   400,
     401,   155,   493,   734,   152,   114,   643,   219,   247,    28,
      22,   614,   402,    22,    28,    22,   428,   402,   201,   202,
     410,   392,   148,   339,    29,   241,   482,   740,   580,    29,
     487,   263,   465,   249,   488,   264,   402,   265,   402,   203,
     436,   143,   454,   149,   252,    22,    22,   425,   344,   253,
     342,   343,   150,    22,   151,   656,    22,   150,   651,   151,
     291,   568,   739,   222,   569,    22,   699,    22,   402,   729,
     194,   500,    22,   634,   502,   402,   500,   379,   750,   762,
     758,   469,    22,   635,   462,   765,   467,   221,   402,   345,
     470,   471,   472,    22,    22,    87,    88,    89,    90,    91,
      92,    93,   267,   478,   738,   317,   768,   481,    94,   484,
     742,   318,   402,    22,   146,   494,   763,   495,   402,   315,
     296,   243,   554,    95,   402,   316,   244,    22,   393,   516,
     258,    22,   245,   517,   394,   518,   549,   419,   476,   552,
     485,   246,   684,   420,   316,   197,   486,    51,   685,    96,
     564,     1,     2,     3,   558,    22,   152,   247,   521,   657,
      97,    98,   510,   511,    22,   629,   410,   468,   575,   630,
      79,   422,    99,   423,   194,   262,   489,   194,   490,   281,
     263,   499,   249,   194,   264,   282,   265,   213,   214,   215,
     140,   627,   628,   252,   553,   216,   217,   218,   253,   505,
     593,   506,   596,   454,   454,   598,   586,   587,   288,    87,
      88,    89,    90,    91,    92,    93,    78,    81,   326,   139,
      83,    85,    94,   366,   367,   368,   566,    22,   297,   567,
     414,   620,   593,   622,   299,   300,   301,    95,   302,   303,
     304,   305,   306,   307,   194,   194,   308,   578,   309,   243,
     579,   311,   312,   313,   244,   314,   322,   323,   341,    22,
     245,   416,   421,    96,   417,   418,   429,   376,   424,   246,
     430,   457,   435,   348,    97,    98,   461,   726,  -241,   475,
     477,   491,   203,   496,   465,   247,    99,   498,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,   507,    79,   513,   263,   377,
     249,   194,   264,   529,   265,   640,   539,   514,   540,    29,
     402,   252,   194,   538,   541,   548,   253,   570,   756,    30,
      31,   571,    32,   572,    33,   573,   574,   576,   647,   583,
     648,   626,   582,    34,    35,    36,   584,    59,   650,    38,
     653,    39,   590,    40,   704,   633,   586,   587,   708,   641,
     152,   645,   644,    55,    69,    41,   649,    73,    74,    76,
      80,    80,    84,    86,   661,   665,   654,   655,   717,   205,
     206,   207,   208,   209,   210,   211,   212,   670,   664,   666,
     672,   101,   107,   686,   454,   454,   688,   113,   691,   692,
     693,   101,   144,   696,   695,    55,   697,   702,   701,   471,
     472,   707,   703,   706,   722,   709,   719,   157,   721,   698,
     732,   243,   198,   198,   723,   748,   244,   752,   753,   771,
     220,    22,   245,   613,   370,    55,   408,   683,   679,   376,
      80,   246,   537,    80,   257,   682,   261,   681,   272,   427,
     460,   646,   724,   559,   718,   694,   720,   247,   689,   434,
     515,     0,   375,   383,   198,   278,     0,   728,     0,     0,
       0,     0,     0,   287,     0,     0,   295,     0,     0,     0,
     473,   377,   249,   101,   250,     0,   251,   745,     0,   746,
     747,     0,     0,   252,     0,     0,     0,   749,   253,   355,
     356,   357,   358,   359,   360,   361,   362,     0,     0,   272,
     272,   198,     0,     0,     0,     0,     0,   101,   757,   340,
       0,     0,     0,     0,     0,     0,   764,     0,     0,     0,
       0,     0,     0,   767,     0,     0,   770,     0,     0,   772,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   198,   198,   278,   346,     0,   198,     0,     0,
       0,   198,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   243,     0,     0,     0,     0,   244,     0,     0,     0,
       0,    22,   245,    55,   371,     0,     0,     0,     0,     0,
     381,   246,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   272,     0,   272,     0,     0,   247,   374,    76,
     272,   272,   387,   390,   391,     0,     0,     0,     0,   272,
     405,     0,    80,     0,   409,   272,   411,   412,   272,     0,
     263,     0,   249,   382,   264,     0,   265,     0,     0,     0,
       0,     0,   107,   252,     0,     0,     0,   287,   253,     0,
       0,   455,   456,     0,   295,     0,     7,     8,     9,    10,
      11,     0,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,   198,
       0,     0,     0,   101,     0,     0,     0,     0,     0,   257,
       0,     0,     0,    29,     0,     0,     0,     0,     0,     0,
       0,     0,   483,    30,    31,     0,    32,     0,    33,     0,
       0,     0,     0,     0,     0,     0,     0,    34,    35,    36,
       0,    37,     0,    38,     0,    39,     0,    40,     0,     0,
       0,     0,    55,     0,     0,     0,     0,     0,     0,    41,
     272,     0,     0,   272,     0,   272,   272,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   257,     0,   272,   272,
     101,   272,   272,   272,   272,   272,   256,   261,     0,   535,
     536,   272,     0,     0,     0,     0,     0,   272,   272,   272,
     272,     0,     0,   198,     0,   278,   198,     0,     0,     0,
       0,   557,   198,     0,     0,     0,   177,   178,   179,   180,
     181,   182,   183,   184,     0,   185,    22,   186,   187,    25,
     188,   189,   455,   455,   456,   565,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   347,     0,     0,
       0,     0,     0,     0,     0,   272,     0,   272,     0,     0,
       0,     0,     0,     0,     0,   581,     0,     0,     0,     0,
       0,     0,   278,   198,   198,   190,     0,   191,   272,   192,
       0,   193,     0,   272,     0,     0,   272,     0,   592,   272,
     272,   272,     0,   597,   272,   272,     0,   243,   272,   603,
     605,     0,   244,     0,     0,     0,     0,    22,   245,     0,
       0,    80,     0,     0,     0,     0,   618,   246,     0,   272,
     272,   272,   272,   261,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   247,     0,     0,     0,     0,     0,     0,
     198,     0,     0,     0,   639,     0,     0,     0,     0,     0,
       0,   198,   380,     0,   385,   388,   263,     0,   249,   503,
     264,   404,   265,   652,     0,     0,     0,     0,     0,   252,
       0,     0,   659,     0,   253,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   272,   272,
     272,   272,   272,     0,   535,     0,   243,     0,   535,   535,
       0,   244,     0,     0,     0,     0,    22,   245,   687,     0,
     557,   690,     0,     0,     0,     0,   246,     0,     0,     0,
       0,     0,     0,   455,   455,     0,     0,     0,     0,     0,
       0,   474,   247,     0,   700,     0,     0,     0,     0,     0,
       0,     0,     0,   272,   272,     0,     0,   272,     0,   272,
     272,   272,   272,   272,   272,   384,     0,   249,     0,   264,
       0,   265,     0,     0,   618,     0,     0,   272,   252,     0,
       0,   525,     0,   253,     0,     0,     0,     0,   725,     0,
       0,     0,   730,   272,     0,   272,     0,     0,   272,   272,
       0,   272,     0,     0,     0,     0,     0,     0,   256,     0,
     519,   520,     0,   522,   523,   524,   526,   527,     0,     0,
       0,     0,   751,     0,     0,     0,     0,     0,     0,   544,
     545,   546,   547,   272,     0,   272,   243,     0,     0,     0,
       0,   244,     0,   759,   272,   272,    22,   245,     0,     0,
       0,   766,     0,     0,     0,     0,   246,     0,   769,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   247,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   380,     0,     0,
       0,     0,     0,     0,     0,   248,     0,   249,     0,   250,
       0,   251,     0,     0,     0,     0,     0,     0,   252,     0,
     588,     0,     0,   253,     0,   589,     0,     0,   591,     0,
       0,   594,   595,     0,     0,     0,   599,   600,     0,     0,
     601,   602,   604,     5,     6,     0,     7,     8,     9,    10,
      11,     0,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    29,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,     0,    32,     0,    33,     0,
       0,     0,     0,     0,     0,     0,     0,    34,    35,    36,
       0,    37,     0,    38,     0,    39,     0,    40,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    41,
     673,   674,   675,   676,   677,     0,     0,     0,     0,     0,
       0,   177,   178,   179,   180,   181,   182,   183,   184,     0,
     185,    22,   186,   187,    25,   188,   189,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    56,     0,   705,     0,    29,     0,
       0,   710,   711,   712,   713,   714,   715,     0,    30,    31,
     190,    32,   191,    33,   192,     0,   193,     0,    58,     0,
       0,     0,    34,    35,    36,     0,    59,   310,    38,     0,
      39,     0,    40,     0,     0,   731,     0,   733,     0,   631,
     735,   736,     0,   737,    41,     0,   177,   178,   179,   180,
     181,   182,   183,   184,     0,   185,    22,   186,   187,    25,
     188,   189,     0,     0,   632,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   754,     0,   755,     0,     0,
       0,     0,     0,     0,     0,     0,   760,   761,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,   190,     0,   191,     0,   192,
       0,   193,     0,   115,   116,   117,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,    30,
      31,     0,    32,     0,    33,     0,     0,   131,   132,    58,
       0,     0,   133,    34,    35,    36,     0,    59,     0,    38,
       0,    39,     0,    40,     0,     0,    60,    61,   134,     0,
       0,     0,     0,     0,     0,    41,   135,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,     0,     0,     0,     0,
       0,     0,   161,   162,   163,   164,   165,   166,    29,   167,
     168,   169,   125,   170,   171,   172,   173,   130,    30,    31,
       0,    32,     0,    33,     0,     0,   174,   175,    58,     0,
       0,   176,    34,    35,    36,     0,    59,     0,    38,     0,
      39,     0,    40,     0,     0,    60,    61,     0,     0,     0,
       0,     0,     0,     0,    41,   437,   438,   439,   440,   441,
     442,   443,   444,    20,   445,    22,   446,   447,    25,   448,
     449,    28,     0,     0,     0,     0,     0,     0,     0,     0,
     161,   162,   163,   164,   165,   166,    29,   167,   168,   169,
     125,   170,   171,   172,   173,   130,    30,    31,     0,    32,
       0,    33,     0,     0,   174,   175,    58,     0,     0,   176,
      34,    35,    36,     0,   450,     0,   451,     0,   452,     0,
     453,     0,     0,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    41,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,     0,     0,     0,    29,     0,     0,     0,     0,     0,
       0,     0,     0,    57,    30,    31,     0,    32,     0,    33,
       0,     0,     0,     0,    58,     0,     0,     0,    34,    35,
      36,   458,   289,     0,    38,     0,   290,   459,    40,     0,
       0,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      41,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,   334,    27,    28,     0,     0,
     335,     0,     0,     0,     0,     0,     0,     0,    56,     0,
       0,     0,    29,     0,     0,     0,     0,     0,     0,   243,
       0,    57,    30,    31,   244,    32,     0,    33,     0,    22,
     245,     0,    58,     0,     0,     0,    34,    35,    36,   246,
      59,     0,    38,     0,    39,     0,    40,     0,     0,    60,
      61,     0,     0,     0,     0,   247,     0,     0,    41,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,   263,     0,
     249,     0,   264,     0,   265,     0,    56,     0,     0,     0,
      29,   252,     0,     0,     0,     0,   253,   243,     0,    57,
      30,    31,   244,    32,     0,    33,     0,    22,   245,     0,
      58,     0,     0,     0,    34,    35,    36,   246,    59,     0,
      38,   466,    39,     0,    40,     0,     0,    60,    61,     0,
       0,     0,     0,   247,     0,     0,    41,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,   327,     0,   249,     0,
     264,     0,   265,     0,    56,     0,     0,     0,    29,   252,
       0,     0,     0,     0,   253,   243,     0,    57,    30,    31,
     244,    32,     0,    33,     0,    22,   245,     0,    58,     0,
       0,     0,    34,    35,    36,   246,    59,     0,    38,   577,
      39,     0,    40,     0,     0,    60,    61,     0,     0,     0,
       0,   247,     0,     0,    41,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,     0,     0,   384,     0,   249,     0,   264,     0,
     265,     0,    56,     0,     0,     0,    29,   252,     0,     0,
       0,     0,   253,     0,     0,    57,    30,    31,     0,    32,
       0,    33,     0,     0,     0,     0,    58,     0,     0,     0,
      34,    35,    36,     0,    59,     0,    38,     0,    39,     0,
      40,     0,     0,    60,    61,     0,     0,     0,     0,     0,
       0,     0,    41,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      56,     0,     0,     0,    29,     0,     0,     0,     0,     0,
       0,     0,     0,    57,    30,    31,     0,    32,     0,    33,
       0,     0,     0,     0,    58,     0,     0,     0,    34,    35,
      36,     0,   289,     0,    38,     0,   290,     0,    40,     0,
       0,    60,    61,     0,     0,     0,     0,     0,     0,     0,
      41,   437,   438,   439,   440,   441,   442,   443,   444,    20,
     445,    22,   446,   447,    25,   448,   449,    28,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    56,     0,
       0,     0,    29,     0,     0,     0,     0,     0,     0,     0,
       0,    57,    30,    31,     0,    32,     0,    33,     0,     0,
       0,     0,    58,     0,     0,     0,    34,    35,    36,     0,
     450,     0,   451,     0,   452,     0,   453,     0,     0,    60,
      61,     0,     0,     0,     0,     0,     0,     0,    41,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    56,     0,     0,     0,
      29,     0,     0,     0,     0,     0,     0,     0,     0,    57,
      30,    31,     0,    32,     0,    33,     0,     0,     0,     0,
      58,     0,     0,     0,    34,    35,    36,     0,    59,     0,
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
      34,    35,    36,     0,    59,   298,    38,     0,    39,     0,
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
       0,     0,   223,   224,   225,   226,   227,   228,   229,   230,
     231,   232,    29,   233,   234,   235,   236,     0,     0,     0,
       0,     0,     0,    31,     0,     0,   237,   238,   283,     0,
       0,   239,     0,     0,     0,     0,    34,    35,     0,     0,
      59,     0,    38,     0,    39,     0,    40,   134,     0,     0,
       0,     0,     0,     0,     0,   135,     0,     0,    41,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,     0,     0,
       0,     0,   177,   178,   179,   180,   181,   182,   183,   184,
      29,   185,    22,   186,   187,    25,   188,   189,     0,     0,
       0,    31,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   349,    34,    35,     0,     0,    59,     0,
      38,     0,    39,     0,    40,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    41,     0,     0,     0,
       0,   190,     0,   191,     0,   192,     0,   193
};

static const yytype_int16 yycheck[] =
{
       2,     3,    52,     8,     9,    60,    61,    38,     1,    86,
     200,   201,   202,   203,    32,    61,    34,    43,   192,    29,
     190,   191,    57,   114,   542,   405,    56,   530,   148,    31,
     611,     8,     8,    25,   615,     8,    38,     8,    33,    41,
      38,   423,    41,    60,    61,    25,    41,     0,    43,    36,
      43,    16,    39,    25,    78,    33,    58,     8,    25,   109,
      25,    26,   112,    41,    15,    43,    25,    25,    25,    20,
      25,    38,    39,    78,    25,    26,    81,    69,    25,    84,
      73,    38,    74,    33,    35,   102,    78,   117,    33,    88,
      88,    41,   110,    88,   149,   130,    41,    43,    39,    79,
      51,    88,    74,    25,    81,    81,    78,    80,   490,    80,
      88,   121,   114,    78,    25,    74,    38,    74,    36,   150,
     240,    79,   242,    74,    79,    76,    38,    78,    36,    80,
      79,    39,   149,   163,    41,   716,    87,    44,    88,    88,
     195,    92,    88,    88,   199,   155,    64,    65,    43,   151,
      25,    41,    74,    34,   156,    46,    77,    28,    33,    43,
      81,   541,    53,    74,    34,   683,    69,    85,    80,   204,
      43,   248,   249,   190,   191,   678,    31,   680,   195,    82,
      80,    88,   199,   213,   214,   215,   263,    69,    88,   266,
      38,    46,    33,    88,    38,     8,    77,    38,    88,    40,
      41,   292,    15,    44,    88,    46,    47,    20,    49,    50,
      80,    81,    25,    26,    69,    88,    31,   219,    38,    74,
      40,    76,    35,    79,    44,    81,    46,    47,   221,    49,
      50,    46,    80,    79,   284,    78,    80,    38,    51,    31,
      25,   744,    88,    25,    31,    25,   281,    88,    64,    65,
     327,   253,    39,    38,    46,   260,    38,    79,    38,    46,
      77,    74,   317,    76,    81,    78,    88,    80,    88,    85,
     288,    20,   289,    60,    87,    25,    25,   279,   452,    92,
     450,   451,    74,    25,    76,    75,    25,    74,    38,    76,
     292,    77,    75,    46,    80,    25,    38,    25,    88,    38,
     317,   378,    25,    16,   381,    88,   383,   384,    38,    79,
      38,   321,    25,    26,   316,    38,   318,     8,    88,    20,
     322,   323,   324,    25,    25,     7,     8,     9,    10,    11,
      12,    13,   409,   335,    80,    80,    38,   339,    20,   341,
      80,    86,    88,    25,   370,   347,    80,   349,    88,    75,
      75,    15,    20,    35,    88,    81,    20,    25,    75,    74,
      32,    25,    26,    78,    81,    80,   421,    75,    75,   424,
      75,    35,    75,    81,    81,   430,    81,   370,    81,    61,
      20,     3,     4,     5,   430,    25,   436,    51,   398,    20,
      72,    73,    80,    81,    25,    34,   473,    79,   475,    38,
     405,    79,    84,    81,   421,    32,    79,   424,    81,    38,
      74,    75,    76,   430,    78,    63,    80,    40,    41,    42,
     451,    80,    81,    87,   426,    43,    44,    45,    92,    79,
     507,    81,   509,   450,   451,   512,   491,   492,    38,     7,
       8,     9,    10,    11,    12,    13,     8,     9,    92,   451,
      10,    11,    20,   216,   217,   218,   458,    25,    75,   461,
     537,   538,   539,   540,    75,    75,    75,    35,    75,    75,
      75,    75,    75,    75,   491,   492,    75,   479,    75,    15,
     482,    75,    75,    75,    20,    75,    38,    38,    67,    25,
      26,    33,    81,    61,    43,    41,    80,    33,    38,    35,
      80,    82,    37,   558,    72,    73,    38,   697,    38,    32,
      77,    38,    85,    46,   569,    51,    84,    75,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    34,   541,    46,    74,    75,
      76,   558,    78,    14,    80,   563,    34,    92,    34,    46,
      88,    87,   569,    81,    14,    75,    92,    36,   748,    56,
      57,    77,    59,    38,    61,    80,    80,    77,   570,    75,
     572,    34,    68,    70,    71,    72,    77,    74,   580,    76,
     582,    78,    80,    80,   661,    80,   641,   642,   665,    38,
     640,    82,    79,     1,     2,    92,    77,     5,     6,     7,
       8,     9,    10,    11,    34,    34,    80,    80,   685,    47,
      48,    49,    50,    51,    52,    53,    54,    34,    80,    80,
      34,    29,    30,    38,   641,   642,    58,    35,   630,   631,
     632,    39,    40,    34,   636,    43,    81,    34,    38,   641,
     642,    39,    38,    38,    33,    39,    38,    55,    38,   651,
      77,    15,    60,    61,    79,    81,    20,    38,    38,    38,
      73,    25,    26,   529,   221,    73,   261,   619,   612,    33,
      78,    35,   409,    81,    82,   617,    84,   616,    86,   280,
     292,   569,   696,   431,   686,   633,   688,    51,   628,   285,
     394,    -1,   247,   249,   102,   103,    -1,   699,    -1,    -1,
      -1,    -1,    -1,   111,    -1,    -1,   114,    -1,    -1,    -1,
      74,    75,    76,   121,    78,    -1,    80,   719,    -1,   721,
     722,    -1,    -1,    87,    -1,    -1,    -1,   729,    92,   205,
     206,   207,   208,   209,   210,   211,   212,    -1,    -1,   147,
     148,   149,    -1,    -1,    -1,    -1,    -1,   155,   750,   157,
      -1,    -1,    -1,    -1,    -1,    -1,   758,    -1,    -1,    -1,
      -1,    -1,    -1,   765,    -1,    -1,   768,    -1,    -1,   771,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   190,   191,   192,   193,    -1,   195,    -1,    -1,
      -1,   199,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    15,    -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,
      -1,    25,    26,   221,   222,    -1,    -1,    -1,    -1,    -1,
      34,    35,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   240,    -1,   242,    -1,    -1,    51,   246,   247,
     248,   249,   250,   251,   252,    -1,    -1,    -1,    -1,   257,
     258,    -1,   260,    -1,   262,   263,   264,   265,   266,    -1,
      74,    -1,    76,    77,    78,    -1,    80,    -1,    -1,    -1,
      -1,    -1,   280,    87,    -1,    -1,    -1,   285,    92,    -1,
      -1,   289,   290,    -1,   292,    -1,     9,    10,    11,    12,
      13,    -1,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,   317,
      -1,    -1,    -1,   321,    -1,    -1,    -1,    -1,    -1,   327,
      -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   340,    56,    57,    -1,    59,    -1,    61,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,
      -1,    -1,   370,    -1,    -1,    -1,    -1,    -1,    -1,    92,
     378,    -1,    -1,   381,    -1,   383,   384,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   394,    -1,   396,   397,
     398,   399,   400,   401,   402,   403,    82,   405,    -1,   407,
     408,   409,    -1,    -1,    -1,    -1,    -1,   415,   416,   417,
     418,    -1,    -1,   421,    -1,   423,   424,    -1,    -1,    -1,
      -1,   429,   430,    -1,    -1,    -1,    15,    16,    17,    18,
      19,    20,    21,    22,    -1,    24,    25,    26,    27,    28,
      29,    30,   450,   451,   452,   453,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   473,    -1,   475,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   483,    -1,    -1,    -1,    -1,
      -1,    -1,   490,   491,   492,    74,    -1,    76,   496,    78,
      -1,    80,    -1,   501,    -1,    -1,   504,    -1,   506,   507,
     508,   509,    -1,   511,   512,   513,    -1,    15,   516,   517,
     518,    -1,    20,    -1,    -1,    -1,    -1,    25,    26,    -1,
      -1,   529,    -1,    -1,    -1,    -1,   534,    35,    -1,   537,
     538,   539,   540,   541,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    51,    -1,    -1,    -1,    -1,    -1,    -1,
     558,    -1,    -1,    -1,   562,    -1,    -1,    -1,    -1,    -1,
      -1,   569,   248,    -1,   250,   251,    74,    -1,    76,    77,
      78,   257,    80,   581,    -1,    -1,    -1,    -1,    -1,    87,
      -1,    -1,   590,    -1,    92,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   606,   607,
     608,   609,   610,    -1,   612,    -1,    15,    -1,   616,   617,
      -1,    20,    -1,    -1,    -1,    -1,    25,    26,   626,    -1,
     628,   629,    -1,    -1,    -1,    -1,    35,    -1,    -1,    -1,
      -1,    -1,    -1,   641,   642,    -1,    -1,    -1,    -1,    -1,
      -1,   327,    51,    -1,   652,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   661,   662,    -1,    -1,   665,    -1,   667,
     668,   669,   670,   671,   672,    74,    -1,    76,    -1,    78,
      -1,    80,    -1,    -1,   682,    -1,    -1,   685,    87,    -1,
      -1,    90,    -1,    92,    -1,    -1,    -1,    -1,   696,    -1,
      -1,    -1,   700,   701,    -1,   703,    -1,    -1,   706,   707,
      -1,   709,    -1,    -1,    -1,    -1,    -1,    -1,   394,    -1,
     396,   397,    -1,   399,   400,   401,   402,   403,    -1,    -1,
      -1,    -1,   730,    -1,    -1,    -1,    -1,    -1,    -1,   415,
     416,   417,   418,   741,    -1,   743,    15,    -1,    -1,    -1,
      -1,    20,    -1,   751,   752,   753,    25,    26,    -1,    -1,
      -1,   759,    -1,    -1,    -1,    -1,    35,    -1,   766,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   473,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    74,    -1,    76,    -1,    78,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,
     496,    -1,    -1,    92,    -1,   501,    -1,    -1,   504,    -1,
      -1,   507,   508,    -1,    -1,    -1,   512,   513,    -1,    -1,
     516,   517,   518,     6,     7,    -1,     9,    10,    11,    12,
      13,    -1,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    56,    57,    -1,    59,    -1,    61,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,
     606,   607,   608,   609,   610,    -1,    -1,    -1,    -1,    -1,
      -1,    15,    16,    17,    18,    19,    20,    21,    22,    -1,
      24,    25,    26,    27,    28,    29,    30,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    42,    -1,   662,    -1,    46,    -1,
      -1,   667,   668,   669,   670,   671,   672,    -1,    56,    57,
      74,    59,    76,    61,    78,    -1,    80,    -1,    66,    -1,
      -1,    -1,    70,    71,    72,    -1,    74,    75,    76,    -1,
      78,    -1,    80,    -1,    -1,   701,    -1,   703,    -1,     8,
     706,   707,    -1,   709,    92,    -1,    15,    16,    17,    18,
      19,    20,    21,    22,    -1,    24,    25,    26,    27,    28,
      29,    30,    -1,    -1,    33,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   741,    -1,   743,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   752,   753,    15,    16,
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
      -1,    -1,    70,    71,    72,    35,    74,    -1,    76,    77,
      78,    -1,    80,    -1,    -1,    83,    84,    -1,    -1,    -1,
      -1,    51,    -1,    -1,    92,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    -1,    74,    -1,    76,    -1,    78,    -1,
      80,    -1,    42,    -1,    -1,    -1,    46,    87,    -1,    -1,
      -1,    -1,    92,    -1,    -1,    55,    56,    57,    -1,    59,
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
      46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,
      56,    57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    70,    71,    72,    -1,    74,    -1,
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
      -1,    -1,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    46,    51,    52,    53,    54,    -1,    -1,    -1,
      -1,    -1,    -1,    57,    -1,    -1,    64,    65,    62,    -1,
      -1,    69,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,
      74,    -1,    76,    -1,    78,    -1,    80,    85,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    93,    -1,    -1,    92,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,
      -1,    -1,    15,    16,    17,    18,    19,    20,    21,    22,
      46,    24,    25,    26,    27,    28,    29,    30,    -1,    -1,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    70,    71,    -1,    -1,    74,    -1,
      76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,
      -1,    74,    -1,    76,    -1,    78,    -1,    80
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
      10,    11,    12,    13,    20,    35,    61,    72,    73,    84,
     152,   173,    74,    78,   124,   125,   145,   173,   117,   131,
     140,    78,   131,   173,    78,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    64,    65,    69,    85,    93,   114,   117,   156,   117,
     156,   151,   152,    20,   173,   117,    98,    39,    39,    60,
      74,    76,   153,    28,    31,    46,    38,   173,   121,   119,
     117,    40,    41,    42,    43,    44,    45,    47,    48,    49,
      51,    52,    53,    54,    64,    65,    69,    15,    16,    17,
      18,    19,    20,    21,    22,    24,    26,    27,    29,    30,
      74,    76,    78,    80,   139,   143,   144,   146,   173,   143,
      36,    64,    65,    85,    69,    47,    48,    49,    50,    51,
      52,    53,    54,    40,    41,    42,    43,    44,    45,    38,
      96,     8,    46,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    51,    52,    53,    54,    64,    65,    69,
      38,   113,    38,    15,    20,    26,    35,    51,    74,    76,
      78,    80,    87,    92,   159,   160,   164,   173,    32,   111,
     112,   173,    32,    74,    78,    80,   116,   163,   164,   167,
     169,   170,   173,   146,   147,   148,   149,   150,   173,    69,
      82,    38,    63,    62,   131,   132,   133,   173,    38,    74,
      78,   117,   126,   127,   145,   173,    75,    75,    75,    75,
      75,    75,    75,    75,    75,    75,    75,    75,    75,    75,
      75,    75,    75,    75,    75,    75,    81,    80,    86,    77,
      79,    81,    38,    38,    38,    80,    92,    74,   157,   158,
     163,   157,   146,   156,    29,    34,   117,   152,   117,    38,
     173,    67,   147,   147,   149,    20,   173,    46,   146,    46,
     118,   118,   118,   118,   119,   120,   120,   120,   120,   120,
     120,   120,   120,   121,   121,   121,   122,   122,   122,   117,
      97,   173,   157,   157,   173,   166,    33,    75,   162,   163,
     164,    34,    77,   162,    74,   164,   171,   173,   164,   172,
     173,   173,   117,    75,    81,    38,    40,    44,    46,    47,
      49,    50,    88,   161,   164,   173,     8,    80,   114,   173,
     163,   173,   173,     8,   163,    41,    33,    43,    41,    75,
      81,    81,    79,    81,    38,   117,    69,   125,   119,    80,
      80,   141,   142,    79,   133,    37,   131,    15,    16,    17,
      18,    19,    20,    21,    22,    24,    26,    27,    29,    30,
      74,    76,    78,    80,   139,   173,   173,    82,    73,    79,
     127,    38,   117,   129,   130,   146,    77,   117,    79,   152,
     117,   117,   117,    74,   164,    32,    75,    77,   117,    34,
      77,   117,    38,   173,   117,    75,    81,    77,    81,    79,
      81,    38,    38,    80,   117,   117,    46,    53,    75,    75,
     163,    43,   163,    77,    43,    79,    81,    34,    41,    44,
      80,    81,    34,    46,    92,   160,    74,    78,    80,   164,
     164,   152,   164,   164,   164,    90,   164,   164,   111,    14,
     106,   107,   104,   105,   115,   173,   173,   116,    81,    34,
      34,    14,   109,   110,   164,   164,   164,   164,    75,   146,
      79,   150,   146,   117,    20,   154,   155,   173,   143,   142,
     134,   135,   136,    69,    20,   173,   117,   117,    77,    80,
      36,    77,    38,    80,    80,   163,    77,    77,   117,   117,
      38,   173,    68,    75,    77,    79,   146,   146,   164,   164,
      80,   164,   173,   163,   164,   164,   163,   173,   163,   164,
     164,   164,   164,   173,   164,   173,    38,    36,    39,    36,
      39,     8,    80,   101,   107,     8,    81,    33,   173,     8,
     163,   168,   163,   102,   111,   110,    34,    80,    81,    34,
      38,     8,    33,    80,    16,    26,    78,   137,   138,   173,
     131,    38,    38,    80,    79,    82,   129,   117,   117,    77,
     117,    38,   173,   117,    80,    80,    75,    20,   165,   173,
      79,    34,    43,    80,    80,    34,    80,    41,    43,    43,
      34,    41,    34,   164,   164,   164,   164,   164,   106,   104,
     106,   105,   115,   109,    75,    81,    38,   173,    58,   155,
     173,   117,   117,   117,   135,   117,    34,    81,   117,    38,
     173,    38,    34,    38,   163,   164,    38,    39,   163,    39,
     164,   164,   164,   164,   164,   164,     8,   163,   117,    38,
     117,    38,    33,    79,   138,   173,   118,   128,   117,    38,
     173,   164,    77,   164,    79,   164,   164,   164,    80,    75,
      79,    43,    80,    41,   106,   117,   117,   117,    81,   117,
      38,   173,    38,    38,   164,   164,   118,   117,    38,   173,
     164,   164,    79,    80,   117,    38,   173,   117,    38,   173,
     117,    38,   117
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
     152,   153,   153,   153,   153,   154,   154,   155,   155,   155,
     156,   156,   156,   157,   157,   158,   159,   159,   160,   160,
     160,   160,   160,   160,   160,   160,   160,   160,   160,   160,
     160,   160,   160,   160,   160,   160,   160,   160,   160,   161,
     161,   162,   162,   163,   163,   163,   164,   164,   164,   164,
     164,   164,   164,   164,   164,   164,   164,   164,   164,   164,
     164,   164,   164,   164,   165,   165,   166,   166,   167,   167,
     168,   168,   169,   169,   170,   170,   171,   171,   172,   172,
     172,   172,   173
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
       1,     3,     2,     2,     1,     1,     3,     3,     5,     5,
       0,     1,     3,     3,     1,     3,     1,     3,     2,     3,
       3,     3,     7,     9,     7,     7,     9,     7,     5,     5,
       5,     5,     7,     7,     9,     9,     7,     7,     5,     1,
       2,     2,     1,     3,     1,     1,     1,     3,     2,     3,
       7,     3,     3,     3,     3,     2,     1,     1,     4,     3,
       3,     4,     1,     3,     1,     1,     1,     3,     1,     5,
       1,     3,     1,     3,     3,     3,     5,     3,     5,     3,
       3,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
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
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
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


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  YYUSE (yylocationp);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  YY_LOCATION_PRINT (yyo, *yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yytype, yyvaluep, yylocationp);
  YYFPRINTF (yyo, ")");
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
  unsigned long yylno = yyrline[yyrule];
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
                       &yyvsp[(yyi + 1) - (yynrhs)]
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
            else
              goto append;

          append:
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

  return (YYSIZE_T) (yystpcpy (yyres, yystr) - yyres);
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
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
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
    default: /* Avoid compiler warnings. */
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
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
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
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yynewstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  *yyssp = (yytype_int16) yystate;

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = (YYSIZE_T) (yyssp - yyss + 1);

# if defined yyoverflow
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
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
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
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

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
| yyreduce -- do a reduction.  |
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

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 484 "hexpr.y" /* yacc.c:1667  */
    { yyParsedModule = (yyvsp[0].module);                     }
#line 2692 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 3:
#line 485 "hexpr.y" /* yacc.c:1667  */
    { yyParsedVar    = *(yyvsp[-2].string); yyParsedExpr = (yyvsp[0].exp); }
#line 2698 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 4:
#line 486 "hexpr.y" /* yacc.c:1667  */
    { yyParsedVar    = "";  yyParsedExpr = (yyvsp[0].exp); }
#line 2704 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 5:
#line 487 "hexpr.y" /* yacc.c:1667  */
    { yyParsedExpr   = (yyvsp[0].exp);                     }
#line 2710 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 6:
#line 490 "hexpr.y" /* yacc.c:1667  */
    { (yyval.module) = (yyvsp[0].module); (yyval.module)->setOption(*(yyvsp[-1].string), m((yylsp[-2]))); }
#line 2716 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 7:
#line 491 "hexpr.y" /* yacc.c:1667  */
    { (yyval.module) = new Module(*(yyvsp[-2].string), *(yyvsp[0].mdefs)); }
#line 2722 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 8:
#line 492 "hexpr.y" /* yacc.c:1667  */
    { (yyval.module) = new Module(freshName(), *(yyvsp[0].mdefs)); }
#line 2728 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 9:
#line 494 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); }
#line 2734 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 10:
#line 495 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdefs) = autorelease(new ModuleDefs()); (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2740 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 11:
#line 496 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdefs) = (yyvsp[-1].mdefs);                            (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2746 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 12:
#line 498 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2752 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 13:
#line 499 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2758 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 14:
#line 500 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = (yyvsp[0].mvtydef); }
#line 2764 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 15:
#line 501 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2770 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 16:
#line 502 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2776 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 17:
#line 504 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2782 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 18:
#line 505 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 2788 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 19:
#line 506 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 2794 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 20:
#line 507 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 2800 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 21:
#line 508 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 2806 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 22:
#line 509 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 2812 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 23:
#line 510 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 2818 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 24:
#line 511 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-9]), (yylsp[0]))); }
#line 2824 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 25:
#line 512 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-10]), (yylsp[0]))); }
#line 2830 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 26:
#line 513 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-11]), (yylsp[0]))); }
#line 2836 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 27:
#line 514 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-12]), (yylsp[0]))); }
#line 2842 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 28:
#line 515 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-13]), (yylsp[0]))); }
#line 2848 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 29:
#line 518 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MVarDef(list(freshName()), let(freshName(), ExprPtr((yyvsp[0].exp)), mktunit(m((yylsp[0]))), m((yylsp[0]))), m((yylsp[0]))); }
#line 2854 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 30:
#line 521 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MImport(yyModulePath, *(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 2860 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 31:
#line 524 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MTypeDef(MTypeDef::Transparent, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 2866 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 32:
#line 525 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new MTypeDef(MTypeDef::Opaque, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 2872 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 33:
#line 528 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mvtydef) = new MVarTypeDef(*(yyvsp[-2].string), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]), (yylsp[0]))); }
#line 2878 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 34:
#line 530 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mvdef) = new MVarDef(*(yyvsp[-2].strings), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2884 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 35:
#line 533 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-3].tconstraints), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2890 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 36:
#line 534 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-6]), (yylsp[0]))); wantIndent(false); }
#line 2896 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 37:
#line 535 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2902 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 38:
#line 536 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new ClassDef(*(yyvsp[-7].tconstraints), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-8]), (yylsp[0])));            wantIndent(false); }
#line 2908 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 39:
#line 537 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2914 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 40:
#line 538 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2920 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 41:
#line 539 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-4]), (yylsp[0])));            wantIndent(false); }
#line 2926 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 42:
#line 540 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 2932 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 43:
#line 542 "hexpr.y" /* yacc.c:1667  */
    { (yyval.fundeps) = autorelease(new CFunDepDefs()); (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2938 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 44:
#line 543 "hexpr.y" /* yacc.c:1667  */
    { (yyval.fundeps) = (yyvsp[-2].fundeps);                             (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 2944 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 45:
#line 545 "hexpr.y" /* yacc.c:1667  */
    { (yyval.fundep) = autorelease(new CFunDepDef(*(yyvsp[-2].strings), *(yyvsp[0].strings))); }
#line 2950 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 46:
#line 547 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mvtydefs) = autorelease(new MVarTypeDefs()); (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2956 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 47:
#line 548 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mvtydefs) = (yyvsp[-1].mvtydefs);                              (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 2962 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 48:
#line 550 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mvtydef) = (yyvsp[0].mvtydef); }
#line 2968 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 49:
#line 553 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 2974 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 50:
#line 554 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-3].tconstraints),           *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 2980 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 51:
#line 555 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-4]), (yylsp[0])));        wantIndent(false); }
#line 2986 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 52:
#line 556 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mdef) = new InstanceDef(*(yyvsp[-5].tconstraints),           *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-6]), (yylsp[0])));        wantIndent(false); }
#line 2992 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 53:
#line 558 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mvdefs) = autorelease(new MVarDefs()); (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 2998 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 54:
#line 559 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mvdefs) = (yyvsp[-1].mvdefs);                          (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 3004 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 55:
#line 561 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mvdef) = (yyvsp[0].mvdef); }
#line 3010 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 56:
#line 564 "hexpr.y" /* yacc.c:1667  */
    { (yyval.strings) = (yyvsp[0].strings); }
#line 3016 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 57:
#line 566 "hexpr.y" /* yacc.c:1667  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[-1].string)); (yyval.strings)->push_back(*(yyvsp[-2].string)); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3022 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 58:
#line 568 "hexpr.y" /* yacc.c:1667  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3028 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 59:
#line 569 "hexpr.y" /* yacc.c:1667  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3034 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 60:
#line 571 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = (yyvsp[0].string); }
#line 3040 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 61:
#line 573 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = (yyvsp[-1].string); }
#line 3046 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 62:
#line 575 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("and")); }
#line 3052 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 63:
#line 576 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("or")); }
#line 3058 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 64:
#line 577 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3064 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 65:
#line 578 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("compose")); }
#line 3070 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 66:
#line 579 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("~")); }
#line 3076 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 67:
#line 580 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("=~")); }
#line 3082 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 68:
#line 581 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("===")); }
#line 3088 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 69:
#line 582 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("==")); }
#line 3094 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 70:
#line 583 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("<")); }
#line 3100 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 71:
#line 584 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("<=")); }
#line 3106 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 72:
#line 585 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string(">")); }
#line 3112 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 73:
#line 586 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string(">=")); }
#line 3118 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 74:
#line 587 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("in")); }
#line 3124 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 75:
#line 588 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("append")); }
#line 3130 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 76:
#line 589 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("+")); }
#line 3136 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 77:
#line 590 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("-")); }
#line 3142 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 78:
#line 591 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("*")); }
#line 3148 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 79:
#line 592 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("/")); }
#line 3154 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 80:
#line 593 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("%")); }
#line 3160 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 81:
#line 595 "hexpr.y" /* yacc.c:1667  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3166 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 82:
#line 596 "hexpr.y" /* yacc.c:1667  */
    { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3172 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 83:
#line 598 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3178 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 84:
#line 599 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes);                           (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3184 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 85:
#line 602 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3190 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 86:
#line 603 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3196 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 87:
#line 604 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Assign(ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3202 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 88:
#line 605 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3208 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 89:
#line 607 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), (yyvsp[0].exp), m((yylsp[-1]),(yylsp[0]))); }
#line 3214 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 90:
#line 608 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("and",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3220 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 91:
#line 609 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("or",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3226 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 92:
#line 610 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("compose",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3232 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 93:
#line 611 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("in",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3238 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 94:
#line 612 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3244 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 95:
#line 614 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP3(var("if",m((yylsp[-5]))), (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-5]), (yylsp[0]))); }
#line 3250 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 96:
#line 615 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3256 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 97:
#line 617 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("~",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3262 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 98:
#line 618 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("===",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3268 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 99:
#line 619 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3274 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 100:
#line 620 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))), m((yylsp[-2]),(yylsp[0]))); }
#line 3280 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 101:
#line 621 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("<",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3286 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 102:
#line 622 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("<=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3292 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 103:
#line 623 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var(">",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3298 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 104:
#line 624 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var(">=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3304 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 105:
#line 625 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3310 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 106:
#line 627 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("+",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3316 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 107:
#line 628 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("-",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3322 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 108:
#line 629 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("append",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3328 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 109:
#line 630 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP1(var("neg",m((yylsp[-1]))), ExprPtr((yyvsp[0].exp)), m((yylsp[-1]),(yylsp[0]))); }
#line 3334 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 110:
#line 631 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3340 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 111:
#line 633 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("*", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3346 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 112:
#line 634 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("/", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3352 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 113:
#line 635 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = TAPP2(var("%", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3358 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 114:
#line 636 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3364 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 115:
#line 638 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = (yyvsp[0].exp); }
#line 3370 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 116:
#line 641 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-2].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3376 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 117:
#line 642 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]),(yylsp[0])))->clone(); }
#line 3382 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 118:
#line 645 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = compileMatch(yyParseCC, *(yyvsp[-2].exps), normPatternRules(*(yyvsp[0].patternexps), m((yylsp[-3]),(yylsp[0]))), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3388 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 119:
#line 648 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = compileMatchTest(yyParseCC, ExprPtr((yyvsp[-2].exp)), PatternPtr((yyvsp[0].pattern)), m((yylsp[-2]),(yylsp[0])))->clone(); }
#line 3394 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 120:
#line 651 "hexpr.y" /* yacc.c:1667  */
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
#line 3409 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 121:
#line 663 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-1].letbindings), ExprPtr(new Unit(m((yylsp[-3]),(yylsp[0])))), m((yylsp[-3]),(yylsp[0]))); }
#line 3415 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 122:
#line 664 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[-1].exp)), m((yylsp[-5]),(yylsp[0]))); }
#line 3421 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 123:
#line 667 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Assump(ExprPtr((yyvsp[-2].exp)), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]),(yylsp[0]))); }
#line 3427 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 124:
#line 669 "hexpr.y" /* yacc.c:1667  */
    { (yyvsp[-2].letbindings)->push_back(*(yyvsp[0].letbinding)); (yyval.letbindings) = (yyvsp[-2].letbindings); }
#line 3433 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 125:
#line 670 "hexpr.y" /* yacc.c:1667  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3439 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 126:
#line 672 "hexpr.y" /* yacc.c:1667  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-2].pattern)), ExprPtr((yyvsp[0].exp)))); }
#line 3445 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 127:
#line 674 "hexpr.y" /* yacc.c:1667  */
    { (yyval.letbindings) = (yyvsp[-1].letbindings); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3451 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 128:
#line 675 "hexpr.y" /* yacc.c:1667  */
    { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3457 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 129:
#line 677 "hexpr.y" /* yacc.c:1667  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)))); }
#line 3463 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 130:
#line 678 "hexpr.y" /* yacc.c:1667  */
    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr(new MatchAny("_",m((yylsp[-1])))), ExprPtr((yyvsp[-1].exp)))); }
#line 3469 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 131:
#line 680 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exps) = (yyvsp[-2].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3475 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 132:
#line 681 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3481 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 133:
#line 683 "hexpr.y" /* yacc.c:1667  */
    { (yyval.cselection) = new CSelection(); (yyval.cselection)->pat = PatternPtr((yyvsp[-4].pattern)); (yyval.cselection)->seq = ExprPtr((yyvsp[-2].exp)); (yyval.cselection)->conds = *(yyvsp[0].exps); }
#line 3487 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 134:
#line 684 "hexpr.y" /* yacc.c:1667  */
    { (yyval.cselection) = new CSelection(); (yyval.cselection)->pat = PatternPtr((yyvsp[-2].pattern)); (yyval.cselection)->seq = ExprPtr((yyvsp[0].exp)); }
#line 3493 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 135:
#line 686 "hexpr.y" /* yacc.c:1667  */
    { (yyval.cselections) = (yyvsp[-2].cselections); (yyval.cselections)->push_back(CSelectionPtr((yyvsp[0].cselection))); }
#line 3499 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 136:
#line 687 "hexpr.y" /* yacc.c:1667  */
    { (yyval.cselections) = autorelease(new CSelections()); (yyval.cselections)->push_back(CSelectionPtr((yyvsp[0].cselection))); }
#line 3505 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 137:
#line 690 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new App(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].exps), m((yylsp[-3]), (yylsp[0]))); }
#line 3511 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 138:
#line 691 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = varCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3517 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 139:
#line 694 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new App(var("range", m((yylsp[-2]))), list(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]), (yylsp[0]))); }
#line 3523 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 140:
#line 695 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new App(var("iterateS", m((yylsp[-1]))), list(ExprPtr((yyvsp[-2].exp)), fn(str::strings(".x"), fncall(var("+", m((yylsp[-1]))), list(var(".x", m((yylsp[-1]))), ExprPtr(new Int(1, m((yylsp[-1]))))), m((yylsp[-1]))), m((yylsp[-1])))), m((yylsp[-3]), (yylsp[0]))); }
#line 3529 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 141:
#line 696 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = desugarComprehension(yyParseCC, ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].cselections), m((yylsp[-4]), (yylsp[0]))); }
#line 3535 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 142:
#line 697 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new MkArray(*(yyvsp[-1].exps), m((yylsp[-2]), (yylsp[0]))); }
#line 3541 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 143:
#line 698 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = maybeArraySliceWithTime(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].string), m((yylsp[-3]), (yylsp[0]))); }
#line 3547 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 144:
#line 699 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = mkAIndex(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3553 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 145:
#line 700 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new App(var("slice", m((yylsp[-2]))), list(ExprPtr((yyvsp[-5].exp)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-5]), (yylsp[0]))); }
#line 3559 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 146:
#line 701 "hexpr.y" /* yacc.c:1667  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-1]))), list(var(vn,m((yylsp[-4]))), ExprPtr((yyvsp[-2].exp)), fncall(var("size",m((yylsp[-1]))), list(var(vn,m((yylsp[-4])))),m((yylsp[-4])))),m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3565 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 147:
#line 702 "hexpr.y" /* yacc.c:1667  */
    { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-2]))), list(var(vn,m((yylsp[-4]))), fncall(var("size",m((yylsp[-2]))), list(var(vn,m((yylsp[-2])))),m((yylsp[-4]))), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3571 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 148:
#line 705 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-3].string), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3577 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 149:
#line 706 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new MkVariant(".f" + str::from((yyvsp[-3].intv)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3583 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 150:
#line 707 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new MkVariant(*(yyvsp[-1].string), ExprPtr(new Unit(m((yylsp[-1])))), m((yylsp[-2]), (yylsp[0]))); }
#line 3589 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 151:
#line 708 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-4].exp)), *(yyvsp[-1].vfields), m((yylsp[-5]), (yylsp[0]))); }
#line 3595 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 152:
#line 709 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Case(ExprPtr((yyvsp[-6].exp)), *(yyvsp[-3].vfields), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3601 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 153:
#line 712 "hexpr.y" /* yacc.c:1667  */
    { if ((yyvsp[-1].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-1].rfields), m((yylsp[-2]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-2]), (yylsp[0]))); } }
#line 3607 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 154:
#line 713 "hexpr.y" /* yacc.c:1667  */
    { if ((yyvsp[-2].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-2].rfields), m((yylsp[-3]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-3]), (yylsp[0]))); } }
#line 3613 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 155:
#line 714 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = makeProjSeq((yyvsp[-1].exp), *(yyvsp[0].strings), m((yylsp[-1]), (yylsp[0]))); }
#line 3619 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 156:
#line 717 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Fn(str::strings("x"), proj(var("x", m((yylsp[0]))), *(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3625 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 157:
#line 720 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = compileRegexFn(yyParseCC, std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0])))->clone(); }
#line 3631 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 158:
#line 723 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Pack(ExprPtr((yyvsp[0].exp)), m((yylsp[-1]), (yylsp[0]))); }
#line 3637 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 159:
#line 724 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Unpack(*(yyvsp[-4].string), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3643 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 160:
#line 727 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Bool((yyvsp[0].boolv), m((yylsp[0]))); }
#line 3649 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 161:
#line 728 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3655 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 162:
#line 729 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3661 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 163:
#line 730 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = mkarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3667 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 164:
#line 731 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Short((yyvsp[0].shortv), m((yylsp[0]))); }
#line 3673 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 165:
#line 732 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Int((yyvsp[0].intv), m((yylsp[0]))); }
#line 3679 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 166:
#line 733 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Long((yyvsp[0].longv), m((yylsp[0]))); }
#line 3685 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 167:
#line 734 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Int128((yyvsp[0].int128v), m((yylsp[0]))); }
#line 3691 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 168:
#line 735 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Float((yyvsp[0].floatv), m((yylsp[0]))); }
#line 3697 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 169:
#line 736 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Double((yyvsp[0].doublev), m((yylsp[0]))); }
#line 3703 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 170:
#line 737 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = mkarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3709 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 171:
#line 738 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0])))->clone(); }
#line 3715 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 172:
#line 739 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = mkTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3721 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 173:
#line 740 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3727 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 174:
#line 743 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = pickNestedExp((yyvsp[-1].exps), m((yylsp[-2]),(yylsp[0]))); }
#line 3733 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 175:
#line 746 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("append", m((yylsp[-1]))); }
#line 3739 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 176:
#line 747 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("+",      m((yylsp[-1]))); }
#line 3745 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 177:
#line 748 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("-",      m((yylsp[-1]))); }
#line 3751 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 178:
#line 749 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("*",      m((yylsp[-1]))); }
#line 3757 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 179:
#line 750 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("/",      m((yylsp[-1]))); }
#line 3763 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 180:
#line 751 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("%",      m((yylsp[-1]))); }
#line 3769 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 181:
#line 752 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("~",      m((yylsp[-1]))); }
#line 3775 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 182:
#line 753 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("===",    m((yylsp[-1]))); }
#line 3781 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 183:
#line 754 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("==",     m((yylsp[-1]))); }
#line 3787 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 184:
#line 755 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("!=",     m((yylsp[-1]))); }
#line 3793 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 185:
#line 756 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("<",      m((yylsp[-1]))); }
#line 3799 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 186:
#line 757 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var(">",      m((yylsp[-1]))); }
#line 3805 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 187:
#line 758 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var(">=",     m((yylsp[-1]))); }
#line 3811 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 188:
#line 759 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("<=",     m((yylsp[-1]))); }
#line 3817 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 189:
#line 760 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("and",    m((yylsp[-1]))); }
#line 3823 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 190:
#line 761 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("or",     m((yylsp[-1]))); }
#line 3829 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 191:
#line 762 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("in",     m((yylsp[-1]))); }
#line 3835 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 192:
#line 763 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Var("not",    m((yylsp[-1]))); }
#line 3841 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 193:
#line 766 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exp) = new Assump(fncall(var("unsafeCast", m((yylsp[-1]))), list(mktunit(m((yylsp[-1])))), m((yylsp[-1]))), qualtype(tapp(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp)))))), m((yylsp[-1]))); }
#line 3847 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 194:
#line 768 "hexpr.y" /* yacc.c:1667  */
    { (yyval.prules) = (yyvsp[-1].prules); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3853 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 195:
#line 769 "hexpr.y" /* yacc.c:1667  */
    { (yyval.prules) = autorelease(new Grammar()); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 3859 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 196:
#line 771 "hexpr.y" /* yacc.c:1667  */
    { (yyval.prule) = autorelease(new Grammar::value_type(*(yyvsp[-2].string), *(yyvsp[0].prdefs))); }
#line 3865 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 197:
#line 773 "hexpr.y" /* yacc.c:1667  */
    { (yyval.prdefs) = (yyvsp[-2].prdefs); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3871 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 198:
#line 774 "hexpr.y" /* yacc.c:1667  */
    { (yyval.prdefs) = autorelease(new GrammarRules()); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 3877 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 199:
#line 776 "hexpr.y" /* yacc.c:1667  */
    { (yyval.prdef) = autorelease(new GrammarRule(*(yyvsp[-3].pbelems), ExprPtr((yyvsp[-1].exp)))); }
#line 3883 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 200:
#line 778 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pbelems) = (yyvsp[-1].pbelems); (yyval.pbelems)->push_back(*(yyvsp[0].pbelem)); }
#line 3889 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 201:
#line 779 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pbelems) = autorelease(new BoundGrammarValues()); }
#line 3895 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 202:
#line 781 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue(*(yyvsp[-2].string), GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3901 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 203:
#line 782 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pbelem) = autorelease(new BoundGrammarValue("_", GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 3907 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 204:
#line 784 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pvalue) = new GSymRef(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3913 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 205:
#line 785 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pvalue) = new GStr(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3919 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 206:
#line 786 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pvalue) = new GStr(std::string(1, str::readCharDef(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3925 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 207:
#line 788 "hexpr.y" /* yacc.c:1667  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3931 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 208:
#line 789 "hexpr.y" /* yacc.c:1667  */
    { (yyval.strings) = (yyvsp[-1].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3937 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 209:
#line 791 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exps) = (yyvsp[-1].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3943 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 210:
#line 792 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3949 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 211:
#line 794 "hexpr.y" /* yacc.c:1667  */
    { (yyval.patternexps) = (yyvsp[-1].patternexps); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3955 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 212:
#line 795 "hexpr.y" /* yacc.c:1667  */
    { (yyval.patternexps) = autorelease(new PatternRows()); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 3961 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 213:
#line 797 "hexpr.y" /* yacc.c:1667  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)))); }
#line 3967 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 214:
#line 798 "hexpr.y" /* yacc.c:1667  */
    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-4].patterns), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)))); }
#line 3973 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 215:
#line 801 "hexpr.y" /* yacc.c:1667  */
    { (yyval.patterns) = (yyvsp[-1].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3979 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 216:
#line 802 "hexpr.y" /* yacc.c:1667  */
    { (yyval.patterns) = autorelease(new Patterns()); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 3985 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 217:
#line 804 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Bool((yyvsp[0].boolv), m((yylsp[0])))), m((yylsp[0]))); }
#line 3991 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 218:
#line 805 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 3997 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 219:
#line 806 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 4003 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 220:
#line 807 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Short((yyvsp[0].shortv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4009 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 221:
#line 808 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int((yyvsp[0].intv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4015 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 222:
#line 809 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Long((yyvsp[0].longv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4021 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 223:
#line 810 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int128((yyvsp[0].int128v), m((yylsp[0])))), m((yylsp[0]))); }
#line 4027 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 224:
#line 811 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Double((yyvsp[0].doublev), m((yylsp[0])))), m((yylsp[0]))); }
#line 4033 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 225:
#line 812 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = mkpatarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 4039 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 226:
#line 813 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = mkpatarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4045 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 227:
#line 814 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchLiteral(mkTimespanPrim(*(yyvsp[0].strings), m((yylsp[0]))), mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 4051 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 228:
#line 815 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchLiteral(mkTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4057 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 229:
#line 816 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchLiteral(mkDateTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4063 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 230:
#line 817 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchRegex(std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0]))); }
#line 4069 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 231:
#line 818 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4075 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 232:
#line 819 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchArray(*(yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4081 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 233:
#line 820 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-1].string), PatternPtr(new MatchLiteral(PrimitivePtr(new Unit(m((yylsp[-1])))), m((yylsp[-1])))), m((yylsp[-2]),(yylsp[0]))); }
#line 4087 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 234:
#line 821 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchVariant(*(yyvsp[-3].string), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4093 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 235:
#line 822 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchVariant(".f" + str::from((yyvsp[-3].intv)), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4099 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 236:
#line 823 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4105 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 237:
#line 824 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4111 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 238:
#line 825 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4117 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 239:
#line 826 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4123 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 240:
#line 827 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = patVarCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4129 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 241:
#line 829 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchAny(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4135 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 242:
#line 830 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4141 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 243:
#line 831 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4147 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 244:
#line 832 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4153 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 245:
#line 833 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4159 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 246:
#line 835 "hexpr.y" /* yacc.c:1667  */
    { (yyval.pattern) = (yyvsp[0].pattern); }
#line 4165 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 247:
#line 837 "hexpr.y" /* yacc.c:1667  */
    { (yyval.patterns) = (yyvsp[0].patterns); }
#line 4171 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 248:
#line 838 "hexpr.y" /* yacc.c:1667  */
    { (yyval.patterns) = new Patterns(); }
#line 4177 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 249:
#line 840 "hexpr.y" /* yacc.c:1667  */
    { (yyval.patterns) = (yyvsp[-2].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4183 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 250:
#line 841 "hexpr.y" /* yacc.c:1667  */
    { (yyval.patterns) = new Patterns(); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4189 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 251:
#line 843 "hexpr.y" /* yacc.c:1667  */
    { (yyval.recpatfields) = (yyvsp[-2].recpatfields); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4195 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 252:
#line 844 "hexpr.y" /* yacc.c:1667  */
    { (yyval.recpatfields) = new MatchRecord::Fields(); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4201 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 253:
#line 846 "hexpr.y" /* yacc.c:1667  */
    { (yyval.recpatfield) = new MatchRecord::Field(*(yyvsp[-2].string), PatternPtr((yyvsp[0].pattern))); }
#line 4207 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 254:
#line 848 "hexpr.y" /* yacc.c:1667  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); }
#line 4213 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 255:
#line 849 "hexpr.y" /* yacc.c:1667  */
    { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4219 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 256:
#line 850 "hexpr.y" /* yacc.c:1667  */
    { (yyval.rfields) = (yyvsp[-4].rfields);                                     (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4225 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 257:
#line 852 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4231 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 258:
#line 853 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("data")); }
#line 4237 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 259:
#line 854 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("type")); }
#line 4243 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 260:
#line 855 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("where")); }
#line 4249 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 261:
#line 856 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("class")); wantIndent(false); }
#line 4255 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 262:
#line 857 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("instance")); wantIndent(false); }
#line 4261 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 263:
#line 858 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("exists")); }
#line 4267 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 264:
#line 859 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("import")); }
#line 4273 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 265:
#line 860 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("module")); }
#line 4279 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 266:
#line 861 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("parse")); }
#line 4285 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 267:
#line 862 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("do")); }
#line 4291 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 268:
#line 863 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("return")); }
#line 4297 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 269:
#line 864 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string("fn")); }
#line 4303 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 270:
#line 865 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = autorelease(new std::string(".f" + str::from((yyvsp[0].intv)))); }
#line 4309 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 271:
#line 867 "hexpr.y" /* yacc.c:1667  */
    { (yyval.strings) = (yyvsp[-2].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4315 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 272:
#line 868 "hexpr.y" /* yacc.c:1667  */
    { (yyval.strings) = (yyvsp[-1].strings); str::seq x = tupSectionFields(*(yyvsp[0].string)); (yyval.strings)->insert((yyval.strings)->end(), x.begin(), x.end()); }
#line 4321 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 273:
#line 869 "hexpr.y" /* yacc.c:1667  */
    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4327 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 274:
#line 870 "hexpr.y" /* yacc.c:1667  */
    { (yyval.strings) = autorelease(new str::seq()); *(yyval.strings) = tupSectionFields(*(yyvsp[0].string)); }
#line 4333 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 275:
#line 872 "hexpr.y" /* yacc.c:1667  */
    { (yyval.vfields) = autorelease(new Case::Bindings()); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4339 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 276:
#line 873 "hexpr.y" /* yacc.c:1667  */
    { (yyval.vfields) = (yyvsp[-2].vfields); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4345 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 277:
#line 875 "hexpr.y" /* yacc.c:1667  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-2].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4351 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 278:
#line 876 "hexpr.y" /* yacc.c:1667  */
    { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-4].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4357 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 279:
#line 877 "hexpr.y" /* yacc.c:1667  */
    { (yyval.vbind) = autorelease(new Case::Binding(".f" + str::from((yyvsp[-4].intv)), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4363 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 280:
#line 879 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exps) = autorelease(new Exprs()); }
#line 4369 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 281:
#line 880 "hexpr.y" /* yacc.c:1667  */
    { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4375 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 282:
#line 881 "hexpr.y" /* yacc.c:1667  */
    { (yyvsp[-2].exps)->push_back(ExprPtr((yyvsp[0].exp))); (yyval.exps) = (yyvsp[-2].exps); }
#line 4381 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 283:
#line 883 "hexpr.y" /* yacc.c:1667  */
    { (yyval.qualtype) = new QualType(*(yyvsp[-2].tconstraints), *(yyvsp[0].mtype)); }
#line 4387 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 284:
#line 884 "hexpr.y" /* yacc.c:1667  */
    { (yyval.qualtype) = new QualType(Constraints(), *(yyvsp[0].mtype)); }
#line 4393 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 285:
#line 887 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraints) = (yyvsp[-1].tconstraints); }
#line 4399 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 286:
#line 889 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraints) = autorelease(new Constraints()); (yyval.tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); }
#line 4405 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 287:
#line 890 "hexpr.y" /* yacc.c:1667  */
    { (yyvsp[-2].tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); (yyval.tconstraints) = (yyvsp[-2].tconstraints); }
#line 4411 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 288:
#line 892 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(*(yyvsp[-1].string), *(yyvsp[0].mtypes)); }
#line 4417 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 289:
#line 893 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(EqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4423 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 290:
#line 894 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(NotEqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4429 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 291:
#line 895 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(FixIsoRecur::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4435 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 292:
#line 896 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4441 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 293:
#line 897 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4447 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 294:
#line 898 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4453 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 295:
#line 899 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4459 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 296:
#line 900 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4465 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 297:
#line 901 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(1), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4471 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 298:
#line 903 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4477 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 299:
#line 904 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4483 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 300:
#line 905 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4489 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 301:
#line 906 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4495 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 302:
#line 908 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4501 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 303:
#line 909 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4507 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 304:
#line 910 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4513 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 305:
#line 911 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4519 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 306:
#line 913 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), TString::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype))); }
#line 4525 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 307:
#line 914 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), *(yyvsp[-5].mtype),                *(yyvsp[-3].mtype))); }
#line 4531 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 308:
#line 915 "hexpr.y" /* yacc.c:1667  */
    { (yyval.tconstraint) = new Constraint(AppendsToUnqualifier::constraintName(), list(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4537 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 309:
#line 917 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4543 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 310:
#line 918 "hexpr.y" /* yacc.c:1667  */
    { (yyvsp[-1].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4549 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 311:
#line 920 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtypes) = (yyvsp[-1].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4555 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 312:
#line 921 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4561 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 313:
#line 923 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Func::make(tuplety(*(yyvsp[-2].mtypes)), *(yyvsp[0].mtype)))); }
#line 4567 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 314:
#line 924 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeTupleType(*(yyvsp[0].mtypes)))); }
#line 4573 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 315:
#line 925 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeSumType(*(yyvsp[0].mtypes)))); }
#line 4579 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 316:
#line 927 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(monoTypeByName(*(yyvsp[0].string)))); }
#line 4585 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 317:
#line 928 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(OpaquePtr::make(str::replace<char>(*(yyvsp[-1].string), ".", "::"), 0, false))); }
#line 4591 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 318:
#line 929 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("[]"))); }
#line 4597 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 319:
#line 930 "hexpr.y" /* yacc.c:1667  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(Array::make(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4603 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 320:
#line 931 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(FixedArray::make(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype)))); }
#line 4609 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 321:
#line 932 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("->"))); }
#line 4615 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 322:
#line 933 "hexpr.y" /* yacc.c:1667  */
    { try { (yyval.mtype) = autorelease(new MonoTypePtr(clone(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4621 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 323:
#line 934 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeRecType(*(yyvsp[-1].mreclist)))); }
#line 4627 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 324:
#line 935 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(makeVarType(*(yyvsp[-1].mvarlist)))); }
#line 4633 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 325:
#line 936 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("unit"))); }
#line 4639 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 326:
#line 937 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(((yyvsp[0].intv) == 0) ? Prim::make("void") : TLong::make((yyvsp[0].intv)))); }
#line 4645 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 327:
#line 938 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr((yyvsp[0].boolv) ? TLong::make(1) : TLong::make(0))); }
#line 4651 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 328:
#line 939 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Exists::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4657 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 329:
#line 940 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype), *(yyvsp[0].mtype)))); }
#line 4663 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 330:
#line 941 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype)))); }
#line 4669 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 331:
#line 942 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(Recursive::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4675 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 332:
#line 943 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TString::make(str::unescape(str::trimq(*(yyvsp[0].string)))))); }
#line 4681 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 333:
#line 944 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp))))))); }
#line 4687 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 334:
#line 946 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TVar::make(*(yyvsp[0].string)))); }
#line 4693 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 335:
#line 947 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtype) = autorelease(new MonoTypePtr(TLong::make((yyvsp[0].intv)))); }
#line 4699 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 336:
#line 949 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = (yyvsp[0].string); }
#line 4705 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 337:
#line 950 "hexpr.y" /* yacc.c:1667  */
    { (yyval.string) = (yyvsp[-2].string); *(yyval.string) += "."; *(yyval.string) += *(yyvsp[0].string); }
#line 4711 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 338:
#line 952 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4717 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 339:
#line 953 "hexpr.y" /* yacc.c:1667  */
    { (yyvsp[-1].mtypes)->insert((yyvsp[-1].mtypes)->begin(), *(yyvsp[-3].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4723 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 340:
#line 955 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4729 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 341:
#line 956 "hexpr.y" /* yacc.c:1667  */
    { (yyvsp[-2].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-2].mtypes); }
#line 4735 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 342:
#line 958 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4741 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 343:
#line 959 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4747 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 344:
#line 961 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[-2].mtype)); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4753 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 345:
#line 962 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4759 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 346:
#line 964 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mreclist) = (yyvsp[-4].mreclist);                                 (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4765 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 347:
#line 965 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mreclist) = autorelease(new Record::Members()); (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4771 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 348:
#line 967 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mvarlist) = (yyvsp[-4].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4777 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 349:
#line 968 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mvarlist) = (yyvsp[-2].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4783 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 350:
#line 969 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4789 "hexpr.parse.C" /* yacc.c:1667  */
    break;

  case 351:
#line 970 "hexpr.y" /* yacc.c:1667  */
    { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4795 "hexpr.parse.C" /* yacc.c:1667  */
    break;


#line 4799 "hexpr.parse.C" /* yacc.c:1667  */
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
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

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
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

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


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
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
#line 974 "hexpr.y" /* yacc.c:1918  */

#pragma GCC diagnostic pop


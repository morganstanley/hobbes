/* A Bison parser, made by GNU Bison 3.7.6.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
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
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30706

/* Bison version string.  */
#define YYBISON_VERSION "3.7.6"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "hexpr.y"

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
#line 25 "hexpr.y"

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
#include <set>

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
  for (const auto& p : ps) {
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

static MonoTypePtr makePVarTypeImp(const Variant::Members &vms,
                                   const LexicalAnnotation *la) {
  const auto sanityCheck = [&] {
    auto tvms = vms;
    std::sort(tvms.begin(), tvms.end(),
              [](const auto &a, const auto &b) { return a.id < b.id; });
    const auto it = std::adjacent_find(
        tvms.cbegin(), tvms.cend(),
        [](const auto &a, const auto &b) { return a.id == b.id; });
    if (it != tvms.cend()) {
      const auto es =
          "penum has duplicated value(" + std::to_string(it->id) + ")";
      if (la != nullptr) {
        throw annotated_error(*la, es);
      } else {
        throw std::runtime_error(es);
      }
    }
  };

  sanityCheck();
  return Variant::make(vms);
}

MonoTypePtr makePVarType(const Variant::Members &vms) {
  return makePVarTypeImp(vms, nullptr);
}

MonoTypePtr makePVarType(const Variant::Members &vms,
                         const LexicalAnnotation &la) {
  return makePVarTypeImp(vms, &la);
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


#line 361 "hexpr.parse.C"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
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

//#include "hexpr.parse.H"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_TPARSEMODULE = 3,               /* "domodule"  */
  YYSYMBOL_TPARSEDEFN = 4,                 /* "dodefn"  */
  YYSYMBOL_TPARSEEXPR = 5,                 /* "doexpr"  */
  YYSYMBOL_TOPTION = 6,                    /* "option"  */
  YYSYMBOL_TMODULE = 7,                    /* "module"  */
  YYSYMBOL_TWHERE = 8,                     /* "where"  */
  YYSYMBOL_TIMPORT = 9,                    /* "import"  */
  YYSYMBOL_TTYPE = 10,                     /* "type"  */
  YYSYMBOL_TDATA = 11,                     /* "data"  */
  YYSYMBOL_TCLASS = 12,                    /* "class"  */
  YYSYMBOL_TINST = 13,                     /* "instance"  */
  YYSYMBOL_TINDENT = 14,                   /* "indent"  */
  YYSYMBOL_TBOOL = 15,                     /* "boolV"  */
  YYSYMBOL_TCHAR = 16,                     /* "charV"  */
  YYSYMBOL_TBYTE = 17,                     /* "byteV"  */
  YYSYMBOL_TBYTES = 18,                    /* "bytesV"  */
  YYSYMBOL_TSHORT = 19,                    /* "shortV"  */
  YYSYMBOL_TINT = 20,                      /* "intV"  */
  YYSYMBOL_TLONG = 21,                     /* "longV"  */
  YYSYMBOL_TINT128 = 22,                   /* "int128V"  */
  YYSYMBOL_TFLOAT = 23,                    /* "floatV"  */
  YYSYMBOL_TDOUBLE = 24,                   /* "doubleV"  */
  YYSYMBOL_TIDENT = 25,                    /* "id"  */
  YYSYMBOL_TSTRING = 26,                   /* "stringV"  */
  YYSYMBOL_TREGEX = 27,                    /* "regexV"  */
  YYSYMBOL_TTIMEINTERVAL = 28,             /* "timespanV"  */
  YYSYMBOL_TTIME = 29,                     /* "timeV"  */
  YYSYMBOL_TDATETIME = 30,                 /* "dateTimeV"  */
  YYSYMBOL_TTUPSECTION = 31,               /* "tupSection"  */
  YYSYMBOL_TCSTARROW = 32,                 /* "=>"  */
  YYSYMBOL_TARROW = 33,                    /* "->"  */
  YYSYMBOL_TCOLON = 34,                    /* ":"  */
  YYSYMBOL_TEXISTS = 35,                   /* "exists"  */
  YYSYMBOL_TASSIGN = 36,                   /* "<-"  */
  YYSYMBOL_TPARROW = 37,                   /* ":="  */
  YYSYMBOL_TEQUALS = 38,                   /* "="  */
  YYSYMBOL_TASSUMP = 39,                   /* "::"  */
  YYSYMBOL_TAPPEND = 40,                   /* "++"  */
  YYSYMBOL_TPLUS = 41,                     /* "+"  */
  YYSYMBOL_TMINUS = 42,                    /* "-"  */
  YYSYMBOL_TTIMES = 43,                    /* "*"  */
  YYSYMBOL_TDIVIDE = 44,                   /* "/"  */
  YYSYMBOL_TREM = 45,                      /* "%"  */
  YYSYMBOL_TDOT = 46,                      /* "."  */
  YYSYMBOL_TEQUIV = 47,                    /* "=="  */
  YYSYMBOL_TEQ = 48,                       /* "==="  */
  YYSYMBOL_TCIEQ = 49,                     /* "~"  */
  YYSYMBOL_TNEQ = 50,                      /* "!="  */
  YYSYMBOL_TLT = 51,                       /* "<"  */
  YYSYMBOL_TLTE = 52,                      /* "<="  */
  YYSYMBOL_TGT = 53,                       /* ">"  */
  YYSYMBOL_TGTE = 54,                      /* ">="  */
  YYSYMBOL_TNOT = 55,                      /* "!"  */
  YYSYMBOL_TLET = 56,                      /* "let"  */
  YYSYMBOL_TCASE = 57,                     /* "case"  */
  YYSYMBOL_TDEFAULT = 58,                  /* "default"  */
  YYSYMBOL_TMATCH = 59,                    /* "match"  */
  YYSYMBOL_TMATCHES = 60,                  /* "matches"  */
  YYSYMBOL_TPARSE = 61,                    /* "parse"  */
  YYSYMBOL_TWITH = 62,                     /* "with"  */
  YYSYMBOL_TOF = 63,                       /* "of"  */
  YYSYMBOL_TAND = 64,                      /* "and"  */
  YYSYMBOL_TOR = 65,                       /* "or"  */
  YYSYMBOL_TIF = 66,                       /* "if"  */
  YYSYMBOL_TTHEN = 67,                     /* "then"  */
  YYSYMBOL_TELSE = 68,                     /* "else"  */
  YYSYMBOL_TIN = 69,                       /* "in"  */
  YYSYMBOL_TPACK = 70,                     /* "pack"  */
  YYSYMBOL_TUNPACK = 71,                   /* "unpack"  */
  YYSYMBOL_TDO = 72,                       /* "do"  */
  YYSYMBOL_TRETURN = 73,                   /* "return"  */
  YYSYMBOL_TLPAREN = 74,                   /* "("  */
  YYSYMBOL_TRPAREN = 75,                   /* ")"  */
  YYSYMBOL_TLBRACKET = 76,                 /* "["  */
  YYSYMBOL_TRBRACKET = 77,                 /* "]"  */
  YYSYMBOL_TLBRACE = 78,                   /* "{"  */
  YYSYMBOL_TRBRACE = 79,                   /* "}"  */
  YYSYMBOL_TBAR = 80,                      /* "|"  */
  YYSYMBOL_TCOMMA = 81,                    /* ","  */
  YYSYMBOL_TSEMICOLON = 82,                /* ";"  */
  YYSYMBOL_TFN = 83,                       /* "\\"  */
  YYSYMBOL_TFNL = 84,                      /* "fn"  */
  YYSYMBOL_TCOMPOSE = 85,                  /* "o"  */
  YYSYMBOL_TUPTO = 86,                     /* ".."  */
  YYSYMBOL_TCARET = 87,                    /* "^"  */
  YYSYMBOL_TAT = 88,                       /* "@"  */
  YYSYMBOL_TDOLLAR = 89,                   /* "$"  */
  YYSYMBOL_TQUESTION = 90,                 /* "?"  */
  YYSYMBOL_TSQUOTE = 91,                   /* "'"  */
  YYSYMBOL_TEQUOTE = 92,                   /* "`"  */
  YYSYMBOL_TUNSAFE = 93,                   /* "UNSAFE"  */
  YYSYMBOL_TSAFE = 94,                     /* "SAFE"  */
  YYSYMBOL_TLPRAGMA = 95,                  /* "{-#"  */
  YYSYMBOL_TRPRAGMA = 96,                  /* "#-}"  */
  YYSYMBOL_97_ = 97,                       /* "=~"  */
  YYSYMBOL_YYACCEPT = 98,                  /* $accept  */
  YYSYMBOL_s = 99,                         /* s  */
  YYSYMBOL_module = 100,                   /* module  */
  YYSYMBOL_defs = 101,                     /* defs  */
  YYSYMBOL_def = 102,                      /* def  */
  YYSYMBOL_importdef = 103,                /* importdef  */
  YYSYMBOL_pragmadef = 104,                /* pragmadef  */
  YYSYMBOL_pragmaty = 105,                 /* pragmaty  */
  YYSYMBOL_tydef = 106,                    /* tydef  */
  YYSYMBOL_vartybind = 107,                /* vartybind  */
  YYSYMBOL_vardef = 108,                   /* vardef  */
  YYSYMBOL_classdef = 109,                 /* classdef  */
  YYSYMBOL_fundeps = 110,                  /* fundeps  */
  YYSYMBOL_fundep = 111,                   /* fundep  */
  YYSYMBOL_cmembers = 112,                 /* cmembers  */
  YYSYMBOL_cmember = 113,                  /* cmember  */
  YYSYMBOL_instdef = 114,                  /* instdef  */
  YYSYMBOL_imembers = 115,                 /* imembers  */
  YYSYMBOL_imember = 116,                  /* imember  */
  YYSYMBOL_names = 117,                    /* names  */
  YYSYMBOL_nameseq = 118,                  /* nameseq  */
  YYSYMBOL_name = 119,                     /* name  */
  YYSYMBOL_opname = 120,                   /* opname  */
  YYSYMBOL_idseq = 121,                    /* idseq  */
  YYSYMBOL_types = 122,                    /* types  */
  YYSYMBOL_l0expr = 123,                   /* l0expr  */
  YYSYMBOL_lhexpr = 124,                   /* lhexpr  */
  YYSYMBOL_l1expr = 125,                   /* l1expr  */
  YYSYMBOL_l2expr = 126,                   /* l2expr  */
  YYSYMBOL_l3expr = 127,                   /* l3expr  */
  YYSYMBOL_l4expr = 128,                   /* l4expr  */
  YYSYMBOL_l5expr = 129,                   /* l5expr  */
  YYSYMBOL_letbindings = 130,              /* letbindings  */
  YYSYMBOL_letbinding = 131,               /* letbinding  */
  YYSYMBOL_dobindings = 132,               /* dobindings  */
  YYSYMBOL_dobinding = 133,                /* dobinding  */
  YYSYMBOL_cselconds = 134,                /* cselconds  */
  YYSYMBOL_cselection = 135,               /* cselection  */
  YYSYMBOL_cselections = 136,              /* cselections  */
  YYSYMBOL_l6expr = 137,                   /* l6expr  */
  YYSYMBOL_prules = 138,                   /* prules  */
  YYSYMBOL_prule = 139,                    /* prule  */
  YYSYMBOL_prdefs = 140,                   /* prdefs  */
  YYSYMBOL_prdef = 141,                    /* prdef  */
  YYSYMBOL_pbelems = 142,                  /* pbelems  */
  YYSYMBOL_pbelem = 143,                   /* pbelem  */
  YYSYMBOL_pvalue = 144,                   /* pvalue  */
  YYSYMBOL_tsseq = 145,                    /* tsseq  */
  YYSYMBOL_l6exprs = 146,                  /* l6exprs  */
  YYSYMBOL_patternexps = 147,              /* patternexps  */
  YYSYMBOL_patternexp = 148,               /* patternexp  */
  YYSYMBOL_patterns = 149,                 /* patterns  */
  YYSYMBOL_refutablep = 150,               /* refutablep  */
  YYSYMBOL_irrefutablep = 151,             /* irrefutablep  */
  YYSYMBOL_pattern = 152,                  /* pattern  */
  YYSYMBOL_patternseq = 153,               /* patternseq  */
  YYSYMBOL_patternseqn = 154,              /* patternseqn  */
  YYSYMBOL_recpatfields = 155,             /* recpatfields  */
  YYSYMBOL_recpatfield = 156,              /* recpatfield  */
  YYSYMBOL_recfields = 157,                /* recfields  */
  YYSYMBOL_recfieldname = 158,             /* recfieldname  */
  YYSYMBOL_recfieldpath = 159,             /* recfieldpath  */
  YYSYMBOL_varfields = 160,                /* varfields  */
  YYSYMBOL_varbind = 161,                  /* varbind  */
  YYSYMBOL_cargs = 162,                    /* cargs  */
  YYSYMBOL_qtype = 163,                    /* qtype  */
  YYSYMBOL_cst = 164,                      /* cst  */
  YYSYMBOL_tpreds = 165,                   /* tpreds  */
  YYSYMBOL_tpred = 166,                    /* tpred  */
  YYSYMBOL_l1mtargl = 167,                 /* l1mtargl  */
  YYSYMBOL_ltmtype = 168,                  /* ltmtype  */
  YYSYMBOL_l0mtype = 169,                  /* l0mtype  */
  YYSYMBOL_l1mtype = 170,                  /* l1mtype  */
  YYSYMBOL_tyind = 171,                    /* tyind  */
  YYSYMBOL_cppid = 172,                    /* cppid  */
  YYSYMBOL_l0mtargl = 173,                 /* l0mtargl  */
  YYSYMBOL_l0mtarglt = 174,                /* l0mtarglt  */
  YYSYMBOL_mtuplist = 175,                 /* mtuplist  */
  YYSYMBOL_msumlist = 176,                 /* msumlist  */
  YYSYMBOL_mreclist = 177,                 /* mreclist  */
  YYSYMBOL_mvarlist = 178,                 /* mvarlist  */
  YYSYMBOL_mpvarlist = 179,                /* mpvarlist  */
  YYSYMBOL_id = 180                        /* id  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

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


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
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

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

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
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
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
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  74
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   3158

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  98
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  83
/* YYNRULES -- Number of rules.  */
#define YYNRULES  377
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  835

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   352


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
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
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   523,   523,   524,   525,   526,   529,   530,   531,   533,
     534,   535,   537,   538,   539,   540,   541,   542,   544,   545,
     546,   547,   548,   549,   550,   551,   552,   553,   554,   555,
     556,   557,   558,   559,   560,   561,   562,   563,   564,   567,
     570,   573,   574,   575,   578,   579,   582,   584,   587,   588,
     589,   590,   591,   592,   593,   594,   596,   597,   599,   601,
     602,   604,   607,   608,   609,   610,   612,   613,   615,   618,
     620,   622,   623,   625,   627,   629,   630,   631,   632,   633,
     634,   635,   636,   637,   638,   639,   640,   641,   642,   643,
     644,   645,   646,   647,   649,   650,   652,   653,   656,   657,
     658,   659,   661,   662,   663,   664,   665,   666,   668,   669,
     671,   672,   673,   674,   675,   676,   677,   678,   679,   681,
     682,   683,   684,   685,   687,   688,   689,   690,   692,   695,
     696,   699,   702,   705,   717,   718,   721,   723,   724,   726,
     728,   729,   731,   732,   734,   735,   737,   738,   740,   741,
     744,   745,   748,   749,   750,   751,   752,   753,   754,   755,
     756,   759,   760,   761,   762,   763,   766,   767,   768,   771,
     774,   777,   778,   781,   782,   783,   784,   785,   786,   787,
     788,   789,   790,   791,   792,   793,   794,   797,   800,   801,
     802,   803,   804,   805,   806,   807,   808,   809,   810,   811,
     812,   813,   814,   815,   816,   817,   820,   822,   823,   825,
     827,   828,   830,   832,   833,   835,   836,   838,   839,   840,
     842,   843,   845,   846,   848,   849,   851,   852,   855,   856,
     858,   859,   860,   861,   862,   863,   864,   865,   866,   867,
     868,   869,   870,   871,   872,   873,   874,   875,   876,   877,
     878,   879,   880,   881,   883,   884,   885,   886,   887,   889,
     891,   892,   894,   895,   897,   898,   900,   902,   903,   904,
     906,   907,   908,   909,   910,   911,   912,   913,   914,   915,
     916,   917,   918,   919,   920,   926,   927,   928,   929,   931,
     932,   934,   935,   936,   938,   939,   940,   942,   943,   946,
     948,   949,   951,   952,   953,   954,   955,   956,   957,   958,
     959,   960,   962,   963,   964,   965,   967,   968,   969,   970,
     972,   973,   974,   976,   977,   979,   980,   982,   983,   984,
     986,   987,   988,   989,   990,   991,   992,   993,   994,   995,
     996,   997,   998,   999,  1000,  1001,  1002,  1003,  1004,  1006,
    1007,  1009,  1010,  1012,  1013,  1015,  1016,  1018,  1019,  1021,
    1022,  1024,  1025,  1027,  1028,  1029,  1030,  1032,  1033,  1034,
    1035,  1036,  1037,  1038,  1039,  1040,  1041,  1043
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "\"domodule\"",
  "\"dodefn\"", "\"doexpr\"", "\"option\"", "\"module\"", "\"where\"",
  "\"import\"", "\"type\"", "\"data\"", "\"class\"", "\"instance\"",
  "\"indent\"", "\"boolV\"", "\"charV\"", "\"byteV\"", "\"bytesV\"",
  "\"shortV\"", "\"intV\"", "\"longV\"", "\"int128V\"", "\"floatV\"",
  "\"doubleV\"", "\"id\"", "\"stringV\"", "\"regexV\"", "\"timespanV\"",
  "\"timeV\"", "\"dateTimeV\"", "\"tupSection\"", "\"=>\"", "\"->\"",
  "\":\"", "\"exists\"", "\"<-\"", "\":=\"", "\"=\"", "\"::\"", "\"++\"",
  "\"+\"", "\"-\"", "\"*\"", "\"/\"", "\"%\"", "\".\"", "\"==\"",
  "\"===\"", "\"~\"", "\"!=\"", "\"<\"", "\"<=\"", "\">\"", "\">=\"",
  "\"!\"", "\"let\"", "\"case\"", "\"default\"", "\"match\"",
  "\"matches\"", "\"parse\"", "\"with\"", "\"of\"", "\"and\"", "\"or\"",
  "\"if\"", "\"then\"", "\"else\"", "\"in\"", "\"pack\"", "\"unpack\"",
  "\"do\"", "\"return\"", "\"(\"", "\")\"", "\"[\"", "\"]\"", "\"{\"",
  "\"}\"", "\"|\"", "\",\"", "\";\"", "\"\\\\\"", "\"fn\"", "\"o\"",
  "\"..\"", "\"^\"", "\"@\"", "\"$\"", "\"?\"", "\"'\"", "\"`\"",
  "\"UNSAFE\"", "\"SAFE\"", "\"{-#\"", "\"#-}\"", "\"=~\"", "$accept", "s",
  "module", "defs", "def", "importdef", "pragmadef", "pragmaty", "tydef",
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
  "mvarlist", "mpvarlist", "id", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
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
     345,   346,   347,   348,   349,   350,   351,   352
};
#endif

#define YYPACT_NINF (-600)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-367)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     442,  1637,  2275,  2275,    50,   106,   106,   106,    65,    65,
      84,    84,  -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,
    -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,   848,
      13,  2275,  2977,   134,  2977,   106,   140,  1526,  2275,   848,
     346,  2275,   -22,  -600,   982,  -600,  -600,  -600,  -600,  -600,
    -600,  -600,   194,  -600,   195,   242,    23,   237,  2743,   476,
    2275,  1885,  3078,  3078,  -600,   100,   245,   427,   443,   465,
    -600,   244,  -600,  -600,  -600,  1637,   377,   371,  -600,  1116,
     123,  -600,  -600,   165,  1328,   331,    65,   367,  1374,  -600,
    -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,
    -600,  -600,  -600,  -600,  -600,  3078,   106,   -12,  -600,   338,
    -600,   370,   125,  2899,   106,   125,   417,  2353,   365,   383,
    2665,   388,   414,   436,   848,   441,   449,   455,   459,   461,
     463,   464,   466,  2509,   468,   469,   470,  -600,  -600,   474,
    -600,   316,   307,   -32,   407,   444,   483,   111,   448,   106,
     106,   462,  -600,  1675,  1675,  3078,  2275,  2041,    23,  -600,
    -600,   848,  2275,   215,  -600,  -600,   486,   365,   383,  2665,
     388,   414,   436,   441,   449,   455,   461,   463,   464,   466,
     468,   469,   470,  -600,  -600,  -600,  -600,  -600,  -600,  -600,
    -600,  -600,  -600,  -600,  -600,  -600,  3078,  3078,   106,   353,
     242,  1120,  -600,  -600,  -600,  3046,  2587,  2587,  2587,  2587,
     476,  2743,  2743,  2743,  2743,  2743,  2743,  2743,  2743,  2743,
    2743,  2743,  2821,  2821,  2821,  2275,  -600,   982,   106,  -600,
    -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,
    -600,  -600,  -600,  -600,  -600,  -600,  1675,  -600,  1675,  -600,
    -600,  -600,   106,   106,   344,   593,  1699,  1699,   106,  2275,
     317,  -600,   158,  1699,   106,    32,    65,  1116,   106,   344,
     106,   106,   137,  -600,    88,   522,   514,   518,  -600,  -600,
     329,   479,   438,  -600,   524,  2275,   109,   476,   484,   485,
     125,     9,  -600,   526,  2977,  1963,   848,   494,  1798,  -600,
     528,   529,  -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,
    -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,
    -600,  -600,  2275,  3078,  2119,  -600,  -600,   562,  2275,  2275,
    2275,  -600,  -600,  -600,  -600,  -600,   891,  -600,   545,  -600,
    -600,  -600,   332,   502,  2275,   132,  -600,  -600,  2275,   252,
    2275,   333,   348,   446,   540,   130,  2275,  -600,  2275,   166,
     495,   495,  -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,
    -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,   982,
    -600,  -600,  -600,   538,    21,   510,  -600,    27,  -600,    56,
    1374,  -600,   708,   344,   157,   447,   552,     7,   167,   219,
     147,   543,   500,  -600,  1328,   310,  1699,  1699,   848,  1699,
    1699,  1699,  1254,  1699,   506,    65,   581,   106,   106,  1374,
     515,   564,    96,   585,  -600,  1699,  1699,  1699,  1699,  -600,
     530,  3078,  -600,    41,  3078,  -600,  2275,  -600,  -600,   386,
    3078,   485,  -600,  -600,  -600,  -600,   204,  -600,  -600,  -600,
    -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,  -600,
    1963,  2431,   848,   396,   242,  -600,   524,  -600,  2275,  -600,
    -600,  2275,  -600,  -600,   355,   567,  -600,   534,  -600,   568,
    -600,   532,   535,   344,    35,  1374,  -600,  -600,   537,  2197,
    -600,  -600,  2275,   256,   548,  -600,   542,  -600,   544,  -600,
      61,  3078,  3078,  -600,  -600,  -600,  1699,  -600,  -600,  -600,
    -600,  1699,   546,  -600,  1699,  -600,   106,  1374,  1699,  1374,
    -600,   106,  -600,   106,  1374,   437,  1699,  -600,  -600,  1699,
    1699,  1699,     5,    20,   402,   506,   506,   506,  -600,   506,
     506,    33,    65,   581,  -600,    25,  -600,   108,  -600,  -600,
     217,  1374,  1374,  1374,    65,   585,  -600,   506,   506,   506,
     506,  -600,  -600,  -600,  -600,  -600,  -600,   586,   368,  -600,
     295,  3011,  -600,   549,  -600,    39,  2977,   584,   181,   553,
     551,  -600,  3078,  2275,  -600,  2275,  -600,  -600,  -600,  -600,
    -600,   547,  -600,  2275,   260,  2275,  -600,  -600,  -600,   556,
     557,   506,   213,   403,   170,   591,  -600,    85,    71,   558,
     597,   565,   560,    94,   570,   575,   576,   577,   579,   506,
     171,   177,   608,    91,   609,  1699,  1699,  1699,  1699,  1699,
     581,   106,  -600,  -600,   581,   106,   106,  -600,   585,  -600,
     334,  -600,  -600,   617,  -600,   106,   598,   386,   106,  2275,
    2275,  2275,  -600,  -600,  -600,  2275,  -600,  -600,   623,   125,
    2431,  2431,  -600,  -600,  -600,  -600,   580,  -600,  -600,  -600,
    2275,   264,  -600,  -600,  -600,   622,  -600,   628,  -600,   625,
    1374,  1699,   626,   627,  1374,   445,   633,  1699,  -600,  -600,
    -600,  -600,  -600,  1699,  1699,  1699,  1699,  1699,   506,   506,
     506,   506,   506,   581,    29,   581,  -600,   106,   585,  -600,
    1374,  2275,   636,  2275,  -600,   637,  -600,   632,  -600,  -600,
     599,   335,  2587,  -600,  2275,   267,  1699,   600,  1699,  -600,
     227,  1699,  1699,  -600,   601,   604,   606,   607,   611,  1699,
     186,   228,   253,   179,   255,   128,   581,  -600,  -600,  2275,
    -600,  2275,  2275,  -600,  -600,  -600,   166,   602,  -600,  2275,
     283,   506,  -600,   506,   646,   506,   506,  -600,  -600,  -600,
    -600,  -600,   506,   649,  -600,  -600,  1699,  -600,  1699,   581,
    -600,  -600,  -600,  2587,  -600,  2275,   286,  1699,  1699,   279,
     294,   166,  -600,  2275,   292,   506,   506,  -600,  -600,  -600,
    2275,   293,  -600,  2275,   298,  -600,  2275,   300,  -600,  2275,
     309,  -600,  2275,   312,  -600,  2275,   315,  -600,  2275,   319,
    -600,  2275,   324,  -600,  2275,   327,  -600,  2275,   330,  -600,
    2275,   650,  -600,  2275,  -600
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     9,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   173,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   377,   183,   170,   220,   185,   186,   288,     0,
       0,     0,     0,     0,     0,     0,     0,   294,   294,   267,
       0,     0,     0,     2,     8,    10,    12,    17,    13,    14,
      15,    16,     0,    39,   128,   184,   169,   151,     0,     0,
       0,   294,     0,     0,     4,   101,   107,   109,   118,   123,
     127,   151,     5,   151,     1,     9,     0,    40,   351,     0,
       0,    71,    73,     0,     0,     0,     0,     0,     0,   278,
     273,   277,   272,   271,   274,   275,   283,   284,   276,   279,
     280,   281,   282,   287,   270,   261,     0,     0,   138,     0,
     254,     0,   223,     0,     0,   171,     0,     0,     0,     0,
       0,     0,     0,     0,    78,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,    80,     0,
     295,     0,   295,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    11,     0,     0,     0,   294,     0,   168,   221,
     286,     0,     0,     0,   122,   102,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   230,   231,   232,   238,   233,   234,   235,
     236,   237,   239,   243,   241,   242,   261,   261,     0,     0,
     240,     0,   259,   229,   253,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     6,     9,     0,    88,
      89,    90,    91,    92,    93,    78,    82,    81,    79,    83,
      84,    85,    86,    75,    76,    87,     0,    72,     0,   342,
     341,   347,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   300,     0,   330,     0,    52,    69,    73,     0,     0,
       0,     0,    62,    96,   357,     0,   328,   329,   330,   263,
       0,   260,     0,   265,     0,     0,     0,     0,     0,     0,
     222,     0,   208,     0,     0,   261,   267,     0,     0,   141,
       0,   151,   188,   189,   190,   191,   192,   193,   196,   195,
     194,   197,   198,   201,   199,   200,   205,   202,   203,   204,
      74,   187,     0,     0,     0,   155,   166,     0,     0,     0,
       0,   163,   206,    42,    43,    41,     0,    46,     0,   298,
     136,   132,     0,   185,     0,     0,   285,    18,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   228,     0,   100,
     103,   104,   105,   106,   112,   111,   110,   113,   114,   115,
     116,   117,   121,   119,   120,   124,   125,   126,     3,     7,
     352,    44,    45,     0,     0,     0,   340,     0,   326,   357,
       0,   332,     0,     0,     0,     0,   330,     0,     0,     0,
     330,     0,     0,   299,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   302,   323,     0,     0,     0,     0,     0,
     326,     0,   366,     0,    97,     0,     0,     0,     0,   255,
       0,     0,   257,     0,     0,   129,     0,   137,   139,     0,
       0,   131,   225,   133,   207,   214,     0,   173,   174,   175,
     176,   177,   178,   179,   180,   182,   183,   170,   185,   186,
     261,   261,   267,     0,   184,   151,     0,   143,     0,   134,
     140,     0,   296,   149,     0,     0,   153,     0,   167,     0,
     268,     0,     0,     0,   357,     0,   150,   156,     0,     0,
     157,    19,     0,     0,     0,   249,     0,   244,     0,   251,
       0,     0,     0,   246,    98,    99,     0,   331,   335,   336,
     325,     0,     0,   333,     0,   337,     0,     0,     0,     0,
     338,     0,   339,     0,     0,     0,     0,   348,   301,     0,
       0,     0,     0,     0,     0,   303,   305,   304,   345,   344,
     324,    48,     0,    54,    59,    53,    56,     0,    94,    70,
      63,     0,     0,     0,     0,    64,    66,   359,   327,   358,
     360,   256,   262,   258,   264,   266,   130,     0,     0,   289,
       0,     0,   224,   209,   211,     0,     0,     0,     0,     0,
       0,   154,     0,     0,   152,     0,   162,   161,   297,   160,
     159,     0,    20,     0,     0,     0,   250,   245,   252,     0,
       0,   343,     0,     0,     0,     0,   362,   357,     0,     0,
     364,     0,   365,   357,     0,     0,     0,     0,     0,   346,
       0,     0,   330,     0,   330,     0,     0,     0,     0,     0,
       0,     0,    61,    60,     0,     0,     0,    95,     0,   355,
       0,   365,    68,     0,    67,     0,   164,     0,     0,     0,
       0,     0,   214,   219,   218,     0,   213,   216,   217,   172,
       0,     0,   163,   135,   142,   148,   147,   269,   158,    21,
       0,     0,   108,   248,   247,     0,   350,     0,   349,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   374,   376,
     375,   373,   372,     0,     0,     0,     0,     0,   322,   315,
     314,   313,   312,    50,    49,    55,    57,    58,    65,   354,
       0,     0,     0,     0,   290,     0,   291,     0,   226,   210,
       0,     0,     0,    22,     0,     0,     0,     0,     0,   361,
       0,     0,     0,   363,     0,     0,     0,     0,     0,     0,
     359,     0,     0,     0,     0,     0,     0,   356,    47,     0,
     165,     0,     0,   212,   215,   217,   145,   146,    23,     0,
       0,   311,   334,   309,     0,   317,   321,   369,   371,   370,
     368,   367,   320,     0,   308,   306,     0,   316,     0,    51,
     293,   292,   227,     0,    24,     0,     0,     0,     0,     0,
       0,   144,    25,     0,     0,   310,   319,   307,   318,    26,
       0,     0,    27,     0,     0,    28,     0,     0,    29,     0,
       0,    30,     0,     0,    31,     0,     0,    32,     0,     0,
      33,     0,     0,    34,     0,     0,    35,     0,     0,    36,
       0,     0,    37,     0,    38
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -600,  -600,   614,   467,   -34,  -600,  -600,  -600,  -600,   148,
    -600,  -600,    60,    68,  -599,  -519,  -600,    57,  -528,  -387,
     418,    -3,   439,    69,   285,    -2,  -193,   -33,   501,   -26,
     291,    17,  -600,   421,  -600,   410,  -600,   142,  -600,   -15,
    -600,   419,  -600,    73,  -600,  -600,     6,   -59,  -600,  -600,
     288,   -54,  -600,   -92,   -51,  -174,  -600,  -178,  -389,  -600,
      -8,   -52,  -600,    89,   -31,  -124,   457,  -600,   328,  -600,
     482,   -80,  1111,  -600,   487,  -600,  -600,  -600,  -600,  -600,
    -600,  -600,   691
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     4,    43,    44,    45,    46,    47,   151,    48,    49,
     642,    50,   545,   546,   543,   544,    51,   555,   556,   265,
     266,    52,   139,   547,   272,   140,    65,    66,    67,    68,
      69,    70,   107,   108,   298,   299,   757,   473,   474,    54,
     291,   292,   573,   574,   575,   656,   657,    55,   113,   441,
     442,   201,   202,   109,   279,   280,   281,   282,   283,   144,
     145,    56,   568,   569,   141,   337,   338,   260,   261,   413,
     387,   339,   274,   677,    77,   275,   640,   276,   277,   395,
     398,   399,    73
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      64,    72,   158,   200,   200,    81,    81,   143,   273,   205,
     152,   203,   203,   359,   360,   361,   362,   112,    53,   115,
     353,   103,   351,   352,   633,   300,   165,   644,   541,   111,
     340,   703,   164,   634,    22,   705,   142,   746,    22,   148,
     416,   630,   249,   625,   564,   325,   200,   250,   518,   322,
      74,   519,    22,   251,   160,   653,   626,   285,   166,   627,
     158,    53,   252,   158,    22,   654,    22,   228,  -353,   161,
     286,   149,   150,   405,   507,   406,   425,   247,   253,   407,
     247,   408,   409,    81,   410,   411,    22,   105,   443,  -353,
      22,   106,    53,   412,   164,   412,   200,   425,   290,   511,
     165,   269,   509,   255,   341,   270,   635,   271,   412,    22,
     635,   564,   417,   631,   258,   297,   103,   655,  -353,   259,
     563,  -353,   381,   412,   382,   342,   425,  -353,   681,   425,
     553,    22,   696,    22,    22,   687,   206,   200,   200,    79,
     598,   636,   200,   164,   412,   423,   200,   779,    22,   330,
     357,   682,   249,   346,   357,   345,    28,   250,    84,   412,
     347,   246,    22,   251,   207,   208,   489,   643,   502,   778,
     525,    29,   252,   412,   388,   388,   412,   363,   436,   412,
     644,   524,   412,   105,   633,   209,   633,   106,   253,   420,
      22,   331,   424,   372,   373,   374,   405,    79,   406,   156,
     514,   157,   407,   248,   408,   409,   300,   410,   411,   490,
     503,   269,   114,   255,   693,   270,   412,   271,   117,   661,
     694,   525,   776,   378,   258,   638,    28,  -366,  -366,   259,
     207,   208,   249,   153,   154,    28,   464,   250,   158,    79,
      22,    29,    22,   251,    53,   412,   412,   520,   521,   679,
      29,   209,   252,   348,   438,   155,   420,   402,   412,   412,
     633,   662,    22,   247,   200,   412,   773,   412,   253,   156,
     159,   157,   475,   576,   412,   162,   -73,    22,   156,   446,
     157,    22,   225,   435,   353,    22,   351,   352,   675,    22,
     492,   269,    22,   255,   593,   270,   297,   271,   670,   522,
     523,   412,   724,   774,   258,   759,   764,   510,    22,   259,
     512,    22,   510,   388,   210,   412,   412,    22,    22,   479,
     472,   785,   477,    22,   793,    22,   480,   481,   482,   648,
     800,   803,   775,   649,    22,   777,   806,    22,   809,   273,
      22,   412,   488,   412,    22,   152,   491,   812,   494,    22,
     815,   653,    22,   818,   504,    22,   505,   821,   797,   249,
      22,   654,   824,   264,   250,   827,   146,   412,   830,    22,
     251,    22,   200,   354,   798,   200,   287,   385,    22,   252,
     562,   200,   412,   565,   529,   227,   571,   323,   530,   203,
     531,   321,   403,   324,   158,   253,    53,   322,   404,   268,
     534,   464,   464,   420,   429,   588,   567,   486,   495,   709,
     430,    22,    81,   322,   496,   710,   577,   228,   269,   386,
     255,    22,   270,   676,   271,   497,    80,    83,    22,   498,
     143,   258,   581,   288,   566,   582,   259,   606,   628,   609,
     302,   629,   200,   200,   612,     1,     2,     3,   646,   647,
     599,   600,   614,   615,   616,   294,   617,   618,   303,   142,
     734,   735,   736,   305,   737,   738,   579,    85,    87,   580,
     424,   639,   606,   641,   211,   212,   213,   214,   215,   216,
     217,   218,   328,   219,   220,   221,   326,   591,   327,   306,
     592,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,   222,   223,
     224,   307,   200,   375,   376,   377,   308,   432,    58,   433,
     357,   329,    29,   200,   309,   499,   515,   500,   516,   756,
     310,   475,    30,    31,   311,    32,   312,    33,   313,   314,
     332,   315,    60,   317,   318,   319,    34,    35,    36,   320,
      61,    81,    38,   350,    39,   426,    40,   427,   335,   428,
     431,   659,   434,   445,   439,   440,   471,  -254,    41,    89,
      90,    91,    92,    93,    94,    95,   467,   485,   501,   487,
     209,   666,    96,   667,   506,   508,   517,    22,    97,   526,
     791,   669,   527,   672,   412,   542,   551,    98,   552,   554,
     729,   464,   464,   583,   733,   561,   585,   158,   249,   599,
     600,   584,   586,   250,   589,   587,   595,   596,    22,   251,
     645,   597,   660,    99,   668,   680,   603,   390,   252,   652,
     747,   684,   663,   664,   100,   101,   673,   674,   683,   685,
     686,   478,   695,   697,   253,   688,   102,   716,   717,   718,
     689,   690,   691,   720,   692,   711,   713,   721,   481,   482,
     726,   722,   727,   728,   731,   752,   732,   269,   723,   255,
     391,   270,   739,   271,   749,   751,   767,   762,   753,   768,
     258,   769,   770,   783,   787,   259,   771,   788,   833,   226,
     632,   704,    57,    71,   379,   708,    75,    76,    78,    82,
      82,    86,    88,   706,   550,   707,   418,   437,   470,   748,
     444,   750,   364,   365,   366,   367,   368,   369,   370,   371,
     104,   110,   758,   249,   665,   719,   116,   754,   250,   572,
     104,   147,   528,    22,   251,    57,   714,   392,     0,     0,
     384,     0,     0,   252,     0,     0,     0,   780,   163,   781,
     782,     0,     0,   204,   204,     0,     0,   784,     0,   253,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,    82,     0,     0,    82,   263,     0,   267,     0,   278,
       0,     0,   269,   792,   255,   513,   270,     0,   271,     0,
       0,   799,     0,     0,     0,   258,   204,   284,   802,     0,
     259,   805,     0,     0,   808,   293,     0,   811,   301,     0,
     814,     0,     0,   817,     0,   104,   820,     0,     0,   823,
       0,     0,   826,     0,     0,   829,     0,     0,   832,     0,
       0,   834,     0,     0,     0,     0,     0,     0,     0,     0,
     333,   334,     0,     0,   278,   278,   204,     0,     0,     0,
       0,     0,   104,     0,   349,    89,    90,    91,    92,    93,
      94,    95,     0,     0,     0,     0,     0,     0,    96,     0,
       0,     0,     0,    22,    97,     0,     0,     0,     0,     0,
       0,     0,     0,    98,     0,     0,     0,   204,   204,   284,
     355,     0,   204,     0,     0,     0,   204,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   249,     0,     0,    99,
       0,   250,     0,     0,     0,     0,    22,   251,    57,   380,
     100,   101,     0,     0,   385,     0,   252,     0,     0,     0,
       0,     0,   102,     0,     0,     0,     0,   278,     0,   278,
       0,     0,   253,   383,    78,   278,   278,   396,   400,   401,
       0,     0,     0,     0,   278,   415,     0,    82,     0,   419,
     278,   421,   422,   278,     0,   483,   386,   255,     0,   256,
       0,   257,     0,     0,     0,     0,     0,   110,   258,     0,
       0,     0,   293,   259,     0,     0,   465,   466,     0,   301,
       0,     7,     8,     9,    10,    11,     0,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,   204,     0,     0,     0,   104,     0,
       0,     0,     0,     0,     0,     0,     0,   263,    29,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
     493,    32,     0,    33,     0,     0,     0,     0,     0,     0,
       0,     0,    34,    35,    36,     0,    37,     0,    38,     0,
      39,     0,    40,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,    41,     0,     0,    42,   278,     0,
       0,   278,     0,   278,   278,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   263,     0,   278,   278,   104,
     278,   278,   278,   278,   278,     0,   267,     0,   548,   549,
     278,     0,     0,     0,     0,     0,   278,   278,   278,   278,
       0,     0,   204,     0,   284,   204,     0,     0,     0,     0,
     570,   204,     0,     0,     0,   183,   184,   185,   186,   187,
     188,   189,   190,     0,   191,    22,   192,   193,    25,   194,
     195,   465,   465,   466,   578,     0,   229,   230,   231,   232,
     233,   234,   235,   236,   237,   238,   356,   239,   240,   241,
     242,     0,     0,     0,   278,     0,   278,     0,     0,     0,
     243,   244,     0,     0,   594,   245,     0,     0,     0,     0,
       0,   284,   204,   204,   196,   262,   197,   278,   198,     0,
     199,   137,   278,     0,     0,   278,     0,   605,   278,   278,
     278,     0,   610,   138,   611,   278,     0,   278,     0,     0,
     278,   622,   624,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    82,     0,     0,     0,     0,   637,     0,
       0,   278,   278,   278,   278,   267,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   204,     0,     0,     0,   658,     0,     0,   249,
       0,     0,     0,   204,   250,     0,     0,     0,     0,    22,
     251,     0,     0,     0,     0,   671,     0,     0,     0,   252,
       0,     0,     0,     0,   678,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   253,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   278,   278,   278,   278,
     278,     0,   548,     0,     0,     0,   548,   548,   393,     0,
     255,     0,   270,     0,   271,     0,   712,     0,   570,   715,
       0,   258,     0,   249,   538,     0,   259,     0,   250,     0,
       0,   465,   465,    22,   251,     0,     0,     0,     0,     0,
       0,     0,   725,   252,     0,   389,     0,   394,   397,     0,
       0,   278,   278,     0,   414,   278,     0,     0,   278,   253,
       0,     0,     0,     0,   278,   278,   278,   278,   278,   249,
       0,     0,     0,     0,   250,     0,     0,     0,   637,    22,
     251,   278,   254,     0,   255,     0,   256,     0,   257,   252,
       0,     0,   755,     0,     0,   258,   760,   278,     0,   278,
     259,     0,   278,   278,     0,   253,     0,     0,     0,     0,
     278,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   484,   269,     0,
     255,   786,   270,     0,   271,     0,     0,     0,     0,     0,
       0,   258,     0,     0,     0,     0,   259,   278,     0,   278,
       0,     0,     0,     0,     0,     0,     0,   794,   278,   278,
       0,     0,     0,     0,     0,   801,     0,     0,     0,     0,
       0,     0,   804,     0,     0,   807,     0,     0,   810,     0,
       0,   813,     0,     0,   816,     0,     0,   819,     0,     0,
     822,     0,     0,   825,     0,   262,   828,   532,   533,   831,
     535,   536,   537,   539,   540,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   557,   558,   559,   560,
       0,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,     0,     0,
       0,     0,     0,     0,     0,     0,   118,   119,   120,   121,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    30,    31,     0,    32,     0,    33,     0,     0,
     134,   135,    60,     0,   389,   136,    34,    35,    36,     0,
      61,     0,    38,     0,    39,     0,    40,     0,     0,    62,
      63,   137,     0,     0,     0,     0,     0,   601,    41,     0,
       0,     0,   602,   138,     0,   604,     0,     0,   607,   608,
       0,     0,     0,     0,     0,   613,     0,   619,     0,     0,
     620,   621,   623,     5,     6,     0,     7,     8,     9,    10,
      11,     0,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    29,     0,     0,     0,     0,     0,     0,
     249,     0,     0,    30,    31,   250,    32,     0,    33,     0,
      22,   251,     0,     0,     0,     0,     0,    34,    35,    36,
     252,    37,     0,    38,   249,    39,     0,    40,     0,   250,
       0,     0,     0,     0,    22,   251,   253,     0,     0,    41,
       0,     0,    42,     0,   252,     0,   698,   699,   700,   701,
     702,     0,     0,     0,     0,     0,     0,     0,     0,   336,
     253,   255,     0,   270,     0,   271,     0,     0,     0,     0,
       0,     0,   258,     0,     0,     0,     0,   259,     0,     0,
       0,     0,     0,   393,     0,   255,     0,   270,     0,   271,
       0,     0,     0,     0,     0,     0,   258,     0,     0,     0,
       0,   259,   730,     0,     0,     0,     0,     0,   740,     0,
       0,     0,     0,     0,   741,   742,   743,   744,   745,     0,
       0,     0,     0,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,   761,     0,   763,
      58,     0,   765,   766,    29,     0,     0,     0,     0,     0,
     772,     0,     0,    59,    30,    31,     0,    32,     0,    33,
       0,     0,     0,     0,    60,     0,     0,     0,    34,    35,
      36,   468,   295,     0,    38,     0,   296,   469,    40,     0,
       0,    62,    63,     0,     0,     0,     0,   789,     0,   790,
      41,     0,     0,     0,     0,     0,     0,     0,   795,   796,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,     0,     0,     0,
       0,     0,     0,     0,     0,   167,   168,   169,   170,   171,
     172,    29,   173,   174,   175,   128,   176,   177,   178,   179,
     133,    30,    31,     0,    32,     0,    33,     0,     0,   180,
     181,    60,     0,     0,   182,    34,    35,    36,     0,    61,
       0,    38,     0,    39,     0,    40,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,    41,   447,   448,
     449,   450,   451,   452,   453,   454,    20,   455,    22,   456,
     457,    25,   458,   459,    28,     0,     0,     0,     0,     0,
       0,     0,     0,   167,   168,   169,   170,   171,   172,    29,
     173,   174,   175,   128,   176,   177,   178,   179,   133,    30,
      31,     0,    32,     0,    33,     0,     0,   180,   181,    60,
       0,     0,   182,    34,    35,    36,     0,   460,     0,   461,
       0,   462,     0,   463,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,    41,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
     343,    27,    28,     0,     0,   344,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,    29,     0,     0,
       0,     0,     0,     0,     0,     0,    59,    30,    31,     0,
      32,     0,    33,     0,     0,     0,     0,    60,     0,     0,
       0,    34,    35,    36,     0,    61,     0,    38,     0,    39,
       0,    40,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,    41,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,    29,     0,     0,     0,     0,
       0,     0,     0,     0,    59,    30,    31,     0,    32,     0,
      33,     0,     0,     0,     0,    60,     0,     0,     0,    34,
      35,    36,     0,    61,     0,    38,   476,    39,     0,    40,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,    41,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,    29,     0,     0,     0,     0,     0,     0,
       0,     0,    59,    30,    31,     0,    32,     0,    33,     0,
       0,     0,     0,    60,     0,     0,     0,    34,    35,    36,
       0,    61,     0,    38,   590,    39,     0,    40,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,    41,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,    29,     0,     0,     0,     0,     0,     0,     0,     0,
      59,    30,    31,     0,    32,     0,    33,     0,     0,     0,
       0,    60,     0,     0,     0,    34,    35,    36,     0,    61,
       0,    38,     0,    39,     0,    40,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,    41,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,    29,
       0,     0,     0,     0,     0,     0,     0,     0,    59,    30,
      31,     0,    32,     0,    33,     0,     0,     0,     0,    60,
       0,     0,     0,    34,    35,    36,     0,   295,     0,    38,
       0,   296,     0,    40,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,    41,   447,   448,   449,   450,
     451,   452,   453,   454,    20,   455,    22,   456,   457,    25,
     458,   459,    28,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,    29,     0,     0,
       0,     0,     0,     0,     0,     0,    59,    30,    31,     0,
      32,     0,    33,     0,     0,     0,     0,    60,     0,     0,
       0,    34,    35,    36,     0,   460,     0,   461,     0,   462,
       0,   463,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,    41,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,    29,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,     0,    32,     0,
      33,     0,     0,     0,     0,    60,     0,     0,     0,    34,
      35,    36,     0,    61,   316,    38,     0,    39,     0,    40,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    41,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,    29,     0,     0,     0,     0,     0,     0,
       0,     0,    59,    30,    31,     0,    32,     0,    33,     0,
       0,     0,     0,    60,     0,     0,     0,    34,    35,    36,
       0,    61,     0,    38,     0,    39,     0,    40,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    41,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,    29,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    30,    31,     0,    32,     0,    33,     0,     0,     0,
       0,     0,     0,     0,     0,    34,    35,    36,     0,    61,
     304,    38,     0,    39,     0,    40,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    41,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,    29,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,     0,    32,     0,    33,     0,     0,     0,     0,     0,
       0,     0,     0,    34,    35,    36,     0,    61,     0,    38,
       0,    39,     0,    40,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    41,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    29,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,     0,
      32,     0,    33,     0,     0,     0,     0,     0,     0,     0,
       0,    34,    35,    36,     0,    61,     0,    38,     0,    39,
       0,    40,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    41,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    29,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    31,     0,     0,     0,
       0,   289,     0,     0,     0,     0,     0,     0,     0,    34,
      35,     0,     0,    61,     0,    38,     0,    39,     0,    40,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    41,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   650,
       0,     0,     0,    29,     0,     0,   183,   184,   185,   186,
     187,   188,   189,   190,    31,   191,    22,   192,   193,    25,
     194,   195,     0,     0,   651,     0,     0,    34,    35,     0,
       0,    61,     0,    38,     0,    39,     0,    40,     0,     0,
       0,   183,   184,   185,   186,   187,   188,   189,   190,    41,
     191,    22,   192,   193,    25,   194,   195,     0,     0,     0,
       0,     0,     0,     0,     0,   196,     0,   197,     0,   198,
       0,   199,   358,   183,   184,   185,   186,   187,   188,   189,
     190,     0,   191,    22,   192,   193,    25,   194,   195,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     196,     0,   197,     0,   198,     0,   199,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   196,     0,   197,     0,   198,     0,   199
};

static const yytype_int16 yycheck[] =
{
       2,     3,    54,    62,    63,     8,     9,    38,    88,    63,
      44,    62,    63,   206,   207,   208,   209,    32,     1,    34,
     198,    29,   196,   197,   543,   117,    59,   555,   415,    31,
     154,   630,    58,     8,    25,   634,    38,     8,    25,    41,
       8,     8,    15,    38,   433,    77,   105,    20,    41,    81,
       0,    44,    25,    26,    31,    16,    36,    69,    60,    39,
     112,    44,    35,   115,    25,    26,    25,    46,    33,    46,
      82,    93,    94,    38,    53,    40,    41,    80,    51,    44,
      83,    46,    47,    86,    49,    50,    25,    74,    79,    33,
      25,    78,    75,    88,   120,    88,   155,    41,   113,    43,
     133,    74,    75,    76,   155,    78,    81,    80,    88,    25,
      81,   500,    80,    80,    87,   117,   124,    78,    33,    92,
      79,    33,   246,    88,   248,   156,    41,    33,    43,    41,
      34,    25,    41,    25,    25,    41,    36,   196,   197,    74,
      79,    33,   201,   169,    88,     8,   205,   746,    25,    38,
     201,    80,    15,   161,   205,   157,    31,    20,    74,    88,
     162,    38,    25,    26,    64,    65,    34,   554,    38,    41,
      74,    46,    35,    88,   254,   255,    88,   210,    69,    88,
     708,    34,    88,    74,   703,    85,   705,    78,    51,   269,
      25,    80,   272,   219,   220,   221,    38,    74,    40,    74,
      43,    76,    44,    38,    46,    47,   298,    49,    50,    77,
      80,    74,    78,    76,    43,    78,    88,    80,    78,    38,
      43,    74,    43,   225,    87,     8,    31,    80,    81,    92,
      64,    65,    15,    39,    39,    31,   295,    20,   290,    74,
      25,    46,    25,    26,   227,    88,    88,    80,    81,    79,
      46,    85,    35,    38,   287,    60,   336,   259,    88,    88,
     779,    80,    25,   266,   323,    88,    80,    88,    51,    74,
      28,    76,   323,    69,    88,    38,    39,    25,    74,   294,
      76,    25,    38,   285,   462,    25,   460,   461,    75,    25,
      38,    74,    25,    76,    38,    78,   298,    80,    38,    80,
      81,    88,    38,    75,    87,    38,    79,   387,    25,    92,
     390,    25,   392,   393,    69,    88,    88,    25,    25,   327,
     322,    38,   324,    25,    38,    25,   328,   329,   330,    34,
      38,    38,    79,    38,    25,    80,    38,    25,    38,   419,
      25,    88,   344,    88,    25,   379,   348,    38,   350,    25,
      38,    16,    25,    38,   356,    25,   358,    38,    79,    15,
      25,    26,    38,    32,    20,    38,    20,    88,    38,    25,
      26,    25,   431,    20,    80,   434,    38,    33,    25,    35,
     431,   440,    88,   434,    74,     8,   440,    80,    78,   440,
      80,    75,    75,    86,   446,    51,   379,    81,    81,    32,
     408,   460,   461,   483,    75,   485,    20,    75,    75,    75,
      81,    25,   415,    81,    81,    81,    20,    46,    74,    75,
      76,    25,    78,    20,    80,    77,     8,     9,    25,    81,
     461,    87,    77,    63,   436,    80,    92,   517,    36,   519,
      75,    39,   501,   502,   524,     3,     4,     5,    80,    81,
     501,   502,    15,    16,    17,    38,    19,    20,    75,   461,
      15,    16,    17,    75,    19,    20,   468,    10,    11,   471,
     550,   551,   552,   553,    47,    48,    49,    50,    51,    52,
      53,    54,    38,    40,    41,    42,    79,   489,    81,    75,
     492,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    43,    44,
      45,    75,   571,   222,   223,   224,    75,    79,    42,    81,
     571,    38,    46,   582,    75,    79,    79,    81,    81,   722,
      75,   582,    56,    57,    75,    59,    75,    61,    75,    75,
      92,    75,    66,    75,    75,    75,    70,    71,    72,    75,
      74,   554,    76,    67,    78,    33,    80,    43,    96,    41,
      81,   576,    38,    37,    80,    80,    38,    38,    92,     7,
       8,     9,    10,    11,    12,    13,    82,    32,    38,    77,
      85,   583,    20,   585,    46,    75,    34,    25,    26,    46,
     783,   593,    92,   595,    88,    14,    81,    35,    34,    14,
     680,   660,   661,    36,   684,    75,    38,   659,    15,   660,
     661,    77,    80,    20,    77,    80,    68,    75,    25,    26,
      34,    77,    38,    61,    77,    34,    80,    34,    35,    80,
     710,    34,    79,    82,    72,    73,    80,    80,    80,    74,
      80,    79,    34,    34,    51,    75,    84,   649,   650,   651,
      75,    75,    75,   655,    75,    38,    58,    34,   660,   661,
      38,    81,    34,    38,    38,    33,    39,    74,   670,    76,
      77,    78,    39,    80,    38,    38,    75,    77,    79,    75,
      87,    75,    75,    81,    38,    92,    75,    38,    38,    75,
     542,   631,     1,     2,   227,   638,     5,     6,     7,     8,
       9,    10,    11,   635,   419,   636,   267,   286,   298,   711,
     291,   713,   211,   212,   213,   214,   215,   216,   217,   218,
      29,    30,   724,    15,   582,   652,    35,   721,    20,   441,
      39,    40,   404,    25,    26,    44,   647,   255,    -1,    -1,
     253,    -1,    -1,    35,    -1,    -1,    -1,   749,    57,   751,
     752,    -1,    -1,    62,    63,    -1,    -1,   759,    -1,    51,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    -1,    -1,    -1,
      -1,    80,    -1,    -1,    83,    84,    -1,    86,    -1,    88,
      -1,    -1,    74,   785,    76,    77,    78,    -1,    80,    -1,
      -1,   793,    -1,    -1,    -1,    87,   105,   106,   800,    -1,
      92,   803,    -1,    -1,   806,   114,    -1,   809,   117,    -1,
     812,    -1,    -1,   815,    -1,   124,   818,    -1,    -1,   821,
      -1,    -1,   824,    -1,    -1,   827,    -1,    -1,   830,    -1,
      -1,   833,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,    -1,   153,   154,   155,    -1,    -1,    -1,
      -1,    -1,   161,    -1,   163,     7,     8,     9,    10,    11,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    20,    -1,
      -1,    -1,    -1,    25,    26,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    35,    -1,    -1,    -1,   196,   197,   198,
     199,    -1,   201,    -1,    -1,    -1,   205,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    15,    -1,    -1,    61,
      -1,    20,    -1,    -1,    -1,    -1,    25,    26,   227,   228,
      72,    73,    -1,    -1,    33,    -1,    35,    -1,    -1,    -1,
      -1,    -1,    84,    -1,    -1,    -1,    -1,   246,    -1,   248,
      -1,    -1,    51,   252,   253,   254,   255,   256,   257,   258,
      -1,    -1,    -1,    -1,   263,   264,    -1,   266,    -1,   268,
     269,   270,   271,   272,    -1,    74,    75,    76,    -1,    78,
      -1,    80,    -1,    -1,    -1,    -1,    -1,   286,    87,    -1,
      -1,    -1,   291,    92,    -1,    -1,   295,   296,    -1,   298,
      -1,     9,    10,    11,    12,    13,    -1,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,   323,    -1,    -1,    -1,   327,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   336,    46,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    56,    57,
     349,    59,    -1,    61,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    -1,    74,    -1,    76,    -1,
      78,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     379,    -1,    -1,    -1,    92,    -1,    -1,    95,   387,    -1,
      -1,   390,    -1,   392,   393,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   404,    -1,   406,   407,   408,
     409,   410,   411,   412,   413,    -1,   415,    -1,   417,   418,
     419,    -1,    -1,    -1,    -1,    -1,   425,   426,   427,   428,
      -1,    -1,   431,    -1,   433,   434,    -1,    -1,    -1,    -1,
     439,   440,    -1,    -1,    -1,    15,    16,    17,    18,    19,
      20,    21,    22,    -1,    24,    25,    26,    27,    28,    29,
      30,   460,   461,   462,   463,    -1,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    46,    51,    52,    53,
      54,    -1,    -1,    -1,   483,    -1,   485,    -1,    -1,    -1,
      64,    65,    -1,    -1,   493,    69,    -1,    -1,    -1,    -1,
      -1,   500,   501,   502,    74,    84,    76,   506,    78,    -1,
      80,    85,   511,    -1,    -1,   514,    -1,   516,   517,   518,
     519,    -1,   521,    97,   523,   524,    -1,   526,    -1,    -1,
     529,   530,   531,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   542,    -1,    -1,    -1,    -1,   547,    -1,
      -1,   550,   551,   552,   553,   554,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   571,    -1,    -1,    -1,   575,    -1,    -1,    15,
      -1,    -1,    -1,   582,    20,    -1,    -1,    -1,    -1,    25,
      26,    -1,    -1,    -1,    -1,   594,    -1,    -1,    -1,    35,
      -1,    -1,    -1,    -1,   603,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    51,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   625,   626,   627,   628,
     629,    -1,   631,    -1,    -1,    -1,   635,   636,    74,    -1,
      76,    -1,    78,    -1,    80,    -1,   645,    -1,   647,   648,
      -1,    87,    -1,    15,    90,    -1,    92,    -1,    20,    -1,
      -1,   660,   661,    25,    26,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   671,    35,    -1,   254,    -1,   256,   257,    -1,
      -1,   680,   681,    -1,   263,   684,    -1,    -1,   687,    51,
      -1,    -1,    -1,    -1,   693,   694,   695,   696,   697,    15,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,   707,    25,
      26,   710,    74,    -1,    76,    -1,    78,    -1,    80,    35,
      -1,    -1,   721,    -1,    -1,    87,   725,   726,    -1,   728,
      92,    -1,   731,   732,    -1,    51,    -1,    -1,    -1,    -1,
     739,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   336,    74,    -1,
      76,   760,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,    -1,    -1,    92,   776,    -1,   778,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   786,   787,   788,
      -1,    -1,    -1,    -1,    -1,   794,    -1,    -1,    -1,    -1,
      -1,    -1,   801,    -1,    -1,   804,    -1,    -1,   807,    -1,
      -1,   810,    -1,    -1,   813,    -1,    -1,   816,    -1,    -1,
     819,    -1,    -1,   822,    -1,   404,   825,   406,   407,   828,
     409,   410,   411,   412,   413,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   425,   426,   427,   428,
      -1,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    -1,    61,    -1,    -1,
      64,    65,    66,    -1,   483,    69,    70,    71,    72,    -1,
      74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,    83,
      84,    85,    -1,    -1,    -1,    -1,    -1,   506,    92,    -1,
      -1,    -1,   511,    97,    -1,   514,    -1,    -1,   517,   518,
      -1,    -1,    -1,    -1,    -1,   524,    -1,   526,    -1,    -1,
     529,   530,   531,     6,     7,    -1,     9,    10,    11,    12,
      13,    -1,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    -1,    -1,    56,    57,    20,    59,    -1,    61,    -1,
      25,    26,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      35,    74,    -1,    76,    15,    78,    -1,    80,    -1,    20,
      -1,    -1,    -1,    -1,    25,    26,    51,    -1,    -1,    92,
      -1,    -1,    95,    -1,    35,    -1,   625,   626,   627,   628,
     629,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      51,    76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    87,    -1,    -1,    -1,    -1,    92,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    76,    -1,    78,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,    -1,
      -1,    92,   681,    -1,    -1,    -1,    -1,    -1,   687,    -1,
      -1,    -1,    -1,    -1,   693,   694,   695,   696,   697,    -1,
      -1,    -1,    -1,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   726,    -1,   728,
      42,    -1,   731,   732,    46,    -1,    -1,    -1,    -1,    -1,
     739,    -1,    -1,    55,    56,    57,    -1,    59,    -1,    61,
      -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    -1,    76,    -1,    78,    79,    80,    -1,
      -1,    83,    84,    -1,    -1,    -1,    -1,   776,    -1,   778,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   787,   788,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    -1,    61,    -1,    -1,    64,
      65,    66,    -1,    -1,    69,    70,    71,    72,    -1,    74,
      -1,    76,    -1,    78,    -1,    80,    -1,    -1,    83,    84,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    59,    -1,    61,    -1,    -1,    64,    65,    66,
      -1,    -1,    69,    70,    71,    72,    -1,    74,    -1,    76,
      -1,    78,    -1,    80,    -1,    -1,    83,    84,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    92,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    -1,    -1,    34,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    42,    -1,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    55,    56,    57,    -1,
      59,    -1,    61,    -1,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    70,    71,    72,    -1,    74,    -1,    76,    -1,    78,
      -1,    80,    -1,    -1,    83,    84,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    92,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    42,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    55,    56,    57,    -1,    59,    -1,
      61,    -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,    70,
      71,    72,    -1,    74,    -1,    76,    77,    78,    -1,    80,
      -1,    -1,    83,    84,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    92,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    55,    56,    57,    -1,    59,    -1,    61,    -1,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    70,    71,    72,
      -1,    74,    -1,    76,    77,    78,    -1,    80,    -1,    -1,
      83,    84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      55,    56,    57,    -1,    59,    -1,    61,    -1,    -1,    -1,
      -1,    66,    -1,    -1,    -1,    70,    71,    72,    -1,    74,
      -1,    76,    -1,    78,    -1,    80,    -1,    -1,    83,    84,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,    46,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    56,
      57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    70,    71,    72,    -1,    74,    -1,    76,
      -1,    78,    -1,    80,    -1,    -1,    83,    84,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    92,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    42,    -1,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    55,    56,    57,    -1,
      59,    -1,    61,    -1,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    70,    71,    72,    -1,    74,    -1,    76,    -1,    78,
      -1,    80,    -1,    -1,    83,    84,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    92,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    42,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    56,    57,    -1,    59,    -1,
      61,    -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,    70,
      71,    72,    -1,    74,    75,    76,    -1,    78,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    92,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    55,    56,    57,    -1,    59,    -1,    61,    -1,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    70,    71,    72,
      -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    56,    57,    -1,    59,    -1,    61,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    -1,    74,
      75,    76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,    46,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    56,
      57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    -1,    74,    -1,    76,
      -1,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    92,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    56,    57,    -1,
      59,    -1,    61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    -1,    74,    -1,    76,    -1,    78,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    92,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,    -1,
      -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    -1,    -1,    74,    -1,    76,    -1,    78,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    92,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     8,
      -1,    -1,    -1,    46,    -1,    -1,    15,    16,    17,    18,
      19,    20,    21,    22,    57,    24,    25,    26,    27,    28,
      29,    30,    -1,    -1,    33,    -1,    -1,    70,    71,    -1,
      -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,
      -1,    15,    16,    17,    18,    19,    20,    21,    22,    92,
      24,    25,    26,    27,    28,    29,    30,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    74,    -1,    76,    -1,    78,
      -1,    80,    46,    15,    16,    17,    18,    19,    20,    21,
      22,    -1,    24,    25,    26,    27,    28,    29,    30,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    76,    -1,    78,    -1,    80
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,    99,     6,     7,     9,    10,    11,
      12,    13,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    46,
      56,    57,    59,    61,    70,    71,    72,    74,    76,    78,
      80,    92,    95,   100,   101,   102,   103,   104,   106,   107,
     109,   114,   119,   129,   137,   145,   159,   180,    42,    55,
      66,    74,    83,    84,   123,   124,   125,   126,   127,   128,
     129,   180,   123,   180,     0,   180,   180,   172,   180,    74,
     118,   119,   180,   118,    74,   164,   180,   164,   180,     7,
       8,     9,    10,    11,    12,    13,    20,    26,    35,    61,
      72,    73,    84,   158,   180,    74,    78,   130,   131,   151,
     180,   123,   137,   146,    78,   137,   180,    78,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    64,    65,    69,    85,    97,   120,
     123,   162,   123,   162,   157,   158,    20,   180,   123,    93,
      94,   105,   102,    39,    39,    60,    74,    76,   159,    28,
      31,    46,    38,   180,   127,   125,   123,    40,    41,    42,
      43,    44,    45,    47,    48,    49,    51,    52,    53,    54,
      64,    65,    69,    15,    16,    17,    18,    19,    20,    21,
      22,    24,    26,    27,    29,    30,    74,    76,    78,    80,
     145,   149,   150,   152,   180,   149,    36,    64,    65,    85,
      69,    47,    48,    49,    50,    51,    52,    53,    54,    40,
      41,    42,    43,    44,    45,    38,   100,     8,    46,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    51,
      52,    53,    54,    64,    65,    69,    38,   119,    38,    15,
      20,    26,    35,    51,    74,    76,    78,    80,    87,    92,
     165,   166,   170,   180,    32,   117,   118,   180,    32,    74,
      78,    80,   122,   169,   170,   173,   175,   176,   180,   152,
     153,   154,   155,   156,   180,    69,    82,    38,    63,    62,
     137,   138,   139,   180,    38,    74,    78,   123,   132,   133,
     151,   180,    75,    75,    75,    75,    75,    75,    75,    75,
      75,    75,    75,    75,    75,    75,    75,    75,    75,    75,
      75,    75,    81,    80,    86,    77,    79,    81,    38,    38,
      38,    80,    92,   180,   180,    96,    74,   163,   164,   169,
     163,   152,   162,    29,    34,   123,   158,   123,    38,   180,
      67,   153,   153,   155,    20,   180,    46,   152,    46,   124,
     124,   124,   124,   125,   126,   126,   126,   126,   126,   126,
     126,   126,   127,   127,   127,   128,   128,   128,   123,   101,
     180,   163,   163,   180,   172,    33,    75,   168,   169,   170,
      34,    77,   168,    74,   170,   177,   180,   170,   178,   179,
     180,   180,   123,    75,    81,    38,    40,    44,    46,    47,
      49,    50,    88,   167,   170,   180,     8,    80,   120,   180,
     169,   180,   180,     8,   169,    41,    33,    43,    41,    75,
      81,    81,    79,    81,    38,   123,    69,   131,   125,    80,
      80,   147,   148,    79,   139,    37,   137,    15,    16,    17,
      18,    19,    20,    21,    22,    24,    26,    27,    29,    30,
      74,    76,    78,    80,   145,   180,   180,    82,    73,    79,
     133,    38,   123,   135,   136,   152,    77,   123,    79,   158,
     123,   123,   123,    74,   170,    32,    75,    77,   123,    34,
      77,   123,    38,   180,   123,    75,    81,    77,    81,    79,
      81,    38,    38,    80,   123,   123,    46,    53,    75,    75,
     169,    43,   169,    77,    43,    79,    81,    34,    41,    44,
      80,    81,    80,    81,    34,    74,    46,    92,   166,    74,
      78,    80,   170,   170,   158,   170,   170,   170,    90,   170,
     170,   117,    14,   112,   113,   110,   111,   121,   180,   180,
     122,    81,    34,    34,    14,   115,   116,   170,   170,   170,
     170,    75,   152,    79,   156,   152,   123,    20,   160,   161,
     180,   149,   148,   140,   141,   142,    69,    20,   180,   123,
     123,    77,    80,    36,    77,    38,    80,    80,   169,    77,
      77,   123,   123,    38,   180,    68,    75,    77,    79,   152,
     152,   170,   170,    80,   170,   180,   169,   170,   170,   169,
     180,   180,   169,   170,    15,    16,    17,    19,    20,   170,
     170,   170,   180,   170,   180,    38,    36,    39,    36,    39,
       8,    80,   107,   113,     8,    81,    33,   180,     8,   169,
     174,   169,   108,   117,   116,    34,    80,    81,    34,    38,
       8,    33,    80,    16,    26,    78,   143,   144,   180,   137,
      38,    38,    80,    79,    82,   135,   123,   123,    77,   123,
      38,   180,   123,    80,    80,    75,    20,   171,   180,    79,
      34,    43,    80,    80,    34,    74,    80,    41,    75,    75,
      75,    75,    75,    43,    43,    34,    41,    34,   170,   170,
     170,   170,   170,   112,   110,   112,   111,   121,   115,    75,
      81,    38,   180,    58,   161,   180,   123,   123,   123,   141,
     123,    34,    81,   123,    38,   180,    38,    34,    38,   169,
     170,    38,    39,   169,    15,    16,    17,    19,    20,    39,
     170,   170,   170,   170,   170,   170,     8,   169,   123,    38,
     123,    38,    33,    79,   144,   180,   124,   134,   123,    38,
     180,   170,    77,   170,    79,   170,   170,    75,    75,    75,
      75,    75,   170,    80,    75,    79,    43,    80,    41,   112,
     123,   123,   123,    81,   123,    38,   180,    38,    38,   170,
     170,   124,   123,    38,   180,   170,   170,    79,    80,   123,
      38,   180,   123,    38,   180,   123,    38,   180,   123,    38,
     180,   123,    38,   180,   123,    38,   180,   123,    38,   180,
     123,    38,   180,   123,    38,   180,   123,    38,   180,   123,
      38,   180,   123,    38,   123
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    98,    99,    99,    99,    99,   100,   100,   100,   101,
     101,   101,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     103,   104,   105,   105,   106,   106,   107,   108,   109,   109,
     109,   109,   109,   109,   109,   109,   110,   110,   111,   112,
     112,   113,   114,   114,   114,   114,   115,   115,   116,   117,
     117,   118,   118,   119,   119,   120,   120,   120,   120,   120,
     120,   120,   120,   120,   120,   120,   120,   120,   120,   120,
     120,   120,   120,   120,   121,   121,   122,   122,   123,   123,
     123,   123,   124,   124,   124,   124,   124,   124,   125,   125,
     126,   126,   126,   126,   126,   126,   126,   126,   126,   127,
     127,   127,   127,   127,   128,   128,   128,   128,   129,   129,
     129,   129,   129,   129,   129,   129,   129,   130,   130,   131,
     132,   132,   133,   133,   134,   134,   135,   135,   136,   136,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   138,   138,   139,
     140,   140,   141,   142,   142,   143,   143,   144,   144,   144,
     145,   145,   146,   146,   147,   147,   148,   148,   149,   149,
     150,   150,   150,   150,   150,   150,   150,   150,   150,   150,
     150,   150,   150,   150,   150,   150,   150,   150,   150,   150,
     150,   150,   150,   150,   151,   151,   151,   151,   151,   152,
     153,   153,   154,   154,   155,   155,   156,   157,   157,   157,
     158,   158,   158,   158,   158,   158,   158,   158,   158,   158,
     158,   158,   158,   158,   158,   159,   159,   159,   159,   160,
     160,   161,   161,   161,   162,   162,   162,   163,   163,   164,
     165,   165,   166,   166,   166,   166,   166,   166,   166,   166,
     166,   166,   166,   166,   166,   166,   166,   166,   166,   166,
     166,   166,   166,   167,   167,   168,   168,   169,   169,   169,
     170,   170,   170,   170,   170,   170,   170,   170,   170,   170,
     170,   170,   170,   170,   170,   170,   170,   170,   170,   171,
     171,   172,   172,   173,   173,   174,   174,   175,   175,   176,
     176,   177,   177,   178,   178,   178,   178,   179,   179,   179,
     179,   179,   179,   179,   179,   179,   179,   180
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     2,     4,     2,     2,     3,     4,     1,     0,
       1,     2,     1,     1,     1,     1,     1,     1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,     1,
       2,     3,     2,     2,     4,     4,     3,     3,     5,     7,
       7,     9,     3,     5,     5,     7,     1,     3,     3,     1,
       2,     2,     3,     5,     5,     7,     1,     2,     2,     1,
       3,     1,     2,     1,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     2,     4,     4,
       3,     1,     2,     3,     3,     3,     3,     1,     6,     1,
       3,     3,     3,     3,     3,     3,     3,     3,     1,     3,
       3,     3,     2,     1,     3,     3,     3,     1,     1,     4,
       5,     4,     3,     4,     4,     6,     3,     3,     1,     3,
       2,     1,     4,     2,     3,     1,     5,     3,     3,     1,
       4,     1,     5,     4,     5,     3,     4,     4,     6,     5,
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
       1,     1,     1,     1,     1,     3,     2,     2,     1,     1,
       3,     3,     5,     5,     0,     1,     3,     3,     1,     3,
       1,     3,     2,     3,     3,     3,     7,     9,     7,     7,
       9,     7,     5,     5,     5,     5,     7,     7,     9,     9,
       7,     7,     5,     1,     2,     2,     1,     3,     1,     1,
       1,     3,     2,     3,     7,     3,     3,     3,     3,     3,
       2,     1,     1,     4,     3,     3,     4,     1,     3,     1,
       1,     1,     3,     1,     5,     1,     3,     1,     3,     3,
       3,     5,     3,     5,     3,     3,     1,     6,     6,     6,
       6,     6,     4,     4,     4,     4,     4,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

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

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

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

# ifndef YY_LOCATION_PRINT
#  if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

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

#   define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

#  else
#   define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#  endif
# endif /* !defined YY_LOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yykind < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yykind], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YY_LOCATION_PRINT (yyo, *yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
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
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]));
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
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
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


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
  YYLTYPE *yylloc;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
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
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
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
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
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

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
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
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
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
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
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
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
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
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

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
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
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
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
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

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

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

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
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
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
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
  case 2: /* s: "domodule" module  */
#line 523 "hexpr.y"
                            { yyParsedModule = (yyvsp[0].module);                     }
#line 2944 "hexpr.parse.C"
    break;

  case 3: /* s: "dodefn" id "=" l0expr  */
#line 524 "hexpr.y"
                            { yyParsedVar    = *(yyvsp[-2].string); yyParsedExpr = (yyvsp[0].exp); }
#line 2950 "hexpr.parse.C"
    break;

  case 4: /* s: "dodefn" l0expr  */
#line 525 "hexpr.y"
                            { yyParsedVar    = "";  yyParsedExpr = (yyvsp[0].exp); }
#line 2956 "hexpr.parse.C"
    break;

  case 5: /* s: "doexpr" l0expr  */
#line 526 "hexpr.y"
                            { yyParsedExpr   = (yyvsp[0].exp);                     }
#line 2962 "hexpr.parse.C"
    break;

  case 6: /* module: "option" id module  */
#line 529 "hexpr.y"
                                 { (yyval.module) = (yyvsp[0].module); (yyval.module)->setOption(*(yyvsp[-1].string), m((yylsp[-2]))); }
#line 2968 "hexpr.parse.C"
    break;

  case 7: /* module: "module" id "where" defs  */
#line 530 "hexpr.y"
                                 { (yyval.module) = new Module(*(yyvsp[-2].string), *(yyvsp[0].mdefs)); }
#line 2974 "hexpr.parse.C"
    break;

  case 8: /* module: defs  */
#line 531 "hexpr.y"
                                 { (yyval.module) = new Module(freshName(), *(yyvsp[0].mdefs)); }
#line 2980 "hexpr.parse.C"
    break;

  case 9: /* defs: %empty  */
#line 533 "hexpr.y"
                    { (yyval.mdefs) = autorelease(new ModuleDefs()); }
#line 2986 "hexpr.parse.C"
    break;

  case 10: /* defs: def  */
#line 534 "hexpr.y"
                    { (yyval.mdefs) = autorelease(new ModuleDefs()); (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2992 "hexpr.parse.C"
    break;

  case 11: /* defs: defs def  */
#line 535 "hexpr.y"
                    { (yyval.mdefs) = (yyvsp[-1].mdefs);                            (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2998 "hexpr.parse.C"
    break;

  case 12: /* def: importdef  */
#line 537 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 3004 "hexpr.parse.C"
    break;

  case 13: /* def: tydef  */
#line 538 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 3010 "hexpr.parse.C"
    break;

  case 14: /* def: vartybind  */
#line 539 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mvtydef); }
#line 3016 "hexpr.parse.C"
    break;

  case 15: /* def: classdef  */
#line 540 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 3022 "hexpr.parse.C"
    break;

  case 16: /* def: instdef  */
#line 541 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 3028 "hexpr.parse.C"
    break;

  case 17: /* def: pragmadef  */
#line 542 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 3034 "hexpr.parse.C"
    break;

  case 18: /* def: id "=" l0expr  */
#line 544 "hexpr.y"
                   { (yyval.mdef) = new MVarDef(list(*(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3040 "hexpr.parse.C"
    break;

  case 19: /* def: id id "=" l0expr  */
#line 545 "hexpr.y"
                      { (yyval.mdef) = new MVarDef(list(*(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3046 "hexpr.parse.C"
    break;

  case 20: /* def: id id id "=" l0expr  */
#line 546 "hexpr.y"
                         { (yyval.mdef) = new MVarDef(list(*(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3052 "hexpr.parse.C"
    break;

  case 21: /* def: id id id id "=" l0expr  */
#line 547 "hexpr.y"
                            { (yyval.mdef) = new MVarDef(list(*(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3058 "hexpr.parse.C"
    break;

  case 22: /* def: id id id id id "=" l0expr  */
#line 548 "hexpr.y"
                               { (yyval.mdef) = new MVarDef(list(*(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 3064 "hexpr.parse.C"
    break;

  case 23: /* def: id id id id id id "=" l0expr  */
#line 549 "hexpr.y"
                                  { (yyval.mdef) = new MVarDef(list(*(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3070 "hexpr.parse.C"
    break;

  case 24: /* def: id id id id id id id "=" l0expr  */
#line 550 "hexpr.y"
                                     { (yyval.mdef) = new MVarDef(list(*(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 3076 "hexpr.parse.C"
    break;

  case 25: /* def: id id id id id id id id "=" l0expr  */
#line 551 "hexpr.y"
                                        { (yyval.mdef) = new MVarDef(list(*(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-9]), (yylsp[0]))); }
#line 3082 "hexpr.parse.C"
    break;

  case 26: /* def: id id id id id id id id id "=" l0expr  */
#line 552 "hexpr.y"
                                           { (yyval.mdef) = new MVarDef(list(*(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-10]), (yylsp[0]))); }
#line 3088 "hexpr.parse.C"
    break;

  case 27: /* def: id id id id id id id id id id "=" l0expr  */
#line 553 "hexpr.y"
                                              { (yyval.mdef) = new MVarDef(list(*(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-11]), (yylsp[0]))); }
#line 3094 "hexpr.parse.C"
    break;

  case 28: /* def: id id id id id id id id id id id "=" l0expr  */
#line 554 "hexpr.y"
                                                 { (yyval.mdef) = new MVarDef(list(*(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-12]), (yylsp[0]))); }
#line 3100 "hexpr.parse.C"
    break;

  case 29: /* def: id id id id id id id id id id id id "=" l0expr  */
#line 555 "hexpr.y"
                                                    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-13]), (yylsp[0]))); }
#line 3106 "hexpr.parse.C"
    break;

  case 30: /* def: id id id id id id id id id id id id id "=" l0expr  */
#line 556 "hexpr.y"
                                                       { (yyval.mdef) = new MVarDef(list(*(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-14]), (yylsp[0]))); }
#line 3112 "hexpr.parse.C"
    break;

  case 31: /* def: id id id id id id id id id id id id id id "=" l0expr  */
#line 557 "hexpr.y"
                                                          { (yyval.mdef) = new MVarDef(list(*(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-15]), (yylsp[0]))); }
#line 3118 "hexpr.parse.C"
    break;

  case 32: /* def: id id id id id id id id id id id id id id id "=" l0expr  */
#line 558 "hexpr.y"
                                                             { (yyval.mdef) = new MVarDef(list(*(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-16]), (yylsp[0]))); }
#line 3124 "hexpr.parse.C"
    break;

  case 33: /* def: id id id id id id id id id id id id id id id id "=" l0expr  */
#line 559 "hexpr.y"
                                                                { (yyval.mdef) = new MVarDef(list(*(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-17]), (yylsp[0]))); }
#line 3130 "hexpr.parse.C"
    break;

  case 34: /* def: id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 560 "hexpr.y"
                                                                   { (yyval.mdef) = new MVarDef(list(*(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-18]), (yylsp[0]))); }
#line 3136 "hexpr.parse.C"
    break;

  case 35: /* def: id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 561 "hexpr.y"
                                                                      { (yyval.mdef) = new MVarDef(list(*(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-19]), (yylsp[0]))); }
#line 3142 "hexpr.parse.C"
    break;

  case 36: /* def: id id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 562 "hexpr.y"
                                                                         { (yyval.mdef) = new MVarDef(list(*(yyvsp[-20].string), *(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-20]), (yylsp[0]))); }
#line 3148 "hexpr.parse.C"
    break;

  case 37: /* def: id id id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 563 "hexpr.y"
                                                                            { (yyval.mdef) = new MVarDef(list(*(yyvsp[-21].string), *(yyvsp[-20].string), *(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-21]), (yylsp[0]))); }
#line 3154 "hexpr.parse.C"
    break;

  case 38: /* def: id id id id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 564 "hexpr.y"
                                                                               { (yyval.mdef) = new MVarDef(list(*(yyvsp[-22].string), *(yyvsp[-21].string), *(yyvsp[-20].string), *(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-22]), (yylsp[0]))); }
#line 3160 "hexpr.parse.C"
    break;

  case 39: /* def: l5expr  */
#line 567 "hexpr.y"
            { (yyval.mdef) = new MVarDef(list(freshName()), let(freshName(), ExprPtr((yyvsp[0].exp)), mktunit(m((yylsp[0]))), m((yylsp[0]))), m((yylsp[0]))); }
#line 3166 "hexpr.parse.C"
    break;

  case 40: /* importdef: "import" cppid  */
#line 570 "hexpr.y"
                          { (yyval.mdef) = new MImport(yyModulePath, *(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3172 "hexpr.parse.C"
    break;

  case 41: /* pragmadef: "{-#" pragmaty "#-}"  */
#line 573 "hexpr.y"
                                { (yyval.mdef) = (yyvsp[-1].mdef); }
#line 3178 "hexpr.parse.C"
    break;

  case 42: /* pragmaty: "UNSAFE" id  */
#line 574 "hexpr.y"
                      { (yyval.mdef) = new MUnsafePragmaDef(*(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3184 "hexpr.parse.C"
    break;

  case 43: /* pragmaty: "SAFE" id  */
#line 575 "hexpr.y"
                    { (yyval.mdef) = new MSafePragmaDef(*(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3190 "hexpr.parse.C"
    break;

  case 44: /* tydef: "type" nameseq "=" qtype  */
#line 578 "hexpr.y"
                                { (yyval.mdef) = new MTypeDef(MTypeDef::Transparent, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 3196 "hexpr.parse.C"
    break;

  case 45: /* tydef: "data" nameseq "=" qtype  */
#line 579 "hexpr.y"
                                { (yyval.mdef) = new MTypeDef(MTypeDef::Opaque, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 3202 "hexpr.parse.C"
    break;

  case 46: /* vartybind: name "::" qtype  */
#line 582 "hexpr.y"
                           { (yyval.mvtydef) = new MVarTypeDef(*(yyvsp[-2].string), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]), (yylsp[0]))); }
#line 3208 "hexpr.parse.C"
    break;

  case 47: /* vardef: names "=" l0expr  */
#line 584 "hexpr.y"
                         { (yyval.mvdef) = new MVarDef(*(yyvsp[-2].strings), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3214 "hexpr.parse.C"
    break;

  case 48: /* classdef: "class" cst "=>" id names  */
#line 587 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-3].tconstraints), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3220 "hexpr.parse.C"
    break;

  case 49: /* classdef: "class" cst "=>" id names "|" fundeps  */
#line 588 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-6]), (yylsp[0]))); wantIndent(false); }
#line 3226 "hexpr.parse.C"
    break;

  case 50: /* classdef: "class" cst "=>" id names "where" cmembers  */
#line 589 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 3232 "hexpr.parse.C"
    break;

  case 51: /* classdef: "class" cst "=>" id names "|" fundeps "where" cmembers  */
#line 590 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-7].tconstraints), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-8]), (yylsp[0])));            wantIndent(false); }
#line 3238 "hexpr.parse.C"
    break;

  case 52: /* classdef: "class" id names  */
#line 591 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 3244 "hexpr.parse.C"
    break;

  case 53: /* classdef: "class" id names "|" fundeps  */
#line 592 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3250 "hexpr.parse.C"
    break;

  case 54: /* classdef: "class" id names "where" cmembers  */
#line 593 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-4]), (yylsp[0])));            wantIndent(false); }
#line 3256 "hexpr.parse.C"
    break;

  case 55: /* classdef: "class" id names "|" fundeps "where" cmembers  */
#line 594 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 3262 "hexpr.parse.C"
    break;

  case 56: /* fundeps: fundep  */
#line 596 "hexpr.y"
                            { (yyval.fundeps) = autorelease(new CFunDepDefs()); (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 3268 "hexpr.parse.C"
    break;

  case 57: /* fundeps: fundeps "," fundep  */
#line 597 "hexpr.y"
                            { (yyval.fundeps) = (yyvsp[-2].fundeps);                             (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 3274 "hexpr.parse.C"
    break;

  case 58: /* fundep: idseq "->" idseq  */
#line 599 "hexpr.y"
                         { (yyval.fundep) = autorelease(new CFunDepDef(*(yyvsp[-2].strings), *(yyvsp[0].strings))); }
#line 3280 "hexpr.parse.C"
    break;

  case 59: /* cmembers: cmember  */
#line 601 "hexpr.y"
                           { (yyval.mvtydefs) = autorelease(new MVarTypeDefs()); (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 3286 "hexpr.parse.C"
    break;

  case 60: /* cmembers: cmembers cmember  */
#line 602 "hexpr.y"
                           { (yyval.mvtydefs) = (yyvsp[-1].mvtydefs);                              (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 3292 "hexpr.parse.C"
    break;

  case 61: /* cmember: "indent" vartybind  */
#line 604 "hexpr.y"
                            { (yyval.mvtydef) = (yyvsp[0].mvtydef); }
#line 3298 "hexpr.parse.C"
    break;

  case 62: /* instdef: "instance" id types  */
#line 607 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 3304 "hexpr.parse.C"
    break;

  case 63: /* instdef: "instance" cst "=>" id types  */
#line 608 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(*(yyvsp[-3].tconstraints),           *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3310 "hexpr.parse.C"
    break;

  case 64: /* instdef: "instance" id types "where" imembers  */
#line 609 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-4]), (yylsp[0])));        wantIndent(false); }
#line 3316 "hexpr.parse.C"
    break;

  case 65: /* instdef: "instance" cst "=>" id types "where" imembers  */
#line 610 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(*(yyvsp[-5].tconstraints),           *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-6]), (yylsp[0])));        wantIndent(false); }
#line 3322 "hexpr.parse.C"
    break;

  case 66: /* imembers: imember  */
#line 612 "hexpr.y"
                           { (yyval.mvdefs) = autorelease(new MVarDefs()); (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 3328 "hexpr.parse.C"
    break;

  case 67: /* imembers: imembers imember  */
#line 613 "hexpr.y"
                           { (yyval.mvdefs) = (yyvsp[-1].mvdefs);                          (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 3334 "hexpr.parse.C"
    break;

  case 68: /* imember: "indent" vardef  */
#line 615 "hexpr.y"
                         { (yyval.mvdef) = (yyvsp[0].mvdef); }
#line 3340 "hexpr.parse.C"
    break;

  case 69: /* names: nameseq  */
#line 618 "hexpr.y"
               { (yyval.strings) = (yyvsp[0].strings); }
#line 3346 "hexpr.parse.C"
    break;

  case 70: /* names: id opname id  */
#line 620 "hexpr.y"
                    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[-1].string)); (yyval.strings)->push_back(*(yyvsp[-2].string)); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3352 "hexpr.parse.C"
    break;

  case 71: /* nameseq: name  */
#line 622 "hexpr.y"
                      { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3358 "hexpr.parse.C"
    break;

  case 72: /* nameseq: nameseq name  */
#line 623 "hexpr.y"
                      { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3364 "hexpr.parse.C"
    break;

  case 73: /* name: id  */
#line 625 "hexpr.y"
         { (yyval.string) = (yyvsp[0].string); }
#line 3370 "hexpr.parse.C"
    break;

  case 74: /* name: "(" opname ")"  */
#line 627 "hexpr.y"
                     { (yyval.string) = (yyvsp[-1].string); }
#line 3376 "hexpr.parse.C"
    break;

  case 75: /* opname: "and"  */
#line 629 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("and")); }
#line 3382 "hexpr.parse.C"
    break;

  case 76: /* opname: "or"  */
#line 630 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("or")); }
#line 3388 "hexpr.parse.C"
    break;

  case 77: /* opname: "o"  */
#line 631 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("compose")); }
#line 3394 "hexpr.parse.C"
    break;

  case 78: /* opname: "."  */
#line 632 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("compose")); }
#line 3400 "hexpr.parse.C"
    break;

  case 79: /* opname: "~"  */
#line 633 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("~")); }
#line 3406 "hexpr.parse.C"
    break;

  case 80: /* opname: "=~"  */
#line 634 "hexpr.y"
               { (yyval.string) = autorelease(new std::string("=~")); }
#line 3412 "hexpr.parse.C"
    break;

  case 81: /* opname: "==="  */
#line 635 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("===")); }
#line 3418 "hexpr.parse.C"
    break;

  case 82: /* opname: "=="  */
#line 636 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("==")); }
#line 3424 "hexpr.parse.C"
    break;

  case 83: /* opname: "<"  */
#line 637 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("<")); }
#line 3430 "hexpr.parse.C"
    break;

  case 84: /* opname: "<="  */
#line 638 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("<=")); }
#line 3436 "hexpr.parse.C"
    break;

  case 85: /* opname: ">"  */
#line 639 "hexpr.y"
              { (yyval.string) = autorelease(new std::string(">")); }
#line 3442 "hexpr.parse.C"
    break;

  case 86: /* opname: ">="  */
#line 640 "hexpr.y"
              { (yyval.string) = autorelease(new std::string(">=")); }
#line 3448 "hexpr.parse.C"
    break;

  case 87: /* opname: "in"  */
#line 641 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("in")); }
#line 3454 "hexpr.parse.C"
    break;

  case 88: /* opname: "++"  */
#line 642 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("append")); }
#line 3460 "hexpr.parse.C"
    break;

  case 89: /* opname: "+"  */
#line 643 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("+")); }
#line 3466 "hexpr.parse.C"
    break;

  case 90: /* opname: "-"  */
#line 644 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("-")); }
#line 3472 "hexpr.parse.C"
    break;

  case 91: /* opname: "*"  */
#line 645 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("*")); }
#line 3478 "hexpr.parse.C"
    break;

  case 92: /* opname: "/"  */
#line 646 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("/")); }
#line 3484 "hexpr.parse.C"
    break;

  case 93: /* opname: "%"  */
#line 647 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("%")); }
#line 3490 "hexpr.parse.C"
    break;

  case 94: /* idseq: id  */
#line 649 "hexpr.y"
                { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3496 "hexpr.parse.C"
    break;

  case 95: /* idseq: idseq id  */
#line 650 "hexpr.y"
                { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3502 "hexpr.parse.C"
    break;

  case 96: /* types: l0mtype  */
#line 652 "hexpr.y"
                     { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3508 "hexpr.parse.C"
    break;

  case 97: /* types: types l0mtype  */
#line 653 "hexpr.y"
                     { (yyval.mtypes) = (yyvsp[-1].mtypes);                           (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3514 "hexpr.parse.C"
    break;

  case 98: /* l0expr: "\\" patterns "." l0expr  */
#line 656 "hexpr.y"
                                 { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3520 "hexpr.parse.C"
    break;

  case 99: /* l0expr: "fn" patterns "." l0expr  */
#line 657 "hexpr.y"
                                 { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3526 "hexpr.parse.C"
    break;

  case 100: /* l0expr: lhexpr "<-" lhexpr  */
#line 658 "hexpr.y"
                                 { (yyval.exp) = new Assign(ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3532 "hexpr.parse.C"
    break;

  case 101: /* l0expr: lhexpr  */
#line 659 "hexpr.y"
                                 { (yyval.exp) = (yyvsp[0].exp); }
#line 3538 "hexpr.parse.C"
    break;

  case 102: /* lhexpr: "!" l1expr  */
#line 661 "hexpr.y"
                                 { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), (yyvsp[0].exp), m((yylsp[-1]),(yylsp[0]))); }
#line 3544 "hexpr.parse.C"
    break;

  case 103: /* lhexpr: lhexpr "and" lhexpr  */
#line 662 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("and",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3550 "hexpr.parse.C"
    break;

  case 104: /* lhexpr: lhexpr "or" lhexpr  */
#line 663 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("or",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3556 "hexpr.parse.C"
    break;

  case 105: /* lhexpr: lhexpr "o" lhexpr  */
#line 664 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("compose",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3562 "hexpr.parse.C"
    break;

  case 106: /* lhexpr: l1expr "in" l1expr  */
#line 665 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("in",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3568 "hexpr.parse.C"
    break;

  case 107: /* lhexpr: l1expr  */
#line 666 "hexpr.y"
                                 { (yyval.exp) = (yyvsp[0].exp); }
#line 3574 "hexpr.parse.C"
    break;

  case 108: /* l1expr: "if" l0expr "then" l0expr "else" l0expr  */
#line 668 "hexpr.y"
                                                { (yyval.exp) = TAPP3(var("if",m((yylsp[-5]))), (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-5]), (yylsp[0]))); }
#line 3580 "hexpr.parse.C"
    break;

  case 109: /* l1expr: l2expr  */
#line 669 "hexpr.y"
                                                { (yyval.exp) = (yyvsp[0].exp); }
#line 3586 "hexpr.parse.C"
    break;

  case 110: /* l2expr: l2expr "~" l2expr  */
#line 671 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("~",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3592 "hexpr.parse.C"
    break;

  case 111: /* l2expr: l2expr "===" l2expr  */
#line 672 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("===",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3598 "hexpr.parse.C"
    break;

  case 112: /* l2expr: l2expr "==" l2expr  */
#line 673 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3604 "hexpr.parse.C"
    break;

  case 113: /* l2expr: l2expr "!=" l2expr  */
#line 674 "hexpr.y"
                            { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))), m((yylsp[-2]),(yylsp[0]))); }
#line 3610 "hexpr.parse.C"
    break;

  case 114: /* l2expr: l2expr "<" l2expr  */
#line 675 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("<",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3616 "hexpr.parse.C"
    break;

  case 115: /* l2expr: l2expr "<=" l2expr  */
#line 676 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("<=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3622 "hexpr.parse.C"
    break;

  case 116: /* l2expr: l2expr ">" l2expr  */
#line 677 "hexpr.y"
                            { (yyval.exp) = TAPP2(var(">",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3628 "hexpr.parse.C"
    break;

  case 117: /* l2expr: l2expr ">=" l2expr  */
#line 678 "hexpr.y"
                            { (yyval.exp) = TAPP2(var(">=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3634 "hexpr.parse.C"
    break;

  case 118: /* l2expr: l3expr  */
#line 679 "hexpr.y"
                            { (yyval.exp) = (yyvsp[0].exp); }
#line 3640 "hexpr.parse.C"
    break;

  case 119: /* l3expr: l3expr "+" l3expr  */
#line 681 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("+",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3646 "hexpr.parse.C"
    break;

  case 120: /* l3expr: l3expr "-" l3expr  */
#line 682 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("-",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3652 "hexpr.parse.C"
    break;

  case 121: /* l3expr: l3expr "++" l3expr  */
#line 683 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("append",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3658 "hexpr.parse.C"
    break;

  case 122: /* l3expr: "-" l3expr  */
#line 684 "hexpr.y"
                           { (yyval.exp) = TAPP1(var("neg",m((yylsp[-1]))), ExprPtr((yyvsp[0].exp)), m((yylsp[-1]),(yylsp[0]))); }
#line 3664 "hexpr.parse.C"
    break;

  case 123: /* l3expr: l4expr  */
#line 685 "hexpr.y"
                           { (yyval.exp) = (yyvsp[0].exp); }
#line 3670 "hexpr.parse.C"
    break;

  case 124: /* l4expr: l4expr "*" l4expr  */
#line 687 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("*", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3676 "hexpr.parse.C"
    break;

  case 125: /* l4expr: l4expr "/" l4expr  */
#line 688 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("/", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3682 "hexpr.parse.C"
    break;

  case 126: /* l4expr: l4expr "%" l4expr  */
#line 689 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("%", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3688 "hexpr.parse.C"
    break;

  case 127: /* l4expr: l5expr  */
#line 690 "hexpr.y"
                          { (yyval.exp) = (yyvsp[0].exp); }
#line 3694 "hexpr.parse.C"
    break;

  case 128: /* l5expr: l6expr  */
#line 692 "hexpr.y"
               { (yyval.exp) = (yyvsp[0].exp); }
#line 3700 "hexpr.parse.C"
    break;

  case 129: /* l5expr: "let" letbindings "in" l0expr  */
#line 695 "hexpr.y"
                                      { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-2].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3706 "hexpr.parse.C"
    break;

  case 130: /* l5expr: "let" letbindings ";" "in" l0expr  */
#line 696 "hexpr.y"
                                          { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]),(yylsp[0])))->clone(); }
#line 3712 "hexpr.parse.C"
    break;

  case 131: /* l5expr: "match" l6exprs "with" patternexps  */
#line 699 "hexpr.y"
                                           { (yyval.exp) = compileMatch(yyParseCC, *(yyvsp[-2].exps), normPatternRules(*(yyvsp[0].patternexps), m((yylsp[-3]),(yylsp[0]))), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3718 "hexpr.parse.C"
    break;

  case 132: /* l5expr: l6expr "matches" pattern  */
#line 702 "hexpr.y"
                                 { (yyval.exp) = compileMatchTest(yyParseCC, ExprPtr((yyvsp[-2].exp)), PatternPtr((yyvsp[0].pattern)), m((yylsp[-2]),(yylsp[0])))->clone(); }
#line 3724 "hexpr.parse.C"
    break;

  case 133: /* l5expr: "parse" "{" prules "}"  */
#line 705 "hexpr.y"
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
#line 3739 "hexpr.parse.C"
    break;

  case 134: /* l5expr: "do" "{" dobindings "}"  */
#line 717 "hexpr.y"
                                                { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-1].letbindings), ExprPtr(new Unit(m((yylsp[-3]),(yylsp[0])))), m((yylsp[-3]),(yylsp[0]))); }
#line 3745 "hexpr.parse.C"
    break;

  case 135: /* l5expr: "do" "{" dobindings "return" l0expr "}"  */
#line 718 "hexpr.y"
                                                { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[-1].exp)), m((yylsp[-5]),(yylsp[0]))); }
#line 3751 "hexpr.parse.C"
    break;

  case 136: /* l5expr: l6expr "::" qtype  */
#line 721 "hexpr.y"
                                { (yyval.exp) = new Assump(ExprPtr((yyvsp[-2].exp)), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]),(yylsp[0]))); }
#line 3757 "hexpr.parse.C"
    break;

  case 137: /* letbindings: letbindings ";" letbinding  */
#line 723 "hexpr.y"
                                        { (yyvsp[-2].letbindings)->push_back(*(yyvsp[0].letbinding)); (yyval.letbindings) = (yyvsp[-2].letbindings); }
#line 3763 "hexpr.parse.C"
    break;

  case 138: /* letbindings: letbinding  */
#line 724 "hexpr.y"
                                        { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3769 "hexpr.parse.C"
    break;

  case 139: /* letbinding: irrefutablep "=" l1expr  */
#line 726 "hexpr.y"
                                    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-2].pattern)), ExprPtr((yyvsp[0].exp)))); }
#line 3775 "hexpr.parse.C"
    break;

  case 140: /* dobindings: dobindings dobinding  */
#line 728 "hexpr.y"
                                 { (yyval.letbindings) = (yyvsp[-1].letbindings); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3781 "hexpr.parse.C"
    break;

  case 141: /* dobindings: dobinding  */
#line 729 "hexpr.y"
                                 { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3787 "hexpr.parse.C"
    break;

  case 142: /* dobinding: irrefutablep "=" l0expr ";"  */
#line 731 "hexpr.y"
                                       { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)))); }
#line 3793 "hexpr.parse.C"
    break;

  case 143: /* dobinding: l0expr ";"  */
#line 732 "hexpr.y"
                                       { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr(new MatchAny("_",m((yylsp[-1])))), ExprPtr((yyvsp[-1].exp)))); }
#line 3799 "hexpr.parse.C"
    break;

  case 144: /* cselconds: cselconds "," lhexpr  */
#line 734 "hexpr.y"
                                { (yyval.exps) = (yyvsp[-2].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3805 "hexpr.parse.C"
    break;

  case 145: /* cselconds: lhexpr  */
#line 735 "hexpr.y"
                                { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3811 "hexpr.parse.C"
    break;

  case 146: /* cselection: pattern "<-" l0expr "," cselconds  */
#line 737 "hexpr.y"
                                              { (yyval.cselection) = new CSelection(); (yyval.cselection)->pat = PatternPtr((yyvsp[-4].pattern)); (yyval.cselection)->seq = ExprPtr((yyvsp[-2].exp)); (yyval.cselection)->conds = *(yyvsp[0].exps); }
#line 3817 "hexpr.parse.C"
    break;

  case 147: /* cselection: pattern "<-" l0expr  */
#line 738 "hexpr.y"
                                              { (yyval.cselection) = new CSelection(); (yyval.cselection)->pat = PatternPtr((yyvsp[-2].pattern)); (yyval.cselection)->seq = ExprPtr((yyvsp[0].exp)); }
#line 3823 "hexpr.parse.C"
    break;

  case 148: /* cselections: cselections "|" cselection  */
#line 740 "hexpr.y"
                                        { (yyval.cselections) = (yyvsp[-2].cselections); (yyval.cselections)->push_back(CSelectionPtr((yyvsp[0].cselection))); }
#line 3829 "hexpr.parse.C"
    break;

  case 149: /* cselections: cselection  */
#line 741 "hexpr.y"
                                        { (yyval.cselections) = autorelease(new CSelections()); (yyval.cselections)->push_back(CSelectionPtr((yyvsp[0].cselection))); }
#line 3835 "hexpr.parse.C"
    break;

  case 150: /* l6expr: l6expr "(" cargs ")"  */
#line 744 "hexpr.y"
                                { (yyval.exp) = new App(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].exps), m((yylsp[-3]), (yylsp[0]))); }
#line 3841 "hexpr.parse.C"
    break;

  case 151: /* l6expr: id  */
#line 745 "hexpr.y"
                                { (yyval.exp) = varCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3847 "hexpr.parse.C"
    break;

  case 152: /* l6expr: "[" l0expr ".." l0expr "]"  */
#line 748 "hexpr.y"
                                                          { (yyval.exp) = new App(var("range", m((yylsp[-2]))), list(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]), (yylsp[0]))); }
#line 3853 "hexpr.parse.C"
    break;

  case 153: /* l6expr: "[" l0expr ".." "]"  */
#line 749 "hexpr.y"
                                                          { (yyval.exp) = new App(var("iterateS", m((yylsp[-1]))), list(ExprPtr((yyvsp[-2].exp)), fn(str::strings(".x"), fncall(var("+", m((yylsp[-1]))), list(var(".x", m((yylsp[-1]))), ExprPtr(new Int(1, m((yylsp[-1]))))), m((yylsp[-1]))), m((yylsp[-1])))), m((yylsp[-3]), (yylsp[0]))); }
#line 3859 "hexpr.parse.C"
    break;

  case 154: /* l6expr: "[" l0expr "|" cselections "]"  */
#line 750 "hexpr.y"
                                                          { (yyval.exp) = desugarComprehension(yyParseCC, ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].cselections), m((yylsp[-4]), (yylsp[0]))); }
#line 3865 "hexpr.parse.C"
    break;

  case 155: /* l6expr: "[" cargs "]"  */
#line 751 "hexpr.y"
                                                          { (yyval.exp) = new MkArray(*(yyvsp[-1].exps), m((yylsp[-2]), (yylsp[0]))); }
#line 3871 "hexpr.parse.C"
    break;

  case 156: /* l6expr: l6expr "[" "timeV" "]"  */
#line 752 "hexpr.y"
                                                          { (yyval.exp) = maybeArraySliceWithTime(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].string), m((yylsp[-3]), (yylsp[0]))); }
#line 3877 "hexpr.parse.C"
    break;

  case 157: /* l6expr: l6expr "[" l0expr "]"  */
#line 753 "hexpr.y"
                                                          { (yyval.exp) = mkAIndex(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3883 "hexpr.parse.C"
    break;

  case 158: /* l6expr: l6expr "[" l0expr ":" l0expr "]"  */
#line 754 "hexpr.y"
                                                          { (yyval.exp) = new App(var("slice", m((yylsp[-2]))), list(ExprPtr((yyvsp[-5].exp)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-5]), (yylsp[0]))); }
#line 3889 "hexpr.parse.C"
    break;

  case 159: /* l6expr: l6expr "[" l0expr ":" "]"  */
#line 755 "hexpr.y"
                                                          { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-1]))), list(var(vn,m((yylsp[-4]))), ExprPtr((yyvsp[-2].exp)), fncall(var("size",m((yylsp[-1]))), list(var(vn,m((yylsp[-4])))),m((yylsp[-4])))),m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3895 "hexpr.parse.C"
    break;

  case 160: /* l6expr: l6expr "[" ":" l0expr "]"  */
#line 756 "hexpr.y"
                                                          { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-2]))), list(var(vn,m((yylsp[-4]))), fncall(var("size",m((yylsp[-2]))), list(var(vn,m((yylsp[-2])))),m((yylsp[-4]))), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3901 "hexpr.parse.C"
    break;

  case 161: /* l6expr: "|" id "=" l0expr "|"  */
#line 759 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(*(yyvsp[-3].string), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3907 "hexpr.parse.C"
    break;

  case 162: /* l6expr: "|" "intV" "=" l0expr "|"  */
#line 760 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(".f" + str::from((yyvsp[-3].intv)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3913 "hexpr.parse.C"
    break;

  case 163: /* l6expr: "|" id "|"  */
#line 761 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(*(yyvsp[-1].string), ExprPtr(new Unit(m((yylsp[-1])))), m((yylsp[-2]), (yylsp[0]))); }
#line 3919 "hexpr.parse.C"
    break;

  case 164: /* l6expr: "case" l0expr "of" "|" varfields "|"  */
#line 762 "hexpr.y"
                                                              { (yyval.exp) = new Case(ExprPtr((yyvsp[-4].exp)), *(yyvsp[-1].vfields), m((yylsp[-5]), (yylsp[0]))); }
#line 3925 "hexpr.parse.C"
    break;

  case 165: /* l6expr: "case" l0expr "of" "|" varfields "|" "default" l0expr  */
#line 763 "hexpr.y"
                                                              { (yyval.exp) = new Case(ExprPtr((yyvsp[-6].exp)), *(yyvsp[-3].vfields), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3931 "hexpr.parse.C"
    break;

  case 166: /* l6expr: "{" recfields "}"  */
#line 766 "hexpr.y"
                              { if ((yyvsp[-1].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-1].rfields), m((yylsp[-2]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-2]), (yylsp[0]))); } }
#line 3937 "hexpr.parse.C"
    break;

  case 167: /* l6expr: "{" recfields "," "}"  */
#line 767 "hexpr.y"
                              { if ((yyvsp[-2].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-2].rfields), m((yylsp[-3]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-3]), (yylsp[0]))); } }
#line 3943 "hexpr.parse.C"
    break;

  case 168: /* l6expr: l6expr recfieldpath  */
#line 768 "hexpr.y"
                              { (yyval.exp) = makeProjSeq((yyvsp[-1].exp), *(yyvsp[0].strings), m((yylsp[-1]), (yylsp[0]))); }
#line 3949 "hexpr.parse.C"
    break;

  case 169: /* l6expr: recfieldpath  */
#line 771 "hexpr.y"
                     { (yyval.exp) = new Fn(str::strings("x"), proj(var("x", m((yylsp[0]))), *(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3955 "hexpr.parse.C"
    break;

  case 170: /* l6expr: "regexV"  */
#line 774 "hexpr.y"
                 { (yyval.exp) = compileRegexFn(yyParseCC, std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0])))->clone(); }
#line 3961 "hexpr.parse.C"
    break;

  case 171: /* l6expr: "pack" l6expr  */
#line 777 "hexpr.y"
                                           { (yyval.exp) = new Pack(ExprPtr((yyvsp[0].exp)), m((yylsp[-1]), (yylsp[0]))); }
#line 3967 "hexpr.parse.C"
    break;

  case 172: /* l6expr: "unpack" id "=" l6expr "in" l6expr  */
#line 778 "hexpr.y"
                                           { (yyval.exp) = new Unpack(*(yyvsp[-4].string), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3973 "hexpr.parse.C"
    break;

  case 173: /* l6expr: "boolV"  */
#line 781 "hexpr.y"
                    { (yyval.exp) = new Bool((yyvsp[0].boolv), m((yylsp[0]))); }
#line 3979 "hexpr.parse.C"
    break;

  case 174: /* l6expr: "charV"  */
#line 782 "hexpr.y"
                    { (yyval.exp) = new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3985 "hexpr.parse.C"
    break;

  case 175: /* l6expr: "byteV"  */
#line 783 "hexpr.y"
                    { (yyval.exp) = new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3991 "hexpr.parse.C"
    break;

  case 176: /* l6expr: "bytesV"  */
#line 784 "hexpr.y"
                    { (yyval.exp) = mkarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3997 "hexpr.parse.C"
    break;

  case 177: /* l6expr: "shortV"  */
#line 785 "hexpr.y"
                    { (yyval.exp) = new Short((yyvsp[0].shortv), m((yylsp[0]))); }
#line 4003 "hexpr.parse.C"
    break;

  case 178: /* l6expr: "intV"  */
#line 786 "hexpr.y"
                    { (yyval.exp) = new Int((yyvsp[0].intv), m((yylsp[0]))); }
#line 4009 "hexpr.parse.C"
    break;

  case 179: /* l6expr: "longV"  */
#line 787 "hexpr.y"
                    { (yyval.exp) = new Long((yyvsp[0].longv), m((yylsp[0]))); }
#line 4015 "hexpr.parse.C"
    break;

  case 180: /* l6expr: "int128V"  */
#line 788 "hexpr.y"
                    { (yyval.exp) = new Int128((yyvsp[0].int128v), m((yylsp[0]))); }
#line 4021 "hexpr.parse.C"
    break;

  case 181: /* l6expr: "floatV"  */
#line 789 "hexpr.y"
                    { (yyval.exp) = new Float((yyvsp[0].floatv), m((yylsp[0]))); }
#line 4027 "hexpr.parse.C"
    break;

  case 182: /* l6expr: "doubleV"  */
#line 790 "hexpr.y"
                    { (yyval.exp) = new Double((yyvsp[0].doublev), m((yylsp[0]))); }
#line 4033 "hexpr.parse.C"
    break;

  case 183: /* l6expr: "stringV"  */
#line 791 "hexpr.y"
                    { (yyval.exp) = mkarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4039 "hexpr.parse.C"
    break;

  case 184: /* l6expr: tsseq  */
#line 792 "hexpr.y"
                    { (yyval.exp) = mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0])))->clone(); }
#line 4045 "hexpr.parse.C"
    break;

  case 185: /* l6expr: "timeV"  */
#line 793 "hexpr.y"
                    { (yyval.exp) = mkTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 4051 "hexpr.parse.C"
    break;

  case 186: /* l6expr: "dateTimeV"  */
#line 794 "hexpr.y"
                    { (yyval.exp) = mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 4057 "hexpr.parse.C"
    break;

  case 187: /* l6expr: "(" cargs ")"  */
#line 797 "hexpr.y"
                      { (yyval.exp) = pickNestedExp((yyvsp[-1].exps), m((yylsp[-2]),(yylsp[0]))); }
#line 4063 "hexpr.parse.C"
    break;

  case 188: /* l6expr: "(" "++" ")"  */
#line 800 "hexpr.y"
                      { (yyval.exp) = new Var("append", m((yylsp[-1]))); }
#line 4069 "hexpr.parse.C"
    break;

  case 189: /* l6expr: "(" "+" ")"  */
#line 801 "hexpr.y"
                      { (yyval.exp) = new Var("+",      m((yylsp[-1]))); }
#line 4075 "hexpr.parse.C"
    break;

  case 190: /* l6expr: "(" "-" ")"  */
#line 802 "hexpr.y"
                      { (yyval.exp) = new Var("-",      m((yylsp[-1]))); }
#line 4081 "hexpr.parse.C"
    break;

  case 191: /* l6expr: "(" "*" ")"  */
#line 803 "hexpr.y"
                      { (yyval.exp) = new Var("*",      m((yylsp[-1]))); }
#line 4087 "hexpr.parse.C"
    break;

  case 192: /* l6expr: "(" "/" ")"  */
#line 804 "hexpr.y"
                      { (yyval.exp) = new Var("/",      m((yylsp[-1]))); }
#line 4093 "hexpr.parse.C"
    break;

  case 193: /* l6expr: "(" "%" ")"  */
#line 805 "hexpr.y"
                      { (yyval.exp) = new Var("%",      m((yylsp[-1]))); }
#line 4099 "hexpr.parse.C"
    break;

  case 194: /* l6expr: "(" "~" ")"  */
#line 806 "hexpr.y"
                      { (yyval.exp) = new Var("~",      m((yylsp[-1]))); }
#line 4105 "hexpr.parse.C"
    break;

  case 195: /* l6expr: "(" "===" ")"  */
#line 807 "hexpr.y"
                      { (yyval.exp) = new Var("===",    m((yylsp[-1]))); }
#line 4111 "hexpr.parse.C"
    break;

  case 196: /* l6expr: "(" "==" ")"  */
#line 808 "hexpr.y"
                      { (yyval.exp) = new Var("==",     m((yylsp[-1]))); }
#line 4117 "hexpr.parse.C"
    break;

  case 197: /* l6expr: "(" "!=" ")"  */
#line 809 "hexpr.y"
                      { (yyval.exp) = new Var("!=",     m((yylsp[-1]))); }
#line 4123 "hexpr.parse.C"
    break;

  case 198: /* l6expr: "(" "<" ")"  */
#line 810 "hexpr.y"
                      { (yyval.exp) = new Var("<",      m((yylsp[-1]))); }
#line 4129 "hexpr.parse.C"
    break;

  case 199: /* l6expr: "(" ">" ")"  */
#line 811 "hexpr.y"
                      { (yyval.exp) = new Var(">",      m((yylsp[-1]))); }
#line 4135 "hexpr.parse.C"
    break;

  case 200: /* l6expr: "(" ">=" ")"  */
#line 812 "hexpr.y"
                      { (yyval.exp) = new Var(">=",     m((yylsp[-1]))); }
#line 4141 "hexpr.parse.C"
    break;

  case 201: /* l6expr: "(" "<=" ")"  */
#line 813 "hexpr.y"
                      { (yyval.exp) = new Var("<=",     m((yylsp[-1]))); }
#line 4147 "hexpr.parse.C"
    break;

  case 202: /* l6expr: "(" "and" ")"  */
#line 814 "hexpr.y"
                      { (yyval.exp) = new Var("and",    m((yylsp[-1]))); }
#line 4153 "hexpr.parse.C"
    break;

  case 203: /* l6expr: "(" "or" ")"  */
#line 815 "hexpr.y"
                      { (yyval.exp) = new Var("or",     m((yylsp[-1]))); }
#line 4159 "hexpr.parse.C"
    break;

  case 204: /* l6expr: "(" "in" ")"  */
#line 816 "hexpr.y"
                      { (yyval.exp) = new Var("in",     m((yylsp[-1]))); }
#line 4165 "hexpr.parse.C"
    break;

  case 205: /* l6expr: "(" "!" ")"  */
#line 817 "hexpr.y"
                      { (yyval.exp) = new Var("not",    m((yylsp[-1]))); }
#line 4171 "hexpr.parse.C"
    break;

  case 206: /* l6expr: "`" l0expr "`"  */
#line 820 "hexpr.y"
                       { (yyval.exp) = new Assump(fncall(var("unsafeCast", m((yylsp[-1]))), list(mktunit(m((yylsp[-1])))), m((yylsp[-1]))), qualtype(tapp(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp)))))), m((yylsp[-1]))); }
#line 4177 "hexpr.parse.C"
    break;

  case 207: /* prules: prules prule  */
#line 822 "hexpr.y"
                     { (yyval.prules) = (yyvsp[-1].prules); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 4183 "hexpr.parse.C"
    break;

  case 208: /* prules: prule  */
#line 823 "hexpr.y"
                     { (yyval.prules) = autorelease(new Grammar()); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 4189 "hexpr.parse.C"
    break;

  case 209: /* prule: id ":=" prdefs  */
#line 825 "hexpr.y"
                      { (yyval.prule) = autorelease(new Grammar::value_type(*(yyvsp[-2].string), *(yyvsp[0].prdefs))); }
#line 4195 "hexpr.parse.C"
    break;

  case 210: /* prdefs: prdefs "|" prdef  */
#line 827 "hexpr.y"
                         { (yyval.prdefs) = (yyvsp[-2].prdefs); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 4201 "hexpr.parse.C"
    break;

  case 211: /* prdefs: prdef  */
#line 828 "hexpr.y"
                         { (yyval.prdefs) = autorelease(new GrammarRules()); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 4207 "hexpr.parse.C"
    break;

  case 212: /* prdef: pbelems "{" l0expr "}"  */
#line 830 "hexpr.y"
                              { (yyval.prdef) = autorelease(new GrammarRule(*(yyvsp[-3].pbelems), ExprPtr((yyvsp[-1].exp)))); }
#line 4213 "hexpr.parse.C"
    break;

  case 213: /* pbelems: pbelems pbelem  */
#line 832 "hexpr.y"
                        { (yyval.pbelems) = (yyvsp[-1].pbelems); (yyval.pbelems)->push_back(*(yyvsp[0].pbelem)); }
#line 4219 "hexpr.parse.C"
    break;

  case 214: /* pbelems: %empty  */
#line 833 "hexpr.y"
                        { (yyval.pbelems) = autorelease(new BoundGrammarValues()); }
#line 4225 "hexpr.parse.C"
    break;

  case 215: /* pbelem: id ":" pvalue  */
#line 835 "hexpr.y"
                      { (yyval.pbelem) = autorelease(new BoundGrammarValue(*(yyvsp[-2].string), GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 4231 "hexpr.parse.C"
    break;

  case 216: /* pbelem: pvalue  */
#line 836 "hexpr.y"
                      { (yyval.pbelem) = autorelease(new BoundGrammarValue("_", GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 4237 "hexpr.parse.C"
    break;

  case 217: /* pvalue: id  */
#line 838 "hexpr.y"
                      { (yyval.pvalue) = new GSymRef(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4243 "hexpr.parse.C"
    break;

  case 218: /* pvalue: "stringV"  */
#line 839 "hexpr.y"
                      { (yyval.pvalue) = new GStr(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4249 "hexpr.parse.C"
    break;

  case 219: /* pvalue: "charV"  */
#line 840 "hexpr.y"
                      { (yyval.pvalue) = new GStr(std::string(1, str::readCharDef(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4255 "hexpr.parse.C"
    break;

  case 220: /* tsseq: "timespanV"  */
#line 842 "hexpr.y"
                         { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4261 "hexpr.parse.C"
    break;

  case 221: /* tsseq: tsseq "timespanV"  */
#line 843 "hexpr.y"
                         { (yyval.strings) = (yyvsp[-1].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4267 "hexpr.parse.C"
    break;

  case 222: /* l6exprs: l6exprs l6expr  */
#line 845 "hexpr.y"
                        { (yyval.exps) = (yyvsp[-1].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4273 "hexpr.parse.C"
    break;

  case 223: /* l6exprs: l6expr  */
#line 846 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4279 "hexpr.parse.C"
    break;

  case 224: /* patternexps: patternexps patternexp  */
#line 848 "hexpr.y"
                                    { (yyval.patternexps) = (yyvsp[-1].patternexps); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 4285 "hexpr.parse.C"
    break;

  case 225: /* patternexps: patternexp  */
#line 849 "hexpr.y"
                                    { (yyval.patternexps) = autorelease(new PatternRows()); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 4291 "hexpr.parse.C"
    break;

  case 226: /* patternexp: "|" patterns "->" l0expr  */
#line 851 "hexpr.y"
                                                    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)))); }
#line 4297 "hexpr.parse.C"
    break;

  case 227: /* patternexp: "|" patterns "where" l0expr "->" l0expr  */
#line 852 "hexpr.y"
                                                    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-4].patterns), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)))); }
#line 4303 "hexpr.parse.C"
    break;

  case 228: /* patterns: patterns pattern  */
#line 855 "hexpr.y"
                           { (yyval.patterns) = (yyvsp[-1].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4309 "hexpr.parse.C"
    break;

  case 229: /* patterns: pattern  */
#line 856 "hexpr.y"
                           { (yyval.patterns) = autorelease(new Patterns()); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4315 "hexpr.parse.C"
    break;

  case 230: /* refutablep: "boolV"  */
#line 858 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Bool((yyvsp[0].boolv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4321 "hexpr.parse.C"
    break;

  case 231: /* refutablep: "charV"  */
#line 859 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 4327 "hexpr.parse.C"
    break;

  case 232: /* refutablep: "byteV"  */
#line 860 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 4333 "hexpr.parse.C"
    break;

  case 233: /* refutablep: "shortV"  */
#line 861 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Short((yyvsp[0].shortv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4339 "hexpr.parse.C"
    break;

  case 234: /* refutablep: "intV"  */
#line 862 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int((yyvsp[0].intv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4345 "hexpr.parse.C"
    break;

  case 235: /* refutablep: "longV"  */
#line 863 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Long((yyvsp[0].longv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4351 "hexpr.parse.C"
    break;

  case 236: /* refutablep: "int128V"  */
#line 864 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int128((yyvsp[0].int128v), m((yylsp[0])))), m((yylsp[0]))); }
#line 4357 "hexpr.parse.C"
    break;

  case 237: /* refutablep: "doubleV"  */
#line 865 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Double((yyvsp[0].doublev), m((yylsp[0])))), m((yylsp[0]))); }
#line 4363 "hexpr.parse.C"
    break;

  case 238: /* refutablep: "bytesV"  */
#line 866 "hexpr.y"
                                       { (yyval.pattern) = mkpatarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 4369 "hexpr.parse.C"
    break;

  case 239: /* refutablep: "stringV"  */
#line 867 "hexpr.y"
                                       { (yyval.pattern) = mkpatarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4375 "hexpr.parse.C"
    break;

  case 240: /* refutablep: tsseq  */
#line 868 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkTimespanPrim(*(yyvsp[0].strings), m((yylsp[0]))), mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 4381 "hexpr.parse.C"
    break;

  case 241: /* refutablep: "timeV"  */
#line 869 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4387 "hexpr.parse.C"
    break;

  case 242: /* refutablep: "dateTimeV"  */
#line 870 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkDateTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4393 "hexpr.parse.C"
    break;

  case 243: /* refutablep: "regexV"  */
#line 871 "hexpr.y"
                                       { (yyval.pattern) = new MatchRegex(std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0]))); }
#line 4399 "hexpr.parse.C"
    break;

  case 244: /* refutablep: "[" patternseq "]"  */
#line 872 "hexpr.y"
                                       { (yyval.pattern) = new MatchArray(*(yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4405 "hexpr.parse.C"
    break;

  case 245: /* refutablep: "[" patternseq "," "]"  */
#line 873 "hexpr.y"
                                       { (yyval.pattern) = new MatchArray(*(yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4411 "hexpr.parse.C"
    break;

  case 246: /* refutablep: "|" id "|"  */
#line 874 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(*(yyvsp[-1].string), PatternPtr(new MatchLiteral(PrimitivePtr(new Unit(m((yylsp[-1])))), m((yylsp[-1])))), m((yylsp[-2]),(yylsp[0]))); }
#line 4417 "hexpr.parse.C"
    break;

  case 247: /* refutablep: "|" id "=" pattern "|"  */
#line 875 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(*(yyvsp[-3].string), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4423 "hexpr.parse.C"
    break;

  case 248: /* refutablep: "|" "intV" "=" pattern "|"  */
#line 876 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(".f" + str::from((yyvsp[-3].intv)), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4429 "hexpr.parse.C"
    break;

  case 249: /* refutablep: "(" patternseq ")"  */
#line 877 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4435 "hexpr.parse.C"
    break;

  case 250: /* refutablep: "(" patternseq "," ")"  */
#line 878 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4441 "hexpr.parse.C"
    break;

  case 251: /* refutablep: "{" recpatfields "}"  */
#line 879 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4447 "hexpr.parse.C"
    break;

  case 252: /* refutablep: "{" recpatfields "," "}"  */
#line 880 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4453 "hexpr.parse.C"
    break;

  case 253: /* refutablep: id  */
#line 881 "hexpr.y"
                                       { (yyval.pattern) = patVarCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4459 "hexpr.parse.C"
    break;

  case 254: /* irrefutablep: id  */
#line 883 "hexpr.y"
                                       { (yyval.pattern) = new MatchAny(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4465 "hexpr.parse.C"
    break;

  case 255: /* irrefutablep: "(" patternseq ")"  */
#line 884 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4471 "hexpr.parse.C"
    break;

  case 256: /* irrefutablep: "(" patternseq "," ")"  */
#line 885 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4477 "hexpr.parse.C"
    break;

  case 257: /* irrefutablep: "{" recpatfields "}"  */
#line 886 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4483 "hexpr.parse.C"
    break;

  case 258: /* irrefutablep: "{" recpatfields "," "}"  */
#line 887 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4489 "hexpr.parse.C"
    break;

  case 259: /* pattern: refutablep  */
#line 889 "hexpr.y"
                    { (yyval.pattern) = (yyvsp[0].pattern); }
#line 4495 "hexpr.parse.C"
    break;

  case 260: /* patternseq: patternseqn  */
#line 891 "hexpr.y"
                          { (yyval.patterns) = (yyvsp[0].patterns); }
#line 4501 "hexpr.parse.C"
    break;

  case 261: /* patternseq: %empty  */
#line 892 "hexpr.y"
                          { (yyval.patterns) = new Patterns(); }
#line 4507 "hexpr.parse.C"
    break;

  case 262: /* patternseqn: patternseqn "," pattern  */
#line 894 "hexpr.y"
                                     { (yyval.patterns) = (yyvsp[-2].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4513 "hexpr.parse.C"
    break;

  case 263: /* patternseqn: pattern  */
#line 895 "hexpr.y"
                                     { (yyval.patterns) = new Patterns(); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4519 "hexpr.parse.C"
    break;

  case 264: /* recpatfields: recpatfields "," recpatfield  */
#line 897 "hexpr.y"
                                           { (yyval.recpatfields) = (yyvsp[-2].recpatfields); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4525 "hexpr.parse.C"
    break;

  case 265: /* recpatfields: recpatfield  */
#line 898 "hexpr.y"
                                           { (yyval.recpatfields) = new MatchRecord::Fields(); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4531 "hexpr.parse.C"
    break;

  case 266: /* recpatfield: id "=" pattern  */
#line 900 "hexpr.y"
                            { (yyval.recpatfield) = new MatchRecord::Field(*(yyvsp[-2].string), PatternPtr((yyvsp[0].pattern))); }
#line 4537 "hexpr.parse.C"
    break;

  case 267: /* recfields: %empty  */
#line 902 "hexpr.y"
                                                 { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); }
#line 4543 "hexpr.parse.C"
    break;

  case 268: /* recfields: recfieldname "=" l0expr  */
#line 903 "hexpr.y"
                                                 { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4549 "hexpr.parse.C"
    break;

  case 269: /* recfields: recfields "," recfieldname "=" l0expr  */
#line 904 "hexpr.y"
                                                 { (yyval.rfields) = (yyvsp[-4].rfields);                                     (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4555 "hexpr.parse.C"
    break;

  case 270: /* recfieldname: id  */
#line 906 "hexpr.y"
                         { (yyval.string) = (yyvsp[0].string); }
#line 4561 "hexpr.parse.C"
    break;

  case 271: /* recfieldname: "data"  */
#line 907 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("data")); }
#line 4567 "hexpr.parse.C"
    break;

  case 272: /* recfieldname: "type"  */
#line 908 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("type")); }
#line 4573 "hexpr.parse.C"
    break;

  case 273: /* recfieldname: "where"  */
#line 909 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("where")); }
#line 4579 "hexpr.parse.C"
    break;

  case 274: /* recfieldname: "class"  */
#line 910 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("class")); wantIndent(false); }
#line 4585 "hexpr.parse.C"
    break;

  case 275: /* recfieldname: "instance"  */
#line 911 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("instance")); wantIndent(false); }
#line 4591 "hexpr.parse.C"
    break;

  case 276: /* recfieldname: "exists"  */
#line 912 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("exists")); }
#line 4597 "hexpr.parse.C"
    break;

  case 277: /* recfieldname: "import"  */
#line 913 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("import")); }
#line 4603 "hexpr.parse.C"
    break;

  case 278: /* recfieldname: "module"  */
#line 914 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("module")); }
#line 4609 "hexpr.parse.C"
    break;

  case 279: /* recfieldname: "parse"  */
#line 915 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("parse")); }
#line 4615 "hexpr.parse.C"
    break;

  case 280: /* recfieldname: "do"  */
#line 916 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("do")); }
#line 4621 "hexpr.parse.C"
    break;

  case 281: /* recfieldname: "return"  */
#line 917 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("return")); }
#line 4627 "hexpr.parse.C"
    break;

  case 282: /* recfieldname: "fn"  */
#line 918 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("fn")); }
#line 4633 "hexpr.parse.C"
    break;

  case 283: /* recfieldname: "intV"  */
#line 919 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string(".f" + str::from((yyvsp[0].intv)))); }
#line 4639 "hexpr.parse.C"
    break;

  case 284: /* recfieldname: "stringV"  */
#line 920 "hexpr.y"
                         { std::string stringField = str::unescape(str::trimq(*(yyvsp[0].string)));
                           if (stringField.size() > 0 && stringField[0] == '.' ) {
                             throw annotated_error(m((yylsp[0])), "Cannot define record string label with leading '.'");
                           }
                           (yyval.string) = autorelease(new std::string(str::unescape(str::trimq(*(yyvsp[0].string))))); }
#line 4649 "hexpr.parse.C"
    break;

  case 285: /* recfieldpath: recfieldpath "." recfieldname  */
#line 926 "hexpr.y"
                                            { (yyval.strings) = (yyvsp[-2].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4655 "hexpr.parse.C"
    break;

  case 286: /* recfieldpath: recfieldpath "tupSection"  */
#line 927 "hexpr.y"
                                            { (yyval.strings) = (yyvsp[-1].strings); str::seq x = tupSectionFields(*(yyvsp[0].string)); (yyval.strings)->insert((yyval.strings)->end(), x.begin(), x.end()); }
#line 4661 "hexpr.parse.C"
    break;

  case 287: /* recfieldpath: "." recfieldname  */
#line 928 "hexpr.y"
                                            { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4667 "hexpr.parse.C"
    break;

  case 288: /* recfieldpath: "tupSection"  */
#line 929 "hexpr.y"
                                            { (yyval.strings) = autorelease(new str::seq()); *(yyval.strings) = tupSectionFields(*(yyvsp[0].string)); }
#line 4673 "hexpr.parse.C"
    break;

  case 289: /* varfields: varbind  */
#line 931 "hexpr.y"
                                 { (yyval.vfields) = autorelease(new Case::Bindings()); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4679 "hexpr.parse.C"
    break;

  case 290: /* varfields: varfields "," varbind  */
#line 932 "hexpr.y"
                                 { (yyval.vfields) = (yyvsp[-2].vfields); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4685 "hexpr.parse.C"
    break;

  case 291: /* varbind: id "=" l0expr  */
#line 934 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-2].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4691 "hexpr.parse.C"
    break;

  case 292: /* varbind: id ":" id "=" l0expr  */
#line 935 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-4].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4697 "hexpr.parse.C"
    break;

  case 293: /* varbind: "intV" ":" id "=" l0expr  */
#line 936 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(".f" + str::from((yyvsp[-4].intv)), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4703 "hexpr.parse.C"
    break;

  case 294: /* cargs: %empty  */
#line 938 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); }
#line 4709 "hexpr.parse.C"
    break;

  case 295: /* cargs: l0expr  */
#line 939 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4715 "hexpr.parse.C"
    break;

  case 296: /* cargs: cargs "," l0expr  */
#line 940 "hexpr.y"
                        { (yyvsp[-2].exps)->push_back(ExprPtr((yyvsp[0].exp))); (yyval.exps) = (yyvsp[-2].exps); }
#line 4721 "hexpr.parse.C"
    break;

  case 297: /* qtype: cst "=>" l0mtype  */
#line 942 "hexpr.y"
                         { (yyval.qualtype) = new QualType(*(yyvsp[-2].tconstraints), *(yyvsp[0].mtype)); }
#line 4727 "hexpr.parse.C"
    break;

  case 298: /* qtype: l0mtype  */
#line 943 "hexpr.y"
                         { (yyval.qualtype) = new QualType(Constraints(), *(yyvsp[0].mtype)); }
#line 4733 "hexpr.parse.C"
    break;

  case 299: /* cst: "(" tpreds ")"  */
#line 946 "hexpr.y"
                    { (yyval.tconstraints) = (yyvsp[-1].tconstraints); }
#line 4739 "hexpr.parse.C"
    break;

  case 300: /* tpreds: tpred  */
#line 948 "hexpr.y"
                         { (yyval.tconstraints) = autorelease(new Constraints()); (yyval.tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); }
#line 4745 "hexpr.parse.C"
    break;

  case 301: /* tpreds: tpreds "," tpred  */
#line 949 "hexpr.y"
                         { (yyvsp[-2].tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); (yyval.tconstraints) = (yyvsp[-2].tconstraints); }
#line 4751 "hexpr.parse.C"
    break;

  case 302: /* tpred: id l1mtargl  */
#line 951 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(*(yyvsp[-1].string), *(yyvsp[0].mtypes)); }
#line 4757 "hexpr.parse.C"
    break;

  case 303: /* tpred: l1mtype "==" l1mtype  */
#line 952 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(EqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4763 "hexpr.parse.C"
    break;

  case 304: /* tpred: l1mtype "!=" l1mtype  */
#line 953 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(NotEqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4769 "hexpr.parse.C"
    break;

  case 305: /* tpred: l1mtype "~" l1mtype  */
#line 954 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(FixIsoRecur::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4775 "hexpr.parse.C"
    break;

  case 306: /* tpred: l1mtype "=" "{" l1mtype "*" l1mtype "}"  */
#line 955 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4781 "hexpr.parse.C"
    break;

  case 307: /* tpred: l1mtype "=" "{" id ":" l1mtype "*" l1mtype "}"  */
#line 956 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4787 "hexpr.parse.C"
    break;

  case 308: /* tpred: l1mtype "=" "(" l1mtype "*" l1mtype ")"  */
#line 957 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4793 "hexpr.parse.C"
    break;

  case 309: /* tpred: "{" l1mtype "*" l1mtype "}" "=" l1mtype  */
#line 958 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4799 "hexpr.parse.C"
    break;

  case 310: /* tpred: "{" id ":" l1mtype "*" l1mtype "}" "=" l1mtype  */
#line 959 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4805 "hexpr.parse.C"
    break;

  case 311: /* tpred: "(" l1mtype "*" l1mtype ")" "=" l1mtype  */
#line 960 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(1), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4811 "hexpr.parse.C"
    break;

  case 312: /* tpred: l1mtype "." recfieldname "::" l1mtype  */
#line 962 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4817 "hexpr.parse.C"
    break;

  case 313: /* tpred: l1mtype "." recfieldname "<-" l1mtype  */
#line 963 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4823 "hexpr.parse.C"
    break;

  case 314: /* tpred: l1mtype "/" l1mtype "::" l1mtype  */
#line 964 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4829 "hexpr.parse.C"
    break;

  case 315: /* tpred: l1mtype "/" l1mtype "<-" l1mtype  */
#line 965 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4835 "hexpr.parse.C"
    break;

  case 316: /* tpred: l1mtype "=" "|" l1mtype "+" l1mtype "|"  */
#line 967 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4841 "hexpr.parse.C"
    break;

  case 317: /* tpred: "|" l1mtype "+" l1mtype "|" "=" l1mtype  */
#line 968 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4847 "hexpr.parse.C"
    break;

  case 318: /* tpred: l1mtype "=" "|" id ":" l1mtype "+" l1mtype "|"  */
#line 969 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4853 "hexpr.parse.C"
    break;

  case 319: /* tpred: "|" id ":" l1mtype "+" l1mtype "|" "=" l1mtype  */
#line 970 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4859 "hexpr.parse.C"
    break;

  case 320: /* tpred: "|" id ":" l0mtype "|" "::" l1mtype  */
#line 972 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), TString::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype))); }
#line 4865 "hexpr.parse.C"
    break;

  case 321: /* tpred: "|" l1mtype "/" l0mtype "|" "::" l1mtype  */
#line 973 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), *(yyvsp[-5].mtype),                *(yyvsp[-3].mtype))); }
#line 4871 "hexpr.parse.C"
    break;

  case 322: /* tpred: l1mtype "++" l1mtype "=" l1mtype  */
#line 974 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(AppendsToUnqualifier::constraintName(), list(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4877 "hexpr.parse.C"
    break;

  case 323: /* l1mtargl: l1mtype  */
#line 976 "hexpr.y"
                           { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4883 "hexpr.parse.C"
    break;

  case 324: /* l1mtargl: l1mtargl l1mtype  */
#line 977 "hexpr.y"
                           { (yyvsp[-1].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4889 "hexpr.parse.C"
    break;

  case 325: /* ltmtype: ltmtype l0mtype  */
#line 979 "hexpr.y"
                          { (yyval.mtypes) = (yyvsp[-1].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4895 "hexpr.parse.C"
    break;

  case 326: /* ltmtype: l0mtype  */
#line 980 "hexpr.y"
                          { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4901 "hexpr.parse.C"
    break;

  case 327: /* l0mtype: l0mtargl "->" l1mtype  */
#line 982 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(Func::make(tuplety(*(yyvsp[-2].mtypes)), *(yyvsp[0].mtype)))); }
#line 4907 "hexpr.parse.C"
    break;

  case 328: /* l0mtype: mtuplist  */
#line 983 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(makeTupleType(*(yyvsp[0].mtypes)))); }
#line 4913 "hexpr.parse.C"
    break;

  case 329: /* l0mtype: msumlist  */
#line 984 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(makeSumType(*(yyvsp[0].mtypes)))); }
#line 4919 "hexpr.parse.C"
    break;

  case 330: /* l1mtype: id  */
#line 986 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(monoTypeByName(*(yyvsp[0].string)))); }
#line 4925 "hexpr.parse.C"
    break;

  case 331: /* l1mtype: "<" cppid ">"  */
#line 987 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(OpaquePtr::make(str::replace<char>(*(yyvsp[-1].string), ".", "::"), 0, false))); }
#line 4931 "hexpr.parse.C"
    break;

  case 332: /* l1mtype: "[" "]"  */
#line 988 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("[]"))); }
#line 4937 "hexpr.parse.C"
    break;

  case 333: /* l1mtype: "[" ltmtype "]"  */
#line 989 "hexpr.y"
                                           { try { (yyval.mtype) = autorelease(new MonoTypePtr(Array::make(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4943 "hexpr.parse.C"
    break;

  case 334: /* l1mtype: "[" ":" l0mtype "|" tyind ":" "]"  */
#line 990 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(FixedArray::make(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype)))); }
#line 4949 "hexpr.parse.C"
    break;

  case 335: /* l1mtype: "(" "->" ")"  */
#line 991 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("->"))); }
#line 4955 "hexpr.parse.C"
    break;

  case 336: /* l1mtype: "(" ltmtype ")"  */
#line 992 "hexpr.y"
                                           { try { (yyval.mtype) = autorelease(new MonoTypePtr(clone(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4961 "hexpr.parse.C"
    break;

  case 337: /* l1mtype: "{" mreclist "}"  */
#line 993 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(makeRecType(*(yyvsp[-1].mreclist)))); }
#line 4967 "hexpr.parse.C"
    break;

  case 338: /* l1mtype: "|" mvarlist "|"  */
#line 994 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(makeVarType(*(yyvsp[-1].mvarlist)))); }
#line 4973 "hexpr.parse.C"
    break;

  case 339: /* l1mtype: "|" mpvarlist "|"  */
#line 995 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(makePVarType(*(yyvsp[-1].mvarlist), m((yylsp[-1]))))); }
#line 4979 "hexpr.parse.C"
    break;

  case 340: /* l1mtype: "(" ")"  */
#line 996 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("unit"))); }
#line 4985 "hexpr.parse.C"
    break;

  case 341: /* l1mtype: "intV"  */
#line 997 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(((yyvsp[0].intv) == 0) ? Prim::make("void") : TLong::make((yyvsp[0].intv)))); }
#line 4991 "hexpr.parse.C"
    break;

  case 342: /* l1mtype: "boolV"  */
#line 998 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr((yyvsp[0].boolv) ? TLong::make(1) : TLong::make(0))); }
#line 4997 "hexpr.parse.C"
    break;

  case 343: /* l1mtype: "exists" id "." l1mtype  */
#line 999 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Exists::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 5003 "hexpr.parse.C"
    break;

  case 344: /* l1mtype: l1mtype "@" l1mtype  */
#line 1000 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype), *(yyvsp[0].mtype)))); }
#line 5009 "hexpr.parse.C"
    break;

  case 345: /* l1mtype: l1mtype "@" "?"  */
#line 1001 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype)))); }
#line 5015 "hexpr.parse.C"
    break;

  case 346: /* l1mtype: "^" id "." l1mtype  */
#line 1002 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Recursive::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 5021 "hexpr.parse.C"
    break;

  case 347: /* l1mtype: "stringV"  */
#line 1003 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(TString::make(str::unescape(str::trimq(*(yyvsp[0].string)))))); }
#line 5027 "hexpr.parse.C"
    break;

  case 348: /* l1mtype: "`" l0expr "`"  */
#line 1004 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp))))))); }
#line 5033 "hexpr.parse.C"
    break;

  case 349: /* tyind: id  */
#line 1006 "hexpr.y"
              { (yyval.mtype) = autorelease(new MonoTypePtr(TVar::make(*(yyvsp[0].string)))); }
#line 5039 "hexpr.parse.C"
    break;

  case 350: /* tyind: "intV"  */
#line 1007 "hexpr.y"
              { (yyval.mtype) = autorelease(new MonoTypePtr(TLong::make((yyvsp[0].intv)))); }
#line 5045 "hexpr.parse.C"
    break;

  case 351: /* cppid: id  */
#line 1009 "hexpr.y"
                    { (yyval.string) = (yyvsp[0].string); }
#line 5051 "hexpr.parse.C"
    break;

  case 352: /* cppid: cppid "." id  */
#line 1010 "hexpr.y"
                    { (yyval.string) = (yyvsp[-2].string); *(yyval.string) += "."; *(yyval.string) += *(yyvsp[0].string); }
#line 5057 "hexpr.parse.C"
    break;

  case 353: /* l0mtargl: l1mtype  */
#line 1012 "hexpr.y"
                                        { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5063 "hexpr.parse.C"
    break;

  case 354: /* l0mtargl: "(" l0mtype "," l0mtarglt ")"  */
#line 1013 "hexpr.y"
                                        { (yyvsp[-1].mtypes)->insert((yyvsp[-1].mtypes)->begin(), *(yyvsp[-3].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 5069 "hexpr.parse.C"
    break;

  case 355: /* l0mtarglt: l0mtype  */
#line 1015 "hexpr.y"
                                 { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5075 "hexpr.parse.C"
    break;

  case 356: /* l0mtarglt: l0mtarglt "," l0mtype  */
#line 1016 "hexpr.y"
                                 { (yyvsp[-2].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-2].mtypes); }
#line 5081 "hexpr.parse.C"
    break;

  case 357: /* mtuplist: l1mtype  */
#line 1018 "hexpr.y"
                               { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5087 "hexpr.parse.C"
    break;

  case 358: /* mtuplist: mtuplist "*" l1mtype  */
#line 1019 "hexpr.y"
                               { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5093 "hexpr.parse.C"
    break;

  case 359: /* msumlist: l1mtype "+" l1mtype  */
#line 1021 "hexpr.y"
                               { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[-2].mtype)); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5099 "hexpr.parse.C"
    break;

  case 360: /* msumlist: msumlist "+" l1mtype  */
#line 1022 "hexpr.y"
                               { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5105 "hexpr.parse.C"
    break;

  case 361: /* mreclist: mreclist "," id ":" l0mtype  */
#line 1024 "hexpr.y"
                                      { (yyval.mreclist) = (yyvsp[-4].mreclist);                                 (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 5111 "hexpr.parse.C"
    break;

  case 362: /* mreclist: id ":" l0mtype  */
#line 1025 "hexpr.y"
                                      { (yyval.mreclist) = autorelease(new Record::Members()); (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 5117 "hexpr.parse.C"
    break;

  case 363: /* mvarlist: mvarlist "," id ":" l0mtype  */
#line 1027 "hexpr.y"
                                      { (yyval.mvarlist) = (yyvsp[-4].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 5123 "hexpr.parse.C"
    break;

  case 364: /* mvarlist: mvarlist "," id  */
#line 1028 "hexpr.y"
                                      { (yyval.mvarlist) = (yyvsp[-2].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 5129 "hexpr.parse.C"
    break;

  case 365: /* mvarlist: id ":" l0mtype  */
#line 1029 "hexpr.y"
                                      { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 5135 "hexpr.parse.C"
    break;

  case 366: /* mvarlist: id  */
#line 1030 "hexpr.y"
                                      { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 5141 "hexpr.parse.C"
    break;

  case 367: /* mpvarlist: mpvarlist "," id "(" "intV" ")"  */
#line 1032 "hexpr.y"
                                             { (yyval.mvarlist) = (yyvsp[-5].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), (yyvsp[-1].intv))); }
#line 5147 "hexpr.parse.C"
    break;

  case 368: /* mpvarlist: mpvarlist "," id "(" "shortV" ")"  */
#line 1033 "hexpr.y"
                                             { (yyval.mvarlist) = (yyvsp[-5].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"),(yyvsp[-1].shortv))); }
#line 5153 "hexpr.parse.C"
    break;

  case 369: /* mpvarlist: mpvarlist "," id "(" "boolV" ")"  */
#line 1034 "hexpr.y"
                                             { (yyval.mvarlist) = (yyvsp[-5].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), (yyvsp[-1].boolv))); }
#line 5159 "hexpr.parse.C"
    break;

  case 370: /* mpvarlist: mpvarlist "," id "(" "byteV" ")"  */
#line 1035 "hexpr.y"
                                             { (yyval.mvarlist) = (yyvsp[-5].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), str::dehex(*(yyvsp[-1].string)))); }
#line 5165 "hexpr.parse.C"
    break;

  case 371: /* mpvarlist: mpvarlist "," id "(" "charV" ")"  */
#line 1036 "hexpr.y"
                                             { (yyval.mvarlist) = (yyvsp[-5].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), str::readCharDef(*(yyvsp[-1].string)))); }
#line 5171 "hexpr.parse.C"
    break;

  case 372: /* mpvarlist: id "(" "intV" ")"  */
#line 1037 "hexpr.y"
                                             { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), (yyvsp[-1].intv))); }
#line 5177 "hexpr.parse.C"
    break;

  case 373: /* mpvarlist: id "(" "shortV" ")"  */
#line 1038 "hexpr.y"
                                             { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), (yyvsp[-1].shortv))); }
#line 5183 "hexpr.parse.C"
    break;

  case 374: /* mpvarlist: id "(" "boolV" ")"  */
#line 1039 "hexpr.y"
                                             { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), (yyvsp[-1].boolv))); }
#line 5189 "hexpr.parse.C"
    break;

  case 375: /* mpvarlist: id "(" "byteV" ")"  */
#line 1040 "hexpr.y"
                                             { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), str::dehex(*(yyvsp[-1].string)))); }
#line 5195 "hexpr.parse.C"
    break;

  case 376: /* mpvarlist: id "(" "charV" ")"  */
#line 1041 "hexpr.y"
                                             { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), str::readCharDef(*(yyvsp[-1].string)))); }
#line 5201 "hexpr.parse.C"
    break;


#line 5205 "hexpr.parse.C"

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
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

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
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken, &yylloc};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          goto yyexhaustedlab;
      }
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

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
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
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

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


#if 1
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturn;
#endif


/*-------------------------------------------------------.
| yyreturn -- parsing is finished, clean up and return.  |
`-------------------------------------------------------*/
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
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

#line 1045 "hexpr.y"

#pragma GCC diagnostic pop


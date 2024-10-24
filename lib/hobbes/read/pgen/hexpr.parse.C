/* A Bison parser, made by GNU Bison 3.8.2.  */

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
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

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

using YYLTYPE = struct YYLTYPE {
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
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

#include <cstdio>
#include <hobbes/db/bindings.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/module.H>
#include <hobbes/lang/pat/pattern.H>
#include <hobbes/lang/type.H>
#include <hobbes/lang/typepreds.H>
#include <hobbes/parse/grammar.H>
#include <hobbes/parse/lalr.H>
#include <hobbes/read/pgen/hexpr.parse.H>
#include <hobbes/util/array.H>
#include <hobbes/util/autorelease.H>
#include <hobbes/util/str.H>
#include <stdexcept>
#include <string>
#include <vector>

using namespace hobbes;

using LetBinding = std::pair<PatternPtr, ExprPtr>;
using LetBindings = std::vector<LetBinding>;

cc*         yyParseCC;
Module*     yyParsedModule = nullptr;
std::string yyParsedVar;
Expr*       yyParsedExpr   = nullptr;
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
  if (exprs->empty()) {
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
  if (pats->empty()) {
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
  if (!rs.empty() && !rs.back().result) {
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
  for (auto b = bs.rbegin(); b != bs.rend(); ++b) {
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
  if (ts.empty()) {
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
using VarCtorFn = Expr *(*)(const std::string &, const LexicalAnnotation &);
using PatVarCtorFn = Pattern *(*)(const std::string &, const LexicalAnnotation &);
extern VarCtorFn varCtorFn;
extern PatVarCtorFn patVarCtorFn;
}


#line 360 "hexpr.parse.C"

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
  YYSYMBOL_mpvar = 180,                    /* mpvar  */
  YYSYMBOL_id = 181                        /* id  */
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

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
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
#define YYLAST   3032

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  98
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  84
/* YYNRULES -- Number of rules.  */
#define YYNRULES  374
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  826

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
       0,   524,   524,   525,   526,   527,   530,   531,   532,   534,
     535,   536,   538,   539,   540,   541,   542,   543,   545,   546,
     547,   548,   549,   550,   551,   552,   553,   554,   555,   556,
     557,   558,   559,   560,   561,   562,   563,   564,   565,   568,
     571,   574,   575,   576,   579,   580,   583,   585,   588,   589,
     590,   591,   592,   593,   594,   595,   597,   598,   600,   602,
     603,   605,   608,   609,   610,   611,   613,   614,   616,   619,
     621,   623,   624,   626,   628,   630,   631,   632,   633,   634,
     635,   636,   637,   638,   639,   640,   641,   642,   643,   644,
     645,   646,   647,   648,   650,   651,   653,   654,   657,   658,
     659,   660,   662,   663,   664,   665,   666,   667,   669,   670,
     672,   673,   674,   675,   676,   677,   678,   679,   680,   682,
     683,   684,   685,   686,   688,   689,   690,   691,   693,   696,
     697,   700,   703,   706,   718,   719,   722,   724,   725,   727,
     729,   730,   732,   733,   735,   736,   738,   739,   741,   742,
     745,   746,   749,   750,   751,   752,   753,   754,   755,   756,
     757,   760,   761,   762,   763,   764,   767,   768,   769,   772,
     775,   778,   779,   782,   783,   784,   785,   786,   787,   788,
     789,   790,   791,   792,   793,   794,   795,   798,   801,   802,
     803,   804,   805,   806,   807,   808,   809,   810,   811,   812,
     813,   814,   815,   816,   817,   818,   821,   823,   824,   826,
     828,   829,   831,   833,   834,   836,   837,   839,   840,   841,
     843,   844,   846,   847,   849,   850,   852,   853,   856,   857,
     859,   860,   861,   862,   863,   864,   865,   866,   867,   868,
     869,   870,   871,   872,   873,   874,   875,   876,   877,   878,
     879,   880,   881,   882,   884,   885,   886,   887,   888,   890,
     892,   893,   895,   896,   898,   899,   901,   903,   904,   905,
     907,   908,   909,   910,   911,   912,   913,   914,   915,   916,
     917,   918,   919,   920,   921,   927,   928,   929,   930,   932,
     933,   935,   936,   937,   939,   940,   941,   943,   944,   947,
     949,   950,   952,   953,   954,   955,   956,   957,   958,   959,
     960,   961,   963,   964,   965,   966,   968,   969,   970,   971,
     973,   974,   975,   977,   978,   980,   981,   983,   984,   985,
     987,   988,   989,   990,   991,   992,   993,   994,   995,   996,
     997,   998,   999,  1000,  1001,  1002,  1003,  1004,  1005,  1007,
    1008,  1010,  1011,  1013,  1014,  1016,  1017,  1019,  1020,  1022,
    1023,  1025,  1026,  1028,  1029,  1030,  1031,  1033,  1034,  1036,
    1037,  1038,  1039,  1040,  1042
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
  "mvarlist", "mpvarlist", "mpvar", "id", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-603)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-367)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     383,  1294,  2083,  2083,    45,    52,    52,    52,    77,    77,
     110,   110,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,
    -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,   121,
     124,  2083,  2863,     1,  2863,    52,   123,  1532,  2083,   121,
     411,  2083,   424,  -603,  1397,  -603,  -603,  -603,  -603,  -603,
    -603,  -603,   176,  -603,   243,   225,   145,   273,  2629,  2473,
    2083,  1615,  2952,  2952,  -603,   161,   212,   478,   454,   434,
    -603,   268,  -603,  -603,  -603,  1294,   391,   309,  -603,   862,
     189,  -603,  -603,   196,  1887,   369,    77,   376,  1965,  -603,
    -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,
    -603,  -603,  -603,  -603,  -603,  2952,    52,   -26,  -603,   380,
    -603,   366,   256,  2785,    52,   256,   416,  2161,   386,   397,
    2551,   406,   415,   417,   121,   422,   432,   479,   484,   486,
     492,   496,   498,  2317,   499,   503,   504,  -603,  -603,   506,
    -603,   258,   333,    96,   408,   525,   545,    30,   394,    52,
      52,   438,  -603,  2043,  2043,  2952,  2083,  1849,   145,  -603,
    -603,   121,  2083,    44,  -603,  -603,   520,   386,   397,  2551,
     406,   415,   417,   422,   432,   479,   486,   492,   496,   498,
     499,   503,   504,  -603,  -603,  -603,  -603,  -603,  -603,  -603,
    -603,  -603,  -603,  -603,  -603,  -603,  2952,  2952,    52,   430,
     225,   866,  -603,  -603,  -603,  2886,  2395,  2395,  2395,  2395,
    2473,  2629,  2629,  2629,  2629,  2629,  2629,  2629,  2629,  2629,
    2629,  2629,  2707,  2707,  2707,  2083,  -603,  1397,    52,  -603,
    -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,
    -603,  -603,  -603,  -603,  -603,  -603,  2043,  -603,  2043,  -603,
    -603,  -603,    52,    52,   217,  1002,  2121,  2121,    52,  2083,
     351,  -603,   360,  2121,    52,    27,    77,   862,    52,   217,
      52,    52,   144,  -603,    20,   555,   550,   554,  -603,  -603,
     358,   515,   431,  -603,   560,  2083,   138,  2473,   528,   530,
     256,    15,  -603,   574,  2863,  1693,   121,   533,  1771,  -603,
     578,   580,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,
    -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,
    -603,  -603,  2083,  2952,  1927,  -603,  -603,    39,  2083,  2083,
    2083,  -603,  -603,  -603,  -603,  -603,  1184,  -603,   588,  -603,
    -603,  -603,   359,   535,  2083,    10,  -603,  -603,  2083,   129,
    2083,   377,   188,   459,   591,   101,  2083,  -603,  2083,   -10,
     541,   541,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,
    -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  1397,
    -603,  -603,  -603,   590,    97,   563,  -603,   290,  -603,    60,
    1965,  -603,   488,   217,   140,   464,   605,   167,   466,   468,
    -603,     4,   594,   551,  -603,  1887,   350,  2121,  2121,   121,
    2121,  2121,  2121,  1036,  2121,   556,    77,   631,    52,    52,
    1965,   566,   614,    79,   638,  -603,  2121,  2121,  2121,  2121,
    -603,   579,  2952,  -603,    16,  2952,  -603,  2083,  -603,  -603,
     437,  2952,   530,  -603,  -603,  -603,  -603,   283,  -603,  -603,
    -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,  -603,
    -603,  1693,  2239,   121,   448,   225,  -603,   560,  -603,  2083,
    -603,  -603,  2083,  -603,  -603,   174,   617,  -603,   582,  -603,
     618,  -603,   585,   586,   217,   581,  1965,  -603,  -603,   595,
    2005,  -603,  -603,  2083,   149,   599,  -603,   583,  -603,   596,
    -603,    17,  2952,  2952,  -603,  -603,  -603,  2121,  -603,  -603,
    -603,  -603,  2121,   597,  -603,  2121,  -603,    52,  1965,  2121,
    1965,  -603,    52,  -603,    52,  1965,   485,  2121,  -603,  -603,
    2121,  2121,  2121,    38,    31,   299,   556,   556,   556,  -603,
     556,   556,    29,    77,   631,  -603,    23,  -603,    48,  -603,
    -603,   184,  1965,  1965,  1965,    77,   638,  -603,   556,   556,
     556,   556,  -603,  -603,  -603,  -603,  -603,  -603,   628,   471,
    -603,   387,  1490,  -603,   598,  -603,    46,  2863,   630,   133,
     592,   593,  -603,  2952,  2083,  -603,  2083,  -603,  -603,  -603,
    -603,  -603,   602,  -603,  2083,   192,  2083,  -603,  -603,  -603,
     600,   601,   556,   173,   449,   255,   640,  -603,    84,   159,
     603,   642,  -603,   611,   606,    57,   612,   620,   621,   622,
     623,   556,   153,   190,   667,    56,   669,  2121,  2121,  2121,
    2121,  2121,   631,    52,  -603,  -603,   631,    52,    52,  -603,
     638,  -603,   378,  -603,  -603,   671,  -603,    52,   653,   437,
      52,  2083,  2083,  2083,  -603,  -603,  -603,  2083,  -603,  -603,
     679,   256,  2239,  2239,  -603,  -603,  -603,  -603,   633,  -603,
    -603,  -603,  2083,   234,  -603,  -603,  -603,   677,  -603,   682,
    -603,   680,  1965,  2121,   681,   678,  1965,   685,  2121,  -603,
    -603,  -603,  -603,  -603,  2121,  2121,  2121,  2121,  2121,   556,
     556,   556,   556,   556,   631,    25,   631,  -603,    52,   638,
    -603,  1965,  2083,   683,  2083,  -603,   687,  -603,   693,  -603,
    -603,   641,   353,  2395,  -603,  2083,   252,  2121,   651,  2121,
    -603,   324,  2121,  2121,  -603,  2121,   219,   213,   326,   206,
     301,    73,   631,  -603,  -603,  2083,  -603,  2083,  2083,  -603,
    -603,  -603,   -10,   648,  -603,  2083,   275,   556,  -603,   556,
     692,   556,   556,   556,   695,  -603,  -603,  2121,  -603,  2121,
     631,  -603,  -603,  -603,  2395,  -603,  2083,   306,  2121,  2121,
     332,   303,   -10,  -603,  2083,   311,   556,   556,  -603,  -603,
    -603,  2083,   312,  -603,  2083,   315,  -603,  2083,   320,  -603,
    2083,   335,  -603,  2083,   336,  -603,  2083,   337,  -603,  2083,
     338,  -603,  2083,   342,  -603,  2083,   346,  -603,  2083,   347,
    -603,  2083,   697,  -603,  2083,  -603
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     9,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   173,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   374,   183,   170,   220,   185,   186,   288,     0,
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
     368,   330,     0,     0,   299,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   302,   323,     0,     0,     0,     0,
       0,   326,     0,   366,     0,    97,     0,     0,     0,     0,
     255,     0,     0,   257,     0,     0,   129,     0,   137,   139,
       0,     0,   131,   225,   133,   207,   214,     0,   173,   174,
     175,   176,   177,   178,   179,   180,   182,   183,   170,   185,
     186,   261,   261,   267,     0,   184,   151,     0,   143,     0,
     134,   140,     0,   296,   149,     0,     0,   153,     0,   167,
       0,   268,     0,     0,     0,   357,     0,   150,   156,     0,
       0,   157,    19,     0,     0,     0,   249,     0,   244,     0,
     251,     0,     0,     0,   246,    98,    99,     0,   331,   335,
     336,   325,     0,     0,   333,     0,   337,     0,     0,     0,
       0,   338,     0,   339,     0,     0,     0,     0,   348,   301,
       0,     0,     0,     0,     0,     0,   303,   305,   304,   345,
     344,   324,    48,     0,    54,    59,    53,    56,     0,    94,
      70,    63,     0,     0,     0,     0,    64,    66,   359,   327,
     358,   360,   256,   262,   258,   264,   266,   130,     0,     0,
     289,     0,     0,   224,   209,   211,     0,     0,     0,     0,
       0,     0,   154,     0,     0,   152,     0,   162,   161,   297,
     160,   159,     0,    20,     0,     0,     0,   250,   245,   252,
       0,     0,   343,     0,     0,     0,     0,   362,   357,     0,
       0,   364,   367,     0,   365,   357,     0,     0,     0,     0,
       0,   346,     0,     0,   330,     0,   330,     0,     0,     0,
       0,     0,     0,     0,    61,    60,     0,     0,     0,    95,
       0,   355,     0,   365,    68,     0,    67,     0,   164,     0,
       0,     0,     0,     0,   214,   219,   218,     0,   213,   216,
     217,   172,     0,     0,   163,   135,   142,   148,   147,   269,
     158,    21,     0,     0,   108,   248,   247,     0,   350,     0,
     349,     0,     0,     0,     0,     0,     0,     0,     0,   371,
     373,   372,   370,   369,     0,     0,     0,     0,     0,   322,
     315,   314,   313,   312,    50,    49,    55,    57,    58,    65,
     354,     0,     0,     0,     0,   290,     0,   291,     0,   226,
     210,     0,     0,     0,    22,     0,     0,     0,     0,     0,
     361,     0,     0,     0,   363,     0,   359,     0,     0,     0,
       0,     0,     0,   356,    47,     0,   165,     0,     0,   212,
     215,   217,   145,   146,    23,     0,     0,   311,   334,   309,
       0,   317,   321,   320,     0,   308,   306,     0,   316,     0,
      51,   293,   292,   227,     0,    24,     0,     0,     0,     0,
       0,     0,   144,    25,     0,     0,   310,   319,   307,   318,
      26,     0,     0,    27,     0,     0,    28,     0,     0,    29,
       0,     0,    30,     0,     0,    31,     0,     0,    32,     0,
       0,    33,     0,     0,    34,     0,     0,    35,     0,     0,
      36,     0,     0,    37,     0,    38
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -603,  -603,   663,   512,   -32,  -603,  -603,  -603,  -603,   197,
    -603,  -603,   108,   105,  -602,  -526,  -603,   104,  -534,  -390,
     549,     0,   480,   111,   328,    -2,  -202,   -42,   389,   -31,
     313,    13,  -603,   465,  -603,   452,  -603,   169,  -603,   -21,
    -603,   463,  -603,   102,  -603,  -603,    33,   -39,  -603,  -603,
     316,   -47,  -603,   -92,    74,  -176,  -603,  -183,  -402,  -603,
     -19,   -52,  -603,   112,   -35,  -126,   559,  -603,   352,  -603,
     505,   -69,   767,  -603,   509,  -603,  -603,  -603,  -603,  -603,
    -603,  -603,   240,   436
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     4,    43,    44,    45,    46,    47,   151,    48,    49,
     644,    50,   546,   547,   544,   545,    51,   556,   557,   265,
     266,    52,   139,   548,   272,   140,    65,    66,    67,    68,
      69,    70,   107,   108,   298,   299,   753,   474,   475,    54,
     291,   292,   574,   575,   576,   658,   659,    55,   113,   442,
     443,   201,   202,   109,   279,   280,   281,   282,   283,   144,
     145,    56,   569,   570,   141,   337,   338,   260,   261,   414,
     387,   339,   274,   679,    77,   275,   642,   276,   277,   395,
     398,   399,   400,    73
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      64,    72,   158,   143,   359,   360,   361,   362,    81,    81,
     103,   112,   152,   115,    53,   353,   205,   165,   635,   273,
     351,   352,   646,   200,   200,   300,   542,   164,   340,   111,
     704,   636,   565,   742,   706,   417,   142,   632,   525,   148,
      22,    22,    22,   285,   490,    74,    89,    90,    91,    92,
      93,    94,    95,  -353,   207,   208,   286,    53,   166,    96,
     158,   426,   655,   158,    22,    97,   200,   628,   330,    22,
     629,    22,   656,    22,    98,   209,   627,    22,   526,   114,
     247,   638,   348,   247,  -366,  -366,    81,   491,    53,   164,
    -353,   165,   290,  -353,   444,   564,   599,   697,   688,   565,
      99,   426,    22,   512,   637,   103,   637,   418,   413,   633,
     331,   100,   101,   554,   769,   297,   200,  -353,   479,   413,
     381,   342,   382,   102,   657,   426,   413,   683,    89,    90,
      91,    92,    93,    94,    95,    22,   203,   203,   164,   503,
     770,    96,   346,   228,   413,   413,    22,    97,   413,    22,
     508,    79,   424,   526,    22,   345,    98,   200,   200,   249,
     347,   413,   200,    22,   250,   645,   200,   493,   363,    22,
     251,   663,   413,   325,    22,   646,   160,   322,   635,   252,
     635,   504,    99,   515,    84,   388,   388,   594,   372,   373,
     374,   161,   640,   100,   101,   253,   694,   206,   105,   249,
     421,   117,   106,   425,   250,   102,   300,   437,   519,    22,
     251,   520,   105,   664,    22,   153,   106,    22,   269,   252,
     255,    22,   270,   378,   271,   207,   208,   246,   413,   341,
     672,   258,   249,   695,   248,   253,   259,   250,   158,   684,
      53,   413,    22,   251,   635,   439,   209,   413,   677,   767,
     385,   582,   252,   159,   583,   413,   465,   403,   269,    22,
     255,   413,   270,    79,   271,   498,   247,   421,   253,   499,
      79,   258,   725,   447,    28,   357,   259,    22,   413,   357,
     353,   210,   154,   436,   200,   351,   352,    28,   765,    29,
     755,   269,   386,   255,   413,   270,   297,   271,    22,   764,
      22,   413,    29,   155,   258,   249,   225,   413,   480,   259,
     250,   162,   -73,   776,    28,    22,   251,   156,   511,   157,
     473,   513,   478,   511,   388,   252,   481,   482,   483,    29,
     156,    22,   157,   321,   681,   630,    22,    22,   631,   322,
      22,   253,   489,   413,   784,    22,   492,   152,   495,   791,
     794,   273,   577,   797,   505,   228,   506,   156,   800,   157,
      22,    22,    22,    22,   269,   510,   255,    22,   270,   655,
     271,    22,    22,   803,   806,   809,   812,   258,    22,   656,
     815,   768,   259,   789,   818,   821,     1,     2,     3,   413,
     535,   413,    53,   200,   572,   158,   200,   476,   406,   227,
     407,   264,   200,   760,   408,   766,   409,   410,   268,   411,
     412,   788,   413,   323,   413,   421,    81,   589,   287,   324,
     413,   650,   465,   465,   530,   651,   404,   143,   531,   288,
     532,   146,   405,   430,   487,   567,    22,    57,    71,   431,
     322,    75,    76,    78,    82,    82,    86,    88,   413,   607,
     354,   610,   496,   710,   294,    22,   614,   568,   497,   711,
     142,   302,    22,   200,   200,   104,   110,   580,   578,   678,
     581,   116,   303,    22,    22,   104,   147,   222,   223,   224,
      57,   305,   425,   641,   607,   643,   332,   326,   592,   327,
     306,   593,   307,   163,   219,   220,   221,   308,   204,   204,
     616,   617,   618,   249,   619,   620,   563,   309,   250,   566,
     433,    57,   434,    22,   251,   203,    82,   149,   150,    82,
     263,   752,   267,   252,   278,   211,   212,   213,   214,   215,
     216,   217,   218,   200,   335,   375,   376,   377,   500,   253,
     501,   204,   284,   516,   200,   517,   521,   522,   523,   524,
     293,   648,   649,   301,   310,    81,   661,    80,    83,   311,
     104,   312,   269,   328,   255,   514,   270,   313,   271,    85,
      87,   314,   782,   315,   317,   258,   600,   601,   318,   319,
     259,   320,   668,   329,   669,   333,   334,   350,   427,   278,
     278,   204,   671,   428,   674,   429,   432,   104,   435,   349,
     364,   365,   366,   367,   368,   369,   370,   371,   440,   158,
     441,   446,   488,   730,  -353,   468,   472,   734,  -254,   406,
     486,   407,   426,   465,   465,   408,   209,   409,   410,   502,
     411,   412,   204,   204,   284,   355,   507,   204,   509,   518,
     527,   204,   743,   528,   413,   543,   357,   552,   553,   717,
     718,   719,   555,   584,   562,   721,   586,   476,   597,   585,
     482,   483,   647,    57,   380,   587,   588,   596,   662,   413,
     724,   665,   590,   598,   682,   666,   686,   604,   654,   670,
     675,   676,   278,   685,   278,   526,   687,   689,   383,    78,
     278,   278,   396,   401,   402,   690,   691,   692,   693,   278,
     416,   696,    82,   698,   420,   278,   422,   423,   278,   712,
     744,   714,   746,   722,   723,   727,   728,   733,   729,   732,
     749,   745,   110,   754,   735,   747,   748,   293,   758,   774,
     778,   466,   467,   779,   301,   824,   600,   601,   226,   379,
     634,   705,   707,   771,   709,   772,   773,   419,   551,   708,
     471,   438,   667,   775,   445,   750,   720,   529,   573,   204,
     392,   715,   384,   104,   612,     0,     0,     0,     0,     0,
       0,     0,   263,     0,   783,     0,     0,     0,     0,     0,
       0,     0,   790,     0,     0,   494,     0,     0,     0,   793,
       0,     0,   796,     0,     0,   799,     0,     0,   802,     0,
       0,   805,     0,     0,   808,     0,     0,   811,     0,     0,
     814,     0,     0,   817,     0,    57,   820,     0,     0,   823,
       0,     0,   825,   278,     0,     0,   278,     0,   278,   278,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   263,     0,   278,   278,   104,   278,   278,   278,   278,
     278,   262,   267,     0,   549,   550,   278,     0,     0,     0,
       0,     0,   278,   278,   278,   278,     0,     0,   204,     0,
     284,   204,     0,     0,     0,     0,   571,   204,     0,     0,
       0,   183,   184,   185,   186,   187,   188,   189,   190,     0,
     191,    22,   192,   193,    25,   194,   195,   466,   466,   467,
     579,     0,   229,   230,   231,   232,   233,   234,   235,   236,
     237,   238,   356,   239,   240,   241,   242,     0,     0,     0,
     278,     0,   278,     0,     0,     0,   243,   244,     0,     0,
     595,   245,     0,     0,     0,     0,     0,   284,   204,   204,
     196,     0,   197,   278,   198,     0,   199,   137,   278,     0,
       0,   278,     0,   606,   278,   278,   278,     0,   611,   138,
     613,   278,     0,   278,     0,     0,   278,   624,   626,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    82,
       0,     0,     0,     0,   639,     0,     0,   278,   278,   278,
     278,   267,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   204,     0,
       0,     0,   660,     0,     0,     0,     0,   249,     0,   204,
       0,   389,   250,   394,   397,     0,     0,    22,   251,     0,
     415,   673,     0,     0,     0,     0,   390,   252,     0,     0,
     680,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   249,     0,   253,     0,     0,   250,     0,     0,     0,
       0,    22,   251,   278,   278,   278,   278,   278,     0,   549,
       0,   252,     0,   549,   549,     0,   269,     0,   255,   391,
     270,     0,   271,   713,     0,   571,   716,   253,     0,   258,
       0,     0,     0,     0,   259,     0,     0,     0,   466,   466,
       0,     0,     0,   485,     0,     0,     0,     0,     0,   726,
     393,     0,   255,     0,   270,     0,   271,     0,   278,   278,
       0,     0,   278,   258,   278,     0,   539,     0,   259,     0,
     278,   278,   278,   278,   278,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   639,     0,     0,   278,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   751,     0,
       0,     0,   756,   278,     0,   278,     0,     0,   278,   278,
       0,   278,   262,     0,   533,   534,     0,   536,   537,   538,
     540,   541,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   777,   558,   559,   560,   561,     0,     0,   249,
       0,     0,     0,   278,   250,   278,     0,     0,     0,    22,
     251,     0,     0,   785,   278,   278,     0,   385,     0,   252,
       0,   792,     0,     0,     0,     0,     0,     0,   795,     0,
       0,   798,     0,     0,   801,   253,     0,   804,     0,     0,
     807,     0,     0,   810,     0,     0,   813,     0,     0,   816,
       0,   389,   819,     0,     0,   822,     0,     0,   484,   386,
     255,     0,   256,     0,   257,     0,     0,     0,     0,     0,
       0,   258,     0,     0,   602,     0,   259,     0,     0,   603,
       0,     0,   605,     0,     0,   608,   609,     0,     0,     0,
       0,     0,   615,     0,   621,     0,     0,   622,   623,   625,
       5,     6,     0,     7,     8,     9,    10,    11,     0,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      29,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,     0,    32,     0,    33,     0,     0,     0,     0,
       0,     0,     0,     0,    34,    35,    36,     0,    37,     0,
      38,     0,    39,     0,    40,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    41,     0,     0,    42,
       0,     0,     0,     0,   699,   700,   701,   702,   703,     0,
       0,     0,     0,     0,     0,     0,     7,     8,     9,    10,
      11,     0,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    29,     0,     0,     0,     0,     0,     0,
     731,     0,     0,    30,    31,   736,    32,     0,    33,     0,
       0,   737,   738,   739,   740,   741,     0,    34,    35,    36,
       0,    37,     0,    38,     0,    39,     0,    40,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    41,
       0,     0,    42,     0,   757,     0,   759,     0,   652,   761,
     762,     0,   763,     0,     0,   183,   184,   185,   186,   187,
     188,   189,   190,     0,   191,    22,   192,   193,    25,   194,
     195,     0,     0,   653,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   780,     0,   781,     0,     0,     0,
       0,     0,     0,     0,     0,   786,   787,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,   196,     0,   197,     0,   198,     0,
     199,     0,   118,   119,   120,   121,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    30,    31,
       0,    32,     0,    33,     0,     0,   134,   135,    60,     0,
       0,   136,    34,    35,    36,     0,    61,     0,    38,     0,
      39,     0,    40,     0,     0,    62,    63,   137,     0,     0,
       0,     0,     0,     0,    41,     0,     0,     0,     0,   138,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,     0,     0,     0,
       0,     0,     0,     0,     0,   167,   168,   169,   170,   171,
     172,    29,   173,   174,   175,   128,   176,   177,   178,   179,
     133,    30,    31,     0,    32,     0,    33,     0,     0,   180,
     181,    60,     0,     0,   182,    34,    35,    36,     0,    61,
       0,    38,     0,    39,     0,    40,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,    41,   448,   449,
     450,   451,   452,   453,   454,   455,    20,   456,    22,   457,
     458,    25,   459,   460,    28,     0,     0,     0,     0,     0,
       0,     0,     0,   167,   168,   169,   170,   171,   172,    29,
     173,   174,   175,   128,   176,   177,   178,   179,   133,    30,
      31,     0,    32,     0,    33,     0,     0,   180,   181,    60,
       0,     0,   182,    34,    35,    36,     0,   461,     0,   462,
       0,   463,     0,   464,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,    41,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,    29,     0,     0,
       0,     0,     0,     0,     0,     0,    59,    30,    31,     0,
      32,     0,    33,     0,     0,     0,     0,    60,     0,     0,
       0,    34,    35,    36,   469,   295,     0,    38,     0,   296,
     470,    40,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,    41,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,   343,    27,
      28,     0,     0,   344,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,    29,     0,     0,     0,     0,
       0,     0,   249,     0,    59,    30,    31,   250,    32,     0,
      33,     0,    22,   251,     0,    60,     0,     0,     0,    34,
      35,    36,   252,    61,     0,    38,     0,    39,     0,    40,
       0,     0,    62,    63,     0,     0,     0,     0,   253,     0,
       0,    41,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,     0,
       0,   254,     0,   255,     0,   256,     0,   257,     0,    58,
       0,     0,     0,    29,   258,     0,     0,     0,     0,   259,
     249,     0,    59,    30,    31,   250,    32,     0,    33,     0,
      22,   251,     0,    60,     0,     0,     0,    34,    35,    36,
     252,    61,     0,    38,   477,    39,     0,    40,     0,     0,
      62,    63,     0,     0,     0,     0,   253,     0,     0,    41,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,     0,     0,   269,
       0,   255,     0,   270,     0,   271,     0,    58,     0,     0,
       0,    29,   258,     0,     0,     0,     0,   259,   249,     0,
      59,    30,    31,   250,    32,     0,    33,     0,    22,   251,
       0,    60,     0,     0,     0,    34,    35,    36,   252,    61,
       0,    38,   591,    39,     0,    40,     0,     0,    62,    63,
       0,     0,     0,     0,   253,     0,     0,    41,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,     0,     0,   336,     0,   255,
       0,   270,     0,   271,     0,    58,     0,     0,     0,    29,
     258,     0,     0,     0,     0,   259,   249,     0,    59,    30,
      31,   250,    32,     0,    33,     0,    22,   251,     0,    60,
       0,     0,     0,    34,    35,    36,   252,    61,     0,    38,
       0,    39,     0,    40,     0,     0,    62,    63,     0,     0,
       0,     0,   253,     0,     0,    41,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,     0,     0,   393,     0,   255,     0,   270,
       0,   271,     0,    58,     0,     0,     0,    29,   258,     0,
       0,     0,     0,   259,     0,     0,    59,    30,    31,     0,
      32,     0,    33,     0,     0,     0,     0,    60,     0,     0,
       0,    34,    35,    36,     0,   295,     0,    38,     0,   296,
       0,    40,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,    41,   448,   449,   450,   451,   452,   453,
     454,   455,    20,   456,    22,   457,   458,    25,   459,   460,
      28,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,    29,     0,     0,     0,     0,
       0,     0,     0,     0,    59,    30,    31,     0,    32,     0,
      33,     0,     0,     0,     0,    60,     0,     0,     0,    34,
      35,    36,     0,   461,     0,   462,     0,   463,     0,   464,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,    41,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,    29,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,     0,    32,     0,    33,     0,
       0,     0,     0,    60,     0,     0,     0,    34,    35,    36,
       0,    61,   316,    38,     0,    39,     0,    40,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    41,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,    29,     0,     0,     0,     0,     0,     0,     0,     0,
      59,    30,    31,     0,    32,     0,    33,     0,     0,     0,
       0,    60,     0,     0,     0,    34,    35,    36,     0,    61,
       0,    38,     0,    39,     0,    40,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    41,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,    29,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,     0,    32,     0,    33,     0,     0,     0,     0,    60,
       0,     0,     0,    34,    35,    36,     0,    61,     0,    38,
       0,    39,     0,    40,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    41,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,    29,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,     0,
      32,     0,    33,     0,     0,     0,     0,     0,     0,     0,
       0,    34,    35,    36,     0,    61,   304,    38,     0,    39,
       0,    40,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    41,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,    29,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,     0,    32,     0,
      33,     0,     0,     0,     0,     0,     0,     0,     0,    34,
      35,    36,     0,    61,     0,    38,     0,    39,     0,    40,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    41,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    29,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,     0,    32,     0,    33,     0,
       0,     0,     0,     0,     0,     0,     0,    34,    35,    36,
       0,    61,     0,    38,     0,    39,     0,    40,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    41,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    29,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    31,     0,     0,     0,     0,   289,     0,     0,
       0,     0,     0,     0,     0,    34,    35,     0,     0,    61,
       0,    38,     0,    39,     0,    40,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    41,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,     0,     0,     0,     0,     0,
       0,   183,   184,   185,   186,   187,   188,   189,   190,    29,
     191,    22,   192,   193,    25,   194,   195,     0,     0,     0,
      31,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   358,    34,    35,     0,     0,    61,     0,    38,
       0,    39,     0,    40,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    41,     0,     0,     0,     0,
     196,     0,   197,     0,   198,     0,   199,   183,   184,   185,
     186,   187,   188,   189,   190,     0,   191,    22,   192,   193,
      25,   194,   195,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   196,     0,   197,     0,
     198,     0,   199
};

static const yytype_int16 yycheck[] =
{
       2,     3,    54,    38,   206,   207,   208,   209,     8,     9,
      29,    32,    44,    34,     1,   198,    63,    59,   544,    88,
     196,   197,   556,    62,    63,   117,   416,    58,   154,    31,
     632,     8,   434,     8,   636,     8,    38,     8,    34,    41,
      25,    25,    25,    69,    34,     0,     7,     8,     9,    10,
      11,    12,    13,    33,    64,    65,    82,    44,    60,    20,
     112,    41,    16,   115,    25,    26,   105,    36,    38,    25,
      39,    25,    26,    25,    35,    85,    38,    25,    74,    78,
      80,    33,    38,    83,    80,    81,    86,    77,    75,   120,
      33,   133,   113,    33,    79,    79,    79,    41,    41,   501,
      61,    41,    25,    43,    81,   124,    81,    80,    88,    80,
      80,    72,    73,    34,    41,   117,   155,    33,    79,    88,
     246,   156,   248,    84,    78,    41,    88,    43,     7,     8,
       9,    10,    11,    12,    13,    25,    62,    63,   169,    38,
     742,    20,   161,    46,    88,    88,    25,    26,    88,    25,
      53,    74,     8,    74,    25,   157,    35,   196,   197,    15,
     162,    88,   201,    25,    20,   555,   205,    38,   210,    25,
      26,    38,    88,    77,    25,   709,    31,    81,   704,    35,
     706,    80,    61,    43,    74,   254,   255,    38,   219,   220,
     221,    46,     8,    72,    73,    51,    43,    36,    74,    15,
     269,    78,    78,   272,    20,    84,   298,    69,    41,    25,
      26,    44,    74,    80,    25,    39,    78,    25,    74,    35,
      76,    25,    78,   225,    80,    64,    65,    38,    88,   155,
      38,    87,    15,    43,    38,    51,    92,    20,   290,    80,
     227,    88,    25,    26,   770,   287,    85,    88,    75,    43,
      33,    77,    35,    28,    80,    88,   295,   259,    74,    25,
      76,    88,    78,    74,    80,    77,   266,   336,    51,    81,
      74,    87,    38,   294,    31,   201,    92,    25,    88,   205,
     463,    69,    39,   285,   323,   461,   462,    31,    75,    46,
      38,    74,    75,    76,    88,    78,   298,    80,    25,    80,
      25,    88,    46,    60,    87,    15,    38,    88,   327,    92,
      20,    38,    39,    38,    31,    25,    26,    74,   387,    76,
     322,   390,   324,   392,   393,    35,   328,   329,   330,    46,
      74,    25,    76,    75,    79,    36,    25,    25,    39,    81,
      25,    51,   344,    88,    38,    25,   348,   379,   350,    38,
      38,   420,    69,    38,   356,    46,   358,    74,    38,    76,
      25,    25,    25,    25,    74,    75,    76,    25,    78,    16,
      80,    25,    25,    38,    38,    38,    38,    87,    25,    26,
      38,    80,    92,    80,    38,    38,     3,     4,     5,    88,
     409,    88,   379,   432,   441,   447,   435,   323,    38,     8,
      40,    32,   441,    79,    44,    79,    46,    47,    32,    49,
      50,    79,    88,    80,    88,   484,   416,   486,    38,    86,
      88,    34,   461,   462,    74,    38,    75,   462,    78,    63,
      80,    20,    81,    75,    75,   437,    25,     1,     2,    81,
      81,     5,     6,     7,     8,     9,    10,    11,    88,   518,
      20,   520,    75,    75,    38,    25,   525,    20,    81,    81,
     462,    75,    25,   502,   503,    29,    30,   469,    20,    20,
     472,    35,    75,    25,    25,    39,    40,    43,    44,    45,
      44,    75,   551,   552,   553,   554,    92,    79,   490,    81,
      75,   493,    75,    57,    40,    41,    42,    75,    62,    63,
      15,    16,    17,    15,    19,    20,   432,    75,    20,   435,
      79,    75,    81,    25,    26,   441,    80,    93,    94,    83,
      84,   723,    86,    35,    88,    47,    48,    49,    50,    51,
      52,    53,    54,   572,    96,   222,   223,   224,    79,    51,
      81,   105,   106,    79,   583,    81,    80,    81,    80,    81,
     114,    80,    81,   117,    75,   555,   577,     8,     9,    75,
     124,    75,    74,    38,    76,    77,    78,    75,    80,    10,
      11,    75,   774,    75,    75,    87,   502,   503,    75,    75,
      92,    75,   584,    38,   586,   149,   150,    67,    33,   153,
     154,   155,   594,    43,   596,    41,    81,   161,    38,   163,
     211,   212,   213,   214,   215,   216,   217,   218,    80,   661,
      80,    37,    77,   682,    33,    82,    38,   686,    38,    38,
      32,    40,    41,   662,   663,    44,    85,    46,    47,    38,
      49,    50,   196,   197,   198,   199,    46,   201,    75,    34,
      46,   205,   711,    92,    88,    14,   572,    81,    34,   651,
     652,   653,    14,    36,    75,   657,    38,   583,    75,    77,
     662,   663,    34,   227,   228,    80,    80,    68,    38,    88,
     672,    79,    77,    77,    34,    82,    34,    80,    80,    77,
      80,    80,   246,    80,   248,    74,    80,    75,   252,   253,
     254,   255,   256,   257,   258,    75,    75,    75,    75,   263,
     264,    34,   266,    34,   268,   269,   270,   271,   272,    38,
     712,    58,   714,    34,    81,    38,    34,    39,    38,    38,
      79,    38,   286,   725,    39,    38,    33,   291,    77,    81,
      38,   295,   296,    38,   298,    38,   662,   663,    75,   227,
     543,   633,   637,   745,   640,   747,   748,   267,   420,   638,
     298,   286,   583,   755,   291,   722,   654,   405,   442,   323,
     255,   649,   253,   327,   524,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   336,    -1,   776,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   784,    -1,    -1,   349,    -1,    -1,    -1,   791,
      -1,    -1,   794,    -1,    -1,   797,    -1,    -1,   800,    -1,
      -1,   803,    -1,    -1,   806,    -1,    -1,   809,    -1,    -1,
     812,    -1,    -1,   815,    -1,   379,   818,    -1,    -1,   821,
      -1,    -1,   824,   387,    -1,    -1,   390,    -1,   392,   393,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   405,    -1,   407,   408,   409,   410,   411,   412,   413,
     414,    84,   416,    -1,   418,   419,   420,    -1,    -1,    -1,
      -1,    -1,   426,   427,   428,   429,    -1,    -1,   432,    -1,
     434,   435,    -1,    -1,    -1,    -1,   440,   441,    -1,    -1,
      -1,    15,    16,    17,    18,    19,    20,    21,    22,    -1,
      24,    25,    26,    27,    28,    29,    30,   461,   462,   463,
     464,    -1,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    46,    51,    52,    53,    54,    -1,    -1,    -1,
     484,    -1,   486,    -1,    -1,    -1,    64,    65,    -1,    -1,
     494,    69,    -1,    -1,    -1,    -1,    -1,   501,   502,   503,
      74,    -1,    76,   507,    78,    -1,    80,    85,   512,    -1,
      -1,   515,    -1,   517,   518,   519,   520,    -1,   522,    97,
     524,   525,    -1,   527,    -1,    -1,   530,   531,   532,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   543,
      -1,    -1,    -1,    -1,   548,    -1,    -1,   551,   552,   553,
     554,   555,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   572,    -1,
      -1,    -1,   576,    -1,    -1,    -1,    -1,    15,    -1,   583,
      -1,   254,    20,   256,   257,    -1,    -1,    25,    26,    -1,
     263,   595,    -1,    -1,    -1,    -1,    34,    35,    -1,    -1,
     604,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    15,    -1,    51,    -1,    -1,    20,    -1,    -1,    -1,
      -1,    25,    26,   627,   628,   629,   630,   631,    -1,   633,
      -1,    35,    -1,   637,   638,    -1,    74,    -1,    76,    77,
      78,    -1,    80,   647,    -1,   649,   650,    51,    -1,    87,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,   662,   663,
      -1,    -1,    -1,   336,    -1,    -1,    -1,    -1,    -1,   673,
      74,    -1,    76,    -1,    78,    -1,    80,    -1,   682,   683,
      -1,    -1,   686,    87,   688,    -1,    90,    -1,    92,    -1,
     694,   695,   696,   697,   698,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   708,    -1,    -1,   711,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   722,    -1,
      -1,    -1,   726,   727,    -1,   729,    -1,    -1,   732,   733,
      -1,   735,   405,    -1,   407,   408,    -1,   410,   411,   412,
     413,   414,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   756,   426,   427,   428,   429,    -1,    -1,    15,
      -1,    -1,    -1,   767,    20,   769,    -1,    -1,    -1,    25,
      26,    -1,    -1,   777,   778,   779,    -1,    33,    -1,    35,
      -1,   785,    -1,    -1,    -1,    -1,    -1,    -1,   792,    -1,
      -1,   795,    -1,    -1,   798,    51,    -1,   801,    -1,    -1,
     804,    -1,    -1,   807,    -1,    -1,   810,    -1,    -1,   813,
      -1,   484,   816,    -1,    -1,   819,    -1,    -1,    74,    75,
      76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,   507,    -1,    92,    -1,    -1,   512,
      -1,    -1,   515,    -1,    -1,   518,   519,    -1,    -1,    -1,
      -1,    -1,   525,    -1,   527,    -1,    -1,   530,   531,   532,
       6,     7,    -1,     9,    10,    11,    12,    13,    -1,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      56,    57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    -1,    74,    -1,
      76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,
      -1,    -1,    -1,    -1,   627,   628,   629,   630,   631,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     9,    10,    11,    12,
      13,    -1,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,
     683,    -1,    -1,    56,    57,   688,    59,    -1,    61,    -1,
      -1,   694,   695,   696,   697,   698,    -1,    70,    71,    72,
      -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,
      -1,    -1,    95,    -1,   727,    -1,   729,    -1,     8,   732,
     733,    -1,   735,    -1,    -1,    15,    16,    17,    18,    19,
      20,    21,    22,    -1,    24,    25,    26,    27,    28,    29,
      30,    -1,    -1,    33,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   767,    -1,   769,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   778,   779,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    74,    -1,    76,    -1,    78,    -1,
      80,    -1,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    59,    -1,    61,    -1,    -1,    64,    65,    66,    -1,
      -1,    69,    70,    71,    72,    -1,    74,    -1,    76,    -1,
      78,    -1,    80,    -1,    -1,    83,    84,    85,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,    97,
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
      29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    42,    -1,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    55,    56,    57,    -1,
      59,    -1,    61,    -1,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    -1,    76,    -1,    78,
      79,    80,    -1,    -1,    83,    84,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    92,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    -1,    -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    42,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      -1,    -1,    15,    -1,    55,    56,    57,    20,    59,    -1,
      61,    -1,    25,    26,    -1,    66,    -1,    -1,    -1,    70,
      71,    72,    35,    74,    -1,    76,    -1,    78,    -1,    80,
      -1,    -1,    83,    84,    -1,    -1,    -1,    -1,    51,    -1,
      -1,    92,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    -1,
      -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,    42,
      -1,    -1,    -1,    46,    87,    -1,    -1,    -1,    -1,    92,
      15,    -1,    55,    56,    57,    20,    59,    -1,    61,    -1,
      25,    26,    -1,    66,    -1,    -1,    -1,    70,    71,    72,
      35,    74,    -1,    76,    77,    78,    -1,    80,    -1,    -1,
      83,    84,    -1,    -1,    -1,    -1,    51,    -1,    -1,    92,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    -1,    -1,    74,
      -1,    76,    -1,    78,    -1,    80,    -1,    42,    -1,    -1,
      -1,    46,    87,    -1,    -1,    -1,    -1,    92,    15,    -1,
      55,    56,    57,    20,    59,    -1,    61,    -1,    25,    26,
      -1,    66,    -1,    -1,    -1,    70,    71,    72,    35,    74,
      -1,    76,    77,    78,    -1,    80,    -1,    -1,    83,    84,
      -1,    -1,    -1,    -1,    51,    -1,    -1,    92,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    -1,    -1,    74,    -1,    76,
      -1,    78,    -1,    80,    -1,    42,    -1,    -1,    -1,    46,
      87,    -1,    -1,    -1,    -1,    92,    15,    -1,    55,    56,
      57,    20,    59,    -1,    61,    -1,    25,    26,    -1,    66,
      -1,    -1,    -1,    70,    71,    72,    35,    74,    -1,    76,
      -1,    78,    -1,    80,    -1,    -1,    83,    84,    -1,    -1,
      -1,    -1,    51,    -1,    -1,    92,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    -1,    -1,    74,    -1,    76,    -1,    78,
      -1,    80,    -1,    42,    -1,    -1,    -1,    46,    87,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    55,    56,    57,    -1,
      59,    -1,    61,    -1,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    70,    71,    72,    -1,    74,    -1,    76,    -1,    78,
      -1,    80,    -1,    -1,    83,    84,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    92,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    42,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    55,    56,    57,    -1,    59,    -1,
      61,    -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,    70,
      71,    72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,
      -1,    -1,    83,    84,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    92,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,
      -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    56,    57,    -1,    59,    -1,    61,    -1,
      -1,    -1,    -1,    66,    -1,    -1,    -1,    70,    71,    72,
      -1,    74,    75,    76,    -1,    78,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      55,    56,    57,    -1,    59,    -1,    61,    -1,    -1,    -1,
      -1,    66,    -1,    -1,    -1,    70,    71,    72,    -1,    74,
      -1,    76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,    46,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    56,
      57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,    66,
      -1,    -1,    -1,    70,    71,    72,    -1,    74,    -1,    76,
      -1,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    92,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    42,    -1,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    56,    57,    -1,
      59,    -1,    61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    -1,    74,    75,    76,    -1,    78,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    92,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    42,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    56,    57,    -1,    59,    -1,
      61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    92,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    56,    57,    -1,    59,    -1,    61,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    57,    -1,    -1,    -1,    -1,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    74,
      -1,    76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,
      -1,    15,    16,    17,    18,    19,    20,    21,    22,    46,
      24,    25,    26,    27,    28,    29,    30,    -1,    -1,    -1,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    70,    71,    -1,    -1,    74,    -1,    76,
      -1,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,
      74,    -1,    76,    -1,    78,    -1,    80,    15,    16,    17,
      18,    19,    20,    21,    22,    -1,    24,    25,    26,    27,
      28,    29,    30,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    76,    -1,
      78,    -1,    80
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,    99,     6,     7,     9,    10,    11,
      12,    13,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    46,
      56,    57,    59,    61,    70,    71,    72,    74,    76,    78,
      80,    92,    95,   100,   101,   102,   103,   104,   106,   107,
     109,   114,   119,   129,   137,   145,   159,   181,    42,    55,
      66,    74,    83,    84,   123,   124,   125,   126,   127,   128,
     129,   181,   123,   181,     0,   181,   181,   172,   181,    74,
     118,   119,   181,   118,    74,   164,   181,   164,   181,     7,
       8,     9,    10,    11,    12,    13,    20,    26,    35,    61,
      72,    73,    84,   158,   181,    74,    78,   130,   131,   151,
     181,   123,   137,   146,    78,   137,   181,    78,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    64,    65,    69,    85,    97,   120,
     123,   162,   123,   162,   157,   158,    20,   181,   123,    93,
      94,   105,   102,    39,    39,    60,    74,    76,   159,    28,
      31,    46,    38,   181,   127,   125,   123,    40,    41,    42,
      43,    44,    45,    47,    48,    49,    51,    52,    53,    54,
      64,    65,    69,    15,    16,    17,    18,    19,    20,    21,
      22,    24,    26,    27,    29,    30,    74,    76,    78,    80,
     145,   149,   150,   152,   181,   149,    36,    64,    65,    85,
      69,    47,    48,    49,    50,    51,    52,    53,    54,    40,
      41,    42,    43,    44,    45,    38,   100,     8,    46,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    51,
      52,    53,    54,    64,    65,    69,    38,   119,    38,    15,
      20,    26,    35,    51,    74,    76,    78,    80,    87,    92,
     165,   166,   170,   181,    32,   117,   118,   181,    32,    74,
      78,    80,   122,   169,   170,   173,   175,   176,   181,   152,
     153,   154,   155,   156,   181,    69,    82,    38,    63,    62,
     137,   138,   139,   181,    38,    74,    78,   123,   132,   133,
     151,   181,    75,    75,    75,    75,    75,    75,    75,    75,
      75,    75,    75,    75,    75,    75,    75,    75,    75,    75,
      75,    75,    81,    80,    86,    77,    79,    81,    38,    38,
      38,    80,    92,   181,   181,    96,    74,   163,   164,   169,
     163,   152,   162,    29,    34,   123,   158,   123,    38,   181,
      67,   153,   153,   155,    20,   181,    46,   152,    46,   124,
     124,   124,   124,   125,   126,   126,   126,   126,   126,   126,
     126,   126,   127,   127,   127,   128,   128,   128,   123,   101,
     181,   163,   163,   181,   172,    33,    75,   168,   169,   170,
      34,    77,   168,    74,   170,   177,   181,   170,   178,   179,
     180,   181,   181,   123,    75,    81,    38,    40,    44,    46,
      47,    49,    50,    88,   167,   170,   181,     8,    80,   120,
     181,   169,   181,   181,     8,   169,    41,    33,    43,    41,
      75,    81,    81,    79,    81,    38,   123,    69,   131,   125,
      80,    80,   147,   148,    79,   139,    37,   137,    15,    16,
      17,    18,    19,    20,    21,    22,    24,    26,    27,    29,
      30,    74,    76,    78,    80,   145,   181,   181,    82,    73,
      79,   133,    38,   123,   135,   136,   152,    77,   123,    79,
     158,   123,   123,   123,    74,   170,    32,    75,    77,   123,
      34,    77,   123,    38,   181,   123,    75,    81,    77,    81,
      79,    81,    38,    38,    80,   123,   123,    46,    53,    75,
      75,   169,    43,   169,    77,    43,    79,    81,    34,    41,
      44,    80,    81,    80,    81,    34,    74,    46,    92,   166,
      74,    78,    80,   170,   170,   158,   170,   170,   170,    90,
     170,   170,   117,    14,   112,   113,   110,   111,   121,   181,
     181,   122,    81,    34,    34,    14,   115,   116,   170,   170,
     170,   170,    75,   152,    79,   156,   152,   123,    20,   160,
     161,   181,   149,   148,   140,   141,   142,    69,    20,   181,
     123,   123,    77,    80,    36,    77,    38,    80,    80,   169,
      77,    77,   123,   123,    38,   181,    68,    75,    77,    79,
     152,   152,   170,   170,    80,   170,   181,   169,   170,   170,
     169,   181,   180,   181,   169,   170,    15,    16,    17,    19,
      20,   170,   170,   170,   181,   170,   181,    38,    36,    39,
      36,    39,     8,    80,   107,   113,     8,    81,    33,   181,
       8,   169,   174,   169,   108,   117,   116,    34,    80,    81,
      34,    38,     8,    33,    80,    16,    26,    78,   143,   144,
     181,   137,    38,    38,    80,    79,    82,   135,   123,   123,
      77,   123,    38,   181,   123,    80,    80,    75,    20,   171,
     181,    79,    34,    43,    80,    80,    34,    80,    41,    75,
      75,    75,    75,    75,    43,    43,    34,    41,    34,   170,
     170,   170,   170,   170,   112,   110,   112,   111,   121,   115,
      75,    81,    38,   181,    58,   161,   181,   123,   123,   123,
     141,   123,    34,    81,   123,    38,   181,    38,    34,    38,
     169,   170,    38,    39,   169,    39,   170,   170,   170,   170,
     170,   170,     8,   169,   123,    38,   123,    38,    33,    79,
     144,   181,   124,   134,   123,    38,   181,   170,    77,   170,
      79,   170,   170,   170,    80,    75,    79,    43,    80,    41,
     112,   123,   123,   123,    81,   123,    38,   181,    38,    38,
     170,   170,   124,   123,    38,   181,   170,   170,    79,    80,
     123,    38,   181,   123,    38,   181,   123,    38,   181,   123,
      38,   181,   123,    38,   181,   123,    38,   181,   123,    38,
     181,   123,    38,   181,   123,    38,   181,   123,    38,   181,
     123,    38,   181,   123,    38,   123
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
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
     176,   177,   177,   178,   178,   178,   178,   179,   179,   180,
     180,   180,   180,   180,   181
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
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
       3,     5,     3,     5,     3,     3,     1,     3,     1,     4,
       4,     4,     4,     4,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


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


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

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

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


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

  YYLOCATION_PRINT (yyo, yylocationp);
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
    YYNOMEM;
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
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
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
#line 524 "hexpr.y"
                            { yyParsedModule = (yyvsp[0].module);                     }
#line 2919 "hexpr.parse.C"
    break;

  case 3: /* s: "dodefn" id "=" l0expr  */
#line 525 "hexpr.y"
                            { yyParsedVar    = *(yyvsp[-2].string); yyParsedExpr = (yyvsp[0].exp); }
#line 2925 "hexpr.parse.C"
    break;

  case 4: /* s: "dodefn" l0expr  */
#line 526 "hexpr.y"
                            { yyParsedVar    = "";  yyParsedExpr = (yyvsp[0].exp); }
#line 2931 "hexpr.parse.C"
    break;

  case 5: /* s: "doexpr" l0expr  */
#line 527 "hexpr.y"
                            { yyParsedExpr   = (yyvsp[0].exp);                     }
#line 2937 "hexpr.parse.C"
    break;

  case 6: /* module: "option" id module  */
#line 530 "hexpr.y"
                                 { (yyval.module) = (yyvsp[0].module); (yyval.module)->setOption(*(yyvsp[-1].string), m((yylsp[-2]))); }
#line 2943 "hexpr.parse.C"
    break;

  case 7: /* module: "module" id "where" defs  */
#line 531 "hexpr.y"
                                 { (yyval.module) = new Module(*(yyvsp[-2].string), *(yyvsp[0].mdefs)); }
#line 2949 "hexpr.parse.C"
    break;

  case 8: /* module: defs  */
#line 532 "hexpr.y"
                                 { (yyval.module) = new Module(freshName(), *(yyvsp[0].mdefs)); }
#line 2955 "hexpr.parse.C"
    break;

  case 9: /* defs: %empty  */
#line 534 "hexpr.y"
                    { (yyval.mdefs) = autorelease(new ModuleDefs()); }
#line 2961 "hexpr.parse.C"
    break;

  case 10: /* defs: def  */
#line 535 "hexpr.y"
                    { (yyval.mdefs) = autorelease(new ModuleDefs()); (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2967 "hexpr.parse.C"
    break;

  case 11: /* defs: defs def  */
#line 536 "hexpr.y"
                    { (yyval.mdefs) = (yyvsp[-1].mdefs);                            (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2973 "hexpr.parse.C"
    break;

  case 12: /* def: importdef  */
#line 538 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2979 "hexpr.parse.C"
    break;

  case 13: /* def: tydef  */
#line 539 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2985 "hexpr.parse.C"
    break;

  case 14: /* def: vartybind  */
#line 540 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mvtydef); }
#line 2991 "hexpr.parse.C"
    break;

  case 15: /* def: classdef  */
#line 541 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2997 "hexpr.parse.C"
    break;

  case 16: /* def: instdef  */
#line 542 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 3003 "hexpr.parse.C"
    break;

  case 17: /* def: pragmadef  */
#line 543 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 3009 "hexpr.parse.C"
    break;

  case 18: /* def: id "=" l0expr  */
#line 545 "hexpr.y"
                   { (yyval.mdef) = new MVarDef(list(*(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3015 "hexpr.parse.C"
    break;

  case 19: /* def: id id "=" l0expr  */
#line 546 "hexpr.y"
                      { (yyval.mdef) = new MVarDef(list(*(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3021 "hexpr.parse.C"
    break;

  case 20: /* def: id id id "=" l0expr  */
#line 547 "hexpr.y"
                         { (yyval.mdef) = new MVarDef(list(*(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3027 "hexpr.parse.C"
    break;

  case 21: /* def: id id id id "=" l0expr  */
#line 548 "hexpr.y"
                            { (yyval.mdef) = new MVarDef(list(*(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3033 "hexpr.parse.C"
    break;

  case 22: /* def: id id id id id "=" l0expr  */
#line 549 "hexpr.y"
                               { (yyval.mdef) = new MVarDef(list(*(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 3039 "hexpr.parse.C"
    break;

  case 23: /* def: id id id id id id "=" l0expr  */
#line 550 "hexpr.y"
                                  { (yyval.mdef) = new MVarDef(list(*(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3045 "hexpr.parse.C"
    break;

  case 24: /* def: id id id id id id id "=" l0expr  */
#line 551 "hexpr.y"
                                     { (yyval.mdef) = new MVarDef(list(*(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 3051 "hexpr.parse.C"
    break;

  case 25: /* def: id id id id id id id id "=" l0expr  */
#line 552 "hexpr.y"
                                        { (yyval.mdef) = new MVarDef(list(*(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-9]), (yylsp[0]))); }
#line 3057 "hexpr.parse.C"
    break;

  case 26: /* def: id id id id id id id id id "=" l0expr  */
#line 553 "hexpr.y"
                                           { (yyval.mdef) = new MVarDef(list(*(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-10]), (yylsp[0]))); }
#line 3063 "hexpr.parse.C"
    break;

  case 27: /* def: id id id id id id id id id id "=" l0expr  */
#line 554 "hexpr.y"
                                              { (yyval.mdef) = new MVarDef(list(*(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-11]), (yylsp[0]))); }
#line 3069 "hexpr.parse.C"
    break;

  case 28: /* def: id id id id id id id id id id id "=" l0expr  */
#line 555 "hexpr.y"
                                                 { (yyval.mdef) = new MVarDef(list(*(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-12]), (yylsp[0]))); }
#line 3075 "hexpr.parse.C"
    break;

  case 29: /* def: id id id id id id id id id id id id "=" l0expr  */
#line 556 "hexpr.y"
                                                    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-13]), (yylsp[0]))); }
#line 3081 "hexpr.parse.C"
    break;

  case 30: /* def: id id id id id id id id id id id id id "=" l0expr  */
#line 557 "hexpr.y"
                                                       { (yyval.mdef) = new MVarDef(list(*(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-14]), (yylsp[0]))); }
#line 3087 "hexpr.parse.C"
    break;

  case 31: /* def: id id id id id id id id id id id id id id "=" l0expr  */
#line 558 "hexpr.y"
                                                          { (yyval.mdef) = new MVarDef(list(*(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-15]), (yylsp[0]))); }
#line 3093 "hexpr.parse.C"
    break;

  case 32: /* def: id id id id id id id id id id id id id id id "=" l0expr  */
#line 559 "hexpr.y"
                                                             { (yyval.mdef) = new MVarDef(list(*(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-16]), (yylsp[0]))); }
#line 3099 "hexpr.parse.C"
    break;

  case 33: /* def: id id id id id id id id id id id id id id id id "=" l0expr  */
#line 560 "hexpr.y"
                                                                { (yyval.mdef) = new MVarDef(list(*(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-17]), (yylsp[0]))); }
#line 3105 "hexpr.parse.C"
    break;

  case 34: /* def: id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 561 "hexpr.y"
                                                                   { (yyval.mdef) = new MVarDef(list(*(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-18]), (yylsp[0]))); }
#line 3111 "hexpr.parse.C"
    break;

  case 35: /* def: id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 562 "hexpr.y"
                                                                      { (yyval.mdef) = new MVarDef(list(*(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-19]), (yylsp[0]))); }
#line 3117 "hexpr.parse.C"
    break;

  case 36: /* def: id id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 563 "hexpr.y"
                                                                         { (yyval.mdef) = new MVarDef(list(*(yyvsp[-20].string), *(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-20]), (yylsp[0]))); }
#line 3123 "hexpr.parse.C"
    break;

  case 37: /* def: id id id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 564 "hexpr.y"
                                                                            { (yyval.mdef) = new MVarDef(list(*(yyvsp[-21].string), *(yyvsp[-20].string), *(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-21]), (yylsp[0]))); }
#line 3129 "hexpr.parse.C"
    break;

  case 38: /* def: id id id id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 565 "hexpr.y"
                                                                               { (yyval.mdef) = new MVarDef(list(*(yyvsp[-22].string), *(yyvsp[-21].string), *(yyvsp[-20].string), *(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-22]), (yylsp[0]))); }
#line 3135 "hexpr.parse.C"
    break;

  case 39: /* def: l5expr  */
#line 568 "hexpr.y"
            { (yyval.mdef) = new MVarDef(list(freshName()), let(freshName(), ExprPtr((yyvsp[0].exp)), mktunit(m((yylsp[0]))), m((yylsp[0]))), m((yylsp[0]))); }
#line 3141 "hexpr.parse.C"
    break;

  case 40: /* importdef: "import" cppid  */
#line 571 "hexpr.y"
                          { (yyval.mdef) = new MImport(yyModulePath, *(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3147 "hexpr.parse.C"
    break;

  case 41: /* pragmadef: "{-#" pragmaty "#-}"  */
#line 574 "hexpr.y"
                                { (yyval.mdef) = (yyvsp[-1].mdef); }
#line 3153 "hexpr.parse.C"
    break;

  case 42: /* pragmaty: "UNSAFE" id  */
#line 575 "hexpr.y"
                      { (yyval.mdef) = new MUnsafePragmaDef(*(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3159 "hexpr.parse.C"
    break;

  case 43: /* pragmaty: "SAFE" id  */
#line 576 "hexpr.y"
                    { (yyval.mdef) = new MSafePragmaDef(*(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3165 "hexpr.parse.C"
    break;

  case 44: /* tydef: "type" nameseq "=" qtype  */
#line 579 "hexpr.y"
                                { (yyval.mdef) = new MTypeDef(MTypeDef::Transparent, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 3171 "hexpr.parse.C"
    break;

  case 45: /* tydef: "data" nameseq "=" qtype  */
#line 580 "hexpr.y"
                                { (yyval.mdef) = new MTypeDef(MTypeDef::Opaque, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 3177 "hexpr.parse.C"
    break;

  case 46: /* vartybind: name "::" qtype  */
#line 583 "hexpr.y"
                           { (yyval.mvtydef) = new MVarTypeDef(*(yyvsp[-2].string), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]), (yylsp[0]))); }
#line 3183 "hexpr.parse.C"
    break;

  case 47: /* vardef: names "=" l0expr  */
#line 585 "hexpr.y"
                         { (yyval.mvdef) = new MVarDef(*(yyvsp[-2].strings), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3189 "hexpr.parse.C"
    break;

  case 48: /* classdef: "class" cst "=>" id names  */
#line 588 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-3].tconstraints), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3195 "hexpr.parse.C"
    break;

  case 49: /* classdef: "class" cst "=>" id names "|" fundeps  */
#line 589 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-6]), (yylsp[0]))); wantIndent(false); }
#line 3201 "hexpr.parse.C"
    break;

  case 50: /* classdef: "class" cst "=>" id names "where" cmembers  */
#line 590 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 3207 "hexpr.parse.C"
    break;

  case 51: /* classdef: "class" cst "=>" id names "|" fundeps "where" cmembers  */
#line 591 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-7].tconstraints), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-8]), (yylsp[0])));            wantIndent(false); }
#line 3213 "hexpr.parse.C"
    break;

  case 52: /* classdef: "class" id names  */
#line 592 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 3219 "hexpr.parse.C"
    break;

  case 53: /* classdef: "class" id names "|" fundeps  */
#line 593 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3225 "hexpr.parse.C"
    break;

  case 54: /* classdef: "class" id names "where" cmembers  */
#line 594 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-4]), (yylsp[0])));            wantIndent(false); }
#line 3231 "hexpr.parse.C"
    break;

  case 55: /* classdef: "class" id names "|" fundeps "where" cmembers  */
#line 595 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 3237 "hexpr.parse.C"
    break;

  case 56: /* fundeps: fundep  */
#line 597 "hexpr.y"
                            { (yyval.fundeps) = autorelease(new CFunDepDefs()); (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 3243 "hexpr.parse.C"
    break;

  case 57: /* fundeps: fundeps "," fundep  */
#line 598 "hexpr.y"
                            { (yyval.fundeps) = (yyvsp[-2].fundeps);                             (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 3249 "hexpr.parse.C"
    break;

  case 58: /* fundep: idseq "->" idseq  */
#line 600 "hexpr.y"
                         { (yyval.fundep) = autorelease(new CFunDepDef(*(yyvsp[-2].strings), *(yyvsp[0].strings))); }
#line 3255 "hexpr.parse.C"
    break;

  case 59: /* cmembers: cmember  */
#line 602 "hexpr.y"
                           { (yyval.mvtydefs) = autorelease(new MVarTypeDefs()); (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 3261 "hexpr.parse.C"
    break;

  case 60: /* cmembers: cmembers cmember  */
#line 603 "hexpr.y"
                           { (yyval.mvtydefs) = (yyvsp[-1].mvtydefs);                              (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 3267 "hexpr.parse.C"
    break;

  case 61: /* cmember: "indent" vartybind  */
#line 605 "hexpr.y"
                            { (yyval.mvtydef) = (yyvsp[0].mvtydef); }
#line 3273 "hexpr.parse.C"
    break;

  case 62: /* instdef: "instance" id types  */
#line 608 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 3279 "hexpr.parse.C"
    break;

  case 63: /* instdef: "instance" cst "=>" id types  */
#line 609 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(*(yyvsp[-3].tconstraints),           *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3285 "hexpr.parse.C"
    break;

  case 64: /* instdef: "instance" id types "where" imembers  */
#line 610 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-4]), (yylsp[0])));        wantIndent(false); }
#line 3291 "hexpr.parse.C"
    break;

  case 65: /* instdef: "instance" cst "=>" id types "where" imembers  */
#line 611 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(*(yyvsp[-5].tconstraints),           *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-6]), (yylsp[0])));        wantIndent(false); }
#line 3297 "hexpr.parse.C"
    break;

  case 66: /* imembers: imember  */
#line 613 "hexpr.y"
                           { (yyval.mvdefs) = autorelease(new MVarDefs()); (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 3303 "hexpr.parse.C"
    break;

  case 67: /* imembers: imembers imember  */
#line 614 "hexpr.y"
                           { (yyval.mvdefs) = (yyvsp[-1].mvdefs);                          (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 3309 "hexpr.parse.C"
    break;

  case 68: /* imember: "indent" vardef  */
#line 616 "hexpr.y"
                         { (yyval.mvdef) = (yyvsp[0].mvdef); }
#line 3315 "hexpr.parse.C"
    break;

  case 69: /* names: nameseq  */
#line 619 "hexpr.y"
               { (yyval.strings) = (yyvsp[0].strings); }
#line 3321 "hexpr.parse.C"
    break;

  case 70: /* names: id opname id  */
#line 621 "hexpr.y"
                    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[-1].string)); (yyval.strings)->push_back(*(yyvsp[-2].string)); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3327 "hexpr.parse.C"
    break;

  case 71: /* nameseq: name  */
#line 623 "hexpr.y"
                      { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3333 "hexpr.parse.C"
    break;

  case 72: /* nameseq: nameseq name  */
#line 624 "hexpr.y"
                      { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3339 "hexpr.parse.C"
    break;

  case 73: /* name: id  */
#line 626 "hexpr.y"
         { (yyval.string) = (yyvsp[0].string); }
#line 3345 "hexpr.parse.C"
    break;

  case 74: /* name: "(" opname ")"  */
#line 628 "hexpr.y"
                     { (yyval.string) = (yyvsp[-1].string); }
#line 3351 "hexpr.parse.C"
    break;

  case 75: /* opname: "and"  */
#line 630 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("and")); }
#line 3357 "hexpr.parse.C"
    break;

  case 76: /* opname: "or"  */
#line 631 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("or")); }
#line 3363 "hexpr.parse.C"
    break;

  case 77: /* opname: "o"  */
#line 632 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("compose")); }
#line 3369 "hexpr.parse.C"
    break;

  case 78: /* opname: "."  */
#line 633 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("compose")); }
#line 3375 "hexpr.parse.C"
    break;

  case 79: /* opname: "~"  */
#line 634 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("~")); }
#line 3381 "hexpr.parse.C"
    break;

  case 80: /* opname: "=~"  */
#line 635 "hexpr.y"
               { (yyval.string) = autorelease(new std::string("=~")); }
#line 3387 "hexpr.parse.C"
    break;

  case 81: /* opname: "==="  */
#line 636 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("===")); }
#line 3393 "hexpr.parse.C"
    break;

  case 82: /* opname: "=="  */
#line 637 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("==")); }
#line 3399 "hexpr.parse.C"
    break;

  case 83: /* opname: "<"  */
#line 638 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("<")); }
#line 3405 "hexpr.parse.C"
    break;

  case 84: /* opname: "<="  */
#line 639 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("<=")); }
#line 3411 "hexpr.parse.C"
    break;

  case 85: /* opname: ">"  */
#line 640 "hexpr.y"
              { (yyval.string) = autorelease(new std::string(">")); }
#line 3417 "hexpr.parse.C"
    break;

  case 86: /* opname: ">="  */
#line 641 "hexpr.y"
              { (yyval.string) = autorelease(new std::string(">=")); }
#line 3423 "hexpr.parse.C"
    break;

  case 87: /* opname: "in"  */
#line 642 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("in")); }
#line 3429 "hexpr.parse.C"
    break;

  case 88: /* opname: "++"  */
#line 643 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("append")); }
#line 3435 "hexpr.parse.C"
    break;

  case 89: /* opname: "+"  */
#line 644 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("+")); }
#line 3441 "hexpr.parse.C"
    break;

  case 90: /* opname: "-"  */
#line 645 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("-")); }
#line 3447 "hexpr.parse.C"
    break;

  case 91: /* opname: "*"  */
#line 646 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("*")); }
#line 3453 "hexpr.parse.C"
    break;

  case 92: /* opname: "/"  */
#line 647 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("/")); }
#line 3459 "hexpr.parse.C"
    break;

  case 93: /* opname: "%"  */
#line 648 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("%")); }
#line 3465 "hexpr.parse.C"
    break;

  case 94: /* idseq: id  */
#line 650 "hexpr.y"
                { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3471 "hexpr.parse.C"
    break;

  case 95: /* idseq: idseq id  */
#line 651 "hexpr.y"
                { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3477 "hexpr.parse.C"
    break;

  case 96: /* types: l0mtype  */
#line 653 "hexpr.y"
                     { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3483 "hexpr.parse.C"
    break;

  case 97: /* types: types l0mtype  */
#line 654 "hexpr.y"
                     { (yyval.mtypes) = (yyvsp[-1].mtypes);                           (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3489 "hexpr.parse.C"
    break;

  case 98: /* l0expr: "\\" patterns "." l0expr  */
#line 657 "hexpr.y"
                                 { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3495 "hexpr.parse.C"
    break;

  case 99: /* l0expr: "fn" patterns "." l0expr  */
#line 658 "hexpr.y"
                                 { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3501 "hexpr.parse.C"
    break;

  case 100: /* l0expr: lhexpr "<-" lhexpr  */
#line 659 "hexpr.y"
                                 { (yyval.exp) = new Assign(ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3507 "hexpr.parse.C"
    break;

  case 101: /* l0expr: lhexpr  */
#line 660 "hexpr.y"
                                 { (yyval.exp) = (yyvsp[0].exp); }
#line 3513 "hexpr.parse.C"
    break;

  case 102: /* lhexpr: "!" l1expr  */
#line 662 "hexpr.y"
                                 { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), (yyvsp[0].exp), m((yylsp[-1]),(yylsp[0]))); }
#line 3519 "hexpr.parse.C"
    break;

  case 103: /* lhexpr: lhexpr "and" lhexpr  */
#line 663 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("and",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3525 "hexpr.parse.C"
    break;

  case 104: /* lhexpr: lhexpr "or" lhexpr  */
#line 664 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("or",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3531 "hexpr.parse.C"
    break;

  case 105: /* lhexpr: lhexpr "o" lhexpr  */
#line 665 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("compose",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3537 "hexpr.parse.C"
    break;

  case 106: /* lhexpr: l1expr "in" l1expr  */
#line 666 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("in",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3543 "hexpr.parse.C"
    break;

  case 107: /* lhexpr: l1expr  */
#line 667 "hexpr.y"
                                 { (yyval.exp) = (yyvsp[0].exp); }
#line 3549 "hexpr.parse.C"
    break;

  case 108: /* l1expr: "if" l0expr "then" l0expr "else" l0expr  */
#line 669 "hexpr.y"
                                                { (yyval.exp) = TAPP3(var("if",m((yylsp[-5]))), (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-5]), (yylsp[0]))); }
#line 3555 "hexpr.parse.C"
    break;

  case 109: /* l1expr: l2expr  */
#line 670 "hexpr.y"
                                                { (yyval.exp) = (yyvsp[0].exp); }
#line 3561 "hexpr.parse.C"
    break;

  case 110: /* l2expr: l2expr "~" l2expr  */
#line 672 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("~",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3567 "hexpr.parse.C"
    break;

  case 111: /* l2expr: l2expr "===" l2expr  */
#line 673 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("===",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3573 "hexpr.parse.C"
    break;

  case 112: /* l2expr: l2expr "==" l2expr  */
#line 674 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3579 "hexpr.parse.C"
    break;

  case 113: /* l2expr: l2expr "!=" l2expr  */
#line 675 "hexpr.y"
                            { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))), m((yylsp[-2]),(yylsp[0]))); }
#line 3585 "hexpr.parse.C"
    break;

  case 114: /* l2expr: l2expr "<" l2expr  */
#line 676 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("<",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3591 "hexpr.parse.C"
    break;

  case 115: /* l2expr: l2expr "<=" l2expr  */
#line 677 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("<=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3597 "hexpr.parse.C"
    break;

  case 116: /* l2expr: l2expr ">" l2expr  */
#line 678 "hexpr.y"
                            { (yyval.exp) = TAPP2(var(">",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3603 "hexpr.parse.C"
    break;

  case 117: /* l2expr: l2expr ">=" l2expr  */
#line 679 "hexpr.y"
                            { (yyval.exp) = TAPP2(var(">=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3609 "hexpr.parse.C"
    break;

  case 118: /* l2expr: l3expr  */
#line 680 "hexpr.y"
                            { (yyval.exp) = (yyvsp[0].exp); }
#line 3615 "hexpr.parse.C"
    break;

  case 119: /* l3expr: l3expr "+" l3expr  */
#line 682 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("+",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3621 "hexpr.parse.C"
    break;

  case 120: /* l3expr: l3expr "-" l3expr  */
#line 683 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("-",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3627 "hexpr.parse.C"
    break;

  case 121: /* l3expr: l3expr "++" l3expr  */
#line 684 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("append",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3633 "hexpr.parse.C"
    break;

  case 122: /* l3expr: "-" l3expr  */
#line 685 "hexpr.y"
                           { (yyval.exp) = TAPP1(var("neg",m((yylsp[-1]))), ExprPtr((yyvsp[0].exp)), m((yylsp[-1]),(yylsp[0]))); }
#line 3639 "hexpr.parse.C"
    break;

  case 123: /* l3expr: l4expr  */
#line 686 "hexpr.y"
                           { (yyval.exp) = (yyvsp[0].exp); }
#line 3645 "hexpr.parse.C"
    break;

  case 124: /* l4expr: l4expr "*" l4expr  */
#line 688 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("*", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3651 "hexpr.parse.C"
    break;

  case 125: /* l4expr: l4expr "/" l4expr  */
#line 689 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("/", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3657 "hexpr.parse.C"
    break;

  case 126: /* l4expr: l4expr "%" l4expr  */
#line 690 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("%", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3663 "hexpr.parse.C"
    break;

  case 127: /* l4expr: l5expr  */
#line 691 "hexpr.y"
                          { (yyval.exp) = (yyvsp[0].exp); }
#line 3669 "hexpr.parse.C"
    break;

  case 128: /* l5expr: l6expr  */
#line 693 "hexpr.y"
               { (yyval.exp) = (yyvsp[0].exp); }
#line 3675 "hexpr.parse.C"
    break;

  case 129: /* l5expr: "let" letbindings "in" l0expr  */
#line 696 "hexpr.y"
                                      { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-2].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3681 "hexpr.parse.C"
    break;

  case 130: /* l5expr: "let" letbindings ";" "in" l0expr  */
#line 697 "hexpr.y"
                                          { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]),(yylsp[0])))->clone(); }
#line 3687 "hexpr.parse.C"
    break;

  case 131: /* l5expr: "match" l6exprs "with" patternexps  */
#line 700 "hexpr.y"
                                           { (yyval.exp) = compileMatch(yyParseCC, *(yyvsp[-2].exps), normPatternRules(*(yyvsp[0].patternexps), m((yylsp[-3]),(yylsp[0]))), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3693 "hexpr.parse.C"
    break;

  case 132: /* l5expr: l6expr "matches" pattern  */
#line 703 "hexpr.y"
                                 { (yyval.exp) = compileMatchTest(yyParseCC, ExprPtr((yyvsp[-2].exp)), PatternPtr((yyvsp[0].pattern)), m((yylsp[-2]),(yylsp[0])))->clone(); }
#line 3699 "hexpr.parse.C"
    break;

  case 133: /* l5expr: "parse" "{" prules "}"  */
#line 706 "hexpr.y"
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
#line 3714 "hexpr.parse.C"
    break;

  case 134: /* l5expr: "do" "{" dobindings "}"  */
#line 718 "hexpr.y"
                                                { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-1].letbindings), ExprPtr(new Unit(m((yylsp[-3]),(yylsp[0])))), m((yylsp[-3]),(yylsp[0]))); }
#line 3720 "hexpr.parse.C"
    break;

  case 135: /* l5expr: "do" "{" dobindings "return" l0expr "}"  */
#line 719 "hexpr.y"
                                                { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[-1].exp)), m((yylsp[-5]),(yylsp[0]))); }
#line 3726 "hexpr.parse.C"
    break;

  case 136: /* l5expr: l6expr "::" qtype  */
#line 722 "hexpr.y"
                                { (yyval.exp) = new Assump(ExprPtr((yyvsp[-2].exp)), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]),(yylsp[0]))); }
#line 3732 "hexpr.parse.C"
    break;

  case 137: /* letbindings: letbindings ";" letbinding  */
#line 724 "hexpr.y"
                                        { (yyvsp[-2].letbindings)->push_back(*(yyvsp[0].letbinding)); (yyval.letbindings) = (yyvsp[-2].letbindings); }
#line 3738 "hexpr.parse.C"
    break;

  case 138: /* letbindings: letbinding  */
#line 725 "hexpr.y"
                                        { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3744 "hexpr.parse.C"
    break;

  case 139: /* letbinding: irrefutablep "=" l1expr  */
#line 727 "hexpr.y"
                                    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-2].pattern)), ExprPtr((yyvsp[0].exp)))); }
#line 3750 "hexpr.parse.C"
    break;

  case 140: /* dobindings: dobindings dobinding  */
#line 729 "hexpr.y"
                                 { (yyval.letbindings) = (yyvsp[-1].letbindings); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3756 "hexpr.parse.C"
    break;

  case 141: /* dobindings: dobinding  */
#line 730 "hexpr.y"
                                 { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3762 "hexpr.parse.C"
    break;

  case 142: /* dobinding: irrefutablep "=" l0expr ";"  */
#line 732 "hexpr.y"
                                       { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)))); }
#line 3768 "hexpr.parse.C"
    break;

  case 143: /* dobinding: l0expr ";"  */
#line 733 "hexpr.y"
                                       { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr(new MatchAny("_",m((yylsp[-1])))), ExprPtr((yyvsp[-1].exp)))); }
#line 3774 "hexpr.parse.C"
    break;

  case 144: /* cselconds: cselconds "," lhexpr  */
#line 735 "hexpr.y"
                                { (yyval.exps) = (yyvsp[-2].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3780 "hexpr.parse.C"
    break;

  case 145: /* cselconds: lhexpr  */
#line 736 "hexpr.y"
                                { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3786 "hexpr.parse.C"
    break;

  case 146: /* cselection: pattern "<-" l0expr "," cselconds  */
#line 738 "hexpr.y"
                                              { (yyval.cselection) = new CSelection(); (yyval.cselection)->pat = PatternPtr((yyvsp[-4].pattern)); (yyval.cselection)->seq = ExprPtr((yyvsp[-2].exp)); (yyval.cselection)->conds = *(yyvsp[0].exps); }
#line 3792 "hexpr.parse.C"
    break;

  case 147: /* cselection: pattern "<-" l0expr  */
#line 739 "hexpr.y"
                                              { (yyval.cselection) = new CSelection(); (yyval.cselection)->pat = PatternPtr((yyvsp[-2].pattern)); (yyval.cselection)->seq = ExprPtr((yyvsp[0].exp)); }
#line 3798 "hexpr.parse.C"
    break;

  case 148: /* cselections: cselections "|" cselection  */
#line 741 "hexpr.y"
                                        { (yyval.cselections) = (yyvsp[-2].cselections); (yyval.cselections)->push_back(CSelectionPtr((yyvsp[0].cselection))); }
#line 3804 "hexpr.parse.C"
    break;

  case 149: /* cselections: cselection  */
#line 742 "hexpr.y"
                                        { (yyval.cselections) = autorelease(new CSelections()); (yyval.cselections)->push_back(CSelectionPtr((yyvsp[0].cselection))); }
#line 3810 "hexpr.parse.C"
    break;

  case 150: /* l6expr: l6expr "(" cargs ")"  */
#line 745 "hexpr.y"
                                { (yyval.exp) = new App(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].exps), m((yylsp[-3]), (yylsp[0]))); }
#line 3816 "hexpr.parse.C"
    break;

  case 151: /* l6expr: id  */
#line 746 "hexpr.y"
                                { (yyval.exp) = varCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3822 "hexpr.parse.C"
    break;

  case 152: /* l6expr: "[" l0expr ".." l0expr "]"  */
#line 749 "hexpr.y"
                                                          { (yyval.exp) = new App(var("range", m((yylsp[-2]))), list(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]), (yylsp[0]))); }
#line 3828 "hexpr.parse.C"
    break;

  case 153: /* l6expr: "[" l0expr ".." "]"  */
#line 750 "hexpr.y"
                                                          { (yyval.exp) = new App(var("iterateS", m((yylsp[-1]))), list(ExprPtr((yyvsp[-2].exp)), fn(str::strings(".x"), fncall(var("+", m((yylsp[-1]))), list(var(".x", m((yylsp[-1]))), ExprPtr(new Int(1, m((yylsp[-1]))))), m((yylsp[-1]))), m((yylsp[-1])))), m((yylsp[-3]), (yylsp[0]))); }
#line 3834 "hexpr.parse.C"
    break;

  case 154: /* l6expr: "[" l0expr "|" cselections "]"  */
#line 751 "hexpr.y"
                                                          { (yyval.exp) = desugarComprehension(yyParseCC, ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].cselections), m((yylsp[-4]), (yylsp[0]))); }
#line 3840 "hexpr.parse.C"
    break;

  case 155: /* l6expr: "[" cargs "]"  */
#line 752 "hexpr.y"
                                                          { (yyval.exp) = new MkArray(*(yyvsp[-1].exps), m((yylsp[-2]), (yylsp[0]))); }
#line 3846 "hexpr.parse.C"
    break;

  case 156: /* l6expr: l6expr "[" "timeV" "]"  */
#line 753 "hexpr.y"
                                                          { (yyval.exp) = maybeArraySliceWithTime(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].string), m((yylsp[-3]), (yylsp[0]))); }
#line 3852 "hexpr.parse.C"
    break;

  case 157: /* l6expr: l6expr "[" l0expr "]"  */
#line 754 "hexpr.y"
                                                          { (yyval.exp) = mkAIndex(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3858 "hexpr.parse.C"
    break;

  case 158: /* l6expr: l6expr "[" l0expr ":" l0expr "]"  */
#line 755 "hexpr.y"
                                                          { (yyval.exp) = new App(var("slice", m((yylsp[-2]))), list(ExprPtr((yyvsp[-5].exp)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-5]), (yylsp[0]))); }
#line 3864 "hexpr.parse.C"
    break;

  case 159: /* l6expr: l6expr "[" l0expr ":" "]"  */
#line 756 "hexpr.y"
                                                          { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-1]))), list(var(vn,m((yylsp[-4]))), ExprPtr((yyvsp[-2].exp)), fncall(var("size",m((yylsp[-1]))), list(var(vn,m((yylsp[-4])))),m((yylsp[-4])))),m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3870 "hexpr.parse.C"
    break;

  case 160: /* l6expr: l6expr "[" ":" l0expr "]"  */
#line 757 "hexpr.y"
                                                          { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-2]))), list(var(vn,m((yylsp[-4]))), fncall(var("size",m((yylsp[-2]))), list(var(vn,m((yylsp[-2])))),m((yylsp[-4]))), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3876 "hexpr.parse.C"
    break;

  case 161: /* l6expr: "|" id "=" l0expr "|"  */
#line 760 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(*(yyvsp[-3].string), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3882 "hexpr.parse.C"
    break;

  case 162: /* l6expr: "|" "intV" "=" l0expr "|"  */
#line 761 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(".f" + str::from((yyvsp[-3].intv)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3888 "hexpr.parse.C"
    break;

  case 163: /* l6expr: "|" id "|"  */
#line 762 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(*(yyvsp[-1].string), ExprPtr(new Unit(m((yylsp[-1])))), m((yylsp[-2]), (yylsp[0]))); }
#line 3894 "hexpr.parse.C"
    break;

  case 164: /* l6expr: "case" l0expr "of" "|" varfields "|"  */
#line 763 "hexpr.y"
                                                              { (yyval.exp) = new Case(ExprPtr((yyvsp[-4].exp)), *(yyvsp[-1].vfields), m((yylsp[-5]), (yylsp[0]))); }
#line 3900 "hexpr.parse.C"
    break;

  case 165: /* l6expr: "case" l0expr "of" "|" varfields "|" "default" l0expr  */
#line 764 "hexpr.y"
                                                              { (yyval.exp) = new Case(ExprPtr((yyvsp[-6].exp)), *(yyvsp[-3].vfields), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3906 "hexpr.parse.C"
    break;

  case 166: /* l6expr: "{" recfields "}"  */
#line 767 "hexpr.y"
                              { if ((yyvsp[-1].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-1].rfields), m((yylsp[-2]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-2]), (yylsp[0]))); } }
#line 3912 "hexpr.parse.C"
    break;

  case 167: /* l6expr: "{" recfields "," "}"  */
#line 768 "hexpr.y"
                              { if ((yyvsp[-2].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-2].rfields), m((yylsp[-3]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-3]), (yylsp[0]))); } }
#line 3918 "hexpr.parse.C"
    break;

  case 168: /* l6expr: l6expr recfieldpath  */
#line 769 "hexpr.y"
                              { (yyval.exp) = makeProjSeq((yyvsp[-1].exp), *(yyvsp[0].strings), m((yylsp[-1]), (yylsp[0]))); }
#line 3924 "hexpr.parse.C"
    break;

  case 169: /* l6expr: recfieldpath  */
#line 772 "hexpr.y"
                     { (yyval.exp) = new Fn(str::strings("x"), proj(var("x", m((yylsp[0]))), *(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3930 "hexpr.parse.C"
    break;

  case 170: /* l6expr: "regexV"  */
#line 775 "hexpr.y"
                 { (yyval.exp) = compileRegexFn(yyParseCC, std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0])))->clone(); }
#line 3936 "hexpr.parse.C"
    break;

  case 171: /* l6expr: "pack" l6expr  */
#line 778 "hexpr.y"
                                           { (yyval.exp) = new Pack(ExprPtr((yyvsp[0].exp)), m((yylsp[-1]), (yylsp[0]))); }
#line 3942 "hexpr.parse.C"
    break;

  case 172: /* l6expr: "unpack" id "=" l6expr "in" l6expr  */
#line 779 "hexpr.y"
                                           { (yyval.exp) = new Unpack(*(yyvsp[-4].string), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3948 "hexpr.parse.C"
    break;

  case 173: /* l6expr: "boolV"  */
#line 782 "hexpr.y"
                    { (yyval.exp) = new Bool((yyvsp[0].boolv), m((yylsp[0]))); }
#line 3954 "hexpr.parse.C"
    break;

  case 174: /* l6expr: "charV"  */
#line 783 "hexpr.y"
                    { (yyval.exp) = new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3960 "hexpr.parse.C"
    break;

  case 175: /* l6expr: "byteV"  */
#line 784 "hexpr.y"
                    { (yyval.exp) = new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3966 "hexpr.parse.C"
    break;

  case 176: /* l6expr: "bytesV"  */
#line 785 "hexpr.y"
                    { (yyval.exp) = mkarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3972 "hexpr.parse.C"
    break;

  case 177: /* l6expr: "shortV"  */
#line 786 "hexpr.y"
                    { (yyval.exp) = new Short((yyvsp[0].shortv), m((yylsp[0]))); }
#line 3978 "hexpr.parse.C"
    break;

  case 178: /* l6expr: "intV"  */
#line 787 "hexpr.y"
                    { (yyval.exp) = new Int((yyvsp[0].intv), m((yylsp[0]))); }
#line 3984 "hexpr.parse.C"
    break;

  case 179: /* l6expr: "longV"  */
#line 788 "hexpr.y"
                    { (yyval.exp) = new Long((yyvsp[0].longv), m((yylsp[0]))); }
#line 3990 "hexpr.parse.C"
    break;

  case 180: /* l6expr: "int128V"  */
#line 789 "hexpr.y"
                    { (yyval.exp) = new Int128((yyvsp[0].int128v), m((yylsp[0]))); }
#line 3996 "hexpr.parse.C"
    break;

  case 181: /* l6expr: "floatV"  */
#line 790 "hexpr.y"
                    { (yyval.exp) = new Float((yyvsp[0].floatv), m((yylsp[0]))); }
#line 4002 "hexpr.parse.C"
    break;

  case 182: /* l6expr: "doubleV"  */
#line 791 "hexpr.y"
                    { (yyval.exp) = new Double((yyvsp[0].doublev), m((yylsp[0]))); }
#line 4008 "hexpr.parse.C"
    break;

  case 183: /* l6expr: "stringV"  */
#line 792 "hexpr.y"
                    { (yyval.exp) = mkarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4014 "hexpr.parse.C"
    break;

  case 184: /* l6expr: tsseq  */
#line 793 "hexpr.y"
                    { (yyval.exp) = mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0])))->clone(); }
#line 4020 "hexpr.parse.C"
    break;

  case 185: /* l6expr: "timeV"  */
#line 794 "hexpr.y"
                    { (yyval.exp) = mkTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 4026 "hexpr.parse.C"
    break;

  case 186: /* l6expr: "dateTimeV"  */
#line 795 "hexpr.y"
                    { (yyval.exp) = mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 4032 "hexpr.parse.C"
    break;

  case 187: /* l6expr: "(" cargs ")"  */
#line 798 "hexpr.y"
                      { (yyval.exp) = pickNestedExp((yyvsp[-1].exps), m((yylsp[-2]),(yylsp[0]))); }
#line 4038 "hexpr.parse.C"
    break;

  case 188: /* l6expr: "(" "++" ")"  */
#line 801 "hexpr.y"
                      { (yyval.exp) = new Var("append", m((yylsp[-1]))); }
#line 4044 "hexpr.parse.C"
    break;

  case 189: /* l6expr: "(" "+" ")"  */
#line 802 "hexpr.y"
                      { (yyval.exp) = new Var("+",      m((yylsp[-1]))); }
#line 4050 "hexpr.parse.C"
    break;

  case 190: /* l6expr: "(" "-" ")"  */
#line 803 "hexpr.y"
                      { (yyval.exp) = new Var("-",      m((yylsp[-1]))); }
#line 4056 "hexpr.parse.C"
    break;

  case 191: /* l6expr: "(" "*" ")"  */
#line 804 "hexpr.y"
                      { (yyval.exp) = new Var("*",      m((yylsp[-1]))); }
#line 4062 "hexpr.parse.C"
    break;

  case 192: /* l6expr: "(" "/" ")"  */
#line 805 "hexpr.y"
                      { (yyval.exp) = new Var("/",      m((yylsp[-1]))); }
#line 4068 "hexpr.parse.C"
    break;

  case 193: /* l6expr: "(" "%" ")"  */
#line 806 "hexpr.y"
                      { (yyval.exp) = new Var("%",      m((yylsp[-1]))); }
#line 4074 "hexpr.parse.C"
    break;

  case 194: /* l6expr: "(" "~" ")"  */
#line 807 "hexpr.y"
                      { (yyval.exp) = new Var("~",      m((yylsp[-1]))); }
#line 4080 "hexpr.parse.C"
    break;

  case 195: /* l6expr: "(" "===" ")"  */
#line 808 "hexpr.y"
                      { (yyval.exp) = new Var("===",    m((yylsp[-1]))); }
#line 4086 "hexpr.parse.C"
    break;

  case 196: /* l6expr: "(" "==" ")"  */
#line 809 "hexpr.y"
                      { (yyval.exp) = new Var("==",     m((yylsp[-1]))); }
#line 4092 "hexpr.parse.C"
    break;

  case 197: /* l6expr: "(" "!=" ")"  */
#line 810 "hexpr.y"
                      { (yyval.exp) = new Var("!=",     m((yylsp[-1]))); }
#line 4098 "hexpr.parse.C"
    break;

  case 198: /* l6expr: "(" "<" ")"  */
#line 811 "hexpr.y"
                      { (yyval.exp) = new Var("<",      m((yylsp[-1]))); }
#line 4104 "hexpr.parse.C"
    break;

  case 199: /* l6expr: "(" ">" ")"  */
#line 812 "hexpr.y"
                      { (yyval.exp) = new Var(">",      m((yylsp[-1]))); }
#line 4110 "hexpr.parse.C"
    break;

  case 200: /* l6expr: "(" ">=" ")"  */
#line 813 "hexpr.y"
                      { (yyval.exp) = new Var(">=",     m((yylsp[-1]))); }
#line 4116 "hexpr.parse.C"
    break;

  case 201: /* l6expr: "(" "<=" ")"  */
#line 814 "hexpr.y"
                      { (yyval.exp) = new Var("<=",     m((yylsp[-1]))); }
#line 4122 "hexpr.parse.C"
    break;

  case 202: /* l6expr: "(" "and" ")"  */
#line 815 "hexpr.y"
                      { (yyval.exp) = new Var("and",    m((yylsp[-1]))); }
#line 4128 "hexpr.parse.C"
    break;

  case 203: /* l6expr: "(" "or" ")"  */
#line 816 "hexpr.y"
                      { (yyval.exp) = new Var("or",     m((yylsp[-1]))); }
#line 4134 "hexpr.parse.C"
    break;

  case 204: /* l6expr: "(" "in" ")"  */
#line 817 "hexpr.y"
                      { (yyval.exp) = new Var("in",     m((yylsp[-1]))); }
#line 4140 "hexpr.parse.C"
    break;

  case 205: /* l6expr: "(" "!" ")"  */
#line 818 "hexpr.y"
                      { (yyval.exp) = new Var("not",    m((yylsp[-1]))); }
#line 4146 "hexpr.parse.C"
    break;

  case 206: /* l6expr: "`" l0expr "`"  */
#line 821 "hexpr.y"
                       { (yyval.exp) = new Assump(fncall(var("unsafeCast", m((yylsp[-1]))), list(mktunit(m((yylsp[-1])))), m((yylsp[-1]))), qualtype(tapp(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp)))))), m((yylsp[-1]))); }
#line 4152 "hexpr.parse.C"
    break;

  case 207: /* prules: prules prule  */
#line 823 "hexpr.y"
                     { (yyval.prules) = (yyvsp[-1].prules); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 4158 "hexpr.parse.C"
    break;

  case 208: /* prules: prule  */
#line 824 "hexpr.y"
                     { (yyval.prules) = autorelease(new Grammar()); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 4164 "hexpr.parse.C"
    break;

  case 209: /* prule: id ":=" prdefs  */
#line 826 "hexpr.y"
                      { (yyval.prule) = autorelease(new Grammar::value_type(*(yyvsp[-2].string), *(yyvsp[0].prdefs))); }
#line 4170 "hexpr.parse.C"
    break;

  case 210: /* prdefs: prdefs "|" prdef  */
#line 828 "hexpr.y"
                         { (yyval.prdefs) = (yyvsp[-2].prdefs); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 4176 "hexpr.parse.C"
    break;

  case 211: /* prdefs: prdef  */
#line 829 "hexpr.y"
                         { (yyval.prdefs) = autorelease(new GrammarRules()); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 4182 "hexpr.parse.C"
    break;

  case 212: /* prdef: pbelems "{" l0expr "}"  */
#line 831 "hexpr.y"
                              { (yyval.prdef) = autorelease(new GrammarRule(*(yyvsp[-3].pbelems), ExprPtr((yyvsp[-1].exp)))); }
#line 4188 "hexpr.parse.C"
    break;

  case 213: /* pbelems: pbelems pbelem  */
#line 833 "hexpr.y"
                        { (yyval.pbelems) = (yyvsp[-1].pbelems); (yyval.pbelems)->push_back(*(yyvsp[0].pbelem)); }
#line 4194 "hexpr.parse.C"
    break;

  case 214: /* pbelems: %empty  */
#line 834 "hexpr.y"
                        { (yyval.pbelems) = autorelease(new BoundGrammarValues()); }
#line 4200 "hexpr.parse.C"
    break;

  case 215: /* pbelem: id ":" pvalue  */
#line 836 "hexpr.y"
                      { (yyval.pbelem) = autorelease(new BoundGrammarValue(*(yyvsp[-2].string), GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 4206 "hexpr.parse.C"
    break;

  case 216: /* pbelem: pvalue  */
#line 837 "hexpr.y"
                      { (yyval.pbelem) = autorelease(new BoundGrammarValue("_", GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 4212 "hexpr.parse.C"
    break;

  case 217: /* pvalue: id  */
#line 839 "hexpr.y"
                      { (yyval.pvalue) = new GSymRef(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4218 "hexpr.parse.C"
    break;

  case 218: /* pvalue: "stringV"  */
#line 840 "hexpr.y"
                      { (yyval.pvalue) = new GStr(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4224 "hexpr.parse.C"
    break;

  case 219: /* pvalue: "charV"  */
#line 841 "hexpr.y"
                      { (yyval.pvalue) = new GStr(std::string(1, str::readCharDef(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4230 "hexpr.parse.C"
    break;

  case 220: /* tsseq: "timespanV"  */
#line 843 "hexpr.y"
                         { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4236 "hexpr.parse.C"
    break;

  case 221: /* tsseq: tsseq "timespanV"  */
#line 844 "hexpr.y"
                         { (yyval.strings) = (yyvsp[-1].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4242 "hexpr.parse.C"
    break;

  case 222: /* l6exprs: l6exprs l6expr  */
#line 846 "hexpr.y"
                        { (yyval.exps) = (yyvsp[-1].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4248 "hexpr.parse.C"
    break;

  case 223: /* l6exprs: l6expr  */
#line 847 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4254 "hexpr.parse.C"
    break;

  case 224: /* patternexps: patternexps patternexp  */
#line 849 "hexpr.y"
                                    { (yyval.patternexps) = (yyvsp[-1].patternexps); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 4260 "hexpr.parse.C"
    break;

  case 225: /* patternexps: patternexp  */
#line 850 "hexpr.y"
                                    { (yyval.patternexps) = autorelease(new PatternRows()); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 4266 "hexpr.parse.C"
    break;

  case 226: /* patternexp: "|" patterns "->" l0expr  */
#line 852 "hexpr.y"
                                                    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)))); }
#line 4272 "hexpr.parse.C"
    break;

  case 227: /* patternexp: "|" patterns "where" l0expr "->" l0expr  */
#line 853 "hexpr.y"
                                                    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-4].patterns), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)))); }
#line 4278 "hexpr.parse.C"
    break;

  case 228: /* patterns: patterns pattern  */
#line 856 "hexpr.y"
                           { (yyval.patterns) = (yyvsp[-1].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4284 "hexpr.parse.C"
    break;

  case 229: /* patterns: pattern  */
#line 857 "hexpr.y"
                           { (yyval.patterns) = autorelease(new Patterns()); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4290 "hexpr.parse.C"
    break;

  case 230: /* refutablep: "boolV"  */
#line 859 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Bool((yyvsp[0].boolv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4296 "hexpr.parse.C"
    break;

  case 231: /* refutablep: "charV"  */
#line 860 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 4302 "hexpr.parse.C"
    break;

  case 232: /* refutablep: "byteV"  */
#line 861 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 4308 "hexpr.parse.C"
    break;

  case 233: /* refutablep: "shortV"  */
#line 862 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Short((yyvsp[0].shortv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4314 "hexpr.parse.C"
    break;

  case 234: /* refutablep: "intV"  */
#line 863 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int((yyvsp[0].intv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4320 "hexpr.parse.C"
    break;

  case 235: /* refutablep: "longV"  */
#line 864 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Long((yyvsp[0].longv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4326 "hexpr.parse.C"
    break;

  case 236: /* refutablep: "int128V"  */
#line 865 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int128((yyvsp[0].int128v), m((yylsp[0])))), m((yylsp[0]))); }
#line 4332 "hexpr.parse.C"
    break;

  case 237: /* refutablep: "doubleV"  */
#line 866 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Double((yyvsp[0].doublev), m((yylsp[0])))), m((yylsp[0]))); }
#line 4338 "hexpr.parse.C"
    break;

  case 238: /* refutablep: "bytesV"  */
#line 867 "hexpr.y"
                                       { (yyval.pattern) = mkpatarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 4344 "hexpr.parse.C"
    break;

  case 239: /* refutablep: "stringV"  */
#line 868 "hexpr.y"
                                       { (yyval.pattern) = mkpatarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4350 "hexpr.parse.C"
    break;

  case 240: /* refutablep: tsseq  */
#line 869 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkTimespanPrim(*(yyvsp[0].strings), m((yylsp[0]))), mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 4356 "hexpr.parse.C"
    break;

  case 241: /* refutablep: "timeV"  */
#line 870 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4362 "hexpr.parse.C"
    break;

  case 242: /* refutablep: "dateTimeV"  */
#line 871 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkDateTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4368 "hexpr.parse.C"
    break;

  case 243: /* refutablep: "regexV"  */
#line 872 "hexpr.y"
                                       { (yyval.pattern) = new MatchRegex(std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0]))); }
#line 4374 "hexpr.parse.C"
    break;

  case 244: /* refutablep: "[" patternseq "]"  */
#line 873 "hexpr.y"
                                       { (yyval.pattern) = new MatchArray(*(yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4380 "hexpr.parse.C"
    break;

  case 245: /* refutablep: "[" patternseq "," "]"  */
#line 874 "hexpr.y"
                                       { (yyval.pattern) = new MatchArray(*(yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4386 "hexpr.parse.C"
    break;

  case 246: /* refutablep: "|" id "|"  */
#line 875 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(*(yyvsp[-1].string), PatternPtr(new MatchLiteral(PrimitivePtr(new Unit(m((yylsp[-1])))), m((yylsp[-1])))), m((yylsp[-2]),(yylsp[0]))); }
#line 4392 "hexpr.parse.C"
    break;

  case 247: /* refutablep: "|" id "=" pattern "|"  */
#line 876 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(*(yyvsp[-3].string), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4398 "hexpr.parse.C"
    break;

  case 248: /* refutablep: "|" "intV" "=" pattern "|"  */
#line 877 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(".f" + str::from((yyvsp[-3].intv)), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4404 "hexpr.parse.C"
    break;

  case 249: /* refutablep: "(" patternseq ")"  */
#line 878 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4410 "hexpr.parse.C"
    break;

  case 250: /* refutablep: "(" patternseq "," ")"  */
#line 879 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4416 "hexpr.parse.C"
    break;

  case 251: /* refutablep: "{" recpatfields "}"  */
#line 880 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4422 "hexpr.parse.C"
    break;

  case 252: /* refutablep: "{" recpatfields "," "}"  */
#line 881 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4428 "hexpr.parse.C"
    break;

  case 253: /* refutablep: id  */
#line 882 "hexpr.y"
                                       { (yyval.pattern) = patVarCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4434 "hexpr.parse.C"
    break;

  case 254: /* irrefutablep: id  */
#line 884 "hexpr.y"
                                       { (yyval.pattern) = new MatchAny(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4440 "hexpr.parse.C"
    break;

  case 255: /* irrefutablep: "(" patternseq ")"  */
#line 885 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4446 "hexpr.parse.C"
    break;

  case 256: /* irrefutablep: "(" patternseq "," ")"  */
#line 886 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4452 "hexpr.parse.C"
    break;

  case 257: /* irrefutablep: "{" recpatfields "}"  */
#line 887 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4458 "hexpr.parse.C"
    break;

  case 258: /* irrefutablep: "{" recpatfields "," "}"  */
#line 888 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4464 "hexpr.parse.C"
    break;

  case 259: /* pattern: refutablep  */
#line 890 "hexpr.y"
                    { (yyval.pattern) = (yyvsp[0].pattern); }
#line 4470 "hexpr.parse.C"
    break;

  case 260: /* patternseq: patternseqn  */
#line 892 "hexpr.y"
                          { (yyval.patterns) = (yyvsp[0].patterns); }
#line 4476 "hexpr.parse.C"
    break;

  case 261: /* patternseq: %empty  */
#line 893 "hexpr.y"
                          { (yyval.patterns) = new Patterns(); }
#line 4482 "hexpr.parse.C"
    break;

  case 262: /* patternseqn: patternseqn "," pattern  */
#line 895 "hexpr.y"
                                     { (yyval.patterns) = (yyvsp[-2].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4488 "hexpr.parse.C"
    break;

  case 263: /* patternseqn: pattern  */
#line 896 "hexpr.y"
                                     { (yyval.patterns) = new Patterns(); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4494 "hexpr.parse.C"
    break;

  case 264: /* recpatfields: recpatfields "," recpatfield  */
#line 898 "hexpr.y"
                                           { (yyval.recpatfields) = (yyvsp[-2].recpatfields); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4500 "hexpr.parse.C"
    break;

  case 265: /* recpatfields: recpatfield  */
#line 899 "hexpr.y"
                                           { (yyval.recpatfields) = new MatchRecord::Fields(); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4506 "hexpr.parse.C"
    break;

  case 266: /* recpatfield: id "=" pattern  */
#line 901 "hexpr.y"
                            { (yyval.recpatfield) = new MatchRecord::Field(*(yyvsp[-2].string), PatternPtr((yyvsp[0].pattern))); }
#line 4512 "hexpr.parse.C"
    break;

  case 267: /* recfields: %empty  */
#line 903 "hexpr.y"
                                                 { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); }
#line 4518 "hexpr.parse.C"
    break;

  case 268: /* recfields: recfieldname "=" l0expr  */
#line 904 "hexpr.y"
                                                 { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4524 "hexpr.parse.C"
    break;

  case 269: /* recfields: recfields "," recfieldname "=" l0expr  */
#line 905 "hexpr.y"
                                                 { (yyval.rfields) = (yyvsp[-4].rfields);                                     (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4530 "hexpr.parse.C"
    break;

  case 270: /* recfieldname: id  */
#line 907 "hexpr.y"
                         { (yyval.string) = (yyvsp[0].string); }
#line 4536 "hexpr.parse.C"
    break;

  case 271: /* recfieldname: "data"  */
#line 908 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("data")); }
#line 4542 "hexpr.parse.C"
    break;

  case 272: /* recfieldname: "type"  */
#line 909 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("type")); }
#line 4548 "hexpr.parse.C"
    break;

  case 273: /* recfieldname: "where"  */
#line 910 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("where")); }
#line 4554 "hexpr.parse.C"
    break;

  case 274: /* recfieldname: "class"  */
#line 911 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("class")); wantIndent(false); }
#line 4560 "hexpr.parse.C"
    break;

  case 275: /* recfieldname: "instance"  */
#line 912 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("instance")); wantIndent(false); }
#line 4566 "hexpr.parse.C"
    break;

  case 276: /* recfieldname: "exists"  */
#line 913 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("exists")); }
#line 4572 "hexpr.parse.C"
    break;

  case 277: /* recfieldname: "import"  */
#line 914 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("import")); }
#line 4578 "hexpr.parse.C"
    break;

  case 278: /* recfieldname: "module"  */
#line 915 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("module")); }
#line 4584 "hexpr.parse.C"
    break;

  case 279: /* recfieldname: "parse"  */
#line 916 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("parse")); }
#line 4590 "hexpr.parse.C"
    break;

  case 280: /* recfieldname: "do"  */
#line 917 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("do")); }
#line 4596 "hexpr.parse.C"
    break;

  case 281: /* recfieldname: "return"  */
#line 918 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("return")); }
#line 4602 "hexpr.parse.C"
    break;

  case 282: /* recfieldname: "fn"  */
#line 919 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("fn")); }
#line 4608 "hexpr.parse.C"
    break;

  case 283: /* recfieldname: "intV"  */
#line 920 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string(".f" + str::from((yyvsp[0].intv)))); }
#line 4614 "hexpr.parse.C"
    break;

  case 284: /* recfieldname: "stringV"  */
#line 921 "hexpr.y"
                         { std::string stringField = str::unescape(str::trimq(*(yyvsp[0].string)));
                           if (stringField.size() > 0 && stringField[0] == '.' ) {
                             throw annotated_error(m((yylsp[0])), "Cannot define record string label with leading '.'");
                           }
                           (yyval.string) = autorelease(new std::string(str::unescape(str::trimq(*(yyvsp[0].string))))); }
#line 4624 "hexpr.parse.C"
    break;

  case 285: /* recfieldpath: recfieldpath "." recfieldname  */
#line 927 "hexpr.y"
                                            { (yyval.strings) = (yyvsp[-2].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4630 "hexpr.parse.C"
    break;

  case 286: /* recfieldpath: recfieldpath "tupSection"  */
#line 928 "hexpr.y"
                                            { (yyval.strings) = (yyvsp[-1].strings); str::seq x = tupSectionFields(*(yyvsp[0].string)); (yyval.strings)->insert((yyval.strings)->end(), x.begin(), x.end()); }
#line 4636 "hexpr.parse.C"
    break;

  case 287: /* recfieldpath: "." recfieldname  */
#line 929 "hexpr.y"
                                            { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4642 "hexpr.parse.C"
    break;

  case 288: /* recfieldpath: "tupSection"  */
#line 930 "hexpr.y"
                                            { (yyval.strings) = autorelease(new str::seq()); *(yyval.strings) = tupSectionFields(*(yyvsp[0].string)); }
#line 4648 "hexpr.parse.C"
    break;

  case 289: /* varfields: varbind  */
#line 932 "hexpr.y"
                                 { (yyval.vfields) = autorelease(new Case::Bindings()); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4654 "hexpr.parse.C"
    break;

  case 290: /* varfields: varfields "," varbind  */
#line 933 "hexpr.y"
                                 { (yyval.vfields) = (yyvsp[-2].vfields); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4660 "hexpr.parse.C"
    break;

  case 291: /* varbind: id "=" l0expr  */
#line 935 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-2].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4666 "hexpr.parse.C"
    break;

  case 292: /* varbind: id ":" id "=" l0expr  */
#line 936 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-4].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4672 "hexpr.parse.C"
    break;

  case 293: /* varbind: "intV" ":" id "=" l0expr  */
#line 937 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(".f" + str::from((yyvsp[-4].intv)), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4678 "hexpr.parse.C"
    break;

  case 294: /* cargs: %empty  */
#line 939 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); }
#line 4684 "hexpr.parse.C"
    break;

  case 295: /* cargs: l0expr  */
#line 940 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4690 "hexpr.parse.C"
    break;

  case 296: /* cargs: cargs "," l0expr  */
#line 941 "hexpr.y"
                        { (yyvsp[-2].exps)->push_back(ExprPtr((yyvsp[0].exp))); (yyval.exps) = (yyvsp[-2].exps); }
#line 4696 "hexpr.parse.C"
    break;

  case 297: /* qtype: cst "=>" l0mtype  */
#line 943 "hexpr.y"
                         { (yyval.qualtype) = new QualType(*(yyvsp[-2].tconstraints), *(yyvsp[0].mtype)); }
#line 4702 "hexpr.parse.C"
    break;

  case 298: /* qtype: l0mtype  */
#line 944 "hexpr.y"
                         { (yyval.qualtype) = new QualType(Constraints(), *(yyvsp[0].mtype)); }
#line 4708 "hexpr.parse.C"
    break;

  case 299: /* cst: "(" tpreds ")"  */
#line 947 "hexpr.y"
                    { (yyval.tconstraints) = (yyvsp[-1].tconstraints); }
#line 4714 "hexpr.parse.C"
    break;

  case 300: /* tpreds: tpred  */
#line 949 "hexpr.y"
                         { (yyval.tconstraints) = autorelease(new Constraints()); (yyval.tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); }
#line 4720 "hexpr.parse.C"
    break;

  case 301: /* tpreds: tpreds "," tpred  */
#line 950 "hexpr.y"
                         { (yyvsp[-2].tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); (yyval.tconstraints) = (yyvsp[-2].tconstraints); }
#line 4726 "hexpr.parse.C"
    break;

  case 302: /* tpred: id l1mtargl  */
#line 952 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(*(yyvsp[-1].string), *(yyvsp[0].mtypes)); }
#line 4732 "hexpr.parse.C"
    break;

  case 303: /* tpred: l1mtype "==" l1mtype  */
#line 953 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(EqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4738 "hexpr.parse.C"
    break;

  case 304: /* tpred: l1mtype "!=" l1mtype  */
#line 954 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(NotEqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4744 "hexpr.parse.C"
    break;

  case 305: /* tpred: l1mtype "~" l1mtype  */
#line 955 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(FixIsoRecur::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4750 "hexpr.parse.C"
    break;

  case 306: /* tpred: l1mtype "=" "{" l1mtype "*" l1mtype "}"  */
#line 956 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4756 "hexpr.parse.C"
    break;

  case 307: /* tpred: l1mtype "=" "{" id ":" l1mtype "*" l1mtype "}"  */
#line 957 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4762 "hexpr.parse.C"
    break;

  case 308: /* tpred: l1mtype "=" "(" l1mtype "*" l1mtype ")"  */
#line 958 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4768 "hexpr.parse.C"
    break;

  case 309: /* tpred: "{" l1mtype "*" l1mtype "}" "=" l1mtype  */
#line 959 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4774 "hexpr.parse.C"
    break;

  case 310: /* tpred: "{" id ":" l1mtype "*" l1mtype "}" "=" l1mtype  */
#line 960 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4780 "hexpr.parse.C"
    break;

  case 311: /* tpred: "(" l1mtype "*" l1mtype ")" "=" l1mtype  */
#line 961 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(1), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4786 "hexpr.parse.C"
    break;

  case 312: /* tpred: l1mtype "." recfieldname "::" l1mtype  */
#line 963 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4792 "hexpr.parse.C"
    break;

  case 313: /* tpred: l1mtype "." recfieldname "<-" l1mtype  */
#line 964 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4798 "hexpr.parse.C"
    break;

  case 314: /* tpred: l1mtype "/" l1mtype "::" l1mtype  */
#line 965 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4804 "hexpr.parse.C"
    break;

  case 315: /* tpred: l1mtype "/" l1mtype "<-" l1mtype  */
#line 966 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4810 "hexpr.parse.C"
    break;

  case 316: /* tpred: l1mtype "=" "|" l1mtype "+" l1mtype "|"  */
#line 968 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4816 "hexpr.parse.C"
    break;

  case 317: /* tpred: "|" l1mtype "+" l1mtype "|" "=" l1mtype  */
#line 969 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4822 "hexpr.parse.C"
    break;

  case 318: /* tpred: l1mtype "=" "|" id ":" l1mtype "+" l1mtype "|"  */
#line 970 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4828 "hexpr.parse.C"
    break;

  case 319: /* tpred: "|" id ":" l1mtype "+" l1mtype "|" "=" l1mtype  */
#line 971 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4834 "hexpr.parse.C"
    break;

  case 320: /* tpred: "|" id ":" l0mtype "|" "::" l1mtype  */
#line 973 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), TString::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype))); }
#line 4840 "hexpr.parse.C"
    break;

  case 321: /* tpred: "|" l1mtype "/" l0mtype "|" "::" l1mtype  */
#line 974 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), *(yyvsp[-5].mtype),                *(yyvsp[-3].mtype))); }
#line 4846 "hexpr.parse.C"
    break;

  case 322: /* tpred: l1mtype "++" l1mtype "=" l1mtype  */
#line 975 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(AppendsToUnqualifier::constraintName(), list(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4852 "hexpr.parse.C"
    break;

  case 323: /* l1mtargl: l1mtype  */
#line 977 "hexpr.y"
                           { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4858 "hexpr.parse.C"
    break;

  case 324: /* l1mtargl: l1mtargl l1mtype  */
#line 978 "hexpr.y"
                           { (yyvsp[-1].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4864 "hexpr.parse.C"
    break;

  case 325: /* ltmtype: ltmtype l0mtype  */
#line 980 "hexpr.y"
                          { (yyval.mtypes) = (yyvsp[-1].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4870 "hexpr.parse.C"
    break;

  case 326: /* ltmtype: l0mtype  */
#line 981 "hexpr.y"
                          { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4876 "hexpr.parse.C"
    break;

  case 327: /* l0mtype: l0mtargl "->" l1mtype  */
#line 983 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(Func::make(tuplety(*(yyvsp[-2].mtypes)), *(yyvsp[0].mtype)))); }
#line 4882 "hexpr.parse.C"
    break;

  case 328: /* l0mtype: mtuplist  */
#line 984 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(makeTupleType(*(yyvsp[0].mtypes)))); }
#line 4888 "hexpr.parse.C"
    break;

  case 329: /* l0mtype: msumlist  */
#line 985 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(makeSumType(*(yyvsp[0].mtypes)))); }
#line 4894 "hexpr.parse.C"
    break;

  case 330: /* l1mtype: id  */
#line 987 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(monoTypeByName(*(yyvsp[0].string)))); }
#line 4900 "hexpr.parse.C"
    break;

  case 331: /* l1mtype: "<" cppid ">"  */
#line 988 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(OpaquePtr::make(str::replace<char>(*(yyvsp[-1].string), ".", "::"), 0, false))); }
#line 4906 "hexpr.parse.C"
    break;

  case 332: /* l1mtype: "[" "]"  */
#line 989 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("[]"))); }
#line 4912 "hexpr.parse.C"
    break;

  case 333: /* l1mtype: "[" ltmtype "]"  */
#line 990 "hexpr.y"
                                           { try { (yyval.mtype) = autorelease(new MonoTypePtr(Array::make(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4918 "hexpr.parse.C"
    break;

  case 334: /* l1mtype: "[" ":" l0mtype "|" tyind ":" "]"  */
#line 991 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(FixedArray::make(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype)))); }
#line 4924 "hexpr.parse.C"
    break;

  case 335: /* l1mtype: "(" "->" ")"  */
#line 992 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("->"))); }
#line 4930 "hexpr.parse.C"
    break;

  case 336: /* l1mtype: "(" ltmtype ")"  */
#line 993 "hexpr.y"
                                           { try { (yyval.mtype) = autorelease(new MonoTypePtr(clone(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4936 "hexpr.parse.C"
    break;

  case 337: /* l1mtype: "{" mreclist "}"  */
#line 994 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(makeRecType(*(yyvsp[-1].mreclist)))); }
#line 4942 "hexpr.parse.C"
    break;

  case 338: /* l1mtype: "|" mvarlist "|"  */
#line 995 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(makeVarType(*(yyvsp[-1].mvarlist)))); }
#line 4948 "hexpr.parse.C"
    break;

  case 339: /* l1mtype: "|" mpvarlist "|"  */
#line 996 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(makePVarType(*(yyvsp[-1].mvarlist), m((yylsp[-1]))))); }
#line 4954 "hexpr.parse.C"
    break;

  case 340: /* l1mtype: "(" ")"  */
#line 997 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("unit"))); }
#line 4960 "hexpr.parse.C"
    break;

  case 341: /* l1mtype: "intV"  */
#line 998 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(((yyvsp[0].intv) == 0) ? Prim::make("void") : TLong::make((yyvsp[0].intv)))); }
#line 4966 "hexpr.parse.C"
    break;

  case 342: /* l1mtype: "boolV"  */
#line 999 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr((yyvsp[0].boolv) ? TLong::make(1) : TLong::make(0))); }
#line 4972 "hexpr.parse.C"
    break;

  case 343: /* l1mtype: "exists" id "." l1mtype  */
#line 1000 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Exists::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4978 "hexpr.parse.C"
    break;

  case 344: /* l1mtype: l1mtype "@" l1mtype  */
#line 1001 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype), *(yyvsp[0].mtype)))); }
#line 4984 "hexpr.parse.C"
    break;

  case 345: /* l1mtype: l1mtype "@" "?"  */
#line 1002 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype)))); }
#line 4990 "hexpr.parse.C"
    break;

  case 346: /* l1mtype: "^" id "." l1mtype  */
#line 1003 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Recursive::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4996 "hexpr.parse.C"
    break;

  case 347: /* l1mtype: "stringV"  */
#line 1004 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(TString::make(str::unescape(str::trimq(*(yyvsp[0].string)))))); }
#line 5002 "hexpr.parse.C"
    break;

  case 348: /* l1mtype: "`" l0expr "`"  */
#line 1005 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp))))))); }
#line 5008 "hexpr.parse.C"
    break;

  case 349: /* tyind: id  */
#line 1007 "hexpr.y"
              { (yyval.mtype) = autorelease(new MonoTypePtr(TVar::make(*(yyvsp[0].string)))); }
#line 5014 "hexpr.parse.C"
    break;

  case 350: /* tyind: "intV"  */
#line 1008 "hexpr.y"
              { (yyval.mtype) = autorelease(new MonoTypePtr(TLong::make((yyvsp[0].intv)))); }
#line 5020 "hexpr.parse.C"
    break;

  case 351: /* cppid: id  */
#line 1010 "hexpr.y"
                    { (yyval.string) = (yyvsp[0].string); }
#line 5026 "hexpr.parse.C"
    break;

  case 352: /* cppid: cppid "." id  */
#line 1011 "hexpr.y"
                    { (yyval.string) = (yyvsp[-2].string); *(yyval.string) += "."; *(yyval.string) += *(yyvsp[0].string); }
#line 5032 "hexpr.parse.C"
    break;

  case 353: /* l0mtargl: l1mtype  */
#line 1013 "hexpr.y"
                                        { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5038 "hexpr.parse.C"
    break;

  case 354: /* l0mtargl: "(" l0mtype "," l0mtarglt ")"  */
#line 1014 "hexpr.y"
                                        { (yyvsp[-1].mtypes)->insert((yyvsp[-1].mtypes)->begin(), *(yyvsp[-3].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 5044 "hexpr.parse.C"
    break;

  case 355: /* l0mtarglt: l0mtype  */
#line 1016 "hexpr.y"
                                 { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5050 "hexpr.parse.C"
    break;

  case 356: /* l0mtarglt: l0mtarglt "," l0mtype  */
#line 1017 "hexpr.y"
                                 { (yyvsp[-2].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-2].mtypes); }
#line 5056 "hexpr.parse.C"
    break;

  case 357: /* mtuplist: l1mtype  */
#line 1019 "hexpr.y"
                               { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5062 "hexpr.parse.C"
    break;

  case 358: /* mtuplist: mtuplist "*" l1mtype  */
#line 1020 "hexpr.y"
                               { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5068 "hexpr.parse.C"
    break;

  case 359: /* msumlist: l1mtype "+" l1mtype  */
#line 1022 "hexpr.y"
                               { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[-2].mtype)); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5074 "hexpr.parse.C"
    break;

  case 360: /* msumlist: msumlist "+" l1mtype  */
#line 1023 "hexpr.y"
                               { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5080 "hexpr.parse.C"
    break;

  case 361: /* mreclist: mreclist "," id ":" l0mtype  */
#line 1025 "hexpr.y"
                                      { (yyval.mreclist) = (yyvsp[-4].mreclist);                                 (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 5086 "hexpr.parse.C"
    break;

  case 362: /* mreclist: id ":" l0mtype  */
#line 1026 "hexpr.y"
                                      { (yyval.mreclist) = autorelease(new Record::Members()); (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 5092 "hexpr.parse.C"
    break;

  case 363: /* mvarlist: mvarlist "," id ":" l0mtype  */
#line 1028 "hexpr.y"
                                      { (yyval.mvarlist) = (yyvsp[-4].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 5098 "hexpr.parse.C"
    break;

  case 364: /* mvarlist: mvarlist "," id  */
#line 1029 "hexpr.y"
                                      { (yyval.mvarlist) = (yyvsp[-2].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 5104 "hexpr.parse.C"
    break;

  case 365: /* mvarlist: id ":" l0mtype  */
#line 1030 "hexpr.y"
                                      { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 5110 "hexpr.parse.C"
    break;

  case 366: /* mvarlist: id  */
#line 1031 "hexpr.y"
                                      { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 5116 "hexpr.parse.C"
    break;

  case 367: /* mpvarlist: mpvarlist "," mpvar  */
#line 1033 "hexpr.y"
                               { (yyval.mvarlist) = (yyvsp[-2].mvarlist);                                  (yyval.mvarlist)->push_back(*(yyvsp[0].mpvar)); }
#line 5122 "hexpr.parse.C"
    break;

  case 368: /* mpvarlist: mpvar  */
#line 1034 "hexpr.y"
                               { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(*(yyvsp[0].mpvar)); }
#line 5128 "hexpr.parse.C"
    break;

  case 369: /* mpvar: id "(" "intV" ")"  */
#line 1036 "hexpr.y"
                           { (yyval.mpvar) = autorelease(new Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), (yyvsp[-1].intv))); }
#line 5134 "hexpr.parse.C"
    break;

  case 370: /* mpvar: id "(" "shortV" ")"  */
#line 1037 "hexpr.y"
                           { (yyval.mpvar) = autorelease(new Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), (yyvsp[-1].shortv))); }
#line 5140 "hexpr.parse.C"
    break;

  case 371: /* mpvar: id "(" "boolV" ")"  */
#line 1038 "hexpr.y"
                           { (yyval.mpvar) = autorelease(new Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), (yyvsp[-1].boolv))); }
#line 5146 "hexpr.parse.C"
    break;

  case 372: /* mpvar: id "(" "byteV" ")"  */
#line 1039 "hexpr.y"
                           { (yyval.mpvar) = autorelease(new Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), str::dehex(*(yyvsp[-1].string)))); }
#line 5152 "hexpr.parse.C"
    break;

  case 373: /* mpvar: id "(" "charV" ")"  */
#line 1040 "hexpr.y"
                           { (yyval.mpvar) = autorelease(new Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), str::readCharDef(*(yyvsp[-1].string)))); }
#line 5158 "hexpr.parse.C"
    break;


#line 5162 "hexpr.parse.C"

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
          YYNOMEM;
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
  ++yynerrs;

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
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
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

#line 1044 "hexpr.y"

#pragma GCC diagnostic pop


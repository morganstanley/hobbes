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

MonoTypePtr makeVarType(const Variant::Members& vms) {
  if (std::any_of(std::cbegin(vms), std::cend(vms), [](const auto& v) { return v.id != 0; })) {
    return Variant::make(vms);
  }

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


#line 331 "hexpr.parse.C"

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
  YYSYMBOL_id = 179                        /* id  */
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
#define YYLAST   2997

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  98
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  82
/* YYNRULES -- Number of rules.  */
#define YYNRULES  376
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  831

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
       0,   492,   492,   493,   494,   495,   498,   499,   500,   502,
     503,   504,   506,   507,   508,   509,   510,   511,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   529,   530,   531,   532,   533,   536,
     539,   542,   543,   544,   547,   548,   551,   553,   556,   557,
     558,   559,   560,   561,   562,   563,   565,   566,   568,   570,
     571,   573,   576,   577,   578,   579,   581,   582,   584,   587,
     589,   591,   592,   594,   596,   598,   599,   600,   601,   602,
     603,   604,   605,   606,   607,   608,   609,   610,   611,   612,
     613,   614,   615,   616,   618,   619,   621,   622,   625,   626,
     627,   628,   630,   631,   632,   633,   634,   635,   637,   638,
     640,   641,   642,   643,   644,   645,   646,   647,   648,   650,
     651,   652,   653,   654,   656,   657,   658,   659,   661,   664,
     665,   668,   671,   674,   686,   687,   690,   692,   693,   695,
     697,   698,   700,   701,   703,   704,   706,   707,   709,   710,
     713,   714,   717,   718,   719,   720,   721,   722,   723,   724,
     725,   728,   729,   730,   731,   732,   735,   736,   737,   740,
     743,   746,   747,   750,   751,   752,   753,   754,   755,   756,
     757,   758,   759,   760,   761,   762,   763,   766,   769,   770,
     771,   772,   773,   774,   775,   776,   777,   778,   779,   780,
     781,   782,   783,   784,   785,   786,   789,   791,   792,   794,
     796,   797,   799,   801,   802,   804,   805,   807,   808,   809,
     811,   812,   814,   815,   817,   818,   820,   821,   824,   825,
     827,   828,   829,   830,   831,   832,   833,   834,   835,   836,
     837,   838,   839,   840,   841,   842,   843,   844,   845,   846,
     847,   848,   849,   850,   852,   853,   854,   855,   856,   858,
     860,   861,   863,   864,   866,   867,   869,   871,   872,   873,
     875,   876,   877,   878,   879,   880,   881,   882,   883,   884,
     885,   886,   887,   888,   889,   895,   896,   897,   898,   900,
     901,   903,   904,   905,   907,   908,   909,   911,   912,   915,
     917,   918,   920,   921,   922,   923,   924,   925,   926,   927,
     928,   929,   931,   932,   933,   934,   936,   937,   938,   939,
     941,   942,   943,   945,   946,   948,   949,   951,   952,   953,
     955,   956,   957,   958,   959,   960,   961,   962,   963,   964,
     965,   966,   967,   968,   969,   970,   971,   972,   974,   975,
     977,   978,   980,   981,   983,   984,   986,   987,   989,   990,
     992,   993,   995,   996,   997,   998,   999,  1000,  1001,  1002,
    1003,  1004,  1005,  1006,  1007,  1008,  1010
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
  "mvarlist", "id", YY_NULLPTR
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

#define YYPACT_NINF (-596)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-366)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     487,  1548,  2190,  2190,    46,    51,    51,    51,    20,    20,
      67,    67,  -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,
    -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,   844,
      58,  2190,   595,   118,   595,    51,   185,  1795,  2190,   844,
     181,  2190,   352,  -596,   978,  -596,  -596,  -596,  -596,  -596,
    -596,  -596,    41,  -596,   242,   329,   168,   194,  2736,  2580,
    2190,  1878,  2902,  2902,  -596,    94,    44,   426,   453,   477,
    -596,   192,  -596,  -596,  -596,  1548,   370,   355,  -596,  2900,
     123,  -596,  -596,   124,  1586,   395,    20,   403,  1619,  -596,
    -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,
    -596,  -596,  -596,  -596,  -596,  2902,    51,     2,  -596,   410,
    -596,   398,   174,   345,    51,   174,   444,  2268,   438,   443,
    2658,   454,   456,   460,   844,   464,   466,   483,   488,   491,
     492,   493,   494,  2424,   495,   496,   497,  -596,  -596,   499,
    -596,     0,   249,   404,   212,   524,   539,    64,   433,    51,
      51,   479,  -596,  2072,  2072,  2902,  2190,  1708,   168,  -596,
    -596,   844,  2190,    49,  -596,  -596,   512,   438,   443,  2658,
     454,   456,   460,   464,   466,   483,   491,   492,   493,   494,
     495,   496,   497,  -596,  -596,  -596,  -596,  -596,  -596,  -596,
    -596,  -596,  -596,  -596,  -596,  -596,  2902,  2902,    51,   409,
     329,  1115,  -596,  -596,  -596,  1275,  2502,  2502,  2502,  2502,
    2580,  2736,  2736,  2736,  2736,  2736,  2736,  2736,  2736,  2736,
    2736,  2736,  2814,  2814,  2814,  2190,  -596,   978,    51,  -596,
    -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,
    -596,  -596,  -596,  -596,  -596,  -596,  2072,  -596,  2072,  -596,
    -596,  -596,    51,    51,   887,   202,  2150,  2150,    51,  2190,
     302,  -596,    93,  2150,    51,    25,    20,  2900,    51,   887,
      51,    51,    42,  -596,    11,   556,   548,   551,  -596,  -596,
     336,   514,   331,  -596,   555,  2190,    17,  2580,   516,   517,
     174,    24,  -596,   564,   595,  1956,   844,   522,   481,  -596,
     589,   591,  -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,
    -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,
    -596,  -596,  2190,  2902,  2034,  -596,  -596,   574,  2190,  2190,
    2190,  -596,  -596,  -596,  -596,  -596,  1197,  -596,   598,  -596,
    -596,  -596,   347,   554,  2190,   134,  -596,  -596,  2190,   196,
    2190,   349,   406,   435,   594,   145,  2190,  -596,  2190,   144,
     549,   549,  -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,
    -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,   978,
    -596,  -596,  -596,   587,   132,   561,  -596,  1307,  -596,    10,
    1619,  -596,   704,   887,    85,   445,   603,   125,   373,    83,
     592,   547,  -596,  1586,   340,  2150,  2150,   844,  2150,  2150,
    2150,  1447,  2150,   552,    20,   628,    51,    51,  1619,   567,
     616,    27,   637,  -596,  2150,  2150,  2150,  2150,  -596,   581,
    2902,  -596,    30,  2902,  -596,  2190,  -596,  -596,   416,  2902,
     517,  -596,  -596,  -596,  -596,   193,  -596,  -596,  -596,  -596,
    -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,  1956,
    2346,   844,   419,   329,  -596,   555,  -596,  2190,  -596,  -596,
    2190,  -596,  -596,   172,   621,  -596,   582,  -596,   622,  -596,
     583,   588,   887,   257,  1619,  -596,  -596,   584,  2112,  -596,
    -596,  2190,   230,   599,  -596,   597,  -596,   585,  -596,    40,
    2902,  2902,  -596,  -596,  -596,  2150,  -596,  -596,  -596,  -596,
    2150,   590,  -596,  2150,  -596,    51,  1619,  2150,  1619,  -596,
      51,  1619,   440,  2150,  -596,  -596,  2150,  2150,  2150,    35,
      33,   319,   552,   552,   552,  -596,   552,   552,    32,    20,
     628,  -596,    26,  -596,   170,  -596,  -596,   167,  1619,  1619,
    1619,    20,   637,  -596,   552,   552,   552,   552,  -596,  -596,
    -596,  -596,  -596,  -596,   640,   463,  -596,   450,  2883,  -596,
     604,  -596,    22,   595,   647,   166,   607,   608,  -596,  2902,
    2190,  -596,  2190,  -596,  -596,  -596,  -596,  -596,   614,  -596,
    2190,   261,  2190,  -596,  -596,  -596,   619,   620,   552,   217,
     422,   147,   667,  -596,    23,   176,   623,   138,   624,    37,
     627,   631,   633,   634,   635,   552,   122,   127,   677,    47,
     678,  2150,  2150,  2150,  2150,  2150,   628,    51,  -596,  -596,
     628,    51,    51,  -596,   637,  -596,   351,  -596,  -596,   675,
    -596,    51,   656,   416,    51,  2190,  2190,  2190,  -596,  -596,
    -596,  2190,  -596,  -596,   681,   174,  2346,  2346,  -596,  -596,
    -596,  -596,   639,  -596,  -596,  -596,  2190,   275,  -596,  -596,
    -596,   683,  -596,   689,  -596,   690,  1619,  2150,   694,   686,
    1619,   447,   695,  2150,  -596,  -596,  -596,  -596,  -596,  2150,
    2150,  2150,  2150,  2150,   552,   552,   552,   552,   552,   628,
      29,   628,  -596,    51,   637,  -596,  1619,  2190,   697,  2190,
    -596,   698,  -596,   700,  -596,  -596,   658,   245,  2502,  -596,
    2190,   283,  2150,   661,  2150,  -596,   152,  2150,  2150,  -596,
     665,   666,   672,   673,   676,  2150,   253,   235,   187,   128,
     271,   112,   628,  -596,  -596,  2190,  -596,  2190,  2190,  -596,
    -596,  -596,   144,   671,  -596,  2190,   286,   552,  -596,   552,
     716,   552,   552,  -596,  -596,  -596,  -596,  -596,   552,   718,
    -596,  -596,  2150,  -596,  2150,   628,  -596,  -596,  -596,  2502,
    -596,  2190,   294,  2150,  2150,   301,   315,   144,  -596,  2190,
     300,   552,   552,  -596,  -596,  -596,  2190,   305,  -596,  2190,
     306,  -596,  2190,   309,  -596,  2190,   311,  -596,  2190,   312,
    -596,  2190,   314,  -596,  2190,   360,  -596,  2190,   361,  -596,
    2190,   362,  -596,  2190,   367,  -596,  2190,   719,  -596,  2190,
    -596
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     9,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   173,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   376,   183,   170,   220,   185,   186,   288,     0,
       0,     0,     0,     0,     0,     0,     0,   294,   294,   267,
       0,     0,     0,     2,     8,    10,    12,    17,    13,    14,
      15,    16,     0,    39,   128,   184,   169,   151,     0,     0,
       0,   294,     0,     0,     4,   101,   107,   109,   118,   123,
     127,   151,     5,   151,     1,     9,     0,    40,   350,     0,
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
      84,    85,    86,    75,    76,    87,     0,    72,     0,   341,
     340,   346,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   300,     0,   330,     0,    52,    69,    73,     0,     0,
       0,     0,    62,    96,   356,     0,   328,   329,   330,   263,
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
     351,    44,    45,     0,     0,     0,   339,     0,   326,   356,
       0,   332,     0,     0,     0,     0,   330,     0,     0,   330,
       0,     0,   299,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   302,   323,     0,     0,     0,     0,     0,   326,
       0,   365,     0,    97,     0,     0,     0,     0,   255,     0,
       0,   257,     0,     0,   129,     0,   137,   139,     0,     0,
     131,   225,   133,   207,   214,     0,   173,   174,   175,   176,
     177,   178,   179,   180,   182,   183,   170,   185,   186,   261,
     261,   267,     0,   184,   151,     0,   143,     0,   134,   140,
       0,   296,   149,     0,     0,   153,     0,   167,     0,   268,
       0,     0,     0,   356,     0,   150,   156,     0,     0,   157,
      19,     0,     0,     0,   249,     0,   244,     0,   251,     0,
       0,     0,   246,    98,    99,     0,   331,   335,   336,   325,
       0,     0,   333,     0,   337,     0,     0,     0,     0,   338,
       0,     0,     0,     0,   347,   301,     0,     0,     0,     0,
       0,     0,   303,   305,   304,   344,   343,   324,    48,     0,
      54,    59,    53,    56,     0,    94,    70,    63,     0,     0,
       0,     0,    64,    66,   358,   327,   357,   359,   256,   262,
     258,   264,   266,   130,     0,     0,   289,     0,     0,   224,
     209,   211,     0,     0,     0,     0,     0,     0,   154,     0,
       0,   152,     0,   162,   161,   297,   160,   159,     0,    20,
       0,     0,     0,   250,   245,   252,     0,     0,   342,     0,
       0,     0,     0,   361,   356,     0,     0,   363,   364,   356,
       0,     0,     0,     0,     0,   345,     0,     0,   330,     0,
     330,     0,     0,     0,     0,     0,     0,     0,    61,    60,
       0,     0,     0,    95,     0,   354,     0,   364,    68,     0,
      67,     0,   164,     0,     0,     0,     0,     0,   214,   219,
     218,     0,   213,   216,   217,   172,     0,     0,   163,   135,
     142,   148,   147,   269,   158,    21,     0,     0,   108,   248,
     247,     0,   349,     0,   348,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,   375,   374,   372,   371,     0,
       0,     0,     0,     0,   322,   315,   314,   313,   312,    50,
      49,    55,    57,    58,    65,   353,     0,     0,     0,     0,
     290,     0,   291,     0,   226,   210,     0,     0,     0,    22,
       0,     0,     0,     0,     0,   360,     0,     0,     0,   362,
       0,     0,     0,     0,     0,     0,   358,     0,     0,     0,
       0,     0,     0,   355,    47,     0,   165,     0,     0,   212,
     215,   217,   145,   146,    23,     0,     0,   311,   334,   309,
       0,   317,   321,   368,   370,   369,   367,   366,   320,     0,
     308,   306,     0,   316,     0,    51,   293,   292,   227,     0,
      24,     0,     0,     0,     0,     0,     0,   144,    25,     0,
       0,   310,   319,   307,   318,    26,     0,     0,    27,     0,
       0,    28,     0,     0,    29,     0,     0,    30,     0,     0,
      31,     0,     0,    32,     0,     0,    33,     0,     0,    34,
       0,     0,    35,     0,     0,    36,     0,     0,    37,     0,
      38
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -596,  -596,   684,   515,   -26,  -596,  -596,  -596,  -596,   219,
    -596,  -596,   133,   130,  -595,  -515,  -596,   129,  -524,  -384,
     537,    -1,   498,   136,   346,    -2,  -203,   -36,   465,   -31,
     310,    15,  -596,   480,  -596,   471,  -596,   195,  -596,   -17,
    -596,   485,  -596,   135,  -596,  -596,    55,   -51,  -596,  -596,
     337,   -43,  -596,   -91,   -49,  -175,  -596,  -174,  -391,  -596,
     -10,   -52,  -596,   142,   -29,  -122,   538,  -596,   383,  -596,
     533,   -78,  1025,  -596,   536,  -596,  -596,  -596,  -596,  -596,
    -596,   687
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     4,    43,    44,    45,    46,    47,   151,    48,    49,
     638,    50,   542,   543,   540,   541,    51,   552,   553,   265,
     266,    52,   139,   544,   272,   140,    65,    66,    67,    68,
      69,    70,   107,   108,   298,   299,   753,   472,   473,    54,
     291,   292,   570,   571,   572,   652,   653,    55,   113,   440,
     441,   201,   202,   109,   279,   280,   281,   282,   283,   144,
     145,    56,   565,   566,   141,   337,   338,   260,   261,   412,
     387,   339,   274,   673,    77,   275,   636,   276,   277,   395,
     398,    73
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      64,    72,   158,   359,   360,   361,   362,    81,    81,   143,
     273,   200,   200,   203,   203,   112,    53,   115,   152,   103,
     205,   351,   352,   165,   353,   629,   300,   164,   640,   111,
     538,   699,   340,   415,   630,   701,   142,   742,   649,   148,
     626,   561,    22,  -352,  -352,    22,    74,    22,   650,    22,
     422,   424,   424,   510,   200,    22,  -352,   249,   166,    53,
     158,   550,   250,   158,   424,    22,   677,    22,   251,   622,
    -352,   285,   623,   621,    22,   321,    22,   252,   683,   247,
     153,   322,   247,    22,   286,    81,   435,   348,   692,   164,
      53,   105,    22,   253,    79,   106,   290,   165,   411,   411,
     651,   522,   330,   442,   200,   416,   341,   631,   561,   560,
     631,   411,   627,   210,   103,   297,   269,   521,   255,   595,
     270,   411,   271,   411,   381,   411,   382,   342,   513,   258,
     206,   404,   105,   405,   259,   411,   106,   406,   164,   407,
     408,    84,   409,   410,   331,   200,   200,   775,    22,    22,
     200,   346,   357,   774,   200,   345,   357,   522,   207,   208,
     347,   246,   248,  -365,  -365,   689,   517,   639,   488,   518,
     690,   772,   680,   411,   363,   634,   388,   388,   228,   209,
     640,   411,   249,   501,   629,   506,   629,   250,   372,   373,
     374,   419,    22,   251,   423,    22,   114,    79,    79,   160,
     411,   146,   252,   632,   657,    28,    22,   300,   207,   208,
     411,   489,   681,   411,   161,   411,   411,   249,   253,    22,
      29,    22,   250,   378,    28,   502,   675,    22,   251,   209,
     225,   760,   162,   -73,   491,   411,   390,   252,   158,    29,
     411,   269,    53,   255,   463,   270,   658,   271,   156,   578,
     157,   437,   579,   253,   258,    22,   678,   401,   419,   259,
     629,   649,   573,   117,   411,   247,   771,   156,   590,   157,
      22,   650,   200,    28,   474,   411,   269,   445,   255,   391,
     270,   154,   271,   434,   351,   352,    22,   353,    29,   258,
    -352,   326,   671,   327,   259,   404,   297,   405,   424,   666,
      22,   406,   155,   407,   408,   411,   409,   410,    22,   509,
     770,    22,   511,   720,   509,   388,   156,   478,   157,    22,
     471,   755,   476,   411,   781,    22,   479,   480,   481,   323,
      22,    22,   789,   769,    22,   324,    22,    22,   796,    22,
     273,   411,   487,   799,   802,   411,   490,   805,   493,   808,
     811,   773,   814,   152,   503,   624,   504,   159,   625,   411,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,   402,   227,   200,
     793,   559,   200,   403,   562,    22,    22,    22,   200,   411,
     203,    29,    22,   158,    53,   794,   568,   531,   817,   820,
     823,   228,    31,   411,   419,   826,   585,   289,   463,   463,
     431,   428,   432,    81,   526,    34,    35,   429,   527,    61,
     528,    38,   485,    39,   494,    40,   705,   264,   322,   354,
     495,   143,   706,   563,    22,   268,   564,    41,   603,   574,
     606,    22,   672,   608,    22,   149,   150,    22,   287,   200,
     200,   596,   597,   519,   520,   610,   611,   612,   142,   613,
     614,   288,   730,   731,   732,   576,   733,   734,   577,   423,
     635,   603,   637,   211,   212,   213,   214,   215,   216,   217,
     218,   325,   294,   496,   644,   322,   588,   497,   645,   589,
       1,     2,     3,   219,   220,   221,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,   302,   498,   752,   499,   200,   303,   357,
     222,   223,   224,    58,   514,   332,   515,    29,   200,   305,
     474,   306,   375,   376,   377,   307,    59,    30,    31,   308,
      32,   309,    33,   642,   643,    80,    83,    60,    85,    87,
      81,    34,    35,    36,   467,   295,   655,    38,   310,   296,
     468,    40,   328,   311,    62,    63,   312,   313,   314,   315,
     317,   318,   319,    41,   320,   335,   787,   329,   662,   350,
     663,    89,    90,    91,    92,    93,    94,    95,   665,   425,
     668,   426,   427,   433,    96,   430,   438,   439,   725,    22,
      97,   444,   729,   158,   466,   463,   463,   596,   597,    98,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,   470,   743,  -254,
     484,   486,   500,   505,   209,    99,   507,   516,   523,   524,
     411,    29,   539,   712,   713,   714,   100,   101,   548,   716,
     549,   551,    31,   477,   480,   481,   558,   580,   102,   581,
     582,   586,   594,   583,   719,    34,    35,   592,   584,    61,
     600,    38,   593,    39,   641,    40,   364,   365,   366,   367,
     368,   369,   370,   371,   648,   656,   659,    41,    57,    71,
     660,   664,    75,    76,    78,    82,    82,    86,    88,   669,
     670,   676,   684,   679,   682,   744,   685,   746,   686,   687,
     688,   691,   693,   707,   709,   717,   104,   110,   754,   249,
     718,   722,   116,   723,   250,   728,   104,   147,   724,    22,
     251,    57,   727,   748,   735,   745,   747,   749,   758,   252,
     763,   764,   379,   776,   163,   777,   778,   765,   766,   204,
     204,   767,   779,   780,   783,   253,   784,   829,   628,   226,
     700,   702,    57,   704,   547,   417,   436,    82,   703,   469,
      82,   263,   750,   267,   661,   278,   443,   569,   269,   788,
     255,   512,   270,   715,   271,   710,   525,   795,   392,   384,
       0,   258,   204,   284,   798,     0,   259,   801,     0,     0,
     804,   293,     0,   807,   301,     0,   810,     0,     0,   813,
       0,   104,   816,     0,     0,   819,     0,     0,   822,     0,
       0,   825,     0,     0,   828,     0,     0,   830,     0,     0,
       0,     0,     0,     0,     0,     0,   333,   334,     0,     0,
     278,   278,   204,     0,     0,     0,     0,     0,   104,     0,
     349,    89,    90,    91,    92,    93,    94,    95,     0,     0,
       0,     0,     0,     0,    96,     0,     0,     0,     0,    22,
      97,     0,     0,     0,     0,     0,     0,     0,     0,    98,
       0,     0,     0,   204,   204,   284,   355,     0,   204,     0,
       0,     0,   204,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   249,     0,     0,    99,     0,   250,     0,     0,
       0,     0,    22,   251,    57,   380,   100,   101,     0,     0,
     385,     0,   252,     0,     0,     0,     0,     0,   102,     0,
       0,     0,     0,   278,     0,   278,     0,     0,   253,   383,
      78,   278,   278,   396,   399,   400,     0,     0,     0,     0,
     278,   414,     0,    82,     0,   418,   278,   420,   421,   278,
       0,   269,   386,   255,     0,   270,     0,   271,     0,     0,
       0,     0,     0,   110,   258,     0,     0,     0,   293,   259,
       0,     0,   464,   465,     0,   301,     0,     7,     8,     9,
      10,    11,     0,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
     204,     0,     0,     0,   104,     0,     0,     0,     0,     0,
       0,     0,     0,   263,    29,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,   492,    32,     0,    33,
       0,     0,     0,     0,     0,     0,     0,     0,    34,    35,
      36,     0,    37,     0,    38,     0,    39,     0,    40,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
      41,     0,     0,    42,   278,     0,     0,   278,     0,   278,
     278,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     263,     0,   278,   278,   104,   278,   278,   278,   278,   278,
       0,   267,     0,   545,   546,   278,     0,     0,     0,   262,
       0,   278,   278,   278,   278,     0,     0,   204,     0,   284,
     204,     0,     0,     0,     0,   567,   204,     0,     0,     0,
     183,   184,   185,   186,   187,   188,   189,   190,     0,   191,
      22,   192,   193,    25,   194,   195,   464,   464,   465,   575,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   356,     0,     0,     0,     0,     0,     0,     0,   278,
       0,   278,     0,     0,     0,     0,     0,     0,     0,   591,
       0,     0,     0,     0,     0,     0,   284,   204,   204,   196,
       0,   197,   278,   198,     0,   199,     0,   278,     0,     0,
     278,     0,   602,   278,   278,   278,     0,   607,   278,     0,
     278,     0,   249,   278,   618,   620,     0,   250,     0,     0,
       0,     0,    22,   251,     0,     0,    82,     0,     0,     0,
     385,   633,   252,     0,   278,   278,   278,   278,   267,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   253,     0,
       0,     0,     0,     0,     0,   204,     0,     0,     0,   654,
       0,     0,     0,     0,     0,     0,   204,     0,     0,     0,
       0,   482,   386,   255,     0,   256,     0,   257,   667,   389,
       0,   394,   397,     0,   258,     0,     0,   674,   413,   259,
     183,   184,   185,   186,   187,   188,   189,   190,     0,   191,
      22,   192,   193,    25,   194,   195,     0,     0,   278,   278,
     278,   278,   278,     0,   545,     0,     0,     0,   545,   545,
       0,   358,   249,     0,     0,     0,     0,   250,   708,     0,
     567,   711,    22,   251,     0,     0,     0,     0,     0,     0,
       0,     0,   252,   464,   464,     0,     0,     0,     0,   196,
       0,   197,     0,   198,   721,   199,     0,     0,   253,     0,
       0,   483,     0,   278,   278,     0,     0,   278,     0,     0,
     278,     0,     0,     0,     0,     0,   278,   278,   278,   278,
     278,   269,   508,   255,     0,   270,     0,   271,     0,     0,
     633,     0,     0,   278,   258,     0,     0,     0,     0,   259,
       0,     0,     0,     0,   751,     0,     0,     0,   756,   278,
       0,   278,     0,     0,   278,   278,     0,     0,     0,     0,
       0,     0,   278,     0,     0,     0,     0,     0,   262,     0,
     529,   530,     0,   532,   533,   534,   536,   537,     0,     0,
       0,     0,     0,   782,     0,     0,     0,     0,     0,   554,
     555,   556,   557,     0,     0,     0,     0,     0,     0,   278,
       0,   278,   249,     0,     0,     0,     0,   250,     0,   790,
     278,   278,    22,   251,     0,     0,     0,   797,     0,     0,
       0,     0,   252,     0,   800,     0,     0,   803,     0,     0,
     806,     0,     0,   809,     0,     0,   812,     0,   253,   815,
       0,     0,   818,     0,     0,   821,     0,   389,   824,     0,
       0,   827,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   393,     0,   255,     0,   270,     0,   271,     0,     0,
     598,     0,     0,     0,   258,   599,     0,   535,   601,   259,
       0,   604,   605,     0,     0,     0,   609,     0,   615,     0,
       0,   616,   617,   619,     5,     6,     0,     7,     8,     9,
      10,    11,     0,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    29,     0,     0,     0,     0,     0,
       0,   249,     0,     0,    30,    31,   250,    32,     0,    33,
       0,    22,   251,     0,     0,     0,     0,     0,    34,    35,
      36,   252,    37,     0,    38,     0,    39,     0,    40,     0,
       0,     0,     0,     0,   249,     0,     0,   253,     0,   250,
      41,     0,     0,    42,    22,   251,   694,   695,   696,   697,
     698,     0,     0,     0,   252,     0,     0,     0,     0,     0,
     254,     0,   255,     0,   256,     0,   257,     0,     0,     0,
     253,     0,     0,   258,     0,     0,     0,     0,   259,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   269,     0,   255,     0,   270,     0,   271,
       0,     0,   726,     0,     0,     0,   258,     0,   736,     0,
       0,   259,     0,     0,   737,   738,   739,   740,   741,     0,
       0,     0,     0,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,   343,    27,    28,
       0,     0,   344,     0,     0,     0,     0,   757,     0,   759,
      58,     0,   761,   762,    29,     0,     0,     0,     0,     0,
     768,     0,     0,    59,    30,    31,     0,    32,     0,    33,
       0,     0,     0,     0,    60,     0,     0,     0,    34,    35,
      36,     0,    61,     0,    38,     0,    39,     0,    40,     0,
       0,    62,    63,     0,     0,     0,     0,   785,     0,   786,
      41,     0,     0,     0,     0,     0,     0,     0,   791,   792,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,     0,     0,     0,
       0,     0,     0,     0,     0,   118,   119,   120,   121,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    30,    31,     0,    32,     0,    33,     0,     0,   134,
     135,    60,     0,     0,   136,    34,    35,    36,     0,    61,
       0,    38,     0,    39,     0,    40,     0,     0,    62,    63,
     137,     0,     0,     0,     0,     0,     0,    41,     0,     0,
       0,     0,   138,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     0,   167,   168,
     169,   170,   171,   172,    29,   173,   174,   175,   128,   176,
     177,   178,   179,   133,    30,    31,     0,    32,     0,    33,
       0,     0,   180,   181,    60,     0,     0,   182,    34,    35,
      36,     0,    61,     0,    38,     0,    39,     0,    40,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
      41,   446,   447,   448,   449,   450,   451,   452,   453,    20,
     454,    22,   455,   456,    25,   457,   458,    28,     0,     0,
       0,     0,     0,     0,     0,     0,   167,   168,   169,   170,
     171,   172,    29,   173,   174,   175,   128,   176,   177,   178,
     179,   133,    30,    31,     0,    32,     0,    33,     0,     0,
     180,   181,    60,     0,     0,   182,    34,    35,    36,     0,
     459,     0,   460,     0,   461,     0,   462,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,    41,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
      29,     0,     0,     0,     0,     0,     0,   249,     0,    59,
      30,    31,   250,    32,     0,    33,     0,    22,   251,     0,
      60,     0,     0,     0,    34,    35,    36,   252,    61,     0,
      38,   475,    39,     0,    40,     0,     0,    62,    63,     0,
       0,     0,     0,   253,     0,     0,    41,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,   336,     0,   255,     0,
     270,     0,   271,     0,    58,     0,     0,     0,    29,   258,
       0,     0,     0,     0,   259,   249,     0,    59,    30,    31,
     250,    32,     0,    33,     0,    22,   251,     0,    60,     0,
       0,     0,    34,    35,    36,   252,    61,     0,    38,   587,
      39,     0,    40,     0,     0,    62,    63,     0,     0,     0,
       0,   253,     0,     0,    41,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,     0,     0,   393,     0,   255,     0,   270,     0,
     271,     0,    58,     0,     0,     0,    29,   258,     0,     0,
       0,     0,   259,     0,     0,    59,    30,    31,     0,    32,
       0,    33,     0,     0,     0,     0,    60,     0,     0,     0,
      34,    35,    36,     0,    61,     0,    38,     0,    39,     0,
      40,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,    41,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,    29,     0,     0,     0,     0,     0,
       0,     0,     0,    59,    30,    31,     0,    32,     0,    33,
       0,     0,     0,     0,    60,     0,     0,     0,    34,    35,
      36,     0,   295,     0,    38,     0,   296,     0,    40,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
      41,   446,   447,   448,   449,   450,   451,   452,   453,    20,
     454,    22,   455,   456,    25,   457,   458,    28,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,    29,     0,     0,     0,     0,     0,     0,     0,
       0,    59,    30,    31,     0,    32,     0,    33,     0,     0,
       0,     0,    60,     0,     0,     0,    34,    35,    36,     0,
     459,     0,   460,     0,   461,     0,   462,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,    41,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
      29,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,     0,    32,     0,    33,     0,     0,     0,     0,
      60,     0,     0,     0,    34,    35,    36,     0,    61,   316,
      38,     0,    39,     0,    40,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    41,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,    29,     0,
       0,     0,     0,     0,     0,     0,     0,    59,    30,    31,
       0,    32,     0,    33,     0,     0,     0,     0,    60,     0,
       0,     0,    34,    35,    36,     0,    61,     0,    38,     0,
      39,     0,    40,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    41,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,    29,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    30,    31,     0,    32,
       0,    33,     0,     0,     0,     0,    60,     0,     0,     0,
      34,    35,    36,     0,    61,     0,    38,     0,    39,     0,
      40,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    41,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,    29,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,     0,    32,     0,    33,
       0,     0,     0,     0,     0,     0,     0,     0,    34,    35,
      36,     0,    61,   304,    38,     0,    39,     0,    40,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      41,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,    29,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    30,    31,     0,    32,     0,    33,     0,     0,
       0,     0,     0,     0,     0,     0,    34,    35,    36,     0,
      61,     0,    38,     0,    39,     0,    40,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    41,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      29,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,     0,    32,     0,    33,     0,     0,     0,     0,
       0,     0,     0,     0,    34,    35,    36,     0,    61,     0,
      38,   646,    39,     0,    40,     0,     0,     0,   183,   184,
     185,   186,   187,   188,   189,   190,    41,   191,    22,   192,
     193,    25,   194,   195,     0,     0,   647,   183,   184,   185,
     186,   187,   188,   189,   190,     0,   191,    22,   192,   193,
      25,   194,   195,     0,     0,     0,     0,     0,     0,     0,
     229,   230,   231,   232,   233,   234,   235,   236,   237,   238,
       0,   239,   240,   241,   242,     0,     0,   196,     0,   197,
       0,   198,     0,   199,   243,   244,     0,     0,     0,   245,
       0,     0,     0,     0,     0,     0,   196,     0,   197,     0,
     198,     0,   199,     0,     0,   137,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   138
};

static const yytype_int16 yycheck[] =
{
       2,     3,    54,   206,   207,   208,   209,     8,     9,    38,
      88,    62,    63,    62,    63,    32,     1,    34,    44,    29,
      63,   196,   197,    59,   198,   540,   117,    58,   552,    31,
     414,   626,   154,     8,     8,   630,    38,     8,    16,    41,
       8,   432,    25,    33,    33,    25,     0,    25,    26,    25,
       8,    41,    41,    43,   105,    25,    33,    15,    60,    44,
     112,    34,    20,   115,    41,    25,    43,    25,    26,    36,
      33,    69,    39,    38,    25,    75,    25,    35,    41,    80,
      39,    81,    83,    25,    82,    86,    69,    38,    41,   120,
      75,    74,    25,    51,    74,    78,   113,   133,    88,    88,
      78,    74,    38,    79,   155,    80,   155,    81,   499,    79,
      81,    88,    80,    69,   124,   117,    74,    34,    76,    79,
      78,    88,    80,    88,   246,    88,   248,   156,    43,    87,
      36,    38,    74,    40,    92,    88,    78,    44,   169,    46,
      47,    74,    49,    50,    80,   196,   197,   742,    25,    25,
     201,   161,   201,    41,   205,   157,   205,    74,    64,    65,
     162,    38,    38,    80,    81,    43,    41,   551,    34,    44,
      43,    43,    34,    88,   210,     8,   254,   255,    46,    85,
     704,    88,    15,    38,   699,    53,   701,    20,   219,   220,
     221,   269,    25,    26,   272,    25,    78,    74,    74,    31,
      88,    20,    35,    33,    38,    31,    25,   298,    64,    65,
      88,    77,    74,    88,    46,    88,    88,    15,    51,    25,
      46,    25,    20,   225,    31,    80,    79,    25,    26,    85,
      38,    79,    38,    39,    38,    88,    34,    35,   290,    46,
      88,    74,   227,    76,   295,    78,    80,    80,    74,    77,
      76,   287,    80,    51,    87,    25,    80,   259,   336,    92,
     775,    16,    69,    78,    88,   266,    79,    74,    38,    76,
      25,    26,   323,    31,   323,    88,    74,   294,    76,    77,
      78,    39,    80,   285,   459,   460,    25,   461,    46,    87,
      33,    79,    75,    81,    92,    38,   298,    40,    41,    38,
      25,    44,    60,    46,    47,    88,    49,    50,    25,   387,
      75,    25,   390,    38,   392,   393,    74,   327,    76,    25,
     322,    38,   324,    88,    38,    25,   328,   329,   330,    80,
      25,    25,    38,    80,    25,    86,    25,    25,    38,    25,
     418,    88,   344,    38,    38,    88,   348,    38,   350,    38,
      38,    80,    38,   379,   356,    36,   358,    28,    39,    88,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    75,     8,   430,
      79,   430,   433,    81,   433,    25,    25,    25,   439,    88,
     439,    46,    25,   445,   379,    80,   439,   407,    38,    38,
      38,    46,    57,    88,   482,    38,   484,    62,   459,   460,
      79,    75,    81,   414,    74,    70,    71,    81,    78,    74,
      80,    76,    75,    78,    75,    80,    75,    32,    81,    20,
      81,   460,    81,   435,    25,    32,    20,    92,   516,    20,
     518,    25,    20,   521,    25,    93,    94,    25,    38,   500,
     501,   500,   501,    80,    81,    15,    16,    17,   460,    19,
      20,    63,    15,    16,    17,   467,    19,    20,   470,   547,
     548,   549,   550,    47,    48,    49,    50,    51,    52,    53,
      54,    77,    38,    77,    34,    81,   488,    81,    38,   491,
       3,     4,     5,    40,    41,    42,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    75,    79,   718,    81,   568,    75,   568,
      43,    44,    45,    42,    79,    92,    81,    46,   579,    75,
     579,    75,   222,   223,   224,    75,    55,    56,    57,    75,
      59,    75,    61,    80,    81,     8,     9,    66,    10,    11,
     551,    70,    71,    72,    73,    74,   573,    76,    75,    78,
      79,    80,    38,    75,    83,    84,    75,    75,    75,    75,
      75,    75,    75,    92,    75,    96,   779,    38,   580,    67,
     582,     7,     8,     9,    10,    11,    12,    13,   590,    33,
     592,    43,    41,    38,    20,    81,    80,    80,   676,    25,
      26,    37,   680,   655,    82,   656,   657,   656,   657,    35,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    38,   706,    38,
      32,    77,    38,    46,    85,    61,    75,    34,    46,    92,
      88,    46,    14,   645,   646,   647,    72,    73,    81,   651,
      34,    14,    57,    79,   656,   657,    75,    36,    84,    77,
      38,    77,    77,    80,   666,    70,    71,    68,    80,    74,
      80,    76,    75,    78,    34,    80,   211,   212,   213,   214,
     215,   216,   217,   218,    80,    38,    79,    92,     1,     2,
      82,    77,     5,     6,     7,     8,     9,    10,    11,    80,
      80,    34,    75,    80,    80,   707,    75,   709,    75,    75,
      75,    34,    34,    38,    58,    34,    29,    30,   720,    15,
      81,    38,    35,    34,    20,    39,    39,    40,    38,    25,
      26,    44,    38,    33,    39,    38,    38,    79,    77,    35,
      75,    75,   227,   745,    57,   747,   748,    75,    75,    62,
      63,    75,    81,   755,    38,    51,    38,    38,   539,    75,
     627,   631,    75,   634,   418,   267,   286,    80,   632,   298,
      83,    84,   717,    86,   579,    88,   291,   440,    74,   781,
      76,    77,    78,   648,    80,   643,   403,   789,   255,   253,
      -1,    87,   105,   106,   796,    -1,    92,   799,    -1,    -1,
     802,   114,    -1,   805,   117,    -1,   808,    -1,    -1,   811,
      -1,   124,   814,    -1,    -1,   817,    -1,    -1,   820,    -1,
      -1,   823,    -1,    -1,   826,    -1,    -1,   829,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,    -1,
     153,   154,   155,    -1,    -1,    -1,    -1,    -1,   161,    -1,
     163,     7,     8,     9,    10,    11,    12,    13,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    -1,    25,
      26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    35,
      -1,    -1,    -1,   196,   197,   198,   199,    -1,   201,    -1,
      -1,    -1,   205,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    15,    -1,    -1,    61,    -1,    20,    -1,    -1,
      -1,    -1,    25,    26,   227,   228,    72,    73,    -1,    -1,
      33,    -1,    35,    -1,    -1,    -1,    -1,    -1,    84,    -1,
      -1,    -1,    -1,   246,    -1,   248,    -1,    -1,    51,   252,
     253,   254,   255,   256,   257,   258,    -1,    -1,    -1,    -1,
     263,   264,    -1,   266,    -1,   268,   269,   270,   271,   272,
      -1,    74,    75,    76,    -1,    78,    -1,    80,    -1,    -1,
      -1,    -1,    -1,   286,    87,    -1,    -1,    -1,   291,    92,
      -1,    -1,   295,   296,    -1,   298,    -1,     9,    10,    11,
      12,    13,    -1,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
     323,    -1,    -1,    -1,   327,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   336,    46,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    56,    57,   349,    59,    -1,    61,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   379,    -1,    -1,    -1,
      92,    -1,    -1,    95,   387,    -1,    -1,   390,    -1,   392,
     393,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     403,    -1,   405,   406,   407,   408,   409,   410,   411,   412,
      -1,   414,    -1,   416,   417,   418,    -1,    -1,    -1,    84,
      -1,   424,   425,   426,   427,    -1,    -1,   430,    -1,   432,
     433,    -1,    -1,    -1,    -1,   438,   439,    -1,    -1,    -1,
      15,    16,    17,    18,    19,    20,    21,    22,    -1,    24,
      25,    26,    27,    28,    29,    30,   459,   460,   461,   462,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   482,
      -1,   484,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   492,
      -1,    -1,    -1,    -1,    -1,    -1,   499,   500,   501,    74,
      -1,    76,   505,    78,    -1,    80,    -1,   510,    -1,    -1,
     513,    -1,   515,   516,   517,   518,    -1,   520,   521,    -1,
     523,    -1,    15,   526,   527,   528,    -1,    20,    -1,    -1,
      -1,    -1,    25,    26,    -1,    -1,   539,    -1,    -1,    -1,
      33,   544,    35,    -1,   547,   548,   549,   550,   551,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    -1,
      -1,    -1,    -1,    -1,    -1,   568,    -1,    -1,    -1,   572,
      -1,    -1,    -1,    -1,    -1,    -1,   579,    -1,    -1,    -1,
      -1,    74,    75,    76,    -1,    78,    -1,    80,   591,   254,
      -1,   256,   257,    -1,    87,    -1,    -1,   600,   263,    92,
      15,    16,    17,    18,    19,    20,    21,    22,    -1,    24,
      25,    26,    27,    28,    29,    30,    -1,    -1,   621,   622,
     623,   624,   625,    -1,   627,    -1,    -1,    -1,   631,   632,
      -1,    46,    15,    -1,    -1,    -1,    -1,    20,   641,    -1,
     643,   644,    25,    26,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    35,   656,   657,    -1,    -1,    -1,    -1,    74,
      -1,    76,    -1,    78,   667,    80,    -1,    -1,    51,    -1,
      -1,   336,    -1,   676,   677,    -1,    -1,   680,    -1,    -1,
     683,    -1,    -1,    -1,    -1,    -1,   689,   690,   691,   692,
     693,    74,    75,    76,    -1,    78,    -1,    80,    -1,    -1,
     703,    -1,    -1,   706,    87,    -1,    -1,    -1,    -1,    92,
      -1,    -1,    -1,    -1,   717,    -1,    -1,    -1,   721,   722,
      -1,   724,    -1,    -1,   727,   728,    -1,    -1,    -1,    -1,
      -1,    -1,   735,    -1,    -1,    -1,    -1,    -1,   403,    -1,
     405,   406,    -1,   408,   409,   410,   411,   412,    -1,    -1,
      -1,    -1,    -1,   756,    -1,    -1,    -1,    -1,    -1,   424,
     425,   426,   427,    -1,    -1,    -1,    -1,    -1,    -1,   772,
      -1,   774,    15,    -1,    -1,    -1,    -1,    20,    -1,   782,
     783,   784,    25,    26,    -1,    -1,    -1,   790,    -1,    -1,
      -1,    -1,    35,    -1,   797,    -1,    -1,   800,    -1,    -1,
     803,    -1,    -1,   806,    -1,    -1,   809,    -1,    51,   812,
      -1,    -1,   815,    -1,    -1,   818,    -1,   482,   821,    -1,
      -1,   824,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,
     505,    -1,    -1,    -1,    87,   510,    -1,    90,   513,    92,
      -1,   516,   517,    -1,    -1,    -1,   521,    -1,   523,    -1,
      -1,   526,   527,   528,     6,     7,    -1,     9,    10,    11,
      12,    13,    -1,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,
      -1,    15,    -1,    -1,    56,    57,    20,    59,    -1,    61,
      -1,    25,    26,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    35,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    15,    -1,    -1,    51,    -1,    20,
      92,    -1,    -1,    95,    25,    26,   621,   622,   623,   624,
     625,    -1,    -1,    -1,    35,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,    -1,
      51,    -1,    -1,    87,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    76,    -1,    78,    -1,    80,
      -1,    -1,   677,    -1,    -1,    -1,    87,    -1,   683,    -1,
      -1,    92,    -1,    -1,   689,   690,   691,   692,   693,    -1,
      -1,    -1,    -1,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    34,    -1,    -1,    -1,    -1,   722,    -1,   724,
      42,    -1,   727,   728,    46,    -1,    -1,    -1,    -1,    -1,
     735,    -1,    -1,    55,    56,    57,    -1,    59,    -1,    61,
      -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,    70,    71,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      -1,    83,    84,    -1,    -1,    -1,    -1,   772,    -1,   774,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   783,   784,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    59,    -1,    61,    -1,    -1,    64,
      65,    66,    -1,    -1,    69,    70,    71,    72,    -1,    74,
      -1,    76,    -1,    78,    -1,    80,    -1,    -1,    83,    84,
      85,    -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,
      -1,    -1,    97,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    -1,    61,
      -1,    -1,    64,    65,    66,    -1,    -1,    69,    70,    71,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      -1,    83,    84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      92,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    59,    -1,    61,    -1,    -1,
      64,    65,    66,    -1,    -1,    69,    70,    71,    72,    -1,
      74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,    83,
      84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    -1,    -1,    15,    -1,    55,
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
      46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      56,    57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    70,    71,    72,    -1,    74,    75,
      76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    56,    57,
      -1,    59,    -1,    61,    -1,    -1,    -1,    -1,    66,    -1,
      -1,    -1,    70,    71,    72,    -1,    74,    -1,    76,    -1,
      78,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    42,    -1,    -1,    -1,    46,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    56,    57,    -1,    59,
      -1,    61,    -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,
      70,    71,    72,    -1,    74,    -1,    76,    -1,    78,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      42,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    56,    57,    -1,    59,    -1,    61,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    -1,    74,    75,    76,    -1,    78,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      92,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,
      -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    56,    57,    -1,    59,    -1,    61,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    -1,
      74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      56,    57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    -1,    74,    -1,
      76,     8,    78,    -1,    80,    -1,    -1,    -1,    15,    16,
      17,    18,    19,    20,    21,    22,    92,    24,    25,    26,
      27,    28,    29,    30,    -1,    -1,    33,    15,    16,    17,
      18,    19,    20,    21,    22,    -1,    24,    25,    26,    27,
      28,    29,    30,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      -1,    51,    52,    53,    54,    -1,    -1,    74,    -1,    76,
      -1,    78,    -1,    80,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    76,    -1,
      78,    -1,    80,    -1,    -1,    85,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    97
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
     109,   114,   119,   129,   137,   145,   159,   179,    42,    55,
      66,    74,    83,    84,   123,   124,   125,   126,   127,   128,
     129,   179,   123,   179,     0,   179,   179,   172,   179,    74,
     118,   119,   179,   118,    74,   164,   179,   164,   179,     7,
       8,     9,    10,    11,    12,    13,    20,    26,    35,    61,
      72,    73,    84,   158,   179,    74,    78,   130,   131,   151,
     179,   123,   137,   146,    78,   137,   179,    78,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    64,    65,    69,    85,    97,   120,
     123,   162,   123,   162,   157,   158,    20,   179,   123,    93,
      94,   105,   102,    39,    39,    60,    74,    76,   159,    28,
      31,    46,    38,   179,   127,   125,   123,    40,    41,    42,
      43,    44,    45,    47,    48,    49,    51,    52,    53,    54,
      64,    65,    69,    15,    16,    17,    18,    19,    20,    21,
      22,    24,    26,    27,    29,    30,    74,    76,    78,    80,
     145,   149,   150,   152,   179,   149,    36,    64,    65,    85,
      69,    47,    48,    49,    50,    51,    52,    53,    54,    40,
      41,    42,    43,    44,    45,    38,   100,     8,    46,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    51,
      52,    53,    54,    64,    65,    69,    38,   119,    38,    15,
      20,    26,    35,    51,    74,    76,    78,    80,    87,    92,
     165,   166,   170,   179,    32,   117,   118,   179,    32,    74,
      78,    80,   122,   169,   170,   173,   175,   176,   179,   152,
     153,   154,   155,   156,   179,    69,    82,    38,    63,    62,
     137,   138,   139,   179,    38,    74,    78,   123,   132,   133,
     151,   179,    75,    75,    75,    75,    75,    75,    75,    75,
      75,    75,    75,    75,    75,    75,    75,    75,    75,    75,
      75,    75,    81,    80,    86,    77,    79,    81,    38,    38,
      38,    80,    92,   179,   179,    96,    74,   163,   164,   169,
     163,   152,   162,    29,    34,   123,   158,   123,    38,   179,
      67,   153,   153,   155,    20,   179,    46,   152,    46,   124,
     124,   124,   124,   125,   126,   126,   126,   126,   126,   126,
     126,   126,   127,   127,   127,   128,   128,   128,   123,   101,
     179,   163,   163,   179,   172,    33,    75,   168,   169,   170,
      34,    77,   168,    74,   170,   177,   179,   170,   178,   179,
     179,   123,    75,    81,    38,    40,    44,    46,    47,    49,
      50,    88,   167,   170,   179,     8,    80,   120,   179,   169,
     179,   179,     8,   169,    41,    33,    43,    41,    75,    81,
      81,    79,    81,    38,   123,    69,   131,   125,    80,    80,
     147,   148,    79,   139,    37,   137,    15,    16,    17,    18,
      19,    20,    21,    22,    24,    26,    27,    29,    30,    74,
      76,    78,    80,   145,   179,   179,    82,    73,    79,   133,
      38,   123,   135,   136,   152,    77,   123,    79,   158,   123,
     123,   123,    74,   170,    32,    75,    77,   123,    34,    77,
     123,    38,   179,   123,    75,    81,    77,    81,    79,    81,
      38,    38,    80,   123,   123,    46,    53,    75,    75,   169,
      43,   169,    77,    43,    79,    81,    34,    41,    44,    80,
      81,    34,    74,    46,    92,   166,    74,    78,    80,   170,
     170,   158,   170,   170,   170,    90,   170,   170,   117,    14,
     112,   113,   110,   111,   121,   179,   179,   122,    81,    34,
      34,    14,   115,   116,   170,   170,   170,   170,    75,   152,
      79,   156,   152,   123,    20,   160,   161,   179,   149,   148,
     140,   141,   142,    69,    20,   179,   123,   123,    77,    80,
      36,    77,    38,    80,    80,   169,    77,    77,   123,   123,
      38,   179,    68,    75,    77,    79,   152,   152,   170,   170,
      80,   170,   179,   169,   170,   170,   169,   179,   169,   170,
      15,    16,    17,    19,    20,   170,   170,   170,   179,   170,
     179,    38,    36,    39,    36,    39,     8,    80,   107,   113,
       8,    81,    33,   179,     8,   169,   174,   169,   108,   117,
     116,    34,    80,    81,    34,    38,     8,    33,    80,    16,
      26,    78,   143,   144,   179,   137,    38,    38,    80,    79,
      82,   135,   123,   123,    77,   123,    38,   179,   123,    80,
      80,    75,    20,   171,   179,    79,    34,    43,    80,    80,
      34,    74,    80,    41,    75,    75,    75,    75,    75,    43,
      43,    34,    41,    34,   170,   170,   170,   170,   170,   112,
     110,   112,   111,   121,   115,    75,    81,    38,   179,    58,
     161,   179,   123,   123,   123,   141,   123,    34,    81,   123,
      38,   179,    38,    34,    38,   169,   170,    38,    39,   169,
      15,    16,    17,    19,    20,    39,   170,   170,   170,   170,
     170,   170,     8,   169,   123,    38,   123,    38,    33,    79,
     144,   179,   124,   134,   123,    38,   179,   170,    77,   170,
      79,   170,   170,    75,    75,    75,    75,    75,   170,    80,
      75,    79,    43,    80,    41,   112,   123,   123,   123,    81,
     123,    38,   179,    38,    38,   170,   170,   124,   123,    38,
     179,   170,   170,    79,    80,   123,    38,   179,   123,    38,
     179,   123,    38,   179,   123,    38,   179,   123,    38,   179,
     123,    38,   179,   123,    38,   179,   123,    38,   179,   123,
      38,   179,   123,    38,   179,   123,    38,   179,   123,    38,
     123
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
     170,   170,   170,   170,   170,   170,   170,   170,   171,   171,
     172,   172,   173,   173,   174,   174,   175,   175,   176,   176,
     177,   177,   178,   178,   178,   178,   178,   178,   178,   178,
     178,   178,   178,   178,   178,   178,   179
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
       1,     3,     2,     3,     7,     3,     3,     3,     3,     2,
       1,     1,     4,     3,     3,     4,     1,     3,     1,     1,
       1,     3,     1,     5,     1,     3,     1,     3,     3,     3,
       5,     3,     5,     3,     3,     1,     6,     6,     6,     6,
       6,     4,     4,     4,     4,     4,     1
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
#line 492 "hexpr.y"
                            { yyParsedModule = (yyvsp[0].module);                     }
#line 2881 "hexpr.parse.C"
    break;

  case 3: /* s: "dodefn" id "=" l0expr  */
#line 493 "hexpr.y"
                            { yyParsedVar    = *(yyvsp[-2].string); yyParsedExpr = (yyvsp[0].exp); }
#line 2887 "hexpr.parse.C"
    break;

  case 4: /* s: "dodefn" l0expr  */
#line 494 "hexpr.y"
                            { yyParsedVar    = "";  yyParsedExpr = (yyvsp[0].exp); }
#line 2893 "hexpr.parse.C"
    break;

  case 5: /* s: "doexpr" l0expr  */
#line 495 "hexpr.y"
                            { yyParsedExpr   = (yyvsp[0].exp);                     }
#line 2899 "hexpr.parse.C"
    break;

  case 6: /* module: "option" id module  */
#line 498 "hexpr.y"
                                 { (yyval.module) = (yyvsp[0].module); (yyval.module)->setOption(*(yyvsp[-1].string), m((yylsp[-2]))); }
#line 2905 "hexpr.parse.C"
    break;

  case 7: /* module: "module" id "where" defs  */
#line 499 "hexpr.y"
                                 { (yyval.module) = new Module(*(yyvsp[-2].string), *(yyvsp[0].mdefs)); }
#line 2911 "hexpr.parse.C"
    break;

  case 8: /* module: defs  */
#line 500 "hexpr.y"
                                 { (yyval.module) = new Module(freshName(), *(yyvsp[0].mdefs)); }
#line 2917 "hexpr.parse.C"
    break;

  case 9: /* defs: %empty  */
#line 502 "hexpr.y"
                    { (yyval.mdefs) = autorelease(new ModuleDefs()); }
#line 2923 "hexpr.parse.C"
    break;

  case 10: /* defs: def  */
#line 503 "hexpr.y"
                    { (yyval.mdefs) = autorelease(new ModuleDefs()); (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2929 "hexpr.parse.C"
    break;

  case 11: /* defs: defs def  */
#line 504 "hexpr.y"
                    { (yyval.mdefs) = (yyvsp[-1].mdefs);                            (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2935 "hexpr.parse.C"
    break;

  case 12: /* def: importdef  */
#line 506 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2941 "hexpr.parse.C"
    break;

  case 13: /* def: tydef  */
#line 507 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2947 "hexpr.parse.C"
    break;

  case 14: /* def: vartybind  */
#line 508 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mvtydef); }
#line 2953 "hexpr.parse.C"
    break;

  case 15: /* def: classdef  */
#line 509 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2959 "hexpr.parse.C"
    break;

  case 16: /* def: instdef  */
#line 510 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2965 "hexpr.parse.C"
    break;

  case 17: /* def: pragmadef  */
#line 511 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2971 "hexpr.parse.C"
    break;

  case 18: /* def: id "=" l0expr  */
#line 513 "hexpr.y"
                   { (yyval.mdef) = new MVarDef(list(*(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2977 "hexpr.parse.C"
    break;

  case 19: /* def: id id "=" l0expr  */
#line 514 "hexpr.y"
                      { (yyval.mdef) = new MVarDef(list(*(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 2983 "hexpr.parse.C"
    break;

  case 20: /* def: id id id "=" l0expr  */
#line 515 "hexpr.y"
                         { (yyval.mdef) = new MVarDef(list(*(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 2989 "hexpr.parse.C"
    break;

  case 21: /* def: id id id id "=" l0expr  */
#line 516 "hexpr.y"
                            { (yyval.mdef) = new MVarDef(list(*(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 2995 "hexpr.parse.C"
    break;

  case 22: /* def: id id id id id "=" l0expr  */
#line 517 "hexpr.y"
                               { (yyval.mdef) = new MVarDef(list(*(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 3001 "hexpr.parse.C"
    break;

  case 23: /* def: id id id id id id "=" l0expr  */
#line 518 "hexpr.y"
                                  { (yyval.mdef) = new MVarDef(list(*(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3007 "hexpr.parse.C"
    break;

  case 24: /* def: id id id id id id id "=" l0expr  */
#line 519 "hexpr.y"
                                     { (yyval.mdef) = new MVarDef(list(*(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 3013 "hexpr.parse.C"
    break;

  case 25: /* def: id id id id id id id id "=" l0expr  */
#line 520 "hexpr.y"
                                        { (yyval.mdef) = new MVarDef(list(*(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-9]), (yylsp[0]))); }
#line 3019 "hexpr.parse.C"
    break;

  case 26: /* def: id id id id id id id id id "=" l0expr  */
#line 521 "hexpr.y"
                                           { (yyval.mdef) = new MVarDef(list(*(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-10]), (yylsp[0]))); }
#line 3025 "hexpr.parse.C"
    break;

  case 27: /* def: id id id id id id id id id id "=" l0expr  */
#line 522 "hexpr.y"
                                              { (yyval.mdef) = new MVarDef(list(*(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-11]), (yylsp[0]))); }
#line 3031 "hexpr.parse.C"
    break;

  case 28: /* def: id id id id id id id id id id id "=" l0expr  */
#line 523 "hexpr.y"
                                                 { (yyval.mdef) = new MVarDef(list(*(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-12]), (yylsp[0]))); }
#line 3037 "hexpr.parse.C"
    break;

  case 29: /* def: id id id id id id id id id id id id "=" l0expr  */
#line 524 "hexpr.y"
                                                    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-13]), (yylsp[0]))); }
#line 3043 "hexpr.parse.C"
    break;

  case 30: /* def: id id id id id id id id id id id id id "=" l0expr  */
#line 525 "hexpr.y"
                                                       { (yyval.mdef) = new MVarDef(list(*(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-14]), (yylsp[0]))); }
#line 3049 "hexpr.parse.C"
    break;

  case 31: /* def: id id id id id id id id id id id id id id "=" l0expr  */
#line 526 "hexpr.y"
                                                          { (yyval.mdef) = new MVarDef(list(*(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-15]), (yylsp[0]))); }
#line 3055 "hexpr.parse.C"
    break;

  case 32: /* def: id id id id id id id id id id id id id id id "=" l0expr  */
#line 527 "hexpr.y"
                                                             { (yyval.mdef) = new MVarDef(list(*(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-16]), (yylsp[0]))); }
#line 3061 "hexpr.parse.C"
    break;

  case 33: /* def: id id id id id id id id id id id id id id id id "=" l0expr  */
#line 528 "hexpr.y"
                                                                { (yyval.mdef) = new MVarDef(list(*(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-17]), (yylsp[0]))); }
#line 3067 "hexpr.parse.C"
    break;

  case 34: /* def: id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 529 "hexpr.y"
                                                                   { (yyval.mdef) = new MVarDef(list(*(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-18]), (yylsp[0]))); }
#line 3073 "hexpr.parse.C"
    break;

  case 35: /* def: id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 530 "hexpr.y"
                                                                      { (yyval.mdef) = new MVarDef(list(*(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-19]), (yylsp[0]))); }
#line 3079 "hexpr.parse.C"
    break;

  case 36: /* def: id id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 531 "hexpr.y"
                                                                         { (yyval.mdef) = new MVarDef(list(*(yyvsp[-20].string), *(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-20]), (yylsp[0]))); }
#line 3085 "hexpr.parse.C"
    break;

  case 37: /* def: id id id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 532 "hexpr.y"
                                                                            { (yyval.mdef) = new MVarDef(list(*(yyvsp[-21].string), *(yyvsp[-20].string), *(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-21]), (yylsp[0]))); }
#line 3091 "hexpr.parse.C"
    break;

  case 38: /* def: id id id id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 533 "hexpr.y"
                                                                               { (yyval.mdef) = new MVarDef(list(*(yyvsp[-22].string), *(yyvsp[-21].string), *(yyvsp[-20].string), *(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-22]), (yylsp[0]))); }
#line 3097 "hexpr.parse.C"
    break;

  case 39: /* def: l5expr  */
#line 536 "hexpr.y"
            { (yyval.mdef) = new MVarDef(list(freshName()), let(freshName(), ExprPtr((yyvsp[0].exp)), mktunit(m((yylsp[0]))), m((yylsp[0]))), m((yylsp[0]))); }
#line 3103 "hexpr.parse.C"
    break;

  case 40: /* importdef: "import" cppid  */
#line 539 "hexpr.y"
                          { (yyval.mdef) = new MImport(yyModulePath, *(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3109 "hexpr.parse.C"
    break;

  case 41: /* pragmadef: "{-#" pragmaty "#-}"  */
#line 542 "hexpr.y"
                                { (yyval.mdef) = (yyvsp[-1].mdef); }
#line 3115 "hexpr.parse.C"
    break;

  case 42: /* pragmaty: "UNSAFE" id  */
#line 543 "hexpr.y"
                      { (yyval.mdef) = new MUnsafePragmaDef(*(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3121 "hexpr.parse.C"
    break;

  case 43: /* pragmaty: "SAFE" id  */
#line 544 "hexpr.y"
                    { (yyval.mdef) = new MSafePragmaDef(*(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3127 "hexpr.parse.C"
    break;

  case 44: /* tydef: "type" nameseq "=" qtype  */
#line 547 "hexpr.y"
                                { (yyval.mdef) = new MTypeDef(MTypeDef::Transparent, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 3133 "hexpr.parse.C"
    break;

  case 45: /* tydef: "data" nameseq "=" qtype  */
#line 548 "hexpr.y"
                                { (yyval.mdef) = new MTypeDef(MTypeDef::Opaque, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 3139 "hexpr.parse.C"
    break;

  case 46: /* vartybind: name "::" qtype  */
#line 551 "hexpr.y"
                           { (yyval.mvtydef) = new MVarTypeDef(*(yyvsp[-2].string), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]), (yylsp[0]))); }
#line 3145 "hexpr.parse.C"
    break;

  case 47: /* vardef: names "=" l0expr  */
#line 553 "hexpr.y"
                         { (yyval.mvdef) = new MVarDef(*(yyvsp[-2].strings), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3151 "hexpr.parse.C"
    break;

  case 48: /* classdef: "class" cst "=>" id names  */
#line 556 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-3].tconstraints), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3157 "hexpr.parse.C"
    break;

  case 49: /* classdef: "class" cst "=>" id names "|" fundeps  */
#line 557 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-6]), (yylsp[0]))); wantIndent(false); }
#line 3163 "hexpr.parse.C"
    break;

  case 50: /* classdef: "class" cst "=>" id names "where" cmembers  */
#line 558 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 3169 "hexpr.parse.C"
    break;

  case 51: /* classdef: "class" cst "=>" id names "|" fundeps "where" cmembers  */
#line 559 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-7].tconstraints), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-8]), (yylsp[0])));            wantIndent(false); }
#line 3175 "hexpr.parse.C"
    break;

  case 52: /* classdef: "class" id names  */
#line 560 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 3181 "hexpr.parse.C"
    break;

  case 53: /* classdef: "class" id names "|" fundeps  */
#line 561 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3187 "hexpr.parse.C"
    break;

  case 54: /* classdef: "class" id names "where" cmembers  */
#line 562 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-4]), (yylsp[0])));            wantIndent(false); }
#line 3193 "hexpr.parse.C"
    break;

  case 55: /* classdef: "class" id names "|" fundeps "where" cmembers  */
#line 563 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 3199 "hexpr.parse.C"
    break;

  case 56: /* fundeps: fundep  */
#line 565 "hexpr.y"
                            { (yyval.fundeps) = autorelease(new CFunDepDefs()); (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 3205 "hexpr.parse.C"
    break;

  case 57: /* fundeps: fundeps "," fundep  */
#line 566 "hexpr.y"
                            { (yyval.fundeps) = (yyvsp[-2].fundeps);                             (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 3211 "hexpr.parse.C"
    break;

  case 58: /* fundep: idseq "->" idseq  */
#line 568 "hexpr.y"
                         { (yyval.fundep) = autorelease(new CFunDepDef(*(yyvsp[-2].strings), *(yyvsp[0].strings))); }
#line 3217 "hexpr.parse.C"
    break;

  case 59: /* cmembers: cmember  */
#line 570 "hexpr.y"
                           { (yyval.mvtydefs) = autorelease(new MVarTypeDefs()); (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 3223 "hexpr.parse.C"
    break;

  case 60: /* cmembers: cmembers cmember  */
#line 571 "hexpr.y"
                           { (yyval.mvtydefs) = (yyvsp[-1].mvtydefs);                              (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 3229 "hexpr.parse.C"
    break;

  case 61: /* cmember: "indent" vartybind  */
#line 573 "hexpr.y"
                            { (yyval.mvtydef) = (yyvsp[0].mvtydef); }
#line 3235 "hexpr.parse.C"
    break;

  case 62: /* instdef: "instance" id types  */
#line 576 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 3241 "hexpr.parse.C"
    break;

  case 63: /* instdef: "instance" cst "=>" id types  */
#line 577 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(*(yyvsp[-3].tconstraints),           *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3247 "hexpr.parse.C"
    break;

  case 64: /* instdef: "instance" id types "where" imembers  */
#line 578 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-4]), (yylsp[0])));        wantIndent(false); }
#line 3253 "hexpr.parse.C"
    break;

  case 65: /* instdef: "instance" cst "=>" id types "where" imembers  */
#line 579 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(*(yyvsp[-5].tconstraints),           *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-6]), (yylsp[0])));        wantIndent(false); }
#line 3259 "hexpr.parse.C"
    break;

  case 66: /* imembers: imember  */
#line 581 "hexpr.y"
                           { (yyval.mvdefs) = autorelease(new MVarDefs()); (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 3265 "hexpr.parse.C"
    break;

  case 67: /* imembers: imembers imember  */
#line 582 "hexpr.y"
                           { (yyval.mvdefs) = (yyvsp[-1].mvdefs);                          (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 3271 "hexpr.parse.C"
    break;

  case 68: /* imember: "indent" vardef  */
#line 584 "hexpr.y"
                         { (yyval.mvdef) = (yyvsp[0].mvdef); }
#line 3277 "hexpr.parse.C"
    break;

  case 69: /* names: nameseq  */
#line 587 "hexpr.y"
               { (yyval.strings) = (yyvsp[0].strings); }
#line 3283 "hexpr.parse.C"
    break;

  case 70: /* names: id opname id  */
#line 589 "hexpr.y"
                    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[-1].string)); (yyval.strings)->push_back(*(yyvsp[-2].string)); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3289 "hexpr.parse.C"
    break;

  case 71: /* nameseq: name  */
#line 591 "hexpr.y"
                      { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3295 "hexpr.parse.C"
    break;

  case 72: /* nameseq: nameseq name  */
#line 592 "hexpr.y"
                      { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3301 "hexpr.parse.C"
    break;

  case 73: /* name: id  */
#line 594 "hexpr.y"
         { (yyval.string) = (yyvsp[0].string); }
#line 3307 "hexpr.parse.C"
    break;

  case 74: /* name: "(" opname ")"  */
#line 596 "hexpr.y"
                     { (yyval.string) = (yyvsp[-1].string); }
#line 3313 "hexpr.parse.C"
    break;

  case 75: /* opname: "and"  */
#line 598 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("and")); }
#line 3319 "hexpr.parse.C"
    break;

  case 76: /* opname: "or"  */
#line 599 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("or")); }
#line 3325 "hexpr.parse.C"
    break;

  case 77: /* opname: "o"  */
#line 600 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("compose")); }
#line 3331 "hexpr.parse.C"
    break;

  case 78: /* opname: "."  */
#line 601 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("compose")); }
#line 3337 "hexpr.parse.C"
    break;

  case 79: /* opname: "~"  */
#line 602 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("~")); }
#line 3343 "hexpr.parse.C"
    break;

  case 80: /* opname: "=~"  */
#line 603 "hexpr.y"
               { (yyval.string) = autorelease(new std::string("=~")); }
#line 3349 "hexpr.parse.C"
    break;

  case 81: /* opname: "==="  */
#line 604 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("===")); }
#line 3355 "hexpr.parse.C"
    break;

  case 82: /* opname: "=="  */
#line 605 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("==")); }
#line 3361 "hexpr.parse.C"
    break;

  case 83: /* opname: "<"  */
#line 606 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("<")); }
#line 3367 "hexpr.parse.C"
    break;

  case 84: /* opname: "<="  */
#line 607 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("<=")); }
#line 3373 "hexpr.parse.C"
    break;

  case 85: /* opname: ">"  */
#line 608 "hexpr.y"
              { (yyval.string) = autorelease(new std::string(">")); }
#line 3379 "hexpr.parse.C"
    break;

  case 86: /* opname: ">="  */
#line 609 "hexpr.y"
              { (yyval.string) = autorelease(new std::string(">=")); }
#line 3385 "hexpr.parse.C"
    break;

  case 87: /* opname: "in"  */
#line 610 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("in")); }
#line 3391 "hexpr.parse.C"
    break;

  case 88: /* opname: "++"  */
#line 611 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("append")); }
#line 3397 "hexpr.parse.C"
    break;

  case 89: /* opname: "+"  */
#line 612 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("+")); }
#line 3403 "hexpr.parse.C"
    break;

  case 90: /* opname: "-"  */
#line 613 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("-")); }
#line 3409 "hexpr.parse.C"
    break;

  case 91: /* opname: "*"  */
#line 614 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("*")); }
#line 3415 "hexpr.parse.C"
    break;

  case 92: /* opname: "/"  */
#line 615 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("/")); }
#line 3421 "hexpr.parse.C"
    break;

  case 93: /* opname: "%"  */
#line 616 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("%")); }
#line 3427 "hexpr.parse.C"
    break;

  case 94: /* idseq: id  */
#line 618 "hexpr.y"
                { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3433 "hexpr.parse.C"
    break;

  case 95: /* idseq: idseq id  */
#line 619 "hexpr.y"
                { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3439 "hexpr.parse.C"
    break;

  case 96: /* types: l0mtype  */
#line 621 "hexpr.y"
                     { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3445 "hexpr.parse.C"
    break;

  case 97: /* types: types l0mtype  */
#line 622 "hexpr.y"
                     { (yyval.mtypes) = (yyvsp[-1].mtypes);                           (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3451 "hexpr.parse.C"
    break;

  case 98: /* l0expr: "\\" patterns "." l0expr  */
#line 625 "hexpr.y"
                                 { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3457 "hexpr.parse.C"
    break;

  case 99: /* l0expr: "fn" patterns "." l0expr  */
#line 626 "hexpr.y"
                                 { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3463 "hexpr.parse.C"
    break;

  case 100: /* l0expr: lhexpr "<-" lhexpr  */
#line 627 "hexpr.y"
                                 { (yyval.exp) = new Assign(ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3469 "hexpr.parse.C"
    break;

  case 101: /* l0expr: lhexpr  */
#line 628 "hexpr.y"
                                 { (yyval.exp) = (yyvsp[0].exp); }
#line 3475 "hexpr.parse.C"
    break;

  case 102: /* lhexpr: "!" l1expr  */
#line 630 "hexpr.y"
                                 { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), (yyvsp[0].exp), m((yylsp[-1]),(yylsp[0]))); }
#line 3481 "hexpr.parse.C"
    break;

  case 103: /* lhexpr: lhexpr "and" lhexpr  */
#line 631 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("and",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3487 "hexpr.parse.C"
    break;

  case 104: /* lhexpr: lhexpr "or" lhexpr  */
#line 632 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("or",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3493 "hexpr.parse.C"
    break;

  case 105: /* lhexpr: lhexpr "o" lhexpr  */
#line 633 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("compose",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3499 "hexpr.parse.C"
    break;

  case 106: /* lhexpr: l1expr "in" l1expr  */
#line 634 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("in",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3505 "hexpr.parse.C"
    break;

  case 107: /* lhexpr: l1expr  */
#line 635 "hexpr.y"
                                 { (yyval.exp) = (yyvsp[0].exp); }
#line 3511 "hexpr.parse.C"
    break;

  case 108: /* l1expr: "if" l0expr "then" l0expr "else" l0expr  */
#line 637 "hexpr.y"
                                                { (yyval.exp) = TAPP3(var("if",m((yylsp[-5]))), (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-5]), (yylsp[0]))); }
#line 3517 "hexpr.parse.C"
    break;

  case 109: /* l1expr: l2expr  */
#line 638 "hexpr.y"
                                                { (yyval.exp) = (yyvsp[0].exp); }
#line 3523 "hexpr.parse.C"
    break;

  case 110: /* l2expr: l2expr "~" l2expr  */
#line 640 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("~",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3529 "hexpr.parse.C"
    break;

  case 111: /* l2expr: l2expr "===" l2expr  */
#line 641 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("===",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3535 "hexpr.parse.C"
    break;

  case 112: /* l2expr: l2expr "==" l2expr  */
#line 642 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3541 "hexpr.parse.C"
    break;

  case 113: /* l2expr: l2expr "!=" l2expr  */
#line 643 "hexpr.y"
                            { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))), m((yylsp[-2]),(yylsp[0]))); }
#line 3547 "hexpr.parse.C"
    break;

  case 114: /* l2expr: l2expr "<" l2expr  */
#line 644 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("<",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3553 "hexpr.parse.C"
    break;

  case 115: /* l2expr: l2expr "<=" l2expr  */
#line 645 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("<=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3559 "hexpr.parse.C"
    break;

  case 116: /* l2expr: l2expr ">" l2expr  */
#line 646 "hexpr.y"
                            { (yyval.exp) = TAPP2(var(">",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3565 "hexpr.parse.C"
    break;

  case 117: /* l2expr: l2expr ">=" l2expr  */
#line 647 "hexpr.y"
                            { (yyval.exp) = TAPP2(var(">=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3571 "hexpr.parse.C"
    break;

  case 118: /* l2expr: l3expr  */
#line 648 "hexpr.y"
                            { (yyval.exp) = (yyvsp[0].exp); }
#line 3577 "hexpr.parse.C"
    break;

  case 119: /* l3expr: l3expr "+" l3expr  */
#line 650 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("+",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3583 "hexpr.parse.C"
    break;

  case 120: /* l3expr: l3expr "-" l3expr  */
#line 651 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("-",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3589 "hexpr.parse.C"
    break;

  case 121: /* l3expr: l3expr "++" l3expr  */
#line 652 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("append",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3595 "hexpr.parse.C"
    break;

  case 122: /* l3expr: "-" l3expr  */
#line 653 "hexpr.y"
                           { (yyval.exp) = TAPP1(var("neg",m((yylsp[-1]))), ExprPtr((yyvsp[0].exp)), m((yylsp[-1]),(yylsp[0]))); }
#line 3601 "hexpr.parse.C"
    break;

  case 123: /* l3expr: l4expr  */
#line 654 "hexpr.y"
                           { (yyval.exp) = (yyvsp[0].exp); }
#line 3607 "hexpr.parse.C"
    break;

  case 124: /* l4expr: l4expr "*" l4expr  */
#line 656 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("*", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3613 "hexpr.parse.C"
    break;

  case 125: /* l4expr: l4expr "/" l4expr  */
#line 657 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("/", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3619 "hexpr.parse.C"
    break;

  case 126: /* l4expr: l4expr "%" l4expr  */
#line 658 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("%", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3625 "hexpr.parse.C"
    break;

  case 127: /* l4expr: l5expr  */
#line 659 "hexpr.y"
                          { (yyval.exp) = (yyvsp[0].exp); }
#line 3631 "hexpr.parse.C"
    break;

  case 128: /* l5expr: l6expr  */
#line 661 "hexpr.y"
               { (yyval.exp) = (yyvsp[0].exp); }
#line 3637 "hexpr.parse.C"
    break;

  case 129: /* l5expr: "let" letbindings "in" l0expr  */
#line 664 "hexpr.y"
                                      { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-2].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3643 "hexpr.parse.C"
    break;

  case 130: /* l5expr: "let" letbindings ";" "in" l0expr  */
#line 665 "hexpr.y"
                                          { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]),(yylsp[0])))->clone(); }
#line 3649 "hexpr.parse.C"
    break;

  case 131: /* l5expr: "match" l6exprs "with" patternexps  */
#line 668 "hexpr.y"
                                           { (yyval.exp) = compileMatch(yyParseCC, *(yyvsp[-2].exps), normPatternRules(*(yyvsp[0].patternexps), m((yylsp[-3]),(yylsp[0]))), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3655 "hexpr.parse.C"
    break;

  case 132: /* l5expr: l6expr "matches" pattern  */
#line 671 "hexpr.y"
                                 { (yyval.exp) = compileMatchTest(yyParseCC, ExprPtr((yyvsp[-2].exp)), PatternPtr((yyvsp[0].pattern)), m((yylsp[-2]),(yylsp[0])))->clone(); }
#line 3661 "hexpr.parse.C"
    break;

  case 133: /* l5expr: "parse" "{" prules "}"  */
#line 674 "hexpr.y"
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
#line 3676 "hexpr.parse.C"
    break;

  case 134: /* l5expr: "do" "{" dobindings "}"  */
#line 686 "hexpr.y"
                                                { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-1].letbindings), ExprPtr(new Unit(m((yylsp[-3]),(yylsp[0])))), m((yylsp[-3]),(yylsp[0]))); }
#line 3682 "hexpr.parse.C"
    break;

  case 135: /* l5expr: "do" "{" dobindings "return" l0expr "}"  */
#line 687 "hexpr.y"
                                                { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[-1].exp)), m((yylsp[-5]),(yylsp[0]))); }
#line 3688 "hexpr.parse.C"
    break;

  case 136: /* l5expr: l6expr "::" qtype  */
#line 690 "hexpr.y"
                                { (yyval.exp) = new Assump(ExprPtr((yyvsp[-2].exp)), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]),(yylsp[0]))); }
#line 3694 "hexpr.parse.C"
    break;

  case 137: /* letbindings: letbindings ";" letbinding  */
#line 692 "hexpr.y"
                                        { (yyvsp[-2].letbindings)->push_back(*(yyvsp[0].letbinding)); (yyval.letbindings) = (yyvsp[-2].letbindings); }
#line 3700 "hexpr.parse.C"
    break;

  case 138: /* letbindings: letbinding  */
#line 693 "hexpr.y"
                                        { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3706 "hexpr.parse.C"
    break;

  case 139: /* letbinding: irrefutablep "=" l1expr  */
#line 695 "hexpr.y"
                                    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-2].pattern)), ExprPtr((yyvsp[0].exp)))); }
#line 3712 "hexpr.parse.C"
    break;

  case 140: /* dobindings: dobindings dobinding  */
#line 697 "hexpr.y"
                                 { (yyval.letbindings) = (yyvsp[-1].letbindings); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3718 "hexpr.parse.C"
    break;

  case 141: /* dobindings: dobinding  */
#line 698 "hexpr.y"
                                 { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3724 "hexpr.parse.C"
    break;

  case 142: /* dobinding: irrefutablep "=" l0expr ";"  */
#line 700 "hexpr.y"
                                       { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)))); }
#line 3730 "hexpr.parse.C"
    break;

  case 143: /* dobinding: l0expr ";"  */
#line 701 "hexpr.y"
                                       { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr(new MatchAny("_",m((yylsp[-1])))), ExprPtr((yyvsp[-1].exp)))); }
#line 3736 "hexpr.parse.C"
    break;

  case 144: /* cselconds: cselconds "," lhexpr  */
#line 703 "hexpr.y"
                                { (yyval.exps) = (yyvsp[-2].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3742 "hexpr.parse.C"
    break;

  case 145: /* cselconds: lhexpr  */
#line 704 "hexpr.y"
                                { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3748 "hexpr.parse.C"
    break;

  case 146: /* cselection: pattern "<-" l0expr "," cselconds  */
#line 706 "hexpr.y"
                                              { (yyval.cselection) = new CSelection(); (yyval.cselection)->pat = PatternPtr((yyvsp[-4].pattern)); (yyval.cselection)->seq = ExprPtr((yyvsp[-2].exp)); (yyval.cselection)->conds = *(yyvsp[0].exps); }
#line 3754 "hexpr.parse.C"
    break;

  case 147: /* cselection: pattern "<-" l0expr  */
#line 707 "hexpr.y"
                                              { (yyval.cselection) = new CSelection(); (yyval.cselection)->pat = PatternPtr((yyvsp[-2].pattern)); (yyval.cselection)->seq = ExprPtr((yyvsp[0].exp)); }
#line 3760 "hexpr.parse.C"
    break;

  case 148: /* cselections: cselections "|" cselection  */
#line 709 "hexpr.y"
                                        { (yyval.cselections) = (yyvsp[-2].cselections); (yyval.cselections)->push_back(CSelectionPtr((yyvsp[0].cselection))); }
#line 3766 "hexpr.parse.C"
    break;

  case 149: /* cselections: cselection  */
#line 710 "hexpr.y"
                                        { (yyval.cselections) = autorelease(new CSelections()); (yyval.cselections)->push_back(CSelectionPtr((yyvsp[0].cselection))); }
#line 3772 "hexpr.parse.C"
    break;

  case 150: /* l6expr: l6expr "(" cargs ")"  */
#line 713 "hexpr.y"
                                { (yyval.exp) = new App(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].exps), m((yylsp[-3]), (yylsp[0]))); }
#line 3778 "hexpr.parse.C"
    break;

  case 151: /* l6expr: id  */
#line 714 "hexpr.y"
                                { (yyval.exp) = varCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3784 "hexpr.parse.C"
    break;

  case 152: /* l6expr: "[" l0expr ".." l0expr "]"  */
#line 717 "hexpr.y"
                                                          { (yyval.exp) = new App(var("range", m((yylsp[-2]))), list(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]), (yylsp[0]))); }
#line 3790 "hexpr.parse.C"
    break;

  case 153: /* l6expr: "[" l0expr ".." "]"  */
#line 718 "hexpr.y"
                                                          { (yyval.exp) = new App(var("iterateS", m((yylsp[-1]))), list(ExprPtr((yyvsp[-2].exp)), fn(str::strings(".x"), fncall(var("+", m((yylsp[-1]))), list(var(".x", m((yylsp[-1]))), ExprPtr(new Int(1, m((yylsp[-1]))))), m((yylsp[-1]))), m((yylsp[-1])))), m((yylsp[-3]), (yylsp[0]))); }
#line 3796 "hexpr.parse.C"
    break;

  case 154: /* l6expr: "[" l0expr "|" cselections "]"  */
#line 719 "hexpr.y"
                                                          { (yyval.exp) = desugarComprehension(yyParseCC, ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].cselections), m((yylsp[-4]), (yylsp[0]))); }
#line 3802 "hexpr.parse.C"
    break;

  case 155: /* l6expr: "[" cargs "]"  */
#line 720 "hexpr.y"
                                                          { (yyval.exp) = new MkArray(*(yyvsp[-1].exps), m((yylsp[-2]), (yylsp[0]))); }
#line 3808 "hexpr.parse.C"
    break;

  case 156: /* l6expr: l6expr "[" "timeV" "]"  */
#line 721 "hexpr.y"
                                                          { (yyval.exp) = maybeArraySliceWithTime(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].string), m((yylsp[-3]), (yylsp[0]))); }
#line 3814 "hexpr.parse.C"
    break;

  case 157: /* l6expr: l6expr "[" l0expr "]"  */
#line 722 "hexpr.y"
                                                          { (yyval.exp) = mkAIndex(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3820 "hexpr.parse.C"
    break;

  case 158: /* l6expr: l6expr "[" l0expr ":" l0expr "]"  */
#line 723 "hexpr.y"
                                                          { (yyval.exp) = new App(var("slice", m((yylsp[-2]))), list(ExprPtr((yyvsp[-5].exp)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-5]), (yylsp[0]))); }
#line 3826 "hexpr.parse.C"
    break;

  case 159: /* l6expr: l6expr "[" l0expr ":" "]"  */
#line 724 "hexpr.y"
                                                          { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-1]))), list(var(vn,m((yylsp[-4]))), ExprPtr((yyvsp[-2].exp)), fncall(var("size",m((yylsp[-1]))), list(var(vn,m((yylsp[-4])))),m((yylsp[-4])))),m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3832 "hexpr.parse.C"
    break;

  case 160: /* l6expr: l6expr "[" ":" l0expr "]"  */
#line 725 "hexpr.y"
                                                          { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-2]))), list(var(vn,m((yylsp[-4]))), fncall(var("size",m((yylsp[-2]))), list(var(vn,m((yylsp[-2])))),m((yylsp[-4]))), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3838 "hexpr.parse.C"
    break;

  case 161: /* l6expr: "|" id "=" l0expr "|"  */
#line 728 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(*(yyvsp[-3].string), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3844 "hexpr.parse.C"
    break;

  case 162: /* l6expr: "|" "intV" "=" l0expr "|"  */
#line 729 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(".f" + str::from((yyvsp[-3].intv)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3850 "hexpr.parse.C"
    break;

  case 163: /* l6expr: "|" id "|"  */
#line 730 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(*(yyvsp[-1].string), ExprPtr(new Unit(m((yylsp[-1])))), m((yylsp[-2]), (yylsp[0]))); }
#line 3856 "hexpr.parse.C"
    break;

  case 164: /* l6expr: "case" l0expr "of" "|" varfields "|"  */
#line 731 "hexpr.y"
                                                              { (yyval.exp) = new Case(ExprPtr((yyvsp[-4].exp)), *(yyvsp[-1].vfields), m((yylsp[-5]), (yylsp[0]))); }
#line 3862 "hexpr.parse.C"
    break;

  case 165: /* l6expr: "case" l0expr "of" "|" varfields "|" "default" l0expr  */
#line 732 "hexpr.y"
                                                              { (yyval.exp) = new Case(ExprPtr((yyvsp[-6].exp)), *(yyvsp[-3].vfields), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3868 "hexpr.parse.C"
    break;

  case 166: /* l6expr: "{" recfields "}"  */
#line 735 "hexpr.y"
                              { if ((yyvsp[-1].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-1].rfields), m((yylsp[-2]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-2]), (yylsp[0]))); } }
#line 3874 "hexpr.parse.C"
    break;

  case 167: /* l6expr: "{" recfields "," "}"  */
#line 736 "hexpr.y"
                              { if ((yyvsp[-2].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-2].rfields), m((yylsp[-3]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-3]), (yylsp[0]))); } }
#line 3880 "hexpr.parse.C"
    break;

  case 168: /* l6expr: l6expr recfieldpath  */
#line 737 "hexpr.y"
                              { (yyval.exp) = makeProjSeq((yyvsp[-1].exp), *(yyvsp[0].strings), m((yylsp[-1]), (yylsp[0]))); }
#line 3886 "hexpr.parse.C"
    break;

  case 169: /* l6expr: recfieldpath  */
#line 740 "hexpr.y"
                     { (yyval.exp) = new Fn(str::strings("x"), proj(var("x", m((yylsp[0]))), *(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3892 "hexpr.parse.C"
    break;

  case 170: /* l6expr: "regexV"  */
#line 743 "hexpr.y"
                 { (yyval.exp) = compileRegexFn(yyParseCC, std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0])))->clone(); }
#line 3898 "hexpr.parse.C"
    break;

  case 171: /* l6expr: "pack" l6expr  */
#line 746 "hexpr.y"
                                           { (yyval.exp) = new Pack(ExprPtr((yyvsp[0].exp)), m((yylsp[-1]), (yylsp[0]))); }
#line 3904 "hexpr.parse.C"
    break;

  case 172: /* l6expr: "unpack" id "=" l6expr "in" l6expr  */
#line 747 "hexpr.y"
                                           { (yyval.exp) = new Unpack(*(yyvsp[-4].string), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3910 "hexpr.parse.C"
    break;

  case 173: /* l6expr: "boolV"  */
#line 750 "hexpr.y"
                    { (yyval.exp) = new Bool((yyvsp[0].boolv), m((yylsp[0]))); }
#line 3916 "hexpr.parse.C"
    break;

  case 174: /* l6expr: "charV"  */
#line 751 "hexpr.y"
                    { (yyval.exp) = new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3922 "hexpr.parse.C"
    break;

  case 175: /* l6expr: "byteV"  */
#line 752 "hexpr.y"
                    { (yyval.exp) = new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3928 "hexpr.parse.C"
    break;

  case 176: /* l6expr: "bytesV"  */
#line 753 "hexpr.y"
                    { (yyval.exp) = mkarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3934 "hexpr.parse.C"
    break;

  case 177: /* l6expr: "shortV"  */
#line 754 "hexpr.y"
                    { (yyval.exp) = new Short((yyvsp[0].shortv), m((yylsp[0]))); }
#line 3940 "hexpr.parse.C"
    break;

  case 178: /* l6expr: "intV"  */
#line 755 "hexpr.y"
                    { (yyval.exp) = new Int((yyvsp[0].intv), m((yylsp[0]))); }
#line 3946 "hexpr.parse.C"
    break;

  case 179: /* l6expr: "longV"  */
#line 756 "hexpr.y"
                    { (yyval.exp) = new Long((yyvsp[0].longv), m((yylsp[0]))); }
#line 3952 "hexpr.parse.C"
    break;

  case 180: /* l6expr: "int128V"  */
#line 757 "hexpr.y"
                    { (yyval.exp) = new Int128((yyvsp[0].int128v), m((yylsp[0]))); }
#line 3958 "hexpr.parse.C"
    break;

  case 181: /* l6expr: "floatV"  */
#line 758 "hexpr.y"
                    { (yyval.exp) = new Float((yyvsp[0].floatv), m((yylsp[0]))); }
#line 3964 "hexpr.parse.C"
    break;

  case 182: /* l6expr: "doubleV"  */
#line 759 "hexpr.y"
                    { (yyval.exp) = new Double((yyvsp[0].doublev), m((yylsp[0]))); }
#line 3970 "hexpr.parse.C"
    break;

  case 183: /* l6expr: "stringV"  */
#line 760 "hexpr.y"
                    { (yyval.exp) = mkarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3976 "hexpr.parse.C"
    break;

  case 184: /* l6expr: tsseq  */
#line 761 "hexpr.y"
                    { (yyval.exp) = mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0])))->clone(); }
#line 3982 "hexpr.parse.C"
    break;

  case 185: /* l6expr: "timeV"  */
#line 762 "hexpr.y"
                    { (yyval.exp) = mkTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3988 "hexpr.parse.C"
    break;

  case 186: /* l6expr: "dateTimeV"  */
#line 763 "hexpr.y"
                    { (yyval.exp) = mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3994 "hexpr.parse.C"
    break;

  case 187: /* l6expr: "(" cargs ")"  */
#line 766 "hexpr.y"
                      { (yyval.exp) = pickNestedExp((yyvsp[-1].exps), m((yylsp[-2]),(yylsp[0]))); }
#line 4000 "hexpr.parse.C"
    break;

  case 188: /* l6expr: "(" "++" ")"  */
#line 769 "hexpr.y"
                      { (yyval.exp) = new Var("append", m((yylsp[-1]))); }
#line 4006 "hexpr.parse.C"
    break;

  case 189: /* l6expr: "(" "+" ")"  */
#line 770 "hexpr.y"
                      { (yyval.exp) = new Var("+",      m((yylsp[-1]))); }
#line 4012 "hexpr.parse.C"
    break;

  case 190: /* l6expr: "(" "-" ")"  */
#line 771 "hexpr.y"
                      { (yyval.exp) = new Var("-",      m((yylsp[-1]))); }
#line 4018 "hexpr.parse.C"
    break;

  case 191: /* l6expr: "(" "*" ")"  */
#line 772 "hexpr.y"
                      { (yyval.exp) = new Var("*",      m((yylsp[-1]))); }
#line 4024 "hexpr.parse.C"
    break;

  case 192: /* l6expr: "(" "/" ")"  */
#line 773 "hexpr.y"
                      { (yyval.exp) = new Var("/",      m((yylsp[-1]))); }
#line 4030 "hexpr.parse.C"
    break;

  case 193: /* l6expr: "(" "%" ")"  */
#line 774 "hexpr.y"
                      { (yyval.exp) = new Var("%",      m((yylsp[-1]))); }
#line 4036 "hexpr.parse.C"
    break;

  case 194: /* l6expr: "(" "~" ")"  */
#line 775 "hexpr.y"
                      { (yyval.exp) = new Var("~",      m((yylsp[-1]))); }
#line 4042 "hexpr.parse.C"
    break;

  case 195: /* l6expr: "(" "===" ")"  */
#line 776 "hexpr.y"
                      { (yyval.exp) = new Var("===",    m((yylsp[-1]))); }
#line 4048 "hexpr.parse.C"
    break;

  case 196: /* l6expr: "(" "==" ")"  */
#line 777 "hexpr.y"
                      { (yyval.exp) = new Var("==",     m((yylsp[-1]))); }
#line 4054 "hexpr.parse.C"
    break;

  case 197: /* l6expr: "(" "!=" ")"  */
#line 778 "hexpr.y"
                      { (yyval.exp) = new Var("!=",     m((yylsp[-1]))); }
#line 4060 "hexpr.parse.C"
    break;

  case 198: /* l6expr: "(" "<" ")"  */
#line 779 "hexpr.y"
                      { (yyval.exp) = new Var("<",      m((yylsp[-1]))); }
#line 4066 "hexpr.parse.C"
    break;

  case 199: /* l6expr: "(" ">" ")"  */
#line 780 "hexpr.y"
                      { (yyval.exp) = new Var(">",      m((yylsp[-1]))); }
#line 4072 "hexpr.parse.C"
    break;

  case 200: /* l6expr: "(" ">=" ")"  */
#line 781 "hexpr.y"
                      { (yyval.exp) = new Var(">=",     m((yylsp[-1]))); }
#line 4078 "hexpr.parse.C"
    break;

  case 201: /* l6expr: "(" "<=" ")"  */
#line 782 "hexpr.y"
                      { (yyval.exp) = new Var("<=",     m((yylsp[-1]))); }
#line 4084 "hexpr.parse.C"
    break;

  case 202: /* l6expr: "(" "and" ")"  */
#line 783 "hexpr.y"
                      { (yyval.exp) = new Var("and",    m((yylsp[-1]))); }
#line 4090 "hexpr.parse.C"
    break;

  case 203: /* l6expr: "(" "or" ")"  */
#line 784 "hexpr.y"
                      { (yyval.exp) = new Var("or",     m((yylsp[-1]))); }
#line 4096 "hexpr.parse.C"
    break;

  case 204: /* l6expr: "(" "in" ")"  */
#line 785 "hexpr.y"
                      { (yyval.exp) = new Var("in",     m((yylsp[-1]))); }
#line 4102 "hexpr.parse.C"
    break;

  case 205: /* l6expr: "(" "!" ")"  */
#line 786 "hexpr.y"
                      { (yyval.exp) = new Var("not",    m((yylsp[-1]))); }
#line 4108 "hexpr.parse.C"
    break;

  case 206: /* l6expr: "`" l0expr "`"  */
#line 789 "hexpr.y"
                       { (yyval.exp) = new Assump(fncall(var("unsafeCast", m((yylsp[-1]))), list(mktunit(m((yylsp[-1])))), m((yylsp[-1]))), qualtype(tapp(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp)))))), m((yylsp[-1]))); }
#line 4114 "hexpr.parse.C"
    break;

  case 207: /* prules: prules prule  */
#line 791 "hexpr.y"
                     { (yyval.prules) = (yyvsp[-1].prules); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 4120 "hexpr.parse.C"
    break;

  case 208: /* prules: prule  */
#line 792 "hexpr.y"
                     { (yyval.prules) = autorelease(new Grammar()); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 4126 "hexpr.parse.C"
    break;

  case 209: /* prule: id ":=" prdefs  */
#line 794 "hexpr.y"
                      { (yyval.prule) = autorelease(new Grammar::value_type(*(yyvsp[-2].string), *(yyvsp[0].prdefs))); }
#line 4132 "hexpr.parse.C"
    break;

  case 210: /* prdefs: prdefs "|" prdef  */
#line 796 "hexpr.y"
                         { (yyval.prdefs) = (yyvsp[-2].prdefs); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 4138 "hexpr.parse.C"
    break;

  case 211: /* prdefs: prdef  */
#line 797 "hexpr.y"
                         { (yyval.prdefs) = autorelease(new GrammarRules()); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 4144 "hexpr.parse.C"
    break;

  case 212: /* prdef: pbelems "{" l0expr "}"  */
#line 799 "hexpr.y"
                              { (yyval.prdef) = autorelease(new GrammarRule(*(yyvsp[-3].pbelems), ExprPtr((yyvsp[-1].exp)))); }
#line 4150 "hexpr.parse.C"
    break;

  case 213: /* pbelems: pbelems pbelem  */
#line 801 "hexpr.y"
                        { (yyval.pbelems) = (yyvsp[-1].pbelems); (yyval.pbelems)->push_back(*(yyvsp[0].pbelem)); }
#line 4156 "hexpr.parse.C"
    break;

  case 214: /* pbelems: %empty  */
#line 802 "hexpr.y"
                        { (yyval.pbelems) = autorelease(new BoundGrammarValues()); }
#line 4162 "hexpr.parse.C"
    break;

  case 215: /* pbelem: id ":" pvalue  */
#line 804 "hexpr.y"
                      { (yyval.pbelem) = autorelease(new BoundGrammarValue(*(yyvsp[-2].string), GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 4168 "hexpr.parse.C"
    break;

  case 216: /* pbelem: pvalue  */
#line 805 "hexpr.y"
                      { (yyval.pbelem) = autorelease(new BoundGrammarValue("_", GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 4174 "hexpr.parse.C"
    break;

  case 217: /* pvalue: id  */
#line 807 "hexpr.y"
                      { (yyval.pvalue) = new GSymRef(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4180 "hexpr.parse.C"
    break;

  case 218: /* pvalue: "stringV"  */
#line 808 "hexpr.y"
                      { (yyval.pvalue) = new GStr(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4186 "hexpr.parse.C"
    break;

  case 219: /* pvalue: "charV"  */
#line 809 "hexpr.y"
                      { (yyval.pvalue) = new GStr(std::string(1, str::readCharDef(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4192 "hexpr.parse.C"
    break;

  case 220: /* tsseq: "timespanV"  */
#line 811 "hexpr.y"
                         { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4198 "hexpr.parse.C"
    break;

  case 221: /* tsseq: tsseq "timespanV"  */
#line 812 "hexpr.y"
                         { (yyval.strings) = (yyvsp[-1].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4204 "hexpr.parse.C"
    break;

  case 222: /* l6exprs: l6exprs l6expr  */
#line 814 "hexpr.y"
                        { (yyval.exps) = (yyvsp[-1].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4210 "hexpr.parse.C"
    break;

  case 223: /* l6exprs: l6expr  */
#line 815 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4216 "hexpr.parse.C"
    break;

  case 224: /* patternexps: patternexps patternexp  */
#line 817 "hexpr.y"
                                    { (yyval.patternexps) = (yyvsp[-1].patternexps); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 4222 "hexpr.parse.C"
    break;

  case 225: /* patternexps: patternexp  */
#line 818 "hexpr.y"
                                    { (yyval.patternexps) = autorelease(new PatternRows()); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 4228 "hexpr.parse.C"
    break;

  case 226: /* patternexp: "|" patterns "->" l0expr  */
#line 820 "hexpr.y"
                                                    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)))); }
#line 4234 "hexpr.parse.C"
    break;

  case 227: /* patternexp: "|" patterns "where" l0expr "->" l0expr  */
#line 821 "hexpr.y"
                                                    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-4].patterns), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)))); }
#line 4240 "hexpr.parse.C"
    break;

  case 228: /* patterns: patterns pattern  */
#line 824 "hexpr.y"
                           { (yyval.patterns) = (yyvsp[-1].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4246 "hexpr.parse.C"
    break;

  case 229: /* patterns: pattern  */
#line 825 "hexpr.y"
                           { (yyval.patterns) = autorelease(new Patterns()); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4252 "hexpr.parse.C"
    break;

  case 230: /* refutablep: "boolV"  */
#line 827 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Bool((yyvsp[0].boolv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4258 "hexpr.parse.C"
    break;

  case 231: /* refutablep: "charV"  */
#line 828 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 4264 "hexpr.parse.C"
    break;

  case 232: /* refutablep: "byteV"  */
#line 829 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 4270 "hexpr.parse.C"
    break;

  case 233: /* refutablep: "shortV"  */
#line 830 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Short((yyvsp[0].shortv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4276 "hexpr.parse.C"
    break;

  case 234: /* refutablep: "intV"  */
#line 831 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int((yyvsp[0].intv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4282 "hexpr.parse.C"
    break;

  case 235: /* refutablep: "longV"  */
#line 832 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Long((yyvsp[0].longv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4288 "hexpr.parse.C"
    break;

  case 236: /* refutablep: "int128V"  */
#line 833 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int128((yyvsp[0].int128v), m((yylsp[0])))), m((yylsp[0]))); }
#line 4294 "hexpr.parse.C"
    break;

  case 237: /* refutablep: "doubleV"  */
#line 834 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Double((yyvsp[0].doublev), m((yylsp[0])))), m((yylsp[0]))); }
#line 4300 "hexpr.parse.C"
    break;

  case 238: /* refutablep: "bytesV"  */
#line 835 "hexpr.y"
                                       { (yyval.pattern) = mkpatarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 4306 "hexpr.parse.C"
    break;

  case 239: /* refutablep: "stringV"  */
#line 836 "hexpr.y"
                                       { (yyval.pattern) = mkpatarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4312 "hexpr.parse.C"
    break;

  case 240: /* refutablep: tsseq  */
#line 837 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkTimespanPrim(*(yyvsp[0].strings), m((yylsp[0]))), mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 4318 "hexpr.parse.C"
    break;

  case 241: /* refutablep: "timeV"  */
#line 838 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4324 "hexpr.parse.C"
    break;

  case 242: /* refutablep: "dateTimeV"  */
#line 839 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkDateTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4330 "hexpr.parse.C"
    break;

  case 243: /* refutablep: "regexV"  */
#line 840 "hexpr.y"
                                       { (yyval.pattern) = new MatchRegex(std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0]))); }
#line 4336 "hexpr.parse.C"
    break;

  case 244: /* refutablep: "[" patternseq "]"  */
#line 841 "hexpr.y"
                                       { (yyval.pattern) = new MatchArray(*(yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4342 "hexpr.parse.C"
    break;

  case 245: /* refutablep: "[" patternseq "," "]"  */
#line 842 "hexpr.y"
                                       { (yyval.pattern) = new MatchArray(*(yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4348 "hexpr.parse.C"
    break;

  case 246: /* refutablep: "|" id "|"  */
#line 843 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(*(yyvsp[-1].string), PatternPtr(new MatchLiteral(PrimitivePtr(new Unit(m((yylsp[-1])))), m((yylsp[-1])))), m((yylsp[-2]),(yylsp[0]))); }
#line 4354 "hexpr.parse.C"
    break;

  case 247: /* refutablep: "|" id "=" pattern "|"  */
#line 844 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(*(yyvsp[-3].string), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4360 "hexpr.parse.C"
    break;

  case 248: /* refutablep: "|" "intV" "=" pattern "|"  */
#line 845 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(".f" + str::from((yyvsp[-3].intv)), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4366 "hexpr.parse.C"
    break;

  case 249: /* refutablep: "(" patternseq ")"  */
#line 846 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4372 "hexpr.parse.C"
    break;

  case 250: /* refutablep: "(" patternseq "," ")"  */
#line 847 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4378 "hexpr.parse.C"
    break;

  case 251: /* refutablep: "{" recpatfields "}"  */
#line 848 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4384 "hexpr.parse.C"
    break;

  case 252: /* refutablep: "{" recpatfields "," "}"  */
#line 849 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4390 "hexpr.parse.C"
    break;

  case 253: /* refutablep: id  */
#line 850 "hexpr.y"
                                       { (yyval.pattern) = patVarCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4396 "hexpr.parse.C"
    break;

  case 254: /* irrefutablep: id  */
#line 852 "hexpr.y"
                                       { (yyval.pattern) = new MatchAny(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4402 "hexpr.parse.C"
    break;

  case 255: /* irrefutablep: "(" patternseq ")"  */
#line 853 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4408 "hexpr.parse.C"
    break;

  case 256: /* irrefutablep: "(" patternseq "," ")"  */
#line 854 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4414 "hexpr.parse.C"
    break;

  case 257: /* irrefutablep: "{" recpatfields "}"  */
#line 855 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4420 "hexpr.parse.C"
    break;

  case 258: /* irrefutablep: "{" recpatfields "," "}"  */
#line 856 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4426 "hexpr.parse.C"
    break;

  case 259: /* pattern: refutablep  */
#line 858 "hexpr.y"
                    { (yyval.pattern) = (yyvsp[0].pattern); }
#line 4432 "hexpr.parse.C"
    break;

  case 260: /* patternseq: patternseqn  */
#line 860 "hexpr.y"
                          { (yyval.patterns) = (yyvsp[0].patterns); }
#line 4438 "hexpr.parse.C"
    break;

  case 261: /* patternseq: %empty  */
#line 861 "hexpr.y"
                          { (yyval.patterns) = new Patterns(); }
#line 4444 "hexpr.parse.C"
    break;

  case 262: /* patternseqn: patternseqn "," pattern  */
#line 863 "hexpr.y"
                                     { (yyval.patterns) = (yyvsp[-2].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4450 "hexpr.parse.C"
    break;

  case 263: /* patternseqn: pattern  */
#line 864 "hexpr.y"
                                     { (yyval.patterns) = new Patterns(); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4456 "hexpr.parse.C"
    break;

  case 264: /* recpatfields: recpatfields "," recpatfield  */
#line 866 "hexpr.y"
                                           { (yyval.recpatfields) = (yyvsp[-2].recpatfields); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4462 "hexpr.parse.C"
    break;

  case 265: /* recpatfields: recpatfield  */
#line 867 "hexpr.y"
                                           { (yyval.recpatfields) = new MatchRecord::Fields(); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4468 "hexpr.parse.C"
    break;

  case 266: /* recpatfield: id "=" pattern  */
#line 869 "hexpr.y"
                            { (yyval.recpatfield) = new MatchRecord::Field(*(yyvsp[-2].string), PatternPtr((yyvsp[0].pattern))); }
#line 4474 "hexpr.parse.C"
    break;

  case 267: /* recfields: %empty  */
#line 871 "hexpr.y"
                                                 { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); }
#line 4480 "hexpr.parse.C"
    break;

  case 268: /* recfields: recfieldname "=" l0expr  */
#line 872 "hexpr.y"
                                                 { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4486 "hexpr.parse.C"
    break;

  case 269: /* recfields: recfields "," recfieldname "=" l0expr  */
#line 873 "hexpr.y"
                                                 { (yyval.rfields) = (yyvsp[-4].rfields);                                     (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4492 "hexpr.parse.C"
    break;

  case 270: /* recfieldname: id  */
#line 875 "hexpr.y"
                         { (yyval.string) = (yyvsp[0].string); }
#line 4498 "hexpr.parse.C"
    break;

  case 271: /* recfieldname: "data"  */
#line 876 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("data")); }
#line 4504 "hexpr.parse.C"
    break;

  case 272: /* recfieldname: "type"  */
#line 877 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("type")); }
#line 4510 "hexpr.parse.C"
    break;

  case 273: /* recfieldname: "where"  */
#line 878 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("where")); }
#line 4516 "hexpr.parse.C"
    break;

  case 274: /* recfieldname: "class"  */
#line 879 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("class")); wantIndent(false); }
#line 4522 "hexpr.parse.C"
    break;

  case 275: /* recfieldname: "instance"  */
#line 880 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("instance")); wantIndent(false); }
#line 4528 "hexpr.parse.C"
    break;

  case 276: /* recfieldname: "exists"  */
#line 881 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("exists")); }
#line 4534 "hexpr.parse.C"
    break;

  case 277: /* recfieldname: "import"  */
#line 882 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("import")); }
#line 4540 "hexpr.parse.C"
    break;

  case 278: /* recfieldname: "module"  */
#line 883 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("module")); }
#line 4546 "hexpr.parse.C"
    break;

  case 279: /* recfieldname: "parse"  */
#line 884 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("parse")); }
#line 4552 "hexpr.parse.C"
    break;

  case 280: /* recfieldname: "do"  */
#line 885 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("do")); }
#line 4558 "hexpr.parse.C"
    break;

  case 281: /* recfieldname: "return"  */
#line 886 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("return")); }
#line 4564 "hexpr.parse.C"
    break;

  case 282: /* recfieldname: "fn"  */
#line 887 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("fn")); }
#line 4570 "hexpr.parse.C"
    break;

  case 283: /* recfieldname: "intV"  */
#line 888 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string(".f" + str::from((yyvsp[0].intv)))); }
#line 4576 "hexpr.parse.C"
    break;

  case 284: /* recfieldname: "stringV"  */
#line 889 "hexpr.y"
                         { std::string stringField = str::unescape(str::trimq(*(yyvsp[0].string)));
                           if (stringField.size() > 0 && stringField[0] == '.' ) {
                             throw annotated_error(m((yylsp[0])), "Cannot define record string label with leading '.'");
                           }
                           (yyval.string) = autorelease(new std::string(str::unescape(str::trimq(*(yyvsp[0].string))))); }
#line 4586 "hexpr.parse.C"
    break;

  case 285: /* recfieldpath: recfieldpath "." recfieldname  */
#line 895 "hexpr.y"
                                            { (yyval.strings) = (yyvsp[-2].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4592 "hexpr.parse.C"
    break;

  case 286: /* recfieldpath: recfieldpath "tupSection"  */
#line 896 "hexpr.y"
                                            { (yyval.strings) = (yyvsp[-1].strings); str::seq x = tupSectionFields(*(yyvsp[0].string)); (yyval.strings)->insert((yyval.strings)->end(), x.begin(), x.end()); }
#line 4598 "hexpr.parse.C"
    break;

  case 287: /* recfieldpath: "." recfieldname  */
#line 897 "hexpr.y"
                                            { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4604 "hexpr.parse.C"
    break;

  case 288: /* recfieldpath: "tupSection"  */
#line 898 "hexpr.y"
                                            { (yyval.strings) = autorelease(new str::seq()); *(yyval.strings) = tupSectionFields(*(yyvsp[0].string)); }
#line 4610 "hexpr.parse.C"
    break;

  case 289: /* varfields: varbind  */
#line 900 "hexpr.y"
                                 { (yyval.vfields) = autorelease(new Case::Bindings()); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4616 "hexpr.parse.C"
    break;

  case 290: /* varfields: varfields "," varbind  */
#line 901 "hexpr.y"
                                 { (yyval.vfields) = (yyvsp[-2].vfields); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4622 "hexpr.parse.C"
    break;

  case 291: /* varbind: id "=" l0expr  */
#line 903 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-2].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4628 "hexpr.parse.C"
    break;

  case 292: /* varbind: id ":" id "=" l0expr  */
#line 904 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-4].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4634 "hexpr.parse.C"
    break;

  case 293: /* varbind: "intV" ":" id "=" l0expr  */
#line 905 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(".f" + str::from((yyvsp[-4].intv)), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4640 "hexpr.parse.C"
    break;

  case 294: /* cargs: %empty  */
#line 907 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); }
#line 4646 "hexpr.parse.C"
    break;

  case 295: /* cargs: l0expr  */
#line 908 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4652 "hexpr.parse.C"
    break;

  case 296: /* cargs: cargs "," l0expr  */
#line 909 "hexpr.y"
                        { (yyvsp[-2].exps)->push_back(ExprPtr((yyvsp[0].exp))); (yyval.exps) = (yyvsp[-2].exps); }
#line 4658 "hexpr.parse.C"
    break;

  case 297: /* qtype: cst "=>" l0mtype  */
#line 911 "hexpr.y"
                         { (yyval.qualtype) = new QualType(*(yyvsp[-2].tconstraints), *(yyvsp[0].mtype)); }
#line 4664 "hexpr.parse.C"
    break;

  case 298: /* qtype: l0mtype  */
#line 912 "hexpr.y"
                         { (yyval.qualtype) = new QualType(Constraints(), *(yyvsp[0].mtype)); }
#line 4670 "hexpr.parse.C"
    break;

  case 299: /* cst: "(" tpreds ")"  */
#line 915 "hexpr.y"
                    { (yyval.tconstraints) = (yyvsp[-1].tconstraints); }
#line 4676 "hexpr.parse.C"
    break;

  case 300: /* tpreds: tpred  */
#line 917 "hexpr.y"
                         { (yyval.tconstraints) = autorelease(new Constraints()); (yyval.tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); }
#line 4682 "hexpr.parse.C"
    break;

  case 301: /* tpreds: tpreds "," tpred  */
#line 918 "hexpr.y"
                         { (yyvsp[-2].tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); (yyval.tconstraints) = (yyvsp[-2].tconstraints); }
#line 4688 "hexpr.parse.C"
    break;

  case 302: /* tpred: id l1mtargl  */
#line 920 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(*(yyvsp[-1].string), *(yyvsp[0].mtypes)); }
#line 4694 "hexpr.parse.C"
    break;

  case 303: /* tpred: l1mtype "==" l1mtype  */
#line 921 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(EqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4700 "hexpr.parse.C"
    break;

  case 304: /* tpred: l1mtype "!=" l1mtype  */
#line 922 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(NotEqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4706 "hexpr.parse.C"
    break;

  case 305: /* tpred: l1mtype "~" l1mtype  */
#line 923 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(FixIsoRecur::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4712 "hexpr.parse.C"
    break;

  case 306: /* tpred: l1mtype "=" "{" l1mtype "*" l1mtype "}"  */
#line 924 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4718 "hexpr.parse.C"
    break;

  case 307: /* tpred: l1mtype "=" "{" id ":" l1mtype "*" l1mtype "}"  */
#line 925 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4724 "hexpr.parse.C"
    break;

  case 308: /* tpred: l1mtype "=" "(" l1mtype "*" l1mtype ")"  */
#line 926 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4730 "hexpr.parse.C"
    break;

  case 309: /* tpred: "{" l1mtype "*" l1mtype "}" "=" l1mtype  */
#line 927 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4736 "hexpr.parse.C"
    break;

  case 310: /* tpred: "{" id ":" l1mtype "*" l1mtype "}" "=" l1mtype  */
#line 928 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4742 "hexpr.parse.C"
    break;

  case 311: /* tpred: "(" l1mtype "*" l1mtype ")" "=" l1mtype  */
#line 929 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(1), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4748 "hexpr.parse.C"
    break;

  case 312: /* tpred: l1mtype "." recfieldname "::" l1mtype  */
#line 931 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4754 "hexpr.parse.C"
    break;

  case 313: /* tpred: l1mtype "." recfieldname "<-" l1mtype  */
#line 932 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4760 "hexpr.parse.C"
    break;

  case 314: /* tpred: l1mtype "/" l1mtype "::" l1mtype  */
#line 933 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4766 "hexpr.parse.C"
    break;

  case 315: /* tpred: l1mtype "/" l1mtype "<-" l1mtype  */
#line 934 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4772 "hexpr.parse.C"
    break;

  case 316: /* tpred: l1mtype "=" "|" l1mtype "+" l1mtype "|"  */
#line 936 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4778 "hexpr.parse.C"
    break;

  case 317: /* tpred: "|" l1mtype "+" l1mtype "|" "=" l1mtype  */
#line 937 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4784 "hexpr.parse.C"
    break;

  case 318: /* tpred: l1mtype "=" "|" id ":" l1mtype "+" l1mtype "|"  */
#line 938 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4790 "hexpr.parse.C"
    break;

  case 319: /* tpred: "|" id ":" l1mtype "+" l1mtype "|" "=" l1mtype  */
#line 939 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4796 "hexpr.parse.C"
    break;

  case 320: /* tpred: "|" id ":" l0mtype "|" "::" l1mtype  */
#line 941 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), TString::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype))); }
#line 4802 "hexpr.parse.C"
    break;

  case 321: /* tpred: "|" l1mtype "/" l0mtype "|" "::" l1mtype  */
#line 942 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), *(yyvsp[-5].mtype),                *(yyvsp[-3].mtype))); }
#line 4808 "hexpr.parse.C"
    break;

  case 322: /* tpred: l1mtype "++" l1mtype "=" l1mtype  */
#line 943 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(AppendsToUnqualifier::constraintName(), list(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4814 "hexpr.parse.C"
    break;

  case 323: /* l1mtargl: l1mtype  */
#line 945 "hexpr.y"
                           { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4820 "hexpr.parse.C"
    break;

  case 324: /* l1mtargl: l1mtargl l1mtype  */
#line 946 "hexpr.y"
                           { (yyvsp[-1].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4826 "hexpr.parse.C"
    break;

  case 325: /* ltmtype: ltmtype l0mtype  */
#line 948 "hexpr.y"
                          { (yyval.mtypes) = (yyvsp[-1].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4832 "hexpr.parse.C"
    break;

  case 326: /* ltmtype: l0mtype  */
#line 949 "hexpr.y"
                          { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4838 "hexpr.parse.C"
    break;

  case 327: /* l0mtype: l0mtargl "->" l1mtype  */
#line 951 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(Func::make(tuplety(*(yyvsp[-2].mtypes)), *(yyvsp[0].mtype)))); }
#line 4844 "hexpr.parse.C"
    break;

  case 328: /* l0mtype: mtuplist  */
#line 952 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(makeTupleType(*(yyvsp[0].mtypes)))); }
#line 4850 "hexpr.parse.C"
    break;

  case 329: /* l0mtype: msumlist  */
#line 953 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(makeSumType(*(yyvsp[0].mtypes)))); }
#line 4856 "hexpr.parse.C"
    break;

  case 330: /* l1mtype: id  */
#line 955 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(monoTypeByName(*(yyvsp[0].string)))); }
#line 4862 "hexpr.parse.C"
    break;

  case 331: /* l1mtype: "<" cppid ">"  */
#line 956 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(OpaquePtr::make(str::replace<char>(*(yyvsp[-1].string), ".", "::"), 0, false))); }
#line 4868 "hexpr.parse.C"
    break;

  case 332: /* l1mtype: "[" "]"  */
#line 957 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("[]"))); }
#line 4874 "hexpr.parse.C"
    break;

  case 333: /* l1mtype: "[" ltmtype "]"  */
#line 958 "hexpr.y"
                                           { try { (yyval.mtype) = autorelease(new MonoTypePtr(Array::make(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4880 "hexpr.parse.C"
    break;

  case 334: /* l1mtype: "[" ":" l0mtype "|" tyind ":" "]"  */
#line 959 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(FixedArray::make(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype)))); }
#line 4886 "hexpr.parse.C"
    break;

  case 335: /* l1mtype: "(" "->" ")"  */
#line 960 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("->"))); }
#line 4892 "hexpr.parse.C"
    break;

  case 336: /* l1mtype: "(" ltmtype ")"  */
#line 961 "hexpr.y"
                                           { try { (yyval.mtype) = autorelease(new MonoTypePtr(clone(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4898 "hexpr.parse.C"
    break;

  case 337: /* l1mtype: "{" mreclist "}"  */
#line 962 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(makeRecType(*(yyvsp[-1].mreclist)))); }
#line 4904 "hexpr.parse.C"
    break;

  case 338: /* l1mtype: "|" mvarlist "|"  */
#line 963 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(makeVarType(*(yyvsp[-1].mvarlist)))); }
#line 4910 "hexpr.parse.C"
    break;

  case 339: /* l1mtype: "(" ")"  */
#line 964 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("unit"))); }
#line 4916 "hexpr.parse.C"
    break;

  case 340: /* l1mtype: "intV"  */
#line 965 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(((yyvsp[0].intv) == 0) ? Prim::make("void") : TLong::make((yyvsp[0].intv)))); }
#line 4922 "hexpr.parse.C"
    break;

  case 341: /* l1mtype: "boolV"  */
#line 966 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr((yyvsp[0].boolv) ? TLong::make(1) : TLong::make(0))); }
#line 4928 "hexpr.parse.C"
    break;

  case 342: /* l1mtype: "exists" id "." l1mtype  */
#line 967 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Exists::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4934 "hexpr.parse.C"
    break;

  case 343: /* l1mtype: l1mtype "@" l1mtype  */
#line 968 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype), *(yyvsp[0].mtype)))); }
#line 4940 "hexpr.parse.C"
    break;

  case 344: /* l1mtype: l1mtype "@" "?"  */
#line 969 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype)))); }
#line 4946 "hexpr.parse.C"
    break;

  case 345: /* l1mtype: "^" id "." l1mtype  */
#line 970 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Recursive::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4952 "hexpr.parse.C"
    break;

  case 346: /* l1mtype: "stringV"  */
#line 971 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(TString::make(str::unescape(str::trimq(*(yyvsp[0].string)))))); }
#line 4958 "hexpr.parse.C"
    break;

  case 347: /* l1mtype: "`" l0expr "`"  */
#line 972 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp))))))); }
#line 4964 "hexpr.parse.C"
    break;

  case 348: /* tyind: id  */
#line 974 "hexpr.y"
              { (yyval.mtype) = autorelease(new MonoTypePtr(TVar::make(*(yyvsp[0].string)))); }
#line 4970 "hexpr.parse.C"
    break;

  case 349: /* tyind: "intV"  */
#line 975 "hexpr.y"
              { (yyval.mtype) = autorelease(new MonoTypePtr(TLong::make((yyvsp[0].intv)))); }
#line 4976 "hexpr.parse.C"
    break;

  case 350: /* cppid: id  */
#line 977 "hexpr.y"
                    { (yyval.string) = (yyvsp[0].string); }
#line 4982 "hexpr.parse.C"
    break;

  case 351: /* cppid: cppid "." id  */
#line 978 "hexpr.y"
                    { (yyval.string) = (yyvsp[-2].string); *(yyval.string) += "."; *(yyval.string) += *(yyvsp[0].string); }
#line 4988 "hexpr.parse.C"
    break;

  case 352: /* l0mtargl: l1mtype  */
#line 980 "hexpr.y"
                                        { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4994 "hexpr.parse.C"
    break;

  case 353: /* l0mtargl: "(" l0mtype "," l0mtarglt ")"  */
#line 981 "hexpr.y"
                                        { (yyvsp[-1].mtypes)->insert((yyvsp[-1].mtypes)->begin(), *(yyvsp[-3].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 5000 "hexpr.parse.C"
    break;

  case 354: /* l0mtarglt: l0mtype  */
#line 983 "hexpr.y"
                                 { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5006 "hexpr.parse.C"
    break;

  case 355: /* l0mtarglt: l0mtarglt "," l0mtype  */
#line 984 "hexpr.y"
                                 { (yyvsp[-2].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-2].mtypes); }
#line 5012 "hexpr.parse.C"
    break;

  case 356: /* mtuplist: l1mtype  */
#line 986 "hexpr.y"
                               { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5018 "hexpr.parse.C"
    break;

  case 357: /* mtuplist: mtuplist "*" l1mtype  */
#line 987 "hexpr.y"
                               { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5024 "hexpr.parse.C"
    break;

  case 358: /* msumlist: l1mtype "+" l1mtype  */
#line 989 "hexpr.y"
                               { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[-2].mtype)); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5030 "hexpr.parse.C"
    break;

  case 359: /* msumlist: msumlist "+" l1mtype  */
#line 990 "hexpr.y"
                               { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5036 "hexpr.parse.C"
    break;

  case 360: /* mreclist: mreclist "," id ":" l0mtype  */
#line 992 "hexpr.y"
                                      { (yyval.mreclist) = (yyvsp[-4].mreclist);                                 (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 5042 "hexpr.parse.C"
    break;

  case 361: /* mreclist: id ":" l0mtype  */
#line 993 "hexpr.y"
                                      { (yyval.mreclist) = autorelease(new Record::Members()); (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 5048 "hexpr.parse.C"
    break;

  case 362: /* mvarlist: mvarlist "," id ":" l0mtype  */
#line 995 "hexpr.y"
                                      { (yyval.mvarlist) = (yyvsp[-4].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 5054 "hexpr.parse.C"
    break;

  case 363: /* mvarlist: mvarlist "," id  */
#line 996 "hexpr.y"
                                      { (yyval.mvarlist) = (yyvsp[-2].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 5060 "hexpr.parse.C"
    break;

  case 364: /* mvarlist: id ":" l0mtype  */
#line 997 "hexpr.y"
                                      { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 5066 "hexpr.parse.C"
    break;

  case 365: /* mvarlist: id  */
#line 998 "hexpr.y"
                                      { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 5072 "hexpr.parse.C"
    break;

  case 366: /* mvarlist: mvarlist "," id "(" "intV" ")"  */
#line 999 "hexpr.y"
                                           { (yyval.mvarlist) = (yyvsp[-5].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), (yyvsp[-1].intv))); }
#line 5078 "hexpr.parse.C"
    break;

  case 367: /* mvarlist: mvarlist "," id "(" "shortV" ")"  */
#line 1000 "hexpr.y"
                                           { (yyval.mvarlist) = (yyvsp[-5].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"),(yyvsp[-1].shortv))); }
#line 5084 "hexpr.parse.C"
    break;

  case 368: /* mvarlist: mvarlist "," id "(" "boolV" ")"  */
#line 1001 "hexpr.y"
                                           { (yyval.mvarlist) = (yyvsp[-5].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), (yyvsp[-1].boolv))); }
#line 5090 "hexpr.parse.C"
    break;

  case 369: /* mvarlist: mvarlist "," id "(" "byteV" ")"  */
#line 1002 "hexpr.y"
                                           { (yyval.mvarlist) = (yyvsp[-5].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), str::dehex(*(yyvsp[-1].string)))); }
#line 5096 "hexpr.parse.C"
    break;

  case 370: /* mvarlist: mvarlist "," id "(" "charV" ")"  */
#line 1003 "hexpr.y"
                                           { (yyval.mvarlist) = (yyvsp[-5].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), str::readCharDef(*(yyvsp[-1].string)))); }
#line 5102 "hexpr.parse.C"
    break;

  case 371: /* mvarlist: id "(" "intV" ")"  */
#line 1004 "hexpr.y"
                                           { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), (yyvsp[-1].intv))); }
#line 5108 "hexpr.parse.C"
    break;

  case 372: /* mvarlist: id "(" "shortV" ")"  */
#line 1005 "hexpr.y"
                                           { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), (yyvsp[-1].shortv))); }
#line 5114 "hexpr.parse.C"
    break;

  case 373: /* mvarlist: id "(" "boolV" ")"  */
#line 1006 "hexpr.y"
                                           { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), (yyvsp[-1].boolv))); }
#line 5120 "hexpr.parse.C"
    break;

  case 374: /* mvarlist: id "(" "byteV" ")"  */
#line 1007 "hexpr.y"
                                           { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), str::dehex(*(yyvsp[-1].string)))); }
#line 5126 "hexpr.parse.C"
    break;

  case 375: /* mvarlist: id "(" "charV" ")"  */
#line 1008 "hexpr.y"
                                           { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-3].string), Prim::make("unit"), str::readCharDef(*(yyvsp[-1].string)))); }
#line 5132 "hexpr.parse.C"
    break;


#line 5136 "hexpr.parse.C"

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

#line 1012 "hexpr.y"

#pragma GCC diagnostic pop


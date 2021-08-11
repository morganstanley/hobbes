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


#line 327 "hexpr.parse.C"

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

// #include "hexpr.parse.H"
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
#define YYLAST   3078

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  98
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  82
/* YYNRULES -- Number of rules.  */
#define YYNRULES  366
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  809

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
       0,   488,   488,   489,   490,   491,   494,   495,   496,   498,
     499,   500,   502,   503,   504,   505,   506,   507,   509,   510,
     511,   512,   513,   514,   515,   516,   517,   518,   519,   520,
     521,   522,   523,   524,   525,   526,   527,   528,   529,   532,
     535,   538,   539,   540,   543,   544,   547,   549,   552,   553,
     554,   555,   556,   557,   558,   559,   561,   562,   564,   566,
     567,   569,   572,   573,   574,   575,   577,   578,   580,   583,
     585,   587,   588,   590,   592,   594,   595,   596,   597,   598,
     599,   600,   601,   602,   603,   604,   605,   606,   607,   608,
     609,   610,   611,   612,   614,   615,   617,   618,   621,   622,
     623,   624,   626,   627,   628,   629,   630,   631,   633,   634,
     636,   637,   638,   639,   640,   641,   642,   643,   644,   646,
     647,   648,   649,   650,   652,   653,   654,   655,   657,   660,
     661,   664,   667,   670,   682,   683,   686,   688,   689,   691,
     693,   694,   696,   697,   699,   700,   702,   703,   705,   706,
     709,   710,   713,   714,   715,   716,   717,   718,   719,   720,
     721,   724,   725,   726,   727,   728,   731,   732,   733,   736,
     739,   742,   743,   746,   747,   748,   749,   750,   751,   752,
     753,   754,   755,   756,   757,   758,   759,   762,   765,   766,
     767,   768,   769,   770,   771,   772,   773,   774,   775,   776,
     777,   778,   779,   780,   781,   782,   785,   787,   788,   790,
     792,   793,   795,   797,   798,   800,   801,   803,   804,   805,
     807,   808,   810,   811,   813,   814,   816,   817,   820,   821,
     823,   824,   825,   826,   827,   828,   829,   830,   831,   832,
     833,   834,   835,   836,   837,   838,   839,   840,   841,   842,
     843,   844,   845,   846,   848,   849,   850,   851,   852,   854,
     856,   857,   859,   860,   862,   863,   865,   867,   868,   869,
     871,   872,   873,   874,   875,   876,   877,   878,   879,   880,
     881,   882,   883,   884,   885,   891,   892,   893,   894,   896,
     897,   899,   900,   901,   903,   904,   905,   907,   908,   911,
     913,   914,   916,   917,   918,   919,   920,   921,   922,   923,
     924,   925,   927,   928,   929,   930,   932,   933,   934,   935,
     937,   938,   939,   941,   942,   944,   945,   947,   948,   949,
     951,   952,   953,   954,   955,   956,   957,   958,   959,   960,
     961,   962,   963,   964,   965,   966,   967,   968,   970,   971,
     973,   974,   976,   977,   979,   980,   982,   983,   985,   986,
     988,   989,   991,   992,   993,   994,   996
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

#define YYPACT_NINF (-590)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-366)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     385,  1512,  2283,  2283,   159,    28,    28,    28,    53,    53,
     131,   131,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,
    -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,    93,
     121,  2283,  2985,   -32,  2985,    28,   185,  1732,  2283,    93,
     137,  2283,   376,  -590,   959,  -590,  -590,  -590,  -590,  -590,
    -590,  -590,   260,  -590,   263,   301,    21,   273,  2751,  2595,
    2283,  1815,  1690,  1690,  -590,   176,   272,   399,   364,   412,
    -590,   321,  -590,  -590,  -590,  1512,   365,   373,  -590,  2981,
     179,  -590,  -590,   218,   868,   409,    53,   445,  1177,  -590,
    -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,
    -590,  -590,  -590,  -590,  -590,  1690,    28,   -14,  -590,   353,
    -590,   430,   259,  2907,    28,   259,   480,  2361,   444,   446,
    2673,   448,   449,   450,    93,   451,   452,   454,   455,   456,
     457,   461,   465,  1611,   466,   468,   469,  -590,  -590,   470,
    -590,   203,   -29,   117,   343,   490,   500,    99,   458,    28,
      28,   460,  -590,  1276,  1276,  1690,  2283,  2049,    21,  -590,
    -590,    93,  2283,   230,  -590,  -590,   481,   444,   446,  2673,
     448,   449,   450,   451,   452,   454,   456,   457,   461,   465,
     466,   468,   469,  -590,  -590,  -590,  -590,  -590,  -590,  -590,
    -590,  -590,  -590,  -590,  -590,  -590,  1690,  1690,    28,   383,
     301,  1096,  -590,  -590,  -590,  1535,  2517,  2517,  2517,  2517,
    2595,  2751,  2751,  2751,  2751,  2751,  2751,  2751,  2751,  2751,
    2751,  2751,  2829,  2829,  2829,  2283,  -590,   959,    28,  -590,
    -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,
    -590,  -590,  -590,  -590,  -590,  -590,  1276,  -590,  1276,  -590,
    -590,  -590,    28,    28,   459,   155,  1399,  1399,    28,  2283,
     345,  -590,   138,  1399,    28,    29,    53,  2981,    28,   459,
      28,    28,    34,  -590,    41,   516,   514,   517,  -590,  -590,
     346,   483,   358,  -590,   523,  2283,    66,  2595,   485,   486,
     259,    19,  -590,   530,  2985,  1893,    93,   487,  1971,  -590,
     532,   533,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,
    -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,
    -590,  -590,  2283,  1690,  2127,  -590,  -590,   403,  2283,  2283,
    2283,  -590,  -590,  -590,  -590,  -590,   527,  -590,   540,  -590,
    -590,  -590,   350,   499,  2283,   124,  -590,  -590,  2283,   234,
    2283,   359,   233,   435,   537,   196,  2283,  -590,  2283,     1,
     495,   495,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,
    -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,   959,
    -590,  -590,  -590,   535,   354,   507,  -590,   548,  -590,     0,
    1177,  -590,   684,   459,    46,   436,   551,   120,   400,   116,
     544,   494,  -590,   868,   184,  1399,  1399,    93,  1399,  1399,
    1399,   800,  1399,   503,    53,   582,    28,    28,  1177,   519,
     563,   570,   592,  -590,  1399,  1399,  1399,  1399,  -590,   534,
    1690,  -590,    45,  1690,  -590,  2283,  -590,  -590,   434,  1690,
     486,  -590,  -590,  -590,  -590,   205,  -590,  -590,  -590,  -590,
    -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  1893,
    2439,    93,   453,   301,  -590,   523,  -590,  2283,  -590,  -590,
    2283,  -590,  -590,    -1,   574,  -590,   536,  -590,   573,  -590,
     538,   541,   459,   178,  1177,  -590,  -590,   539,  2205,  -590,
    -590,  2283,   255,   547,  -590,   542,  -590,   543,  -590,    52,
    1690,  1690,  -590,  -590,  -590,  1399,  -590,  -590,  -590,  -590,
    1399,   545,  -590,  1399,  -590,    28,  1177,  1399,  1177,  -590,
      28,  1177,  1399,  -590,  -590,  1399,  1399,  1399,   114,   112,
     304,   503,   503,   503,  -590,   503,   503,    37,    53,   582,
    -590,    26,  -590,   351,  -590,  -590,   195,  1177,  1177,  1177,
      53,   592,  -590,   503,   503,   503,   503,  -590,  -590,  -590,
    -590,  -590,  -590,   578,   410,  -590,   433,   828,  -590,   549,
    -590,    47,  2985,   589,   211,   552,   550,  -590,  1690,  2283,
    -590,  2283,  -590,  -590,  -590,  -590,  -590,   553,  -590,  2283,
     270,  2283,  -590,  -590,  -590,   554,   556,   503,   225,   463,
      54,   599,  -590,     7,   298,   561,   608,   564,    42,   503,
     166,   201,   611,   126,   612,  1399,  1399,  1399,  1399,  1399,
     582,    28,  -590,  -590,   582,    28,    28,  -590,   592,  -590,
     391,  -590,  -590,   609,  -590,    28,   593,   434,    28,  2283,
    2283,  2283,  -590,  -590,  -590,  2283,  -590,  -590,   616,   259,
    2439,  2439,  -590,  -590,  -590,  -590,   571,  -590,  -590,  -590,
    2283,   278,  -590,  -590,  -590,   615,  -590,   620,  -590,   617,
    1177,  1399,   618,   621,  1177,   622,  1399,  1399,  1399,  1399,
    1399,  1399,   503,   503,   503,   503,   503,   582,    30,   582,
    -590,    28,   592,  -590,  1177,  2283,   619,  2283,  -590,   624,
    -590,   626,  -590,  -590,   584,    55,  2517,  -590,  2283,   292,
    1399,   587,  1399,  -590,   236,  1399,  1399,  -590,  1399,   305,
     243,   282,   209,   307,   172,   582,  -590,  -590,  2283,  -590,
    2283,  2283,  -590,  -590,  -590,     1,   585,  -590,  2283,   296,
     503,  -590,   503,   627,   503,   503,   503,   629,  -590,  -590,
    1399,  -590,  1399,   582,  -590,  -590,  -590,  2517,  -590,  2283,
     300,  1399,  1399,   283,   329,     1,  -590,  2283,   311,   503,
     503,  -590,  -590,  -590,  2283,   319,  -590,  2283,   320,  -590,
    2283,   322,  -590,  2283,   325,  -590,  2283,   326,  -590,  2283,
     327,  -590,  2283,   330,  -590,  2283,   341,  -590,  2283,   342,
    -590,  2283,   344,  -590,  2283,   630,  -590,  2283,  -590
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     9,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   173,   174,   175,   176,   177,   178,   179,   180,
     181,   182,   366,   183,   170,   220,   185,   186,   288,     0,
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
       0,     0,     0,   347,   301,     0,     0,     0,     0,     0,
       0,   303,   305,   304,   344,   343,   324,    48,     0,    54,
      59,    53,    56,     0,    94,    70,    63,     0,     0,     0,
       0,    64,    66,   358,   327,   357,   359,   256,   262,   258,
     264,   266,   130,     0,     0,   289,     0,     0,   224,   209,
     211,     0,     0,     0,     0,     0,     0,   154,     0,     0,
     152,     0,   162,   161,   297,   160,   159,     0,    20,     0,
       0,     0,   250,   245,   252,     0,     0,   342,     0,     0,
       0,     0,   361,   356,     0,     0,   363,   364,   356,   345,
       0,     0,   330,     0,   330,     0,     0,     0,     0,     0,
       0,     0,    61,    60,     0,     0,     0,    95,     0,   354,
       0,   364,    68,     0,    67,     0,   164,     0,     0,     0,
       0,     0,   214,   219,   218,     0,   213,   216,   217,   172,
       0,     0,   163,   135,   142,   148,   147,   269,   158,    21,
       0,     0,   108,   248,   247,     0,   349,     0,   348,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   322,   315,   314,   313,   312,    50,    49,    55,
      57,    58,    65,   353,     0,     0,     0,     0,   290,     0,
     291,     0,   226,   210,     0,     0,     0,    22,     0,     0,
       0,     0,     0,   360,     0,     0,     0,   362,     0,   358,
       0,     0,     0,     0,     0,     0,   355,    47,     0,   165,
       0,     0,   212,   215,   217,   145,   146,    23,     0,     0,
     311,   334,   309,     0,   317,   321,   320,     0,   308,   306,
       0,   316,     0,    51,   293,   292,   227,     0,    24,     0,
       0,     0,     0,     0,     0,   144,    25,     0,     0,   310,
     319,   307,   318,    26,     0,     0,    27,     0,     0,    28,
       0,     0,    29,     0,     0,    30,     0,     0,    31,     0,
       0,    32,     0,     0,    33,     0,     0,    34,     0,     0,
      35,     0,     0,    36,     0,     0,    37,     0,    38
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -590,  -590,   596,   462,   -26,  -590,  -590,  -590,  -590,   134,
    -590,  -590,    59,    56,  -589,  -515,  -590,    57,  -519,  -387,
     498,     4,   415,    58,   265,    -2,  -198,   -39,   284,   -28,
     281,    18,  -590,   401,  -590,   388,  -590,   110,  -590,   -17,
    -590,   405,  -590,    48,  -590,  -590,   -13,   -58,  -590,  -590,
     251,   -47,  -590,   -91,   -56,  -174,  -590,  -173,  -376,  -590,
      -8,   -51,  -590,    63,   -24,  -126,   502,  -590,   291,  -590,
     447,   -86,   984,  -590,   464,  -590,  -590,  -590,  -590,  -590,
    -590,   668
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     4,    43,    44,    45,    46,    47,   151,    48,    49,
     632,    50,   541,   542,   539,   540,    51,   551,   552,   265,
     266,    52,   139,   543,   272,   140,    65,    66,    67,    68,
      69,    70,   107,   108,   298,   299,   736,   472,   473,    54,
     291,   292,   569,   570,   571,   646,   647,    55,   113,   440,
     441,   201,   202,   109,   279,   280,   281,   282,   283,   144,
     145,    56,   564,   565,   141,   337,   338,   260,   261,   412,
     387,   339,   274,   667,    77,   275,   630,   276,   277,   395,
     398,    73
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      64,    72,   273,   158,   200,   200,   203,   203,   359,   360,
     361,   362,    81,    81,   143,   112,   205,   115,   152,    53,
     165,   103,   351,   352,   623,   353,   300,   537,   340,   111,
     164,   687,   634,  -352,   624,   689,   142,   415,   725,   148,
    -352,   424,   422,   510,    22,   620,   114,   200,   424,   249,
     671,   323,   160,    22,   250,   285,   560,   324,   166,    22,
     251,   158,    53,   643,   158,   207,   208,   161,   286,   252,
      22,   643,    22,   644,  -352,  -352,   577,    22,    22,   578,
      22,   644,   424,   676,   247,   253,   209,   247,   411,   513,
      81,    22,   164,    53,   165,   411,   290,   200,   442,   341,
      89,    90,    91,    92,    93,    94,    95,   625,   269,   416,
     255,   625,   270,    96,   271,   297,   103,   621,    22,    97,
     381,   258,   382,   560,   559,   645,   259,    79,    98,   411,
     411,   594,   342,   669,   411,   435,   753,   330,   200,   200,
     105,   164,   411,   200,   106,   357,    22,   200,   616,   357,
     521,   617,   615,   346,    99,   345,    22,   146,   488,    74,
     347,   517,    22,   633,   518,   100,   101,   680,   388,   388,
     249,   363,   623,   634,   623,   250,   404,   102,   405,   331,
      22,   251,   406,   419,   407,   408,   423,   409,   410,   390,
     252,   372,   373,   374,   325,   105,  -365,  -365,   322,   106,
     411,   489,   411,   628,    22,    84,   253,   300,   411,   677,
     249,  -352,   206,   752,   411,   250,   404,   246,   405,   424,
      22,   251,   406,   378,   407,   408,   411,   409,   410,   269,
     252,   255,   391,   270,   501,   271,    28,   463,   623,   158,
     207,   208,   258,    22,   678,    53,   253,   259,   437,   651,
     419,    29,   750,    79,   411,    22,   248,   401,   525,    22,
     411,   209,   526,   117,   527,   200,   411,   474,   348,   269,
     247,   255,   491,   270,   572,   271,   502,   445,   321,   156,
      22,   157,   258,   434,   322,   351,   352,   259,   353,   411,
      28,   652,    79,   589,    28,    22,   297,   411,    22,   153,
     665,   509,   154,    22,   511,    29,   509,   388,   660,    29,
     496,   162,   -73,   411,   497,   743,   708,    22,   748,   478,
     471,    22,   476,   155,   411,    22,   479,   480,   481,   159,
     738,   411,   273,   156,   759,   157,    22,   156,   767,   157,
     618,   210,   487,   619,    22,    22,   490,    22,   493,   774,
      22,    22,    22,   152,   503,    22,   504,   777,   780,   225,
     783,   749,   771,   786,   789,   792,    22,    22,   795,    22,
     411,   411,   200,   227,   558,   200,    22,   561,   672,   798,
     801,   200,   804,   203,   626,   747,   411,   751,     1,     2,
       3,   287,   567,   411,   158,   411,   419,    53,   584,   530,
     228,   463,   463,   354,   219,   220,   221,   506,    22,   772,
      89,    90,    91,    92,    93,    94,    95,   411,    81,   228,
     402,   428,   326,    96,   327,   485,   403,   429,    22,    97,
     602,   322,   605,   562,   494,   607,   143,   431,    98,   432,
     495,   264,   200,   200,   595,   596,   211,   212,   213,   214,
     215,   216,   217,   218,   563,   222,   223,   224,   142,    22,
     423,   629,   602,   631,    99,   575,   693,   638,   576,   149,
     150,   639,   694,   573,   249,   100,   101,   268,    22,   250,
     519,   520,   477,   666,    22,   251,   587,   102,    22,   588,
     636,   637,   385,   288,   252,   364,   365,   366,   367,   368,
     369,   370,   371,   375,   376,   377,    80,    83,   735,   200,
     253,   357,    85,    87,   498,   514,   499,   515,   294,   302,
     200,   303,   474,   305,   306,   307,   308,   309,   328,   310,
     311,   312,   313,   269,   386,   255,   314,   270,   329,   271,
     315,   317,   249,   318,   319,   320,   258,   250,   350,   425,
     332,   259,    22,   251,    81,   649,   335,   426,   427,   765,
     385,   433,   252,   249,   430,   438,   439,   444,   250,   466,
     470,  -254,   484,    22,   251,   500,   486,   656,   253,   657,
     209,   505,   507,   252,   713,   516,   523,   659,   717,   662,
     522,   411,   463,   463,   595,   596,   538,   548,   158,   253,
     547,   482,   386,   255,   549,   256,   550,   257,   726,   557,
     579,   581,   635,   580,   258,   591,   585,   592,   582,   259,
     593,   583,   269,   508,   255,   599,   270,   650,   271,   642,
     658,   653,   654,   670,   663,   258,   664,   700,   701,   702,
     259,   673,   674,   704,   675,   679,   681,   695,   480,   481,
     705,   697,   706,   710,   711,   712,   715,   728,   707,   731,
     716,   718,   730,   732,   741,   761,   757,   762,   807,    57,
      71,   226,   622,    75,    76,    78,    82,    82,    86,    88,
     688,   690,   417,   546,   691,   692,   469,   436,   655,   379,
     703,   568,   733,   727,   524,   729,   443,   104,   110,   249,
     698,     0,   392,   116,   250,     0,   737,   104,   147,    22,
     251,     0,    57,     0,     0,     0,     0,   384,     0,   252,
       0,     0,     0,     0,     0,   163,   754,     0,   755,   756,
     204,   204,     0,     0,     0,   253,   758,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,    82,     0,
       0,    82,   263,     0,   267,     0,   278,   766,   269,     0,
     255,   512,   270,     0,   271,   773,     0,     0,     0,     0,
       0,   258,   776,   204,   284,   779,   259,     0,   782,     0,
       0,   785,   293,     0,   788,   301,     0,   791,     0,     0,
     794,     0,   104,   797,     0,     0,   800,     0,     0,   803,
       0,     0,   806,     0,     0,   808,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   249,     0,   333,   334,     0,
     250,   278,   278,   204,     0,    22,   251,     0,     0,   104,
       0,   349,     0,     0,     0,   252,   640,     0,     0,     0,
       0,     0,     0,   183,   184,   185,   186,   187,   188,   189,
     190,   253,   191,    22,   192,   193,    25,   194,   195,     0,
       0,   641,     0,     0,   204,   204,   284,   355,     0,   204,
       0,     0,     0,   204,   393,     0,   255,     0,   270,     0,
     271,     0,     0,   249,     0,     0,     0,   258,   250,     0,
     534,     0,   259,    22,   251,    57,   380,     0,     0,     0,
       0,     0,   196,   252,   197,     0,   198,     0,   199,     0,
       0,     0,     0,     0,   278,     0,   278,     0,     0,   253,
     383,    78,   278,   278,   396,   399,   400,     0,     0,     0,
       0,   278,   414,     0,    82,     0,   418,   278,   420,   421,
     278,     0,   254,     0,   255,     0,   256,     0,   257,     0,
       0,     0,     0,     0,   110,   258,     0,     0,     0,   293,
     259,     0,     0,   464,   465,     0,   301,     0,     7,     8,
       9,    10,    11,     0,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,   204,     0,     0,     0,   104,     0,     0,     0,     0,
       0,     0,     0,     0,   263,    29,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,   492,    32,     0,
      33,     0,     0,     0,     0,     0,     0,     0,     0,    34,
      35,    36,     0,    37,     0,    38,     0,    39,     0,    40,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,    41,     0,     0,    42,   278,     0,     0,   278,     0,
     278,   278,     0,     0,     0,     0,     0,     0,   262,     0,
       0,   263,     0,   278,   278,   104,   278,   278,   278,   278,
     278,     0,   267,     0,   544,   545,   278,     0,     0,     0,
       0,     0,   278,   278,   278,   278,     0,     0,   204,     0,
     284,   204,     0,     0,     0,     0,   566,   204,     0,     0,
       0,   183,   184,   185,   186,   187,   188,   189,   190,     0,
     191,    22,   192,   193,    25,   194,   195,   464,   464,   465,
     574,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   356,     0,     0,     0,     0,     0,     0,     0,
     278,     0,   278,     0,     0,     0,     0,     0,     0,     0,
     590,     0,     0,     0,     0,     0,     0,   284,   204,   204,
     196,     0,   197,   278,   198,     0,   199,     0,   278,     0,
       0,   278,     0,   601,   278,   278,   278,     0,   606,   278,
     278,     0,   249,   278,   612,   614,     0,   250,     0,     0,
       0,     0,    22,   251,     0,     0,    82,     0,     0,     0,
       0,   627,   252,     0,   278,   278,   278,   278,   267,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   253,     0,
       0,     0,     0,     0,     0,   204,     0,     0,   389,   648,
     394,   397,     0,     0,     0,     0,   204,   413,     0,     0,
       0,   269,     0,   255,     0,   270,     0,   271,   661,     0,
       0,     0,     0,     0,   258,     0,     0,   668,     0,   259,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   278,   278,   278,   278,   278,     0,   544,
       0,   249,     0,   544,   544,     0,   250,     0,     0,     0,
       0,    22,   251,   696,     0,   566,   699,     0,     0,     0,
       0,   252,     0,     0,     0,     0,     0,     0,   464,   464,
     483,     0,     0,     0,     0,     0,     0,   253,     0,   709,
       0,     0,     0,     0,     0,     0,     0,     0,   278,   278,
       0,     0,   278,     0,   278,   278,   278,   278,   278,   278,
     336,     0,   255,     0,   270,     0,   271,     0,     0,   627,
       0,     0,   278,   258,     0,     0,     0,     0,   259,     0,
       0,     0,     0,   734,     0,     0,     0,   739,   278,     0,
     278,     0,     0,   278,   278,     0,   278,   262,     0,   528,
     529,     0,   531,   532,   533,   535,   536,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   760,   553,   554,
     555,   556,     0,     0,   249,     0,     0,     0,   278,   250,
     278,     0,     0,     0,    22,   251,     0,     0,   768,   278,
     278,     0,     0,     0,   252,     0,   775,     0,     0,     0,
       0,     0,     0,   778,     0,     0,   781,     0,     0,   784,
     253,     0,   787,     0,     0,   790,     0,     0,   793,     0,
       0,   796,     0,     0,   799,     0,   389,   802,     0,     0,
     805,     0,     0,   393,     0,   255,     0,   270,     0,   271,
       0,     0,     0,     0,     0,     0,   258,     0,     0,   597,
       0,   259,     0,     0,   598,     0,     0,   600,     0,     0,
     603,   604,     0,     0,     0,   608,   609,     0,     0,   610,
     611,   613,     0,     0,     0,     0,     0,     0,     5,     6,
       0,     7,     8,     9,    10,    11,     0,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,     0,     0,     0,     0,
     183,   184,   185,   186,   187,   188,   189,   190,    29,   191,
      22,   192,   193,    25,   194,   195,     0,     0,    30,    31,
       0,    32,     0,    33,     0,     0,     0,     0,     0,     0,
       0,   358,    34,    35,    36,     0,    37,     0,    38,     0,
      39,     0,    40,     0,     0,     0,     0,     0,     0,   682,
     683,   684,   685,   686,    41,     0,     0,    42,     0,   196,
       0,   197,     0,   198,     0,   199,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,   714,     0,    29,     0,     0,
     719,   720,   721,   722,   723,   724,     0,    30,    31,     0,
      32,     0,    33,     0,     0,     0,     0,    60,     0,     0,
       0,    34,    35,    36,     0,    61,   316,    38,     0,    39,
       0,    40,     0,     0,   740,     0,   742,     0,     0,   744,
     745,     0,   746,    41,     0,   183,   184,   185,   186,   187,
     188,   189,   190,     0,   191,    22,   192,   193,    25,   194,
     195,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   763,     0,   764,     0,     0,     0,
       0,     0,     0,     0,     0,   769,   770,    12,    13,    14,
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
       0,     0,     0,     0,     0,     0,     0,    41,   446,   447,
     448,   449,   450,   451,   452,   453,    20,   454,    22,   455,
     456,    25,   457,   458,    28,     0,     0,     0,     0,     0,
       0,     0,     0,   167,   168,   169,   170,   171,   172,    29,
     173,   174,   175,   128,   176,   177,   178,   179,   133,    30,
      31,     0,    32,     0,    33,     0,     0,   180,   181,    60,
       0,     0,   182,    34,    35,    36,     0,   459,     0,   460,
       0,   461,     0,   462,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,    41,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,    29,     0,     0,
       0,     0,     0,     0,     0,     0,    59,    30,    31,     0,
      32,     0,    33,     0,     0,     0,     0,    60,     0,     0,
       0,    34,    35,    36,   467,   295,     0,    38,     0,   296,
     468,    40,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,    41,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,   343,    27,
      28,     0,     0,   344,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,    29,     0,     0,     0,     0,
       0,     0,     0,     0,    59,    30,    31,     0,    32,     0,
      33,     0,     0,     0,     0,    60,     0,     0,     0,    34,
      35,    36,     0,    61,     0,    38,     0,    39,     0,    40,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,    41,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,    29,     0,     0,     0,     0,     0,     0,
       0,     0,    59,    30,    31,     0,    32,     0,    33,     0,
       0,     0,     0,    60,     0,     0,     0,    34,    35,    36,
       0,    61,     0,    38,   475,    39,     0,    40,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,    41,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,    29,     0,     0,     0,     0,     0,     0,     0,     0,
      59,    30,    31,     0,    32,     0,    33,     0,     0,     0,
       0,    60,     0,     0,     0,    34,    35,    36,     0,    61,
       0,    38,   586,    39,     0,    40,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,    41,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,    29,
       0,     0,     0,     0,     0,     0,     0,     0,    59,    30,
      31,     0,    32,     0,    33,     0,     0,     0,     0,    60,
       0,     0,     0,    34,    35,    36,     0,    61,     0,    38,
       0,    39,     0,    40,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,    41,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,    29,     0,     0,
       0,     0,     0,     0,     0,     0,    59,    30,    31,     0,
      32,     0,    33,     0,     0,     0,     0,    60,     0,     0,
       0,    34,    35,    36,     0,   295,     0,    38,     0,   296,
       0,    40,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,    41,   446,   447,   448,   449,   450,   451,
     452,   453,    20,   454,    22,   455,   456,    25,   457,   458,
      28,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,    29,     0,     0,     0,     0,
       0,     0,     0,     0,    59,    30,    31,     0,    32,     0,
      33,     0,     0,     0,     0,    60,     0,     0,     0,    34,
      35,    36,     0,   459,     0,   460,     0,   461,     0,   462,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
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
       0,    60,     0,     0,     0,    34,    35,    36,     0,    61,
       0,    38,     0,    39,     0,    40,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    41,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,    29,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,     0,    32,     0,    33,     0,     0,     0,     0,     0,
       0,     0,     0,    34,    35,    36,     0,    61,   304,    38,
       0,    39,     0,    40,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    41,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,    29,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,     0,
      32,     0,    33,     0,     0,     0,     0,     0,     0,     0,
       0,    34,    35,    36,     0,    61,     0,    38,     0,    39,
       0,    40,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    41,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    29,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,     0,    32,     0,
      33,     0,     0,     0,     0,     0,     0,     0,     0,    34,
      35,    36,     0,    61,     0,    38,     0,    39,     0,    40,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    41,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    29,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    31,     0,     0,     0,     0,   289,
       0,     0,     0,     0,     0,     0,     0,    34,    35,     0,
       0,    61,     0,    38,     0,    39,     0,    40,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    41,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,     0,     0,     0,
       0,   229,   230,   231,   232,   233,   234,   235,   236,   237,
     238,    29,   239,   240,   241,   242,     0,     0,     0,     0,
       0,     0,    31,     0,     0,   243,   244,     0,     0,     0,
     245,     0,     0,     0,     0,    34,    35,     0,     0,    61,
       0,    38,     0,    39,     0,    40,   137,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    41,   138
};

static const yytype_int16 yycheck[] =
{
       2,     3,    88,    54,    62,    63,    62,    63,   206,   207,
     208,   209,     8,     9,    38,    32,    63,    34,    44,     1,
      59,    29,   196,   197,   539,   198,   117,   414,   154,    31,
      58,   620,   551,    33,     8,   624,    38,     8,     8,    41,
      33,    41,     8,    43,    25,     8,    78,   105,    41,    15,
      43,    80,    31,    25,    20,    69,   432,    86,    60,    25,
      26,   112,    44,    16,   115,    64,    65,    46,    82,    35,
      25,    16,    25,    26,    33,    33,    77,    25,    25,    80,
      25,    26,    41,    41,    80,    51,    85,    83,    88,    43,
      86,    25,   120,    75,   133,    88,   113,   155,    79,   155,
       7,     8,     9,    10,    11,    12,    13,    81,    74,    80,
      76,    81,    78,    20,    80,   117,   124,    80,    25,    26,
     246,    87,   248,   499,    79,    78,    92,    74,    35,    88,
      88,    79,   156,    79,    88,    69,   725,    38,   196,   197,
      74,   169,    88,   201,    78,   201,    25,   205,    36,   205,
      34,    39,    38,   161,    61,   157,    25,    20,    34,     0,
     162,    41,    25,   550,    44,    72,    73,    41,   254,   255,
      15,   210,   687,   692,   689,    20,    38,    84,    40,    80,
      25,    26,    44,   269,    46,    47,   272,    49,    50,    34,
      35,   219,   220,   221,    77,    74,    80,    81,    81,    78,
      88,    77,    88,     8,    25,    74,    51,   298,    88,    43,
      15,    33,    36,    41,    88,    20,    38,    38,    40,    41,
      25,    26,    44,   225,    46,    47,    88,    49,    50,    74,
      35,    76,    77,    78,    38,    80,    31,   295,   753,   290,
      64,    65,    87,    25,    43,   227,    51,    92,   287,    38,
     336,    46,    43,    74,    88,    25,    38,   259,    74,    25,
      88,    85,    78,    78,    80,   323,    88,   323,    38,    74,
     266,    76,    38,    78,    69,    80,    80,   294,    75,    74,
      25,    76,    87,   285,    81,   459,   460,    92,   461,    88,
      31,    80,    74,    38,    31,    25,   298,    88,    25,    39,
      75,   387,    39,    25,   390,    46,   392,   393,    38,    46,
      77,    38,    39,    88,    81,    79,    38,    25,    75,   327,
     322,    25,   324,    60,    88,    25,   328,   329,   330,    28,
      38,    88,   418,    74,    38,    76,    25,    74,    38,    76,
      36,    69,   344,    39,    25,    25,   348,    25,   350,    38,
      25,    25,    25,   379,   356,    25,   358,    38,    38,    38,
      38,    79,    79,    38,    38,    38,    25,    25,    38,    25,
      88,    88,   430,     8,   430,   433,    25,   433,    80,    38,
      38,   439,    38,   439,    33,    80,    88,    80,     3,     4,
       5,    38,   439,    88,   445,    88,   482,   379,   484,   407,
      46,   459,   460,    20,    40,    41,    42,    53,    25,    80,
       7,     8,     9,    10,    11,    12,    13,    88,   414,    46,
      75,    75,    79,    20,    81,    75,    81,    81,    25,    26,
     516,    81,   518,   435,    75,   521,   460,    79,    35,    81,
      81,    32,   500,   501,   500,   501,    47,    48,    49,    50,
      51,    52,    53,    54,    20,    43,    44,    45,   460,    25,
     546,   547,   548,   549,    61,   467,    75,    34,   470,    93,
      94,    38,    81,    20,    15,    72,    73,    32,    25,    20,
      80,    81,    79,    20,    25,    26,   488,    84,    25,   491,
      80,    81,    33,    63,    35,   211,   212,   213,   214,   215,
     216,   217,   218,   222,   223,   224,     8,     9,   706,   567,
      51,   567,    10,    11,    79,    79,    81,    81,    38,    75,
     578,    75,   578,    75,    75,    75,    75,    75,    38,    75,
      75,    75,    75,    74,    75,    76,    75,    78,    38,    80,
      75,    75,    15,    75,    75,    75,    87,    20,    67,    33,
      92,    92,    25,    26,   550,   572,    96,    43,    41,   757,
      33,    38,    35,    15,    81,    80,    80,    37,    20,    82,
      38,    38,    32,    25,    26,    38,    77,   579,    51,   581,
      85,    46,    75,    35,   670,    34,    92,   589,   674,   591,
      46,    88,   650,   651,   650,   651,    14,    34,   649,    51,
      81,    74,    75,    76,    34,    78,    14,    80,   694,    75,
      36,    38,    34,    77,    87,    68,    77,    75,    80,    92,
      77,    80,    74,    75,    76,    80,    78,    38,    80,    80,
      77,    79,    82,    34,    80,    87,    80,   639,   640,   641,
      92,    80,    34,   645,    80,    34,    34,    38,   650,   651,
      34,    58,    81,    38,    34,    38,    38,    38,   660,    33,
      39,    39,    38,    79,    77,    38,    81,    38,    38,     1,
       2,    75,   538,     5,     6,     7,     8,     9,    10,    11,
     621,   625,   267,   418,   626,   628,   298,   286,   578,   227,
     642,   440,   705,   695,   403,   697,   291,    29,    30,    15,
     637,    -1,   255,    35,    20,    -1,   708,    39,    40,    25,
      26,    -1,    44,    -1,    -1,    -1,    -1,   253,    -1,    35,
      -1,    -1,    -1,    -1,    -1,    57,   728,    -1,   730,   731,
      62,    63,    -1,    -1,    -1,    51,   738,    -1,    -1,    -1,
      -1,    -1,    -1,    75,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    83,    84,    -1,    86,    -1,    88,   759,    74,    -1,
      76,    77,    78,    -1,    80,   767,    -1,    -1,    -1,    -1,
      -1,    87,   774,   105,   106,   777,    92,    -1,   780,    -1,
      -1,   783,   114,    -1,   786,   117,    -1,   789,    -1,    -1,
     792,    -1,   124,   795,    -1,    -1,   798,    -1,    -1,   801,
      -1,    -1,   804,    -1,    -1,   807,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    15,    -1,   149,   150,    -1,
      20,   153,   154,   155,    -1,    25,    26,    -1,    -1,   161,
      -1,   163,    -1,    -1,    -1,    35,     8,    -1,    -1,    -1,
      -1,    -1,    -1,    15,    16,    17,    18,    19,    20,    21,
      22,    51,    24,    25,    26,    27,    28,    29,    30,    -1,
      -1,    33,    -1,    -1,   196,   197,   198,   199,    -1,   201,
      -1,    -1,    -1,   205,    74,    -1,    76,    -1,    78,    -1,
      80,    -1,    -1,    15,    -1,    -1,    -1,    87,    20,    -1,
      90,    -1,    92,    25,    26,   227,   228,    -1,    -1,    -1,
      -1,    -1,    74,    35,    76,    -1,    78,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   246,    -1,   248,    -1,    -1,    51,
     252,   253,   254,   255,   256,   257,   258,    -1,    -1,    -1,
      -1,   263,   264,    -1,   266,    -1,   268,   269,   270,   271,
     272,    -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      -1,    -1,    -1,    -1,   286,    87,    -1,    -1,    -1,   291,
      92,    -1,    -1,   295,   296,    -1,   298,    -1,     9,    10,
      11,    12,    13,    -1,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,   323,    -1,    -1,    -1,   327,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   336,    46,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    56,    57,   349,    59,    -1,
      61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   379,    -1,    -1,
      -1,    92,    -1,    -1,    95,   387,    -1,    -1,   390,    -1,
     392,   393,    -1,    -1,    -1,    -1,    -1,    -1,    84,    -1,
      -1,   403,    -1,   405,   406,   407,   408,   409,   410,   411,
     412,    -1,   414,    -1,   416,   417,   418,    -1,    -1,    -1,
      -1,    -1,   424,   425,   426,   427,    -1,    -1,   430,    -1,
     432,   433,    -1,    -1,    -1,    -1,   438,   439,    -1,    -1,
      -1,    15,    16,    17,    18,    19,    20,    21,    22,    -1,
      24,    25,    26,    27,    28,    29,    30,   459,   460,   461,
     462,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     482,    -1,   484,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     492,    -1,    -1,    -1,    -1,    -1,    -1,   499,   500,   501,
      74,    -1,    76,   505,    78,    -1,    80,    -1,   510,    -1,
      -1,   513,    -1,   515,   516,   517,   518,    -1,   520,   521,
     522,    -1,    15,   525,   526,   527,    -1,    20,    -1,    -1,
      -1,    -1,    25,    26,    -1,    -1,   538,    -1,    -1,    -1,
      -1,   543,    35,    -1,   546,   547,   548,   549,   550,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    -1,
      -1,    -1,    -1,    -1,    -1,   567,    -1,    -1,   254,   571,
     256,   257,    -1,    -1,    -1,    -1,   578,   263,    -1,    -1,
      -1,    74,    -1,    76,    -1,    78,    -1,    80,   590,    -1,
      -1,    -1,    -1,    -1,    87,    -1,    -1,   599,    -1,    92,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   615,   616,   617,   618,   619,    -1,   621,
      -1,    15,    -1,   625,   626,    -1,    20,    -1,    -1,    -1,
      -1,    25,    26,   635,    -1,   637,   638,    -1,    -1,    -1,
      -1,    35,    -1,    -1,    -1,    -1,    -1,    -1,   650,   651,
     336,    -1,    -1,    -1,    -1,    -1,    -1,    51,    -1,   661,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   670,   671,
      -1,    -1,   674,    -1,   676,   677,   678,   679,   680,   681,
      74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,   691,
      -1,    -1,   694,    87,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    -1,    -1,   705,    -1,    -1,    -1,   709,   710,    -1,
     712,    -1,    -1,   715,   716,    -1,   718,   403,    -1,   405,
     406,    -1,   408,   409,   410,   411,   412,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   739,   424,   425,
     426,   427,    -1,    -1,    15,    -1,    -1,    -1,   750,    20,
     752,    -1,    -1,    -1,    25,    26,    -1,    -1,   760,   761,
     762,    -1,    -1,    -1,    35,    -1,   768,    -1,    -1,    -1,
      -1,    -1,    -1,   775,    -1,    -1,   778,    -1,    -1,   781,
      51,    -1,   784,    -1,    -1,   787,    -1,    -1,   790,    -1,
      -1,   793,    -1,    -1,   796,    -1,   482,   799,    -1,    -1,
     802,    -1,    -1,    74,    -1,    76,    -1,    78,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,   505,
      -1,    92,    -1,    -1,   510,    -1,    -1,   513,    -1,    -1,
     516,   517,    -1,    -1,    -1,   521,   522,    -1,    -1,   525,
     526,   527,    -1,    -1,    -1,    -1,    -1,    -1,     6,     7,
      -1,     9,    10,    11,    12,    13,    -1,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    16,    17,    18,    19,    20,    21,    22,    46,    24,
      25,    26,    27,    28,    29,    30,    -1,    -1,    56,    57,
      -1,    59,    -1,    61,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    46,    70,    71,    72,    -1,    74,    -1,    76,    -1,
      78,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,   615,
     616,   617,   618,   619,    92,    -1,    -1,    95,    -1,    74,
      -1,    76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    42,    -1,   671,    -1,    46,    -1,    -1,
     676,   677,   678,   679,   680,   681,    -1,    56,    57,    -1,
      59,    -1,    61,    -1,    -1,    -1,    -1,    66,    -1,    -1,
      -1,    70,    71,    72,    -1,    74,    75,    76,    -1,    78,
      -1,    80,    -1,    -1,   710,    -1,   712,    -1,    -1,   715,
     716,    -1,   718,    92,    -1,    15,    16,    17,    18,    19,
      20,    21,    22,    -1,    24,    25,    26,    27,    28,    29,
      30,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   750,    -1,   752,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   761,   762,    15,    16,    17,
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
      -1,    -1,    -1,    -1,    55,    56,    57,    -1,    59,    -1,
      61,    -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,    70,
      71,    72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,
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
      -1,    76,    77,    78,    -1,    80,    -1,    -1,    83,    84,
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
      -1,    -1,    -1,    -1,    55,    56,    57,    -1,    59,    -1,
      61,    -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,    70,
      71,    72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,
      -1,    -1,    83,    84,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    66,    -1,    -1,    -1,    70,    71,    72,    -1,    74,
      -1,    76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,    46,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    56,
      57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    -1,    74,    75,    76,
      -1,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    92,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    42,    -1,    -1,    -1,    46,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    56,    57,    -1,
      59,    -1,    61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    -1,    74,    -1,    76,    -1,    78,
      -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    92,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    56,    57,    -1,    59,    -1,
      61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    92,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    -1,
      -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    -1,    -1,    -1,
      -1,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    46,    51,    52,    53,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    57,    -1,    -1,    64,    65,    -1,    -1,    -1,
      69,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    74,
      -1,    76,    -1,    78,    -1,    80,    85,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    97
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
      81,    34,    46,    92,   166,    74,    78,    80,   170,   170,
     158,   170,   170,   170,    90,   170,   170,   117,    14,   112,
     113,   110,   111,   121,   179,   179,   122,    81,    34,    34,
      14,   115,   116,   170,   170,   170,   170,    75,   152,    79,
     156,   152,   123,    20,   160,   161,   179,   149,   148,   140,
     141,   142,    69,    20,   179,   123,   123,    77,    80,    36,
      77,    38,    80,    80,   169,    77,    77,   123,   123,    38,
     179,    68,    75,    77,    79,   152,   152,   170,   170,    80,
     170,   179,   169,   170,   170,   169,   179,   169,   170,   170,
     170,   170,   179,   170,   179,    38,    36,    39,    36,    39,
       8,    80,   107,   113,     8,    81,    33,   179,     8,   169,
     174,   169,   108,   117,   116,    34,    80,    81,    34,    38,
       8,    33,    80,    16,    26,    78,   143,   144,   179,   137,
      38,    38,    80,    79,    82,   135,   123,   123,    77,   123,
      38,   179,   123,    80,    80,    75,    20,   171,   179,    79,
      34,    43,    80,    80,    34,    80,    41,    43,    43,    34,
      41,    34,   170,   170,   170,   170,   170,   112,   110,   112,
     111,   121,   115,    75,    81,    38,   179,    58,   161,   179,
     123,   123,   123,   141,   123,    34,    81,   123,    38,   179,
      38,    34,    38,   169,   170,    38,    39,   169,    39,   170,
     170,   170,   170,   170,   170,     8,   169,   123,    38,   123,
      38,    33,    79,   144,   179,   124,   134,   123,    38,   179,
     170,    77,   170,    79,   170,   170,   170,    80,    75,    79,
      43,    80,    41,   112,   123,   123,   123,    81,   123,    38,
     179,    38,    38,   170,   170,   124,   123,    38,   179,   170,
     170,    79,    80,   123,    38,   179,   123,    38,   179,   123,
      38,   179,   123,    38,   179,   123,    38,   179,   123,    38,
     179,   123,    38,   179,   123,    38,   179,   123,    38,   179,
     123,    38,   179,   123,    38,   179,   123,    38,   123
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
     177,   177,   178,   178,   178,   178,   179
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
       5,     3,     5,     3,     3,     1,     1
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
#line 488 "hexpr.y"
                            { yyParsedModule = (yyvsp[0].module);                     }
#line 2881 "hexpr.parse.C"
    break;

  case 3: /* s: "dodefn" id "=" l0expr  */
#line 489 "hexpr.y"
                            { yyParsedVar    = *(yyvsp[-2].string); yyParsedExpr = (yyvsp[0].exp); }
#line 2887 "hexpr.parse.C"
    break;

  case 4: /* s: "dodefn" l0expr  */
#line 490 "hexpr.y"
                            { yyParsedVar    = "";  yyParsedExpr = (yyvsp[0].exp); }
#line 2893 "hexpr.parse.C"
    break;

  case 5: /* s: "doexpr" l0expr  */
#line 491 "hexpr.y"
                            { yyParsedExpr   = (yyvsp[0].exp);                     }
#line 2899 "hexpr.parse.C"
    break;

  case 6: /* module: "option" id module  */
#line 494 "hexpr.y"
                                 { (yyval.module) = (yyvsp[0].module); (yyval.module)->setOption(*(yyvsp[-1].string), m((yylsp[-2]))); }
#line 2905 "hexpr.parse.C"
    break;

  case 7: /* module: "module" id "where" defs  */
#line 495 "hexpr.y"
                                 { (yyval.module) = new Module(*(yyvsp[-2].string), *(yyvsp[0].mdefs)); }
#line 2911 "hexpr.parse.C"
    break;

  case 8: /* module: defs  */
#line 496 "hexpr.y"
                                 { (yyval.module) = new Module(freshName(), *(yyvsp[0].mdefs)); }
#line 2917 "hexpr.parse.C"
    break;

  case 9: /* defs: %empty  */
#line 498 "hexpr.y"
                    { (yyval.mdefs) = autorelease(new ModuleDefs()); }
#line 2923 "hexpr.parse.C"
    break;

  case 10: /* defs: def  */
#line 499 "hexpr.y"
                    { (yyval.mdefs) = autorelease(new ModuleDefs()); (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2929 "hexpr.parse.C"
    break;

  case 11: /* defs: defs def  */
#line 500 "hexpr.y"
                    { (yyval.mdefs) = (yyvsp[-1].mdefs);                            (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2935 "hexpr.parse.C"
    break;

  case 12: /* def: importdef  */
#line 502 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2941 "hexpr.parse.C"
    break;

  case 13: /* def: tydef  */
#line 503 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2947 "hexpr.parse.C"
    break;

  case 14: /* def: vartybind  */
#line 504 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mvtydef); }
#line 2953 "hexpr.parse.C"
    break;

  case 15: /* def: classdef  */
#line 505 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2959 "hexpr.parse.C"
    break;

  case 16: /* def: instdef  */
#line 506 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2965 "hexpr.parse.C"
    break;

  case 17: /* def: pragmadef  */
#line 507 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2971 "hexpr.parse.C"
    break;

  case 18: /* def: id "=" l0expr  */
#line 509 "hexpr.y"
                   { (yyval.mdef) = new MVarDef(list(*(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2977 "hexpr.parse.C"
    break;

  case 19: /* def: id id "=" l0expr  */
#line 510 "hexpr.y"
                      { (yyval.mdef) = new MVarDef(list(*(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 2983 "hexpr.parse.C"
    break;

  case 20: /* def: id id id "=" l0expr  */
#line 511 "hexpr.y"
                         { (yyval.mdef) = new MVarDef(list(*(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 2989 "hexpr.parse.C"
    break;

  case 21: /* def: id id id id "=" l0expr  */
#line 512 "hexpr.y"
                            { (yyval.mdef) = new MVarDef(list(*(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 2995 "hexpr.parse.C"
    break;

  case 22: /* def: id id id id id "=" l0expr  */
#line 513 "hexpr.y"
                               { (yyval.mdef) = new MVarDef(list(*(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 3001 "hexpr.parse.C"
    break;

  case 23: /* def: id id id id id id "=" l0expr  */
#line 514 "hexpr.y"
                                  { (yyval.mdef) = new MVarDef(list(*(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3007 "hexpr.parse.C"
    break;

  case 24: /* def: id id id id id id id "=" l0expr  */
#line 515 "hexpr.y"
                                     { (yyval.mdef) = new MVarDef(list(*(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 3013 "hexpr.parse.C"
    break;

  case 25: /* def: id id id id id id id id "=" l0expr  */
#line 516 "hexpr.y"
                                        { (yyval.mdef) = new MVarDef(list(*(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-9]), (yylsp[0]))); }
#line 3019 "hexpr.parse.C"
    break;

  case 26: /* def: id id id id id id id id id "=" l0expr  */
#line 517 "hexpr.y"
                                           { (yyval.mdef) = new MVarDef(list(*(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-10]), (yylsp[0]))); }
#line 3025 "hexpr.parse.C"
    break;

  case 27: /* def: id id id id id id id id id id "=" l0expr  */
#line 518 "hexpr.y"
                                              { (yyval.mdef) = new MVarDef(list(*(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-11]), (yylsp[0]))); }
#line 3031 "hexpr.parse.C"
    break;

  case 28: /* def: id id id id id id id id id id id "=" l0expr  */
#line 519 "hexpr.y"
                                                 { (yyval.mdef) = new MVarDef(list(*(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-12]), (yylsp[0]))); }
#line 3037 "hexpr.parse.C"
    break;

  case 29: /* def: id id id id id id id id id id id id "=" l0expr  */
#line 520 "hexpr.y"
                                                    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-13]), (yylsp[0]))); }
#line 3043 "hexpr.parse.C"
    break;

  case 30: /* def: id id id id id id id id id id id id id "=" l0expr  */
#line 521 "hexpr.y"
                                                       { (yyval.mdef) = new MVarDef(list(*(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-14]), (yylsp[0]))); }
#line 3049 "hexpr.parse.C"
    break;

  case 31: /* def: id id id id id id id id id id id id id id "=" l0expr  */
#line 522 "hexpr.y"
                                                          { (yyval.mdef) = new MVarDef(list(*(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-15]), (yylsp[0]))); }
#line 3055 "hexpr.parse.C"
    break;

  case 32: /* def: id id id id id id id id id id id id id id id "=" l0expr  */
#line 523 "hexpr.y"
                                                             { (yyval.mdef) = new MVarDef(list(*(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-16]), (yylsp[0]))); }
#line 3061 "hexpr.parse.C"
    break;

  case 33: /* def: id id id id id id id id id id id id id id id id "=" l0expr  */
#line 524 "hexpr.y"
                                                                { (yyval.mdef) = new MVarDef(list(*(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-17]), (yylsp[0]))); }
#line 3067 "hexpr.parse.C"
    break;

  case 34: /* def: id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 525 "hexpr.y"
                                                                   { (yyval.mdef) = new MVarDef(list(*(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-18]), (yylsp[0]))); }
#line 3073 "hexpr.parse.C"
    break;

  case 35: /* def: id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 526 "hexpr.y"
                                                                      { (yyval.mdef) = new MVarDef(list(*(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-19]), (yylsp[0]))); }
#line 3079 "hexpr.parse.C"
    break;

  case 36: /* def: id id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 527 "hexpr.y"
                                                                         { (yyval.mdef) = new MVarDef(list(*(yyvsp[-20].string), *(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-20]), (yylsp[0]))); }
#line 3085 "hexpr.parse.C"
    break;

  case 37: /* def: id id id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 528 "hexpr.y"
                                                                            { (yyval.mdef) = new MVarDef(list(*(yyvsp[-21].string), *(yyvsp[-20].string), *(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-21]), (yylsp[0]))); }
#line 3091 "hexpr.parse.C"
    break;

  case 38: /* def: id id id id id id id id id id id id id id id id id id id id id "=" l0expr  */
#line 529 "hexpr.y"
                                                                               { (yyval.mdef) = new MVarDef(list(*(yyvsp[-22].string), *(yyvsp[-21].string), *(yyvsp[-20].string), *(yyvsp[-19].string), *(yyvsp[-18].string), *(yyvsp[-17].string), *(yyvsp[-16].string), *(yyvsp[-15].string), *(yyvsp[-14].string), *(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-22]), (yylsp[0]))); }
#line 3097 "hexpr.parse.C"
    break;

  case 39: /* def: l5expr  */
#line 532 "hexpr.y"
            { (yyval.mdef) = new MVarDef(list(freshName()), let(freshName(), ExprPtr((yyvsp[0].exp)), mktunit(m((yylsp[0]))), m((yylsp[0]))), m((yylsp[0]))); }
#line 3103 "hexpr.parse.C"
    break;

  case 40: /* importdef: "import" cppid  */
#line 535 "hexpr.y"
                          { (yyval.mdef) = new MImport(yyModulePath, *(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3109 "hexpr.parse.C"
    break;

  case 41: /* pragmadef: "{-#" pragmaty "#-}"  */
#line 538 "hexpr.y"
                                { (yyval.mdef) = (yyvsp[-1].mdef); }
#line 3115 "hexpr.parse.C"
    break;

  case 42: /* pragmaty: "UNSAFE" id  */
#line 539 "hexpr.y"
                      { (yyval.mdef) = new MUnsafePragmaDef(*(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3121 "hexpr.parse.C"
    break;

  case 43: /* pragmaty: "SAFE" id  */
#line 540 "hexpr.y"
                    { (yyval.mdef) = new MSafePragmaDef(*(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3127 "hexpr.parse.C"
    break;

  case 44: /* tydef: "type" nameseq "=" qtype  */
#line 543 "hexpr.y"
                                { (yyval.mdef) = new MTypeDef(MTypeDef::Transparent, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 3133 "hexpr.parse.C"
    break;

  case 45: /* tydef: "data" nameseq "=" qtype  */
#line 544 "hexpr.y"
                                { (yyval.mdef) = new MTypeDef(MTypeDef::Opaque, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 3139 "hexpr.parse.C"
    break;

  case 46: /* vartybind: name "::" qtype  */
#line 547 "hexpr.y"
                           { (yyval.mvtydef) = new MVarTypeDef(*(yyvsp[-2].string), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]), (yylsp[0]))); }
#line 3145 "hexpr.parse.C"
    break;

  case 47: /* vardef: names "=" l0expr  */
#line 549 "hexpr.y"
                         { (yyval.mvdef) = new MVarDef(*(yyvsp[-2].strings), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3151 "hexpr.parse.C"
    break;

  case 48: /* classdef: "class" cst "=>" id names  */
#line 552 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-3].tconstraints), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3157 "hexpr.parse.C"
    break;

  case 49: /* classdef: "class" cst "=>" id names "|" fundeps  */
#line 553 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-6]), (yylsp[0]))); wantIndent(false); }
#line 3163 "hexpr.parse.C"
    break;

  case 50: /* classdef: "class" cst "=>" id names "where" cmembers  */
#line 554 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 3169 "hexpr.parse.C"
    break;

  case 51: /* classdef: "class" cst "=>" id names "|" fundeps "where" cmembers  */
#line 555 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-7].tconstraints), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-8]), (yylsp[0])));            wantIndent(false); }
#line 3175 "hexpr.parse.C"
    break;

  case 52: /* classdef: "class" id names  */
#line 556 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 3181 "hexpr.parse.C"
    break;

  case 53: /* classdef: "class" id names "|" fundeps  */
#line 557 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3187 "hexpr.parse.C"
    break;

  case 54: /* classdef: "class" id names "where" cmembers  */
#line 558 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-4]), (yylsp[0])));            wantIndent(false); }
#line 3193 "hexpr.parse.C"
    break;

  case 55: /* classdef: "class" id names "|" fundeps "where" cmembers  */
#line 559 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 3199 "hexpr.parse.C"
    break;

  case 56: /* fundeps: fundep  */
#line 561 "hexpr.y"
                            { (yyval.fundeps) = autorelease(new CFunDepDefs()); (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 3205 "hexpr.parse.C"
    break;

  case 57: /* fundeps: fundeps "," fundep  */
#line 562 "hexpr.y"
                            { (yyval.fundeps) = (yyvsp[-2].fundeps);                             (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 3211 "hexpr.parse.C"
    break;

  case 58: /* fundep: idseq "->" idseq  */
#line 564 "hexpr.y"
                         { (yyval.fundep) = autorelease(new CFunDepDef(*(yyvsp[-2].strings), *(yyvsp[0].strings))); }
#line 3217 "hexpr.parse.C"
    break;

  case 59: /* cmembers: cmember  */
#line 566 "hexpr.y"
                           { (yyval.mvtydefs) = autorelease(new MVarTypeDefs()); (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 3223 "hexpr.parse.C"
    break;

  case 60: /* cmembers: cmembers cmember  */
#line 567 "hexpr.y"
                           { (yyval.mvtydefs) = (yyvsp[-1].mvtydefs);                              (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 3229 "hexpr.parse.C"
    break;

  case 61: /* cmember: "indent" vartybind  */
#line 569 "hexpr.y"
                            { (yyval.mvtydef) = (yyvsp[0].mvtydef); }
#line 3235 "hexpr.parse.C"
    break;

  case 62: /* instdef: "instance" id types  */
#line 572 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 3241 "hexpr.parse.C"
    break;

  case 63: /* instdef: "instance" cst "=>" id types  */
#line 573 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(*(yyvsp[-3].tconstraints),           *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3247 "hexpr.parse.C"
    break;

  case 64: /* instdef: "instance" id types "where" imembers  */
#line 574 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-4]), (yylsp[0])));        wantIndent(false); }
#line 3253 "hexpr.parse.C"
    break;

  case 65: /* instdef: "instance" cst "=>" id types "where" imembers  */
#line 575 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(*(yyvsp[-5].tconstraints),           *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-6]), (yylsp[0])));        wantIndent(false); }
#line 3259 "hexpr.parse.C"
    break;

  case 66: /* imembers: imember  */
#line 577 "hexpr.y"
                           { (yyval.mvdefs) = autorelease(new MVarDefs()); (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 3265 "hexpr.parse.C"
    break;

  case 67: /* imembers: imembers imember  */
#line 578 "hexpr.y"
                           { (yyval.mvdefs) = (yyvsp[-1].mvdefs);                          (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 3271 "hexpr.parse.C"
    break;

  case 68: /* imember: "indent" vardef  */
#line 580 "hexpr.y"
                         { (yyval.mvdef) = (yyvsp[0].mvdef); }
#line 3277 "hexpr.parse.C"
    break;

  case 69: /* names: nameseq  */
#line 583 "hexpr.y"
               { (yyval.strings) = (yyvsp[0].strings); }
#line 3283 "hexpr.parse.C"
    break;

  case 70: /* names: id opname id  */
#line 585 "hexpr.y"
                    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[-1].string)); (yyval.strings)->push_back(*(yyvsp[-2].string)); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3289 "hexpr.parse.C"
    break;

  case 71: /* nameseq: name  */
#line 587 "hexpr.y"
                      { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3295 "hexpr.parse.C"
    break;

  case 72: /* nameseq: nameseq name  */
#line 588 "hexpr.y"
                      { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3301 "hexpr.parse.C"
    break;

  case 73: /* name: id  */
#line 590 "hexpr.y"
         { (yyval.string) = (yyvsp[0].string); }
#line 3307 "hexpr.parse.C"
    break;

  case 74: /* name: "(" opname ")"  */
#line 592 "hexpr.y"
                     { (yyval.string) = (yyvsp[-1].string); }
#line 3313 "hexpr.parse.C"
    break;

  case 75: /* opname: "and"  */
#line 594 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("and")); }
#line 3319 "hexpr.parse.C"
    break;

  case 76: /* opname: "or"  */
#line 595 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("or")); }
#line 3325 "hexpr.parse.C"
    break;

  case 77: /* opname: "o"  */
#line 596 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("compose")); }
#line 3331 "hexpr.parse.C"
    break;

  case 78: /* opname: "."  */
#line 597 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("compose")); }
#line 3337 "hexpr.parse.C"
    break;

  case 79: /* opname: "~"  */
#line 598 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("~")); }
#line 3343 "hexpr.parse.C"
    break;

  case 80: /* opname: "=~"  */
#line 599 "hexpr.y"
               { (yyval.string) = autorelease(new std::string("=~")); }
#line 3349 "hexpr.parse.C"
    break;

  case 81: /* opname: "==="  */
#line 600 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("===")); }
#line 3355 "hexpr.parse.C"
    break;

  case 82: /* opname: "=="  */
#line 601 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("==")); }
#line 3361 "hexpr.parse.C"
    break;

  case 83: /* opname: "<"  */
#line 602 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("<")); }
#line 3367 "hexpr.parse.C"
    break;

  case 84: /* opname: "<="  */
#line 603 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("<=")); }
#line 3373 "hexpr.parse.C"
    break;

  case 85: /* opname: ">"  */
#line 604 "hexpr.y"
              { (yyval.string) = autorelease(new std::string(">")); }
#line 3379 "hexpr.parse.C"
    break;

  case 86: /* opname: ">="  */
#line 605 "hexpr.y"
              { (yyval.string) = autorelease(new std::string(">=")); }
#line 3385 "hexpr.parse.C"
    break;

  case 87: /* opname: "in"  */
#line 606 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("in")); }
#line 3391 "hexpr.parse.C"
    break;

  case 88: /* opname: "++"  */
#line 607 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("append")); }
#line 3397 "hexpr.parse.C"
    break;

  case 89: /* opname: "+"  */
#line 608 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("+")); }
#line 3403 "hexpr.parse.C"
    break;

  case 90: /* opname: "-"  */
#line 609 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("-")); }
#line 3409 "hexpr.parse.C"
    break;

  case 91: /* opname: "*"  */
#line 610 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("*")); }
#line 3415 "hexpr.parse.C"
    break;

  case 92: /* opname: "/"  */
#line 611 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("/")); }
#line 3421 "hexpr.parse.C"
    break;

  case 93: /* opname: "%"  */
#line 612 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("%")); }
#line 3427 "hexpr.parse.C"
    break;

  case 94: /* idseq: id  */
#line 614 "hexpr.y"
                { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3433 "hexpr.parse.C"
    break;

  case 95: /* idseq: idseq id  */
#line 615 "hexpr.y"
                { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3439 "hexpr.parse.C"
    break;

  case 96: /* types: l0mtype  */
#line 617 "hexpr.y"
                     { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3445 "hexpr.parse.C"
    break;

  case 97: /* types: types l0mtype  */
#line 618 "hexpr.y"
                     { (yyval.mtypes) = (yyvsp[-1].mtypes);                           (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3451 "hexpr.parse.C"
    break;

  case 98: /* l0expr: "\\" patterns "." l0expr  */
#line 621 "hexpr.y"
                                 { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3457 "hexpr.parse.C"
    break;

  case 99: /* l0expr: "fn" patterns "." l0expr  */
#line 622 "hexpr.y"
                                 { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3463 "hexpr.parse.C"
    break;

  case 100: /* l0expr: lhexpr "<-" lhexpr  */
#line 623 "hexpr.y"
                                 { (yyval.exp) = new Assign(ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3469 "hexpr.parse.C"
    break;

  case 101: /* l0expr: lhexpr  */
#line 624 "hexpr.y"
                                 { (yyval.exp) = (yyvsp[0].exp); }
#line 3475 "hexpr.parse.C"
    break;

  case 102: /* lhexpr: "!" l1expr  */
#line 626 "hexpr.y"
                                 { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), (yyvsp[0].exp), m((yylsp[-1]),(yylsp[0]))); }
#line 3481 "hexpr.parse.C"
    break;

  case 103: /* lhexpr: lhexpr "and" lhexpr  */
#line 627 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("and",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3487 "hexpr.parse.C"
    break;

  case 104: /* lhexpr: lhexpr "or" lhexpr  */
#line 628 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("or",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3493 "hexpr.parse.C"
    break;

  case 105: /* lhexpr: lhexpr "o" lhexpr  */
#line 629 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("compose",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3499 "hexpr.parse.C"
    break;

  case 106: /* lhexpr: l1expr "in" l1expr  */
#line 630 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("in",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3505 "hexpr.parse.C"
    break;

  case 107: /* lhexpr: l1expr  */
#line 631 "hexpr.y"
                                 { (yyval.exp) = (yyvsp[0].exp); }
#line 3511 "hexpr.parse.C"
    break;

  case 108: /* l1expr: "if" l0expr "then" l0expr "else" l0expr  */
#line 633 "hexpr.y"
                                                { (yyval.exp) = TAPP3(var("if",m((yylsp[-5]))), (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-5]), (yylsp[0]))); }
#line 3517 "hexpr.parse.C"
    break;

  case 109: /* l1expr: l2expr  */
#line 634 "hexpr.y"
                                                { (yyval.exp) = (yyvsp[0].exp); }
#line 3523 "hexpr.parse.C"
    break;

  case 110: /* l2expr: l2expr "~" l2expr  */
#line 636 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("~",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3529 "hexpr.parse.C"
    break;

  case 111: /* l2expr: l2expr "===" l2expr  */
#line 637 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("===",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3535 "hexpr.parse.C"
    break;

  case 112: /* l2expr: l2expr "==" l2expr  */
#line 638 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3541 "hexpr.parse.C"
    break;

  case 113: /* l2expr: l2expr "!=" l2expr  */
#line 639 "hexpr.y"
                            { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))), m((yylsp[-2]),(yylsp[0]))); }
#line 3547 "hexpr.parse.C"
    break;

  case 114: /* l2expr: l2expr "<" l2expr  */
#line 640 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("<",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3553 "hexpr.parse.C"
    break;

  case 115: /* l2expr: l2expr "<=" l2expr  */
#line 641 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("<=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3559 "hexpr.parse.C"
    break;

  case 116: /* l2expr: l2expr ">" l2expr  */
#line 642 "hexpr.y"
                            { (yyval.exp) = TAPP2(var(">",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3565 "hexpr.parse.C"
    break;

  case 117: /* l2expr: l2expr ">=" l2expr  */
#line 643 "hexpr.y"
                            { (yyval.exp) = TAPP2(var(">=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3571 "hexpr.parse.C"
    break;

  case 118: /* l2expr: l3expr  */
#line 644 "hexpr.y"
                            { (yyval.exp) = (yyvsp[0].exp); }
#line 3577 "hexpr.parse.C"
    break;

  case 119: /* l3expr: l3expr "+" l3expr  */
#line 646 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("+",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3583 "hexpr.parse.C"
    break;

  case 120: /* l3expr: l3expr "-" l3expr  */
#line 647 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("-",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3589 "hexpr.parse.C"
    break;

  case 121: /* l3expr: l3expr "++" l3expr  */
#line 648 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("append",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3595 "hexpr.parse.C"
    break;

  case 122: /* l3expr: "-" l3expr  */
#line 649 "hexpr.y"
                           { (yyval.exp) = TAPP1(var("neg",m((yylsp[-1]))), ExprPtr((yyvsp[0].exp)), m((yylsp[-1]),(yylsp[0]))); }
#line 3601 "hexpr.parse.C"
    break;

  case 123: /* l3expr: l4expr  */
#line 650 "hexpr.y"
                           { (yyval.exp) = (yyvsp[0].exp); }
#line 3607 "hexpr.parse.C"
    break;

  case 124: /* l4expr: l4expr "*" l4expr  */
#line 652 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("*", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3613 "hexpr.parse.C"
    break;

  case 125: /* l4expr: l4expr "/" l4expr  */
#line 653 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("/", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3619 "hexpr.parse.C"
    break;

  case 126: /* l4expr: l4expr "%" l4expr  */
#line 654 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("%", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3625 "hexpr.parse.C"
    break;

  case 127: /* l4expr: l5expr  */
#line 655 "hexpr.y"
                          { (yyval.exp) = (yyvsp[0].exp); }
#line 3631 "hexpr.parse.C"
    break;

  case 128: /* l5expr: l6expr  */
#line 657 "hexpr.y"
               { (yyval.exp) = (yyvsp[0].exp); }
#line 3637 "hexpr.parse.C"
    break;

  case 129: /* l5expr: "let" letbindings "in" l0expr  */
#line 660 "hexpr.y"
                                      { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-2].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3643 "hexpr.parse.C"
    break;

  case 130: /* l5expr: "let" letbindings ";" "in" l0expr  */
#line 661 "hexpr.y"
                                          { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]),(yylsp[0])))->clone(); }
#line 3649 "hexpr.parse.C"
    break;

  case 131: /* l5expr: "match" l6exprs "with" patternexps  */
#line 664 "hexpr.y"
                                           { (yyval.exp) = compileMatch(yyParseCC, *(yyvsp[-2].exps), normPatternRules(*(yyvsp[0].patternexps), m((yylsp[-3]),(yylsp[0]))), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3655 "hexpr.parse.C"
    break;

  case 132: /* l5expr: l6expr "matches" pattern  */
#line 667 "hexpr.y"
                                 { (yyval.exp) = compileMatchTest(yyParseCC, ExprPtr((yyvsp[-2].exp)), PatternPtr((yyvsp[0].pattern)), m((yylsp[-2]),(yylsp[0])))->clone(); }
#line 3661 "hexpr.parse.C"
    break;

  case 133: /* l5expr: "parse" "{" prules "}"  */
#line 670 "hexpr.y"
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
#line 682 "hexpr.y"
                                                { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-1].letbindings), ExprPtr(new Unit(m((yylsp[-3]),(yylsp[0])))), m((yylsp[-3]),(yylsp[0]))); }
#line 3682 "hexpr.parse.C"
    break;

  case 135: /* l5expr: "do" "{" dobindings "return" l0expr "}"  */
#line 683 "hexpr.y"
                                                { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[-1].exp)), m((yylsp[-5]),(yylsp[0]))); }
#line 3688 "hexpr.parse.C"
    break;

  case 136: /* l5expr: l6expr "::" qtype  */
#line 686 "hexpr.y"
                                { (yyval.exp) = new Assump(ExprPtr((yyvsp[-2].exp)), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]),(yylsp[0]))); }
#line 3694 "hexpr.parse.C"
    break;

  case 137: /* letbindings: letbindings ";" letbinding  */
#line 688 "hexpr.y"
                                        { (yyvsp[-2].letbindings)->push_back(*(yyvsp[0].letbinding)); (yyval.letbindings) = (yyvsp[-2].letbindings); }
#line 3700 "hexpr.parse.C"
    break;

  case 138: /* letbindings: letbinding  */
#line 689 "hexpr.y"
                                        { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3706 "hexpr.parse.C"
    break;

  case 139: /* letbinding: irrefutablep "=" l1expr  */
#line 691 "hexpr.y"
                                    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-2].pattern)), ExprPtr((yyvsp[0].exp)))); }
#line 3712 "hexpr.parse.C"
    break;

  case 140: /* dobindings: dobindings dobinding  */
#line 693 "hexpr.y"
                                 { (yyval.letbindings) = (yyvsp[-1].letbindings); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3718 "hexpr.parse.C"
    break;

  case 141: /* dobindings: dobinding  */
#line 694 "hexpr.y"
                                 { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3724 "hexpr.parse.C"
    break;

  case 142: /* dobinding: irrefutablep "=" l0expr ";"  */
#line 696 "hexpr.y"
                                       { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)))); }
#line 3730 "hexpr.parse.C"
    break;

  case 143: /* dobinding: l0expr ";"  */
#line 697 "hexpr.y"
                                       { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr(new MatchAny("_",m((yylsp[-1])))), ExprPtr((yyvsp[-1].exp)))); }
#line 3736 "hexpr.parse.C"
    break;

  case 144: /* cselconds: cselconds "," lhexpr  */
#line 699 "hexpr.y"
                                { (yyval.exps) = (yyvsp[-2].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3742 "hexpr.parse.C"
    break;

  case 145: /* cselconds: lhexpr  */
#line 700 "hexpr.y"
                                { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3748 "hexpr.parse.C"
    break;

  case 146: /* cselection: pattern "<-" l0expr "," cselconds  */
#line 702 "hexpr.y"
                                              { (yyval.cselection) = new CSelection(); (yyval.cselection)->pat = PatternPtr((yyvsp[-4].pattern)); (yyval.cselection)->seq = ExprPtr((yyvsp[-2].exp)); (yyval.cselection)->conds = *(yyvsp[0].exps); }
#line 3754 "hexpr.parse.C"
    break;

  case 147: /* cselection: pattern "<-" l0expr  */
#line 703 "hexpr.y"
                                              { (yyval.cselection) = new CSelection(); (yyval.cselection)->pat = PatternPtr((yyvsp[-2].pattern)); (yyval.cselection)->seq = ExprPtr((yyvsp[0].exp)); }
#line 3760 "hexpr.parse.C"
    break;

  case 148: /* cselections: cselections "|" cselection  */
#line 705 "hexpr.y"
                                        { (yyval.cselections) = (yyvsp[-2].cselections); (yyval.cselections)->push_back(CSelectionPtr((yyvsp[0].cselection))); }
#line 3766 "hexpr.parse.C"
    break;

  case 149: /* cselections: cselection  */
#line 706 "hexpr.y"
                                        { (yyval.cselections) = autorelease(new CSelections()); (yyval.cselections)->push_back(CSelectionPtr((yyvsp[0].cselection))); }
#line 3772 "hexpr.parse.C"
    break;

  case 150: /* l6expr: l6expr "(" cargs ")"  */
#line 709 "hexpr.y"
                                { (yyval.exp) = new App(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].exps), m((yylsp[-3]), (yylsp[0]))); }
#line 3778 "hexpr.parse.C"
    break;

  case 151: /* l6expr: id  */
#line 710 "hexpr.y"
                                { (yyval.exp) = varCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3784 "hexpr.parse.C"
    break;

  case 152: /* l6expr: "[" l0expr ".." l0expr "]"  */
#line 713 "hexpr.y"
                                                          { (yyval.exp) = new App(var("range", m((yylsp[-2]))), list(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]), (yylsp[0]))); }
#line 3790 "hexpr.parse.C"
    break;

  case 153: /* l6expr: "[" l0expr ".." "]"  */
#line 714 "hexpr.y"
                                                          { (yyval.exp) = new App(var("iterateS", m((yylsp[-1]))), list(ExprPtr((yyvsp[-2].exp)), fn(str::strings(".x"), fncall(var("+", m((yylsp[-1]))), list(var(".x", m((yylsp[-1]))), ExprPtr(new Int(1, m((yylsp[-1]))))), m((yylsp[-1]))), m((yylsp[-1])))), m((yylsp[-3]), (yylsp[0]))); }
#line 3796 "hexpr.parse.C"
    break;

  case 154: /* l6expr: "[" l0expr "|" cselections "]"  */
#line 715 "hexpr.y"
                                                          { (yyval.exp) = desugarComprehension(yyParseCC, ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].cselections), m((yylsp[-4]), (yylsp[0]))); }
#line 3802 "hexpr.parse.C"
    break;

  case 155: /* l6expr: "[" cargs "]"  */
#line 716 "hexpr.y"
                                                          { (yyval.exp) = new MkArray(*(yyvsp[-1].exps), m((yylsp[-2]), (yylsp[0]))); }
#line 3808 "hexpr.parse.C"
    break;

  case 156: /* l6expr: l6expr "[" "timeV" "]"  */
#line 717 "hexpr.y"
                                                          { (yyval.exp) = maybeArraySliceWithTime(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].string), m((yylsp[-3]), (yylsp[0]))); }
#line 3814 "hexpr.parse.C"
    break;

  case 157: /* l6expr: l6expr "[" l0expr "]"  */
#line 718 "hexpr.y"
                                                          { (yyval.exp) = mkAIndex(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3820 "hexpr.parse.C"
    break;

  case 158: /* l6expr: l6expr "[" l0expr ":" l0expr "]"  */
#line 719 "hexpr.y"
                                                          { (yyval.exp) = new App(var("slice", m((yylsp[-2]))), list(ExprPtr((yyvsp[-5].exp)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-5]), (yylsp[0]))); }
#line 3826 "hexpr.parse.C"
    break;

  case 159: /* l6expr: l6expr "[" l0expr ":" "]"  */
#line 720 "hexpr.y"
                                                          { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-1]))), list(var(vn,m((yylsp[-4]))), ExprPtr((yyvsp[-2].exp)), fncall(var("size",m((yylsp[-1]))), list(var(vn,m((yylsp[-4])))),m((yylsp[-4])))),m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3832 "hexpr.parse.C"
    break;

  case 160: /* l6expr: l6expr "[" ":" l0expr "]"  */
#line 721 "hexpr.y"
                                                          { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-2]))), list(var(vn,m((yylsp[-4]))), fncall(var("size",m((yylsp[-2]))), list(var(vn,m((yylsp[-2])))),m((yylsp[-4]))), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3838 "hexpr.parse.C"
    break;

  case 161: /* l6expr: "|" id "=" l0expr "|"  */
#line 724 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(*(yyvsp[-3].string), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3844 "hexpr.parse.C"
    break;

  case 162: /* l6expr: "|" "intV" "=" l0expr "|"  */
#line 725 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(".f" + str::from((yyvsp[-3].intv)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3850 "hexpr.parse.C"
    break;

  case 163: /* l6expr: "|" id "|"  */
#line 726 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(*(yyvsp[-1].string), ExprPtr(new Unit(m((yylsp[-1])))), m((yylsp[-2]), (yylsp[0]))); }
#line 3856 "hexpr.parse.C"
    break;

  case 164: /* l6expr: "case" l0expr "of" "|" varfields "|"  */
#line 727 "hexpr.y"
                                                              { (yyval.exp) = new Case(ExprPtr((yyvsp[-4].exp)), *(yyvsp[-1].vfields), m((yylsp[-5]), (yylsp[0]))); }
#line 3862 "hexpr.parse.C"
    break;

  case 165: /* l6expr: "case" l0expr "of" "|" varfields "|" "default" l0expr  */
#line 728 "hexpr.y"
                                                              { (yyval.exp) = new Case(ExprPtr((yyvsp[-6].exp)), *(yyvsp[-3].vfields), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3868 "hexpr.parse.C"
    break;

  case 166: /* l6expr: "{" recfields "}"  */
#line 731 "hexpr.y"
                              { if ((yyvsp[-1].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-1].rfields), m((yylsp[-2]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-2]), (yylsp[0]))); } }
#line 3874 "hexpr.parse.C"
    break;

  case 167: /* l6expr: "{" recfields "," "}"  */
#line 732 "hexpr.y"
                              { if ((yyvsp[-2].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-2].rfields), m((yylsp[-3]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-3]), (yylsp[0]))); } }
#line 3880 "hexpr.parse.C"
    break;

  case 168: /* l6expr: l6expr recfieldpath  */
#line 733 "hexpr.y"
                              { (yyval.exp) = makeProjSeq((yyvsp[-1].exp), *(yyvsp[0].strings), m((yylsp[-1]), (yylsp[0]))); }
#line 3886 "hexpr.parse.C"
    break;

  case 169: /* l6expr: recfieldpath  */
#line 736 "hexpr.y"
                     { (yyval.exp) = new Fn(str::strings("x"), proj(var("x", m((yylsp[0]))), *(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3892 "hexpr.parse.C"
    break;

  case 170: /* l6expr: "regexV"  */
#line 739 "hexpr.y"
                 { (yyval.exp) = compileRegexFn(yyParseCC, std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0])))->clone(); }
#line 3898 "hexpr.parse.C"
    break;

  case 171: /* l6expr: "pack" l6expr  */
#line 742 "hexpr.y"
                                           { (yyval.exp) = new Pack(ExprPtr((yyvsp[0].exp)), m((yylsp[-1]), (yylsp[0]))); }
#line 3904 "hexpr.parse.C"
    break;

  case 172: /* l6expr: "unpack" id "=" l6expr "in" l6expr  */
#line 743 "hexpr.y"
                                           { (yyval.exp) = new Unpack(*(yyvsp[-4].string), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3910 "hexpr.parse.C"
    break;

  case 173: /* l6expr: "boolV"  */
#line 746 "hexpr.y"
                    { (yyval.exp) = new Bool((yyvsp[0].boolv), m((yylsp[0]))); }
#line 3916 "hexpr.parse.C"
    break;

  case 174: /* l6expr: "charV"  */
#line 747 "hexpr.y"
                    { (yyval.exp) = new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3922 "hexpr.parse.C"
    break;

  case 175: /* l6expr: "byteV"  */
#line 748 "hexpr.y"
                    { (yyval.exp) = new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3928 "hexpr.parse.C"
    break;

  case 176: /* l6expr: "bytesV"  */
#line 749 "hexpr.y"
                    { (yyval.exp) = mkarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3934 "hexpr.parse.C"
    break;

  case 177: /* l6expr: "shortV"  */
#line 750 "hexpr.y"
                    { (yyval.exp) = new Short((yyvsp[0].shortv), m((yylsp[0]))); }
#line 3940 "hexpr.parse.C"
    break;

  case 178: /* l6expr: "intV"  */
#line 751 "hexpr.y"
                    { (yyval.exp) = new Int((yyvsp[0].intv), m((yylsp[0]))); }
#line 3946 "hexpr.parse.C"
    break;

  case 179: /* l6expr: "longV"  */
#line 752 "hexpr.y"
                    { (yyval.exp) = new Long((yyvsp[0].longv), m((yylsp[0]))); }
#line 3952 "hexpr.parse.C"
    break;

  case 180: /* l6expr: "int128V"  */
#line 753 "hexpr.y"
                    { (yyval.exp) = new Int128((yyvsp[0].int128v), m((yylsp[0]))); }
#line 3958 "hexpr.parse.C"
    break;

  case 181: /* l6expr: "floatV"  */
#line 754 "hexpr.y"
                    { (yyval.exp) = new Float((yyvsp[0].floatv), m((yylsp[0]))); }
#line 3964 "hexpr.parse.C"
    break;

  case 182: /* l6expr: "doubleV"  */
#line 755 "hexpr.y"
                    { (yyval.exp) = new Double((yyvsp[0].doublev), m((yylsp[0]))); }
#line 3970 "hexpr.parse.C"
    break;

  case 183: /* l6expr: "stringV"  */
#line 756 "hexpr.y"
                    { (yyval.exp) = mkarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3976 "hexpr.parse.C"
    break;

  case 184: /* l6expr: tsseq  */
#line 757 "hexpr.y"
                    { (yyval.exp) = mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0])))->clone(); }
#line 3982 "hexpr.parse.C"
    break;

  case 185: /* l6expr: "timeV"  */
#line 758 "hexpr.y"
                    { (yyval.exp) = mkTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3988 "hexpr.parse.C"
    break;

  case 186: /* l6expr: "dateTimeV"  */
#line 759 "hexpr.y"
                    { (yyval.exp) = mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3994 "hexpr.parse.C"
    break;

  case 187: /* l6expr: "(" cargs ")"  */
#line 762 "hexpr.y"
                      { (yyval.exp) = pickNestedExp((yyvsp[-1].exps), m((yylsp[-2]),(yylsp[0]))); }
#line 4000 "hexpr.parse.C"
    break;

  case 188: /* l6expr: "(" "++" ")"  */
#line 765 "hexpr.y"
                      { (yyval.exp) = new Var("append", m((yylsp[-1]))); }
#line 4006 "hexpr.parse.C"
    break;

  case 189: /* l6expr: "(" "+" ")"  */
#line 766 "hexpr.y"
                      { (yyval.exp) = new Var("+",      m((yylsp[-1]))); }
#line 4012 "hexpr.parse.C"
    break;

  case 190: /* l6expr: "(" "-" ")"  */
#line 767 "hexpr.y"
                      { (yyval.exp) = new Var("-",      m((yylsp[-1]))); }
#line 4018 "hexpr.parse.C"
    break;

  case 191: /* l6expr: "(" "*" ")"  */
#line 768 "hexpr.y"
                      { (yyval.exp) = new Var("*",      m((yylsp[-1]))); }
#line 4024 "hexpr.parse.C"
    break;

  case 192: /* l6expr: "(" "/" ")"  */
#line 769 "hexpr.y"
                      { (yyval.exp) = new Var("/",      m((yylsp[-1]))); }
#line 4030 "hexpr.parse.C"
    break;

  case 193: /* l6expr: "(" "%" ")"  */
#line 770 "hexpr.y"
                      { (yyval.exp) = new Var("%",      m((yylsp[-1]))); }
#line 4036 "hexpr.parse.C"
    break;

  case 194: /* l6expr: "(" "~" ")"  */
#line 771 "hexpr.y"
                      { (yyval.exp) = new Var("~",      m((yylsp[-1]))); }
#line 4042 "hexpr.parse.C"
    break;

  case 195: /* l6expr: "(" "===" ")"  */
#line 772 "hexpr.y"
                      { (yyval.exp) = new Var("===",    m((yylsp[-1]))); }
#line 4048 "hexpr.parse.C"
    break;

  case 196: /* l6expr: "(" "==" ")"  */
#line 773 "hexpr.y"
                      { (yyval.exp) = new Var("==",     m((yylsp[-1]))); }
#line 4054 "hexpr.parse.C"
    break;

  case 197: /* l6expr: "(" "!=" ")"  */
#line 774 "hexpr.y"
                      { (yyval.exp) = new Var("!=",     m((yylsp[-1]))); }
#line 4060 "hexpr.parse.C"
    break;

  case 198: /* l6expr: "(" "<" ")"  */
#line 775 "hexpr.y"
                      { (yyval.exp) = new Var("<",      m((yylsp[-1]))); }
#line 4066 "hexpr.parse.C"
    break;

  case 199: /* l6expr: "(" ">" ")"  */
#line 776 "hexpr.y"
                      { (yyval.exp) = new Var(">",      m((yylsp[-1]))); }
#line 4072 "hexpr.parse.C"
    break;

  case 200: /* l6expr: "(" ">=" ")"  */
#line 777 "hexpr.y"
                      { (yyval.exp) = new Var(">=",     m((yylsp[-1]))); }
#line 4078 "hexpr.parse.C"
    break;

  case 201: /* l6expr: "(" "<=" ")"  */
#line 778 "hexpr.y"
                      { (yyval.exp) = new Var("<=",     m((yylsp[-1]))); }
#line 4084 "hexpr.parse.C"
    break;

  case 202: /* l6expr: "(" "and" ")"  */
#line 779 "hexpr.y"
                      { (yyval.exp) = new Var("and",    m((yylsp[-1]))); }
#line 4090 "hexpr.parse.C"
    break;

  case 203: /* l6expr: "(" "or" ")"  */
#line 780 "hexpr.y"
                      { (yyval.exp) = new Var("or",     m((yylsp[-1]))); }
#line 4096 "hexpr.parse.C"
    break;

  case 204: /* l6expr: "(" "in" ")"  */
#line 781 "hexpr.y"
                      { (yyval.exp) = new Var("in",     m((yylsp[-1]))); }
#line 4102 "hexpr.parse.C"
    break;

  case 205: /* l6expr: "(" "!" ")"  */
#line 782 "hexpr.y"
                      { (yyval.exp) = new Var("not",    m((yylsp[-1]))); }
#line 4108 "hexpr.parse.C"
    break;

  case 206: /* l6expr: "`" l0expr "`"  */
#line 785 "hexpr.y"
                       { (yyval.exp) = new Assump(fncall(var("unsafeCast", m((yylsp[-1]))), list(mktunit(m((yylsp[-1])))), m((yylsp[-1]))), qualtype(tapp(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp)))))), m((yylsp[-1]))); }
#line 4114 "hexpr.parse.C"
    break;

  case 207: /* prules: prules prule  */
#line 787 "hexpr.y"
                     { (yyval.prules) = (yyvsp[-1].prules); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 4120 "hexpr.parse.C"
    break;

  case 208: /* prules: prule  */
#line 788 "hexpr.y"
                     { (yyval.prules) = autorelease(new Grammar()); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 4126 "hexpr.parse.C"
    break;

  case 209: /* prule: id ":=" prdefs  */
#line 790 "hexpr.y"
                      { (yyval.prule) = autorelease(new Grammar::value_type(*(yyvsp[-2].string), *(yyvsp[0].prdefs))); }
#line 4132 "hexpr.parse.C"
    break;

  case 210: /* prdefs: prdefs "|" prdef  */
#line 792 "hexpr.y"
                         { (yyval.prdefs) = (yyvsp[-2].prdefs); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 4138 "hexpr.parse.C"
    break;

  case 211: /* prdefs: prdef  */
#line 793 "hexpr.y"
                         { (yyval.prdefs) = autorelease(new GrammarRules()); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 4144 "hexpr.parse.C"
    break;

  case 212: /* prdef: pbelems "{" l0expr "}"  */
#line 795 "hexpr.y"
                              { (yyval.prdef) = autorelease(new GrammarRule(*(yyvsp[-3].pbelems), ExprPtr((yyvsp[-1].exp)))); }
#line 4150 "hexpr.parse.C"
    break;

  case 213: /* pbelems: pbelems pbelem  */
#line 797 "hexpr.y"
                        { (yyval.pbelems) = (yyvsp[-1].pbelems); (yyval.pbelems)->push_back(*(yyvsp[0].pbelem)); }
#line 4156 "hexpr.parse.C"
    break;

  case 214: /* pbelems: %empty  */
#line 798 "hexpr.y"
                        { (yyval.pbelems) = autorelease(new BoundGrammarValues()); }
#line 4162 "hexpr.parse.C"
    break;

  case 215: /* pbelem: id ":" pvalue  */
#line 800 "hexpr.y"
                      { (yyval.pbelem) = autorelease(new BoundGrammarValue(*(yyvsp[-2].string), GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 4168 "hexpr.parse.C"
    break;

  case 216: /* pbelem: pvalue  */
#line 801 "hexpr.y"
                      { (yyval.pbelem) = autorelease(new BoundGrammarValue("_", GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 4174 "hexpr.parse.C"
    break;

  case 217: /* pvalue: id  */
#line 803 "hexpr.y"
                      { (yyval.pvalue) = new GSymRef(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4180 "hexpr.parse.C"
    break;

  case 218: /* pvalue: "stringV"  */
#line 804 "hexpr.y"
                      { (yyval.pvalue) = new GStr(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4186 "hexpr.parse.C"
    break;

  case 219: /* pvalue: "charV"  */
#line 805 "hexpr.y"
                      { (yyval.pvalue) = new GStr(std::string(1, str::readCharDef(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4192 "hexpr.parse.C"
    break;

  case 220: /* tsseq: "timespanV"  */
#line 807 "hexpr.y"
                         { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4198 "hexpr.parse.C"
    break;

  case 221: /* tsseq: tsseq "timespanV"  */
#line 808 "hexpr.y"
                         { (yyval.strings) = (yyvsp[-1].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4204 "hexpr.parse.C"
    break;

  case 222: /* l6exprs: l6exprs l6expr  */
#line 810 "hexpr.y"
                        { (yyval.exps) = (yyvsp[-1].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4210 "hexpr.parse.C"
    break;

  case 223: /* l6exprs: l6expr  */
#line 811 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4216 "hexpr.parse.C"
    break;

  case 224: /* patternexps: patternexps patternexp  */
#line 813 "hexpr.y"
                                    { (yyval.patternexps) = (yyvsp[-1].patternexps); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 4222 "hexpr.parse.C"
    break;

  case 225: /* patternexps: patternexp  */
#line 814 "hexpr.y"
                                    { (yyval.patternexps) = autorelease(new PatternRows()); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 4228 "hexpr.parse.C"
    break;

  case 226: /* patternexp: "|" patterns "->" l0expr  */
#line 816 "hexpr.y"
                                                    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)))); }
#line 4234 "hexpr.parse.C"
    break;

  case 227: /* patternexp: "|" patterns "where" l0expr "->" l0expr  */
#line 817 "hexpr.y"
                                                    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-4].patterns), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)))); }
#line 4240 "hexpr.parse.C"
    break;

  case 228: /* patterns: patterns pattern  */
#line 820 "hexpr.y"
                           { (yyval.patterns) = (yyvsp[-1].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4246 "hexpr.parse.C"
    break;

  case 229: /* patterns: pattern  */
#line 821 "hexpr.y"
                           { (yyval.patterns) = autorelease(new Patterns()); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4252 "hexpr.parse.C"
    break;

  case 230: /* refutablep: "boolV"  */
#line 823 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Bool((yyvsp[0].boolv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4258 "hexpr.parse.C"
    break;

  case 231: /* refutablep: "charV"  */
#line 824 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 4264 "hexpr.parse.C"
    break;

  case 232: /* refutablep: "byteV"  */
#line 825 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 4270 "hexpr.parse.C"
    break;

  case 233: /* refutablep: "shortV"  */
#line 826 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Short((yyvsp[0].shortv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4276 "hexpr.parse.C"
    break;

  case 234: /* refutablep: "intV"  */
#line 827 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int((yyvsp[0].intv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4282 "hexpr.parse.C"
    break;

  case 235: /* refutablep: "longV"  */
#line 828 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Long((yyvsp[0].longv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4288 "hexpr.parse.C"
    break;

  case 236: /* refutablep: "int128V"  */
#line 829 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int128((yyvsp[0].int128v), m((yylsp[0])))), m((yylsp[0]))); }
#line 4294 "hexpr.parse.C"
    break;

  case 237: /* refutablep: "doubleV"  */
#line 830 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Double((yyvsp[0].doublev), m((yylsp[0])))), m((yylsp[0]))); }
#line 4300 "hexpr.parse.C"
    break;

  case 238: /* refutablep: "bytesV"  */
#line 831 "hexpr.y"
                                       { (yyval.pattern) = mkpatarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 4306 "hexpr.parse.C"
    break;

  case 239: /* refutablep: "stringV"  */
#line 832 "hexpr.y"
                                       { (yyval.pattern) = mkpatarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4312 "hexpr.parse.C"
    break;

  case 240: /* refutablep: tsseq  */
#line 833 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkTimespanPrim(*(yyvsp[0].strings), m((yylsp[0]))), mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 4318 "hexpr.parse.C"
    break;

  case 241: /* refutablep: "timeV"  */
#line 834 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4324 "hexpr.parse.C"
    break;

  case 242: /* refutablep: "dateTimeV"  */
#line 835 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkDateTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4330 "hexpr.parse.C"
    break;

  case 243: /* refutablep: "regexV"  */
#line 836 "hexpr.y"
                                       { (yyval.pattern) = new MatchRegex(std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0]))); }
#line 4336 "hexpr.parse.C"
    break;

  case 244: /* refutablep: "[" patternseq "]"  */
#line 837 "hexpr.y"
                                       { (yyval.pattern) = new MatchArray(*(yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4342 "hexpr.parse.C"
    break;

  case 245: /* refutablep: "[" patternseq "," "]"  */
#line 838 "hexpr.y"
                                       { (yyval.pattern) = new MatchArray(*(yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4348 "hexpr.parse.C"
    break;

  case 246: /* refutablep: "|" id "|"  */
#line 839 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(*(yyvsp[-1].string), PatternPtr(new MatchLiteral(PrimitivePtr(new Unit(m((yylsp[-1])))), m((yylsp[-1])))), m((yylsp[-2]),(yylsp[0]))); }
#line 4354 "hexpr.parse.C"
    break;

  case 247: /* refutablep: "|" id "=" pattern "|"  */
#line 840 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(*(yyvsp[-3].string), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4360 "hexpr.parse.C"
    break;

  case 248: /* refutablep: "|" "intV" "=" pattern "|"  */
#line 841 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(".f" + str::from((yyvsp[-3].intv)), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4366 "hexpr.parse.C"
    break;

  case 249: /* refutablep: "(" patternseq ")"  */
#line 842 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4372 "hexpr.parse.C"
    break;

  case 250: /* refutablep: "(" patternseq "," ")"  */
#line 843 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4378 "hexpr.parse.C"
    break;

  case 251: /* refutablep: "{" recpatfields "}"  */
#line 844 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4384 "hexpr.parse.C"
    break;

  case 252: /* refutablep: "{" recpatfields "," "}"  */
#line 845 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4390 "hexpr.parse.C"
    break;

  case 253: /* refutablep: id  */
#line 846 "hexpr.y"
                                       { (yyval.pattern) = patVarCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4396 "hexpr.parse.C"
    break;

  case 254: /* irrefutablep: id  */
#line 848 "hexpr.y"
                                       { (yyval.pattern) = new MatchAny(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4402 "hexpr.parse.C"
    break;

  case 255: /* irrefutablep: "(" patternseq ")"  */
#line 849 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4408 "hexpr.parse.C"
    break;

  case 256: /* irrefutablep: "(" patternseq "," ")"  */
#line 850 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4414 "hexpr.parse.C"
    break;

  case 257: /* irrefutablep: "{" recpatfields "}"  */
#line 851 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4420 "hexpr.parse.C"
    break;

  case 258: /* irrefutablep: "{" recpatfields "," "}"  */
#line 852 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4426 "hexpr.parse.C"
    break;

  case 259: /* pattern: refutablep  */
#line 854 "hexpr.y"
                    { (yyval.pattern) = (yyvsp[0].pattern); }
#line 4432 "hexpr.parse.C"
    break;

  case 260: /* patternseq: patternseqn  */
#line 856 "hexpr.y"
                          { (yyval.patterns) = (yyvsp[0].patterns); }
#line 4438 "hexpr.parse.C"
    break;

  case 261: /* patternseq: %empty  */
#line 857 "hexpr.y"
                          { (yyval.patterns) = new Patterns(); }
#line 4444 "hexpr.parse.C"
    break;

  case 262: /* patternseqn: patternseqn "," pattern  */
#line 859 "hexpr.y"
                                     { (yyval.patterns) = (yyvsp[-2].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4450 "hexpr.parse.C"
    break;

  case 263: /* patternseqn: pattern  */
#line 860 "hexpr.y"
                                     { (yyval.patterns) = new Patterns(); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4456 "hexpr.parse.C"
    break;

  case 264: /* recpatfields: recpatfields "," recpatfield  */
#line 862 "hexpr.y"
                                           { (yyval.recpatfields) = (yyvsp[-2].recpatfields); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4462 "hexpr.parse.C"
    break;

  case 265: /* recpatfields: recpatfield  */
#line 863 "hexpr.y"
                                           { (yyval.recpatfields) = new MatchRecord::Fields(); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4468 "hexpr.parse.C"
    break;

  case 266: /* recpatfield: id "=" pattern  */
#line 865 "hexpr.y"
                            { (yyval.recpatfield) = new MatchRecord::Field(*(yyvsp[-2].string), PatternPtr((yyvsp[0].pattern))); }
#line 4474 "hexpr.parse.C"
    break;

  case 267: /* recfields: %empty  */
#line 867 "hexpr.y"
                                                 { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); }
#line 4480 "hexpr.parse.C"
    break;

  case 268: /* recfields: recfieldname "=" l0expr  */
#line 868 "hexpr.y"
                                                 { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4486 "hexpr.parse.C"
    break;

  case 269: /* recfields: recfields "," recfieldname "=" l0expr  */
#line 869 "hexpr.y"
                                                 { (yyval.rfields) = (yyvsp[-4].rfields);                                     (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4492 "hexpr.parse.C"
    break;

  case 270: /* recfieldname: id  */
#line 871 "hexpr.y"
                         { (yyval.string) = (yyvsp[0].string); }
#line 4498 "hexpr.parse.C"
    break;

  case 271: /* recfieldname: "data"  */
#line 872 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("data")); }
#line 4504 "hexpr.parse.C"
    break;

  case 272: /* recfieldname: "type"  */
#line 873 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("type")); }
#line 4510 "hexpr.parse.C"
    break;

  case 273: /* recfieldname: "where"  */
#line 874 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("where")); }
#line 4516 "hexpr.parse.C"
    break;

  case 274: /* recfieldname: "class"  */
#line 875 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("class")); wantIndent(false); }
#line 4522 "hexpr.parse.C"
    break;

  case 275: /* recfieldname: "instance"  */
#line 876 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("instance")); wantIndent(false); }
#line 4528 "hexpr.parse.C"
    break;

  case 276: /* recfieldname: "exists"  */
#line 877 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("exists")); }
#line 4534 "hexpr.parse.C"
    break;

  case 277: /* recfieldname: "import"  */
#line 878 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("import")); }
#line 4540 "hexpr.parse.C"
    break;

  case 278: /* recfieldname: "module"  */
#line 879 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("module")); }
#line 4546 "hexpr.parse.C"
    break;

  case 279: /* recfieldname: "parse"  */
#line 880 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("parse")); }
#line 4552 "hexpr.parse.C"
    break;

  case 280: /* recfieldname: "do"  */
#line 881 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("do")); }
#line 4558 "hexpr.parse.C"
    break;

  case 281: /* recfieldname: "return"  */
#line 882 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("return")); }
#line 4564 "hexpr.parse.C"
    break;

  case 282: /* recfieldname: "fn"  */
#line 883 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("fn")); }
#line 4570 "hexpr.parse.C"
    break;

  case 283: /* recfieldname: "intV"  */
#line 884 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string(".f" + str::from((yyvsp[0].intv)))); }
#line 4576 "hexpr.parse.C"
    break;

  case 284: /* recfieldname: "stringV"  */
#line 885 "hexpr.y"
                         { std::string stringField = str::unescape(str::trimq(*(yyvsp[0].string)));
                           if (stringField.size() > 0 && stringField[0] == '.' ) {
                             throw annotated_error(m((yylsp[0])), "Cannot define record string label with leading '.'");
                           }
                           (yyval.string) = autorelease(new std::string(str::unescape(str::trimq(*(yyvsp[0].string))))); }
#line 4586 "hexpr.parse.C"
    break;

  case 285: /* recfieldpath: recfieldpath "." recfieldname  */
#line 891 "hexpr.y"
                                            { (yyval.strings) = (yyvsp[-2].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4592 "hexpr.parse.C"
    break;

  case 286: /* recfieldpath: recfieldpath "tupSection"  */
#line 892 "hexpr.y"
                                            { (yyval.strings) = (yyvsp[-1].strings); str::seq x = tupSectionFields(*(yyvsp[0].string)); (yyval.strings)->insert((yyval.strings)->end(), x.begin(), x.end()); }
#line 4598 "hexpr.parse.C"
    break;

  case 287: /* recfieldpath: "." recfieldname  */
#line 893 "hexpr.y"
                                            { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4604 "hexpr.parse.C"
    break;

  case 288: /* recfieldpath: "tupSection"  */
#line 894 "hexpr.y"
                                            { (yyval.strings) = autorelease(new str::seq()); *(yyval.strings) = tupSectionFields(*(yyvsp[0].string)); }
#line 4610 "hexpr.parse.C"
    break;

  case 289: /* varfields: varbind  */
#line 896 "hexpr.y"
                                 { (yyval.vfields) = autorelease(new Case::Bindings()); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4616 "hexpr.parse.C"
    break;

  case 290: /* varfields: varfields "," varbind  */
#line 897 "hexpr.y"
                                 { (yyval.vfields) = (yyvsp[-2].vfields); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4622 "hexpr.parse.C"
    break;

  case 291: /* varbind: id "=" l0expr  */
#line 899 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-2].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4628 "hexpr.parse.C"
    break;

  case 292: /* varbind: id ":" id "=" l0expr  */
#line 900 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-4].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4634 "hexpr.parse.C"
    break;

  case 293: /* varbind: "intV" ":" id "=" l0expr  */
#line 901 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(".f" + str::from((yyvsp[-4].intv)), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4640 "hexpr.parse.C"
    break;

  case 294: /* cargs: %empty  */
#line 903 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); }
#line 4646 "hexpr.parse.C"
    break;

  case 295: /* cargs: l0expr  */
#line 904 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4652 "hexpr.parse.C"
    break;

  case 296: /* cargs: cargs "," l0expr  */
#line 905 "hexpr.y"
                        { (yyvsp[-2].exps)->push_back(ExprPtr((yyvsp[0].exp))); (yyval.exps) = (yyvsp[-2].exps); }
#line 4658 "hexpr.parse.C"
    break;

  case 297: /* qtype: cst "=>" l0mtype  */
#line 907 "hexpr.y"
                         { (yyval.qualtype) = new QualType(*(yyvsp[-2].tconstraints), *(yyvsp[0].mtype)); }
#line 4664 "hexpr.parse.C"
    break;

  case 298: /* qtype: l0mtype  */
#line 908 "hexpr.y"
                         { (yyval.qualtype) = new QualType(Constraints(), *(yyvsp[0].mtype)); }
#line 4670 "hexpr.parse.C"
    break;

  case 299: /* cst: "(" tpreds ")"  */
#line 911 "hexpr.y"
                    { (yyval.tconstraints) = (yyvsp[-1].tconstraints); }
#line 4676 "hexpr.parse.C"
    break;

  case 300: /* tpreds: tpred  */
#line 913 "hexpr.y"
                         { (yyval.tconstraints) = autorelease(new Constraints()); (yyval.tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); }
#line 4682 "hexpr.parse.C"
    break;

  case 301: /* tpreds: tpreds "," tpred  */
#line 914 "hexpr.y"
                         { (yyvsp[-2].tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); (yyval.tconstraints) = (yyvsp[-2].tconstraints); }
#line 4688 "hexpr.parse.C"
    break;

  case 302: /* tpred: id l1mtargl  */
#line 916 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(*(yyvsp[-1].string), *(yyvsp[0].mtypes)); }
#line 4694 "hexpr.parse.C"
    break;

  case 303: /* tpred: l1mtype "==" l1mtype  */
#line 917 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(EqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4700 "hexpr.parse.C"
    break;

  case 304: /* tpred: l1mtype "!=" l1mtype  */
#line 918 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(NotEqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4706 "hexpr.parse.C"
    break;

  case 305: /* tpred: l1mtype "~" l1mtype  */
#line 919 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(FixIsoRecur::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4712 "hexpr.parse.C"
    break;

  case 306: /* tpred: l1mtype "=" "{" l1mtype "*" l1mtype "}"  */
#line 920 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4718 "hexpr.parse.C"
    break;

  case 307: /* tpred: l1mtype "=" "{" id ":" l1mtype "*" l1mtype "}"  */
#line 921 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4724 "hexpr.parse.C"
    break;

  case 308: /* tpred: l1mtype "=" "(" l1mtype "*" l1mtype ")"  */
#line 922 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4730 "hexpr.parse.C"
    break;

  case 309: /* tpred: "{" l1mtype "*" l1mtype "}" "=" l1mtype  */
#line 923 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4736 "hexpr.parse.C"
    break;

  case 310: /* tpred: "{" id ":" l1mtype "*" l1mtype "}" "=" l1mtype  */
#line 924 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4742 "hexpr.parse.C"
    break;

  case 311: /* tpred: "(" l1mtype "*" l1mtype ")" "=" l1mtype  */
#line 925 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(1), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4748 "hexpr.parse.C"
    break;

  case 312: /* tpred: l1mtype "." recfieldname "::" l1mtype  */
#line 927 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4754 "hexpr.parse.C"
    break;

  case 313: /* tpred: l1mtype "." recfieldname "<-" l1mtype  */
#line 928 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4760 "hexpr.parse.C"
    break;

  case 314: /* tpred: l1mtype "/" l1mtype "::" l1mtype  */
#line 929 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4766 "hexpr.parse.C"
    break;

  case 315: /* tpred: l1mtype "/" l1mtype "<-" l1mtype  */
#line 930 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4772 "hexpr.parse.C"
    break;

  case 316: /* tpred: l1mtype "=" "|" l1mtype "+" l1mtype "|"  */
#line 932 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4778 "hexpr.parse.C"
    break;

  case 317: /* tpred: "|" l1mtype "+" l1mtype "|" "=" l1mtype  */
#line 933 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4784 "hexpr.parse.C"
    break;

  case 318: /* tpred: l1mtype "=" "|" id ":" l1mtype "+" l1mtype "|"  */
#line 934 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4790 "hexpr.parse.C"
    break;

  case 319: /* tpred: "|" id ":" l1mtype "+" l1mtype "|" "=" l1mtype  */
#line 935 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4796 "hexpr.parse.C"
    break;

  case 320: /* tpred: "|" id ":" l0mtype "|" "::" l1mtype  */
#line 937 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), TString::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype))); }
#line 4802 "hexpr.parse.C"
    break;

  case 321: /* tpred: "|" l1mtype "/" l0mtype "|" "::" l1mtype  */
#line 938 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), *(yyvsp[-5].mtype),                *(yyvsp[-3].mtype))); }
#line 4808 "hexpr.parse.C"
    break;

  case 322: /* tpred: l1mtype "++" l1mtype "=" l1mtype  */
#line 939 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(AppendsToUnqualifier::constraintName(), list(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4814 "hexpr.parse.C"
    break;

  case 323: /* l1mtargl: l1mtype  */
#line 941 "hexpr.y"
                           { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4820 "hexpr.parse.C"
    break;

  case 324: /* l1mtargl: l1mtargl l1mtype  */
#line 942 "hexpr.y"
                           { (yyvsp[-1].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4826 "hexpr.parse.C"
    break;

  case 325: /* ltmtype: ltmtype l0mtype  */
#line 944 "hexpr.y"
                          { (yyval.mtypes) = (yyvsp[-1].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4832 "hexpr.parse.C"
    break;

  case 326: /* ltmtype: l0mtype  */
#line 945 "hexpr.y"
                          { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4838 "hexpr.parse.C"
    break;

  case 327: /* l0mtype: l0mtargl "->" l1mtype  */
#line 947 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(Func::make(tuplety(*(yyvsp[-2].mtypes)), *(yyvsp[0].mtype)))); }
#line 4844 "hexpr.parse.C"
    break;

  case 328: /* l0mtype: mtuplist  */
#line 948 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(makeTupleType(*(yyvsp[0].mtypes)))); }
#line 4850 "hexpr.parse.C"
    break;

  case 329: /* l0mtype: msumlist  */
#line 949 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(makeSumType(*(yyvsp[0].mtypes)))); }
#line 4856 "hexpr.parse.C"
    break;

  case 330: /* l1mtype: id  */
#line 951 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(monoTypeByName(*(yyvsp[0].string)))); }
#line 4862 "hexpr.parse.C"
    break;

  case 331: /* l1mtype: "<" cppid ">"  */
#line 952 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(OpaquePtr::make(str::replace<char>(*(yyvsp[-1].string), ".", "::"), 0, false))); }
#line 4868 "hexpr.parse.C"
    break;

  case 332: /* l1mtype: "[" "]"  */
#line 953 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("[]"))); }
#line 4874 "hexpr.parse.C"
    break;

  case 333: /* l1mtype: "[" ltmtype "]"  */
#line 954 "hexpr.y"
                                           { try { (yyval.mtype) = autorelease(new MonoTypePtr(Array::make(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4880 "hexpr.parse.C"
    break;

  case 334: /* l1mtype: "[" ":" l0mtype "|" tyind ":" "]"  */
#line 955 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(FixedArray::make(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype)))); }
#line 4886 "hexpr.parse.C"
    break;

  case 335: /* l1mtype: "(" "->" ")"  */
#line 956 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("->"))); }
#line 4892 "hexpr.parse.C"
    break;

  case 336: /* l1mtype: "(" ltmtype ")"  */
#line 957 "hexpr.y"
                                           { try { (yyval.mtype) = autorelease(new MonoTypePtr(clone(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4898 "hexpr.parse.C"
    break;

  case 337: /* l1mtype: "{" mreclist "}"  */
#line 958 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(makeRecType(*(yyvsp[-1].mreclist)))); }
#line 4904 "hexpr.parse.C"
    break;

  case 338: /* l1mtype: "|" mvarlist "|"  */
#line 959 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(makeVarType(*(yyvsp[-1].mvarlist)))); }
#line 4910 "hexpr.parse.C"
    break;

  case 339: /* l1mtype: "(" ")"  */
#line 960 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("unit"))); }
#line 4916 "hexpr.parse.C"
    break;

  case 340: /* l1mtype: "intV"  */
#line 961 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(((yyvsp[0].intv) == 0) ? Prim::make("void") : TLong::make((yyvsp[0].intv)))); }
#line 4922 "hexpr.parse.C"
    break;

  case 341: /* l1mtype: "boolV"  */
#line 962 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr((yyvsp[0].boolv) ? TLong::make(1) : TLong::make(0))); }
#line 4928 "hexpr.parse.C"
    break;

  case 342: /* l1mtype: "exists" id "." l1mtype  */
#line 963 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Exists::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4934 "hexpr.parse.C"
    break;

  case 343: /* l1mtype: l1mtype "@" l1mtype  */
#line 964 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype), *(yyvsp[0].mtype)))); }
#line 4940 "hexpr.parse.C"
    break;

  case 344: /* l1mtype: l1mtype "@" "?"  */
#line 965 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype)))); }
#line 4946 "hexpr.parse.C"
    break;

  case 345: /* l1mtype: "^" id "." l1mtype  */
#line 966 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Recursive::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4952 "hexpr.parse.C"
    break;

  case 346: /* l1mtype: "stringV"  */
#line 967 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(TString::make(str::unescape(str::trimq(*(yyvsp[0].string)))))); }
#line 4958 "hexpr.parse.C"
    break;

  case 347: /* l1mtype: "`" l0expr "`"  */
#line 968 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp))))))); }
#line 4964 "hexpr.parse.C"
    break;

  case 348: /* tyind: id  */
#line 970 "hexpr.y"
              { (yyval.mtype) = autorelease(new MonoTypePtr(TVar::make(*(yyvsp[0].string)))); }
#line 4970 "hexpr.parse.C"
    break;

  case 349: /* tyind: "intV"  */
#line 971 "hexpr.y"
              { (yyval.mtype) = autorelease(new MonoTypePtr(TLong::make((yyvsp[0].intv)))); }
#line 4976 "hexpr.parse.C"
    break;

  case 350: /* cppid: id  */
#line 973 "hexpr.y"
                    { (yyval.string) = (yyvsp[0].string); }
#line 4982 "hexpr.parse.C"
    break;

  case 351: /* cppid: cppid "." id  */
#line 974 "hexpr.y"
                    { (yyval.string) = (yyvsp[-2].string); *(yyval.string) += "."; *(yyval.string) += *(yyvsp[0].string); }
#line 4988 "hexpr.parse.C"
    break;

  case 352: /* l0mtargl: l1mtype  */
#line 976 "hexpr.y"
                                        { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4994 "hexpr.parse.C"
    break;

  case 353: /* l0mtargl: "(" l0mtype "," l0mtarglt ")"  */
#line 977 "hexpr.y"
                                        { (yyvsp[-1].mtypes)->insert((yyvsp[-1].mtypes)->begin(), *(yyvsp[-3].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 5000 "hexpr.parse.C"
    break;

  case 354: /* l0mtarglt: l0mtype  */
#line 979 "hexpr.y"
                                 { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5006 "hexpr.parse.C"
    break;

  case 355: /* l0mtarglt: l0mtarglt "," l0mtype  */
#line 980 "hexpr.y"
                                 { (yyvsp[-2].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-2].mtypes); }
#line 5012 "hexpr.parse.C"
    break;

  case 356: /* mtuplist: l1mtype  */
#line 982 "hexpr.y"
                               { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5018 "hexpr.parse.C"
    break;

  case 357: /* mtuplist: mtuplist "*" l1mtype  */
#line 983 "hexpr.y"
                               { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5024 "hexpr.parse.C"
    break;

  case 358: /* msumlist: l1mtype "+" l1mtype  */
#line 985 "hexpr.y"
                               { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[-2].mtype)); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5030 "hexpr.parse.C"
    break;

  case 359: /* msumlist: msumlist "+" l1mtype  */
#line 986 "hexpr.y"
                               { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 5036 "hexpr.parse.C"
    break;

  case 360: /* mreclist: mreclist "," id ":" l0mtype  */
#line 988 "hexpr.y"
                                      { (yyval.mreclist) = (yyvsp[-4].mreclist);                                 (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 5042 "hexpr.parse.C"
    break;

  case 361: /* mreclist: id ":" l0mtype  */
#line 989 "hexpr.y"
                                      { (yyval.mreclist) = autorelease(new Record::Members()); (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 5048 "hexpr.parse.C"
    break;

  case 362: /* mvarlist: mvarlist "," id ":" l0mtype  */
#line 991 "hexpr.y"
                                      { (yyval.mvarlist) = (yyvsp[-4].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 5054 "hexpr.parse.C"
    break;

  case 363: /* mvarlist: mvarlist "," id  */
#line 992 "hexpr.y"
                                      { (yyval.mvarlist) = (yyvsp[-2].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 5060 "hexpr.parse.C"
    break;

  case 364: /* mvarlist: id ":" l0mtype  */
#line 993 "hexpr.y"
                                      { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 5066 "hexpr.parse.C"
    break;

  case 365: /* mvarlist: id  */
#line 994 "hexpr.y"
                                      { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 5072 "hexpr.parse.C"
    break;


#line 5076 "hexpr.parse.C"

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

#line 998 "hexpr.y"

#pragma GCC diagnostic pop


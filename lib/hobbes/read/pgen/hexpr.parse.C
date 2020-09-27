/* A Bison parser, made by GNU Bison 3.7.1.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.7.1"

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
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
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
#define YYLAST   2999

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  98
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  82
/* YYNRULES -- Number of rules.  */
#define YYNRULES  357
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  782

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
     523,   526,   529,   530,   531,   534,   535,   538,   540,   543,
     544,   545,   546,   547,   548,   549,   550,   552,   553,   555,
     557,   558,   560,   563,   564,   565,   566,   568,   569,   571,
     574,   576,   578,   579,   581,   583,   585,   586,   587,   588,
     589,   590,   591,   592,   593,   594,   595,   596,   597,   598,
     599,   600,   601,   602,   603,   605,   606,   608,   609,   612,
     613,   614,   615,   617,   618,   619,   620,   621,   622,   624,
     625,   627,   628,   629,   630,   631,   632,   633,   634,   635,
     637,   638,   639,   640,   641,   643,   644,   645,   646,   648,
     651,   652,   655,   658,   661,   673,   674,   677,   679,   680,
     682,   684,   685,   687,   688,   690,   691,   693,   694,   696,
     697,   700,   701,   704,   705,   706,   707,   708,   709,   710,
     711,   712,   715,   716,   717,   718,   719,   722,   723,   724,
     727,   730,   733,   734,   737,   738,   739,   740,   741,   742,
     743,   744,   745,   746,   747,   748,   749,   750,   753,   756,
     757,   758,   759,   760,   761,   762,   763,   764,   765,   766,
     767,   768,   769,   770,   771,   772,   773,   776,   778,   779,
     781,   783,   784,   786,   788,   789,   791,   792,   794,   795,
     796,   798,   799,   801,   802,   804,   805,   807,   808,   811,
     812,   814,   815,   816,   817,   818,   819,   820,   821,   822,
     823,   824,   825,   826,   827,   828,   829,   830,   831,   832,
     833,   834,   835,   836,   837,   839,   840,   841,   842,   843,
     845,   847,   848,   850,   851,   853,   854,   856,   858,   859,
     860,   862,   863,   864,   865,   866,   867,   868,   869,   870,
     871,   872,   873,   874,   875,   876,   882,   883,   884,   885,
     887,   888,   890,   891,   892,   894,   895,   896,   898,   899,
     902,   904,   905,   907,   908,   909,   910,   911,   912,   913,
     914,   915,   916,   918,   919,   920,   921,   923,   924,   925,
     926,   928,   929,   930,   932,   933,   935,   936,   938,   939,
     940,   942,   943,   944,   945,   946,   947,   948,   949,   950,
     951,   952,   953,   954,   955,   956,   957,   958,   959,   961,
     962,   964,   965,   967,   968,   970,   971,   973,   974,   976,
     977,   979,   980,   982,   983,   984,   985,   987
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

#define YYPACT_NINF (-597)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-357)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     356,  1283,  2038,  2038,    33,   127,   127,   127,    42,    42,
     105,   105,  -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,
    -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,   501,
      67,  2038,  2818,    11,  2818,   127,   141,  1487,  2038,   501,
     197,  2038,   -42,  -597,  1406,  -597,  -597,  -597,  -597,  -597,
    -597,  -597,   195,  -597,   175,   177,     7,   188,  2584,  2428,
    2038,  1570,  2919,  2919,  -597,   111,   200,   317,   422,   432,
    -597,   217,  -597,  -597,  -597,  1283,   301,   355,  -597,   928,
     155,  -597,  -597,   156,  1311,   307,    42,   351,  1842,  -597,
    -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,
    -597,  -597,  -597,  -597,  -597,  2919,   127,    64,  -597,   383,
    -597,   372,   194,  2740,   127,   194,   410,  2116,   378,   380,
    2506,   384,   385,   391,   501,   392,   398,   403,   412,   413,
     415,   416,   418,  2272,   419,   420,   421,  -597,  -597,   423,
    -597,   -26,   295,   248,   329,   461,   464,    30,   414,   127,
     127,   411,  -597,  1920,  1920,  2919,  2038,  1804,     7,  -597,
    -597,   501,  2038,    25,  -597,  -597,   448,   378,   380,  2506,
     384,   385,   391,   392,   398,   403,   413,   415,   416,   418,
     419,   420,   421,  -597,  -597,  -597,  -597,  -597,  -597,  -597,
    -597,  -597,  -597,  -597,  -597,  -597,  2919,  2919,   127,   225,
     177,   845,  -597,  -597,  -597,  2887,  2350,  2350,  2350,  2350,
    2428,  2584,  2584,  2584,  2584,  2584,  2584,  2584,  2584,  2584,
    2584,  2584,  2662,  2662,  2662,  2038,  -597,  1406,   127,  -597,
    -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,
    -597,  -597,  -597,  -597,  -597,  -597,  1920,  -597,  1920,  -597,
    -597,  -597,   127,   127,   227,   260,  1998,  1998,   127,  2038,
     191,  -597,   119,  1998,   127,    26,    42,   928,   127,   227,
     127,   127,    22,  -597,    13,   483,   474,   477,  -597,  -597,
     304,   438,   350,  -597,   482,  2038,    54,  2428,   444,   445,
     194,    20,  -597,   491,  2818,  1648,   501,   447,  1726,  -597,
     492,   494,  -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,
    -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,
    -597,  -597,  2038,  2919,  1882,  -597,  -597,   992,  2038,  2038,
    2038,  -597,  -597,  -597,  -597,  -597,   676,  -597,   503,  -597,
    -597,  -597,   309,   456,  2038,   139,  -597,  -597,  2038,   186,
    2038,   311,   321,   353,   499,   152,  2038,  -597,  2038,   173,
     453,   453,  -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,
    -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,  1406,
    -597,  -597,  -597,   493,    89,   465,  -597,   362,  -597,    29,
    1842,  -597,  1148,   227,    68,   364,   508,   110,   210,   128,
     497,   452,  -597,  1311,   298,  1998,  1998,   501,  1998,  1998,
    1998,  1194,  1998,   457,    42,   532,   127,   127,  1842,   466,
     515,   516,   539,  -597,  1998,  1998,  1998,  1998,  -597,   479,
    2919,  -597,    34,  2919,  -597,  2038,  -597,  -597,   267,  2919,
     445,  -597,  -597,  -597,  -597,   187,  -597,  -597,  -597,  -597,
    -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,  1648,
    2194,   501,   337,   177,  -597,   482,  -597,  2038,  -597,  -597,
    2038,  -597,  -597,   326,   520,  -597,   480,  -597,   522,  -597,
     478,   481,   227,    36,  1842,  -597,  -597,   486,  1960,  -597,
    -597,  2038,   206,   496,  -597,   490,  -597,   498,  -597,    46,
    2919,  2919,  -597,  -597,  -597,  1998,  -597,  -597,  -597,  -597,
    1998,   488,  -597,  1998,  -597,   127,  1842,  1998,  1842,  -597,
     127,  1842,  1998,  -597,  -597,  1998,  1998,  1998,     2,    39,
     373,   457,   457,   457,  -597,   457,   457,    27,    42,   532,
    -597,    23,  -597,   240,  -597,  -597,   123,  1842,  1842,  1842,
      42,   539,  -597,   457,   457,   457,   457,  -597,  -597,  -597,
    -597,  -597,  -597,   535,   336,  -597,   366,  2852,  -597,   502,
    -597,    40,  2818,   538,   166,   504,   506,  -597,  2919,  2038,
    -597,  2038,  -597,  -597,  -597,  -597,  -597,   507,  -597,  2038,
     216,  2038,  -597,  -597,  -597,   510,   511,   457,   189,   371,
     244,   547,  -597,    79,   132,   512,   552,   514,    62,   457,
      96,    97,   561,   103,   562,  1998,  1998,  1998,  1998,  1998,
     532,   127,  -597,  -597,   532,   127,   127,  -597,   539,  -597,
     318,  -597,  -597,   569,  -597,   127,   550,   267,   127,  2038,
    2038,  2038,  -597,  -597,  -597,  2038,  -597,  -597,   575,   194,
    2194,  2194,  -597,  -597,  -597,  -597,   529,  -597,  -597,  -597,
    2038,   246,  -597,  -597,  -597,   573,  -597,   578,  -597,   581,
    1842,  1998,   582,   584,  1842,   585,  1998,  1998,  1998,  1998,
    1998,  1998,   457,   457,   457,   457,   457,   532,    24,   532,
    -597,   127,   539,  -597,  1842,  2038,   583,  2038,  -597,   587,
    -597,   593,  -597,  -597,   548,   272,  2350,  -597,  2038,   251,
    1998,   551,  1998,  -597,   245,  1998,  1998,  -597,  1998,   263,
     218,   256,   140,   265,   112,   532,  -597,  -597,  2038,  -597,
    2038,  2038,  -597,  -597,  -597,   173,   549,  -597,  2038,   274,
     457,  -597,   457,   591,   457,   457,   457,   595,  -597,  -597,
    1998,  -597,  1998,   532,  -597,  -597,  -597,  2350,  -597,  2038,
     283,  1998,  1998,   270,   275,   173,  -597,  2038,   292,   457,
     457,  -597,  -597,  -597,  2038,   293,  -597,  2038,   596,  -597,
    2038,  -597
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     9,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   164,   165,   166,   167,   168,   169,   170,   171,
     172,   173,   357,   174,   161,   211,   176,   177,   279,     0,
       0,     0,     0,     0,     0,     0,     0,   285,   285,   258,
       0,     0,     0,     2,     8,    10,    12,    17,    13,    14,
      15,    16,     0,    30,   119,   175,   160,   142,     0,     0,
       0,   285,     0,     0,     4,    92,    98,   100,   109,   114,
     118,   142,     5,   142,     1,     9,     0,    31,   341,     0,
       0,    62,    64,     0,     0,     0,     0,     0,     0,   269,
     264,   268,   263,   262,   265,   266,   274,   275,   267,   270,
     271,   272,   273,   278,   261,   252,     0,     0,   129,     0,
     245,     0,   214,     0,     0,   162,     0,     0,     0,     0,
       0,     0,     0,     0,    69,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    68,    71,     0,
     286,     0,   286,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    11,     0,     0,     0,   285,     0,   159,   212,
     277,     0,     0,     0,   113,    93,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   221,   222,   223,   229,   224,   225,   226,
     227,   228,   230,   234,   232,   233,   252,   252,     0,     0,
     231,     0,   250,   220,   244,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     6,     9,     0,    79,
      80,    81,    82,    83,    84,    69,    73,    72,    70,    74,
      75,    76,    77,    66,    67,    78,     0,    63,     0,   332,
     331,   337,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   291,     0,   321,     0,    43,    60,    64,     0,     0,
       0,     0,    53,    87,   347,     0,   319,   320,   321,   254,
       0,   251,     0,   256,     0,     0,     0,     0,     0,     0,
     213,     0,   199,     0,     0,   252,   258,     0,     0,   132,
       0,   142,   179,   180,   181,   182,   183,   184,   187,   186,
     185,   188,   189,   192,   190,   191,   196,   193,   194,   195,
      65,   178,     0,     0,     0,   146,   157,     0,     0,     0,
       0,   154,   197,    33,    34,    32,     0,    37,     0,   289,
     127,   123,     0,   176,     0,     0,   276,    18,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   219,     0,    91,
      94,    95,    96,    97,   103,   102,   101,   104,   105,   106,
     107,   108,   112,   110,   111,   115,   116,   117,     3,     7,
     342,    35,    36,     0,     0,     0,   330,     0,   317,   347,
       0,   323,     0,     0,     0,     0,   321,     0,     0,   321,
       0,     0,   290,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   293,   314,     0,     0,     0,     0,     0,   317,
       0,   356,     0,    88,     0,     0,     0,     0,   246,     0,
       0,   248,     0,     0,   120,     0,   128,   130,     0,     0,
     122,   216,   124,   198,   205,     0,   164,   165,   166,   167,
     168,   169,   170,   171,   173,   174,   161,   176,   177,   252,
     252,   258,     0,   175,   142,     0,   134,     0,   125,   131,
       0,   287,   140,     0,     0,   144,     0,   158,     0,   259,
       0,     0,     0,   347,     0,   141,   147,     0,     0,   148,
      19,     0,     0,     0,   240,     0,   235,     0,   242,     0,
       0,     0,   237,    89,    90,     0,   322,   326,   327,   316,
       0,     0,   324,     0,   328,     0,     0,     0,     0,   329,
       0,     0,     0,   338,   292,     0,     0,     0,     0,     0,
       0,   294,   296,   295,   335,   334,   315,    39,     0,    45,
      50,    44,    47,     0,    85,    61,    54,     0,     0,     0,
       0,    55,    57,   349,   318,   348,   350,   247,   253,   249,
     255,   257,   121,     0,     0,   280,     0,     0,   215,   200,
     202,     0,     0,     0,     0,     0,     0,   145,     0,     0,
     143,     0,   153,   152,   288,   151,   150,     0,    20,     0,
       0,     0,   241,   236,   243,     0,     0,   333,     0,     0,
       0,     0,   352,   347,     0,     0,   354,   355,   347,   336,
       0,     0,   321,     0,   321,     0,     0,     0,     0,     0,
       0,     0,    52,    51,     0,     0,     0,    86,     0,   345,
       0,   355,    59,     0,    58,     0,   155,     0,     0,     0,
       0,     0,   205,   210,   209,     0,   204,   207,   208,   163,
       0,     0,   154,   126,   133,   139,   138,   260,   149,    21,
       0,     0,    99,   239,   238,     0,   340,     0,   339,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   313,   306,   305,   304,   303,    41,    40,    46,
      48,    49,    56,   344,     0,     0,     0,     0,   281,     0,
     282,     0,   217,   201,     0,     0,     0,    22,     0,     0,
       0,     0,     0,   351,     0,     0,     0,   353,     0,   349,
       0,     0,     0,     0,     0,     0,   346,    38,     0,   156,
       0,     0,   203,   206,   208,   136,   137,    23,     0,     0,
     302,   325,   300,     0,   308,   312,   311,     0,   299,   297,
       0,   307,     0,    42,   284,   283,   218,     0,    24,     0,
       0,     0,     0,     0,     0,   135,    25,     0,     0,   301,
     310,   298,   309,    26,     0,     0,    27,     0,     0,    28,
       0,    29
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -597,  -597,   560,   409,   -29,  -597,  -597,  -597,  -597,   102,
    -597,  -597,    21,    28,  -596,  -517,  -597,    18,  -528,  -389,
     365,     1,   374,    31,   229,    -2,  -202,   -39,   388,   -32,
     261,    16,  -597,   368,  -597,   352,  -597,    73,  -597,   -20,
    -597,   361,  -597,    14,  -597,  -597,   -46,   823,  -597,  -597,
     215,   -50,  -597,   -96,   -19,  -178,  -597,  -182,  -391,  -597,
     -27,   -51,  -597,    41,   -30,  -127,   440,  -597,   257,  -597,
     406,   -77,   733,  -597,   424,  -597,  -597,  -597,  -597,  -597,
    -597,   417
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,    43,    44,    45,    46,    47,   151,    48,    49,
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
      64,    72,   103,   158,   359,   360,   361,   362,   143,    81,
      81,   273,   112,   205,   115,   152,   353,    53,   351,   352,
     165,   300,   623,   634,   687,   537,   164,   340,   689,   111,
     422,   624,   725,    74,   415,   620,   142,   249,   160,   148,
     615,   560,   250,   203,   203,    22,  -343,    22,   251,   321,
      22,   149,   150,   161,   424,   322,   643,   252,   166,    22,
      53,   158,  -343,   348,   158,    22,   644,    22,   330,  -343,
     424,    22,   510,   253,   404,   616,   405,   424,   617,    22,
     406,   247,   407,   408,   247,   409,   410,    81,   164,   114,
     411,    53,    22,   290,   165,  -343,   269,   103,   255,   442,
     270,   411,   271,   676,   625,   625,   416,   621,   560,   258,
     331,   513,  -343,   559,   259,   297,    79,   411,   645,   381,
     424,   382,   671,   435,   411,   594,   342,   411,   105,   753,
      22,   628,   106,   285,   346,   228,   341,   164,   249,   677,
     678,   105,   506,   250,   680,   106,   286,   206,    22,   251,
     411,   517,    22,   752,   518,   345,   411,   404,   252,   405,
     347,   633,   521,   406,   634,   407,   408,   411,   409,   410,
     623,   363,   623,   488,   253,   207,   208,   388,   388,    84,
      22,    22,   357,   750,   411,   411,   357,   372,   373,   374,
     501,   411,   419,   246,   248,   423,   209,   269,   411,   255,
     411,   270,   300,   271,   651,   159,    28,   411,  -356,  -356,
     258,    22,   672,    22,   154,   259,   489,   146,    28,   117,
     411,    29,    22,   378,   491,    28,   162,   -64,   411,    79,
      79,    22,   502,    29,   153,   155,   623,   207,   208,   158,
      29,    22,   249,    53,   589,   354,   652,   250,   437,   156,
      22,   157,    22,   251,   660,   225,   572,   401,   209,   419,
     385,   156,   252,   157,   665,    22,   402,   247,   156,   210,
     157,    22,   403,   626,   445,   249,    22,   411,   253,   353,
     250,   351,   352,   434,   708,    22,   251,   563,   643,   738,
     519,   520,    22,   748,   390,   252,   297,    22,   644,    22,
     478,   269,   386,   255,   474,   270,   411,   271,    22,   227,
     509,   253,   759,   511,   258,   509,   388,    22,    22,   259,
     471,   767,   476,   669,   743,   325,   479,   480,   481,   322,
     774,   777,   411,   411,   269,   749,   255,   391,   270,   264,
     271,   273,   487,   747,   411,   751,   490,   258,   493,   771,
     152,   411,   259,   411,   503,   772,   504,   573,   411,     1,
       2,     3,    22,   411,   211,   212,   213,   214,   215,   216,
     217,   218,   525,    80,    83,   323,   526,   249,   527,   428,
     530,   324,   250,   268,   485,   429,   494,    22,   251,   567,
     322,   666,   495,   693,   158,    53,    22,   252,   496,   694,
     638,   228,   497,   577,   639,   419,   578,   584,   326,   618,
     327,   558,   619,   253,   561,    81,   636,   637,    57,    71,
     203,   287,    75,    76,    78,    82,    82,    86,    88,   431,
     143,   432,   498,   562,   499,   288,   269,   508,   255,   602,
     270,   605,   271,   514,   607,   515,   104,   110,   294,   258,
      85,    87,   116,   302,   259,   303,   104,   147,   142,   305,
     306,    57,   219,   220,   221,   575,   307,   308,   576,   423,
     629,   602,   631,   309,   163,   222,   223,   224,   310,   204,
     204,   595,   596,   375,   376,   377,   587,   311,   312,   588,
     313,   314,    57,   315,   317,   318,   319,    82,   320,   328,
      82,   263,   329,   267,   735,   278,   332,   335,    89,    90,
      91,    92,    93,    94,    95,   350,   425,   426,   427,   430,
     433,    96,   204,   284,   438,   439,    22,    97,   444,   466,
     470,   293,  -245,   486,   301,   484,    98,   500,   209,   505,
     507,   104,   516,   522,   523,   411,   538,   547,   357,   548,
     549,    81,   649,   550,   557,   765,   579,   580,   582,   474,
     581,   583,    99,   585,   591,   592,   333,   334,   599,   635,
     278,   278,   204,   100,   101,   593,   650,   656,   104,   657,
     349,   670,   642,   653,   658,   102,   674,   659,   654,   662,
     663,   664,   673,   713,   675,   679,   681,   717,   158,   364,
     365,   366,   367,   368,   369,   370,   371,   695,   697,   705,
     706,   710,   711,   204,   204,   284,   355,   726,   204,   712,
     715,   728,   204,   716,   718,   730,   731,   732,   741,   761,
     757,   595,   596,   762,   780,   226,   379,   700,   701,   702,
     622,   417,   688,   704,    57,   380,   692,   546,   480,   481,
     469,   655,   443,   690,   436,   568,   703,   691,   707,   733,
     524,   392,     0,   278,     0,   278,     0,     0,     0,   383,
      78,   278,   278,   396,   399,   400,     0,   384,   698,     0,
     278,   414,     0,    82,     0,   418,   278,   420,   421,   278,
       0,   249,     0,   727,     0,   729,   250,     0,     0,     0,
       0,    22,   251,   110,     0,     0,   737,     0,   293,   385,
       0,   252,   464,   465,     0,   301,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   754,   253,   755,   756,
       0,     0,     0,     0,     0,     0,   758,     0,     0,     0,
     204,     0,     0,     0,   104,     0,     0,     0,     0,     0,
     482,   386,   255,   263,   256,     0,   257,   766,     0,     0,
       0,     0,     0,   258,     0,   773,   492,     0,   259,     0,
       0,     0,   776,     0,     0,   779,     0,     0,   781,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,   278,     0,     0,   278,     0,   278,
     278,     0,     0,     0,     0,     0,     0,   262,     0,     0,
     263,     0,   278,   278,   104,   278,   278,   278,   278,   278,
       0,   267,     0,   544,   545,   278,     0,     0,     0,     0,
       0,   278,   278,   278,   278,     0,     0,   204,     0,   284,
     204,     0,     0,     0,     0,   566,   204,     0,     0,     0,
     183,   184,   185,   186,   187,   188,   189,   190,     0,   191,
      22,   192,   193,    25,   194,   195,   464,   464,   465,   574,
       0,     0,     0,     0,     0,   200,   200,     0,     0,     0,
       0,   356,     0,     0,     0,     0,     0,     0,     0,   278,
       0,   278,     0,     0,     0,     0,     0,     0,     0,   590,
       0,     0,     0,     0,     0,     0,   284,   204,   204,   196,
       0,   197,   278,   198,     0,   199,     0,   278,   200,     0,
     278,     0,   601,   278,   278,   278,     0,   606,   278,   278,
       0,     0,   278,   612,   614,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    82,     0,     0,     0,     0,
     627,     0,     0,   278,   278,   278,   278,   267,   229,   230,
     231,   232,   233,   234,   235,   236,   237,   238,   200,   239,
     240,   241,   242,     0,   204,     0,     0,   389,   648,   394,
     397,     0,   243,   244,     0,   204,   413,   245,     0,    89,
      90,    91,    92,    93,    94,    95,     0,   661,     0,     0,
       0,     0,    96,   137,     0,     0,   668,    22,    97,   200,
     200,     0,     0,     0,   200,   138,     0,    98,   200,     0,
       0,     0,   278,   278,   278,   278,   278,     0,   544,     0,
       0,     0,   544,   544,     0,     0,     0,     0,     0,     0,
       0,     0,   696,    99,   566,   699,     0,     0,     0,     0,
       0,     0,     0,     0,   100,   101,     0,   464,   464,   483,
       0,   477,     0,     0,     0,     0,   102,     0,   709,     0,
       0,     0,     0,     0,     0,     0,     0,   278,   278,     0,
       0,   278,     0,   278,   278,   278,   278,   278,   278,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   627,     0,
       0,   278,     0,     0,     0,     0,     0,     0,   463,     0,
       0,     0,   734,     0,     0,     0,   739,   278,     0,   278,
       0,     0,   278,   278,     0,   278,   262,     0,   528,   529,
       0,   531,   532,   533,   535,   536,   200,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   760,   553,   554,   555,
     556,     0,     0,   249,     0,     0,     0,   278,   250,   278,
       0,     0,     0,    22,   251,     0,     0,   768,   278,   278,
       0,     0,     0,   252,     0,   775,     0,     0,     0,     0,
       0,     0,   778,     0,     0,     0,     0,     0,     0,   253,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   249,
       0,     0,     0,     0,   250,   389,     0,     0,     0,    22,
     251,     0,   269,     0,   255,   512,   270,     0,   271,   252,
       0,     0,     0,     0,     0,   258,     0,     0,   597,     0,
     259,     0,     0,   598,     0,   253,   600,     0,     0,   603,
     604,     0,     0,   200,   608,   609,   200,     0,   610,   611,
     613,     0,   200,     0,     0,     0,     0,     0,   393,     0,
     255,     0,   270,     0,   271,     0,     0,     0,     0,     0,
       0,   258,   463,   463,   534,     0,   259,     0,     0,     5,
       6,     0,     7,     8,     9,    10,    11,     0,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,     0,     0,     0,     0,     0,
       0,     0,     0,   200,   200,     0,   249,     0,     0,    29,
       0,   250,     0,     0,     0,     0,    22,   251,     0,    30,
      31,     0,    32,     0,    33,     0,   252,     0,   682,   683,
     684,   685,   686,    34,    35,    36,     0,    37,     0,    38,
       0,    39,   253,    40,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    41,     0,     0,    42,     0,
       0,     0,     0,     0,     0,   254,     0,   255,     0,   256,
     200,   257,     0,     0,     0,     0,     0,     0,   258,     0,
       0,   200,     0,   259,   714,     0,     0,     0,     0,   719,
     720,   721,   722,   723,   724,     7,     8,     9,    10,    11,
       0,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,     0,     0,
       0,     0,     0,   740,     0,   742,     0,     0,   744,   745,
       0,   746,    29,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    30,    31,     0,    32,     0,    33,     0,     0,
       0,     0,     0,   463,   463,     0,    34,    35,    36,     0,
      37,     0,    38,   763,    39,   764,    40,     0,     0,     0,
       0,     0,     0,     0,   769,   770,     0,     0,    41,     0,
       0,    42,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,     0,
       0,     0,     0,     0,     0,     0,     0,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    30,    31,     0,    32,     0,    33,     0,
       0,   134,   135,    60,     0,     0,   136,    34,    35,    36,
       0,    61,     0,    38,     0,    39,     0,    40,     0,     0,
      62,    63,   137,     0,     0,     0,     0,     0,     0,    41,
       0,     0,     0,     0,   138,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,     0,     0,     0,     0,     0,     0,     0,     0,
     167,   168,   169,   170,   171,   172,    29,   173,   174,   175,
     128,   176,   177,   178,   179,   133,    30,    31,     0,    32,
       0,    33,     0,     0,   180,   181,    60,     0,     0,   182,
      34,    35,    36,     0,    61,     0,    38,     0,    39,     0,
      40,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,    41,   446,   447,   448,   449,   450,   451,   452,
     453,    20,   454,    22,   455,   456,    25,   457,   458,    28,
       0,     0,     0,     0,     0,     0,     0,     0,   167,   168,
     169,   170,   171,   172,    29,   173,   174,   175,   128,   176,
     177,   178,   179,   133,    30,    31,     0,    32,     0,    33,
       0,     0,   180,   181,    60,     0,     0,   182,    34,    35,
      36,     0,   459,     0,   460,     0,   461,     0,   462,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
      41,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,    29,     0,     0,     0,     0,     0,     0,     0,
       0,    59,    30,    31,     0,    32,     0,    33,     0,     0,
       0,     0,    60,     0,     0,     0,    34,    35,    36,   467,
     295,     0,    38,     0,   296,   468,    40,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,    41,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,   343,    27,    28,     0,     0,   344,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
      29,     0,     0,     0,     0,     0,     0,   249,     0,    59,
      30,    31,   250,    32,     0,    33,     0,    22,   251,     0,
      60,     0,     0,     0,    34,    35,    36,   252,    61,     0,
      38,     0,    39,     0,    40,     0,     0,    62,    63,     0,
       0,     0,     0,   253,     0,     0,    41,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,   269,     0,   255,     0,
     270,     0,   271,     0,    58,     0,     0,     0,    29,   258,
       0,     0,     0,     0,   259,   249,     0,    59,    30,    31,
     250,    32,     0,    33,     0,    22,   251,     0,    60,     0,
       0,     0,    34,    35,    36,   252,    61,     0,    38,   475,
      39,     0,    40,     0,     0,    62,    63,     0,     0,     0,
       0,   253,     0,     0,    41,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,     0,     0,   336,     0,   255,     0,   270,     0,
     271,     0,    58,     0,     0,     0,    29,   258,     0,     0,
       0,     0,   259,   249,     0,    59,    30,    31,   250,    32,
       0,    33,     0,    22,   251,     0,    60,     0,     0,     0,
      34,    35,    36,   252,    61,     0,    38,   586,    39,     0,
      40,     0,     0,    62,    63,     0,     0,     0,     0,   253,
       0,     0,    41,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,   393,     0,   255,     0,   270,     0,   271,     0,
      58,     0,     0,     0,    29,   258,     0,     0,     0,     0,
     259,     0,     0,    59,    30,    31,     0,    32,     0,    33,
       0,     0,     0,     0,    60,     0,     0,     0,    34,    35,
      36,     0,    61,     0,    38,     0,    39,     0,    40,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
      41,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,    29,     0,     0,     0,     0,     0,     0,     0,
       0,    59,    30,    31,     0,    32,     0,    33,     0,     0,
       0,     0,    60,     0,     0,     0,    34,    35,    36,     0,
     295,     0,    38,     0,   296,     0,    40,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,    41,   446,
     447,   448,   449,   450,   451,   452,   453,    20,   454,    22,
     455,   456,    25,   457,   458,    28,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
      29,     0,     0,     0,     0,     0,     0,     0,     0,    59,
      30,    31,     0,    32,     0,    33,     0,     0,     0,     0,
      60,     0,     0,     0,    34,    35,    36,     0,   459,     0,
     460,     0,   461,     0,   462,     0,     0,    62,    63,     0,
       0,     0,     0,     0,     0,     0,    41,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,    29,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
       0,    32,     0,    33,     0,     0,     0,     0,    60,     0,
       0,     0,    34,    35,    36,     0,    61,   316,    38,     0,
      39,     0,    40,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    41,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,    29,     0,     0,     0,
       0,     0,     0,     0,     0,    59,    30,    31,     0,    32,
       0,    33,     0,     0,     0,     0,    60,     0,     0,     0,
      34,    35,    36,     0,    61,     0,    38,     0,    39,     0,
      40,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    41,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,    29,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,     0,    32,     0,    33,
       0,     0,     0,     0,    60,     0,     0,     0,    34,    35,
      36,     0,    61,     0,    38,     0,    39,     0,    40,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      41,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,    29,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    30,    31,     0,    32,     0,    33,     0,     0,
       0,     0,     0,     0,     0,     0,    34,    35,    36,     0,
      61,   304,    38,     0,    39,     0,    40,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    41,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
      29,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,     0,    32,     0,    33,     0,     0,     0,     0,
       0,     0,     0,     0,    34,    35,    36,     0,    61,     0,
      38,     0,    39,     0,    40,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    41,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    29,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
       0,    32,     0,    33,     0,     0,     0,     0,     0,     0,
       0,     0,    34,    35,    36,     0,    61,     0,    38,     0,
      39,     0,    40,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    41,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    29,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    31,     0,     0,
       0,     0,   289,     0,     0,     0,     0,     0,     0,     0,
      34,    35,     0,     0,    61,     0,    38,     0,    39,     0,
      40,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    41,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     640,     0,     0,     0,    29,     0,     0,   183,   184,   185,
     186,   187,   188,   189,   190,    31,   191,    22,   192,   193,
      25,   194,   195,     0,     0,   641,     0,     0,    34,    35,
       0,     0,    61,     0,    38,     0,    39,     0,    40,     0,
       0,     0,   183,   184,   185,   186,   187,   188,   189,   190,
      41,   191,    22,   192,   193,    25,   194,   195,     0,     0,
       0,     0,     0,     0,     0,     0,   196,     0,   197,     0,
     198,     0,   199,   358,   183,   184,   185,   186,   187,   188,
     189,   190,     0,   191,    22,   192,   193,    25,   194,   195,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   196,     0,   197,     0,   198,     0,   199,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   196,     0,   197,     0,   198,     0,   199
};

static const yytype_int16 yycheck[] =
{
       2,     3,    29,    54,   206,   207,   208,   209,    38,     8,
       9,    88,    32,    63,    34,    44,   198,     1,   196,   197,
      59,   117,   539,   551,   620,   414,    58,   154,   624,    31,
       8,     8,     8,     0,     8,     8,    38,    15,    31,    41,
      38,   432,    20,    62,    63,    25,    33,    25,    26,    75,
      25,    93,    94,    46,    41,    81,    16,    35,    60,    25,
      44,   112,    33,    38,   115,    25,    26,    25,    38,    33,
      41,    25,    43,    51,    38,    36,    40,    41,    39,    25,
      44,    80,    46,    47,    83,    49,    50,    86,   120,    78,
      88,    75,    25,   113,   133,    33,    74,   124,    76,    79,
      78,    88,    80,    41,    81,    81,    80,    80,   499,    87,
      80,    43,    33,    79,    92,   117,    74,    88,    78,   246,
      41,   248,    43,    69,    88,    79,   156,    88,    74,   725,
      25,     8,    78,    69,   161,    46,   155,   169,    15,    43,
      43,    74,    53,    20,    41,    78,    82,    36,    25,    26,
      88,    41,    25,    41,    44,   157,    88,    38,    35,    40,
     162,   550,    34,    44,   692,    46,    47,    88,    49,    50,
     687,   210,   689,    34,    51,    64,    65,   254,   255,    74,
      25,    25,   201,    43,    88,    88,   205,   219,   220,   221,
      38,    88,   269,    38,    38,   272,    85,    74,    88,    76,
      88,    78,   298,    80,    38,    28,    31,    88,    80,    81,
      87,    25,    80,    25,    39,    92,    77,    20,    31,    78,
      88,    46,    25,   225,    38,    31,    38,    39,    88,    74,
      74,    25,    80,    46,    39,    60,   753,    64,    65,   290,
      46,    25,    15,   227,    38,    20,    80,    20,   287,    74,
      25,    76,    25,    26,    38,    38,    69,   259,    85,   336,
      33,    74,    35,    76,    75,    25,    75,   266,    74,    69,
      76,    25,    81,    33,   294,    15,    25,    88,    51,   461,
      20,   459,   460,   285,    38,    25,    26,    20,    16,    38,
      80,    81,    25,    75,    34,    35,   298,    25,    26,    25,
     327,    74,    75,    76,   323,    78,    88,    80,    25,     8,
     387,    51,    38,   390,    87,   392,   393,    25,    25,    92,
     322,    38,   324,    79,    79,    77,   328,   329,   330,    81,
      38,    38,    88,    88,    74,    79,    76,    77,    78,    32,
      80,   418,   344,    80,    88,    80,   348,    87,   350,    79,
     379,    88,    92,    88,   356,    80,   358,    20,    88,     3,
       4,     5,    25,    88,    47,    48,    49,    50,    51,    52,
      53,    54,    74,     8,     9,    80,    78,    15,    80,    75,
     407,    86,    20,    32,    75,    81,    75,    25,    26,   439,
      81,    20,    81,    75,   445,   379,    25,    35,    77,    81,
      34,    46,    81,    77,    38,   482,    80,   484,    79,    36,
      81,   430,    39,    51,   433,   414,    80,    81,     1,     2,
     439,    38,     5,     6,     7,     8,     9,    10,    11,    79,
     460,    81,    79,   435,    81,    63,    74,    75,    76,   516,
      78,   518,    80,    79,   521,    81,    29,    30,    38,    87,
      10,    11,    35,    75,    92,    75,    39,    40,   460,    75,
      75,    44,    40,    41,    42,   467,    75,    75,   470,   546,
     547,   548,   549,    75,    57,    43,    44,    45,    75,    62,
      63,   500,   501,   222,   223,   224,   488,    75,    75,   491,
      75,    75,    75,    75,    75,    75,    75,    80,    75,    38,
      83,    84,    38,    86,   706,    88,    92,    96,     7,     8,
       9,    10,    11,    12,    13,    67,    33,    43,    41,    81,
      38,    20,   105,   106,    80,    80,    25,    26,    37,    82,
      38,   114,    38,    77,   117,    32,    35,    38,    85,    46,
      75,   124,    34,    46,    92,    88,    14,    81,   567,    34,
      34,   550,   572,    14,    75,   757,    36,    77,    80,   578,
      38,    80,    61,    77,    68,    75,   149,   150,    80,    34,
     153,   154,   155,    72,    73,    77,    38,   579,   161,   581,
     163,    34,    80,    79,    77,    84,    34,   589,    82,   591,
      80,    80,    80,   670,    80,    34,    34,   674,   649,   211,
     212,   213,   214,   215,   216,   217,   218,    38,    58,    34,
      81,    38,    34,   196,   197,   198,   199,   694,   201,    38,
      38,    38,   205,    39,    39,    38,    33,    79,    77,    38,
      81,   650,   651,    38,    38,    75,   227,   639,   640,   641,
     538,   267,   621,   645,   227,   228,   628,   418,   650,   651,
     298,   578,   291,   625,   286,   440,   642,   626,   660,   705,
     403,   255,    -1,   246,    -1,   248,    -1,    -1,    -1,   252,
     253,   254,   255,   256,   257,   258,    -1,   253,   637,    -1,
     263,   264,    -1,   266,    -1,   268,   269,   270,   271,   272,
      -1,    15,    -1,   695,    -1,   697,    20,    -1,    -1,    -1,
      -1,    25,    26,   286,    -1,    -1,   708,    -1,   291,    33,
      -1,    35,   295,   296,    -1,   298,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   728,    51,   730,   731,
      -1,    -1,    -1,    -1,    -1,    -1,   738,    -1,    -1,    -1,
     323,    -1,    -1,    -1,   327,    -1,    -1,    -1,    -1,    -1,
      74,    75,    76,   336,    78,    -1,    80,   759,    -1,    -1,
      -1,    -1,    -1,    87,    -1,   767,   349,    -1,    92,    -1,
      -1,    -1,   774,    -1,    -1,   777,    -1,    -1,   780,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   379,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   387,    -1,    -1,   390,    -1,   392,
     393,    -1,    -1,    -1,    -1,    -1,    -1,    84,    -1,    -1,
     403,    -1,   405,   406,   407,   408,   409,   410,   411,   412,
      -1,   414,    -1,   416,   417,   418,    -1,    -1,    -1,    -1,
      -1,   424,   425,   426,   427,    -1,    -1,   430,    -1,   432,
     433,    -1,    -1,    -1,    -1,   438,   439,    -1,    -1,    -1,
      15,    16,    17,    18,    19,    20,    21,    22,    -1,    24,
      25,    26,    27,    28,    29,    30,   459,   460,   461,   462,
      -1,    -1,    -1,    -1,    -1,    62,    63,    -1,    -1,    -1,
      -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   482,
      -1,   484,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   492,
      -1,    -1,    -1,    -1,    -1,    -1,   499,   500,   501,    74,
      -1,    76,   505,    78,    -1,    80,    -1,   510,   105,    -1,
     513,    -1,   515,   516,   517,   518,    -1,   520,   521,   522,
      -1,    -1,   525,   526,   527,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   538,    -1,    -1,    -1,    -1,
     543,    -1,    -1,   546,   547,   548,   549,   550,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,   155,    51,
      52,    53,    54,    -1,   567,    -1,    -1,   254,   571,   256,
     257,    -1,    64,    65,    -1,   578,   263,    69,    -1,     7,
       8,     9,    10,    11,    12,    13,    -1,   590,    -1,    -1,
      -1,    -1,    20,    85,    -1,    -1,   599,    25,    26,   196,
     197,    -1,    -1,    -1,   201,    97,    -1,    35,   205,    -1,
      -1,    -1,   615,   616,   617,   618,   619,    -1,   621,    -1,
      -1,    -1,   625,   626,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   635,    61,   637,   638,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,   650,   651,   336,
      -1,    79,    -1,    -1,    -1,    -1,    84,    -1,   661,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   670,   671,    -1,
      -1,   674,    -1,   676,   677,   678,   679,   680,   681,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   691,    -1,
      -1,   694,    -1,    -1,    -1,    -1,    -1,    -1,   295,    -1,
      -1,    -1,   705,    -1,    -1,    -1,   709,   710,    -1,   712,
      -1,    -1,   715,   716,    -1,   718,   403,    -1,   405,   406,
      -1,   408,   409,   410,   411,   412,   323,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   739,   424,   425,   426,
     427,    -1,    -1,    15,    -1,    -1,    -1,   750,    20,   752,
      -1,    -1,    -1,    25,    26,    -1,    -1,   760,   761,   762,
      -1,    -1,    -1,    35,    -1,   768,    -1,    -1,    -1,    -1,
      -1,    -1,   775,    -1,    -1,    -1,    -1,    -1,    -1,    51,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    15,
      -1,    -1,    -1,    -1,    20,   482,    -1,    -1,    -1,    25,
      26,    -1,    74,    -1,    76,    77,    78,    -1,    80,    35,
      -1,    -1,    -1,    -1,    -1,    87,    -1,    -1,   505,    -1,
      92,    -1,    -1,   510,    -1,    51,   513,    -1,    -1,   516,
     517,    -1,    -1,   430,   521,   522,   433,    -1,   525,   526,
     527,    -1,   439,    -1,    -1,    -1,    -1,    -1,    74,    -1,
      76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    87,   459,   460,    90,    -1,    92,    -1,    -1,     6,
       7,    -1,     9,    10,    11,    12,    13,    -1,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   500,   501,    -1,    15,    -1,    -1,    46,
      -1,    20,    -1,    -1,    -1,    -1,    25,    26,    -1,    56,
      57,    -1,    59,    -1,    61,    -1,    35,    -1,   615,   616,
     617,   618,   619,    70,    71,    72,    -1,    74,    -1,    76,
      -1,    78,    51,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    -1,
      -1,    -1,    -1,    -1,    -1,    74,    -1,    76,    -1,    78,
     567,    80,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,
      -1,   578,    -1,    92,   671,    -1,    -1,    -1,    -1,   676,
     677,   678,   679,   680,   681,     9,    10,    11,    12,    13,
      -1,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    -1,    -1,
      -1,    -1,    -1,   710,    -1,   712,    -1,    -1,   715,   716,
      -1,   718,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    56,    57,    -1,    59,    -1,    61,    -1,    -1,
      -1,    -1,    -1,   650,   651,    -1,    70,    71,    72,    -1,
      74,    -1,    76,   750,    78,   752,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   761,   762,    -1,    -1,    92,    -1,
      -1,    95,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    -1,    59,    -1,    61,    -1,
      -1,    64,    65,    66,    -1,    -1,    69,    70,    71,    72,
      -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,
      83,    84,    85,    -1,    -1,    -1,    -1,    -1,    -1,    92,
      -1,    -1,    -1,    -1,    97,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    59,
      -1,    61,    -1,    -1,    64,    65,    66,    -1,    -1,    69,
      70,    71,    72,    -1,    74,    -1,    76,    -1,    78,    -1,
      80,    -1,    -1,    83,    84,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    59,    -1,    61,
      -1,    -1,    64,    65,    66,    -1,    -1,    69,    70,    71,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      -1,    83,    84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      92,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,
      -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    55,    56,    57,    -1,    59,    -1,    61,    -1,    -1,
      -1,    -1,    66,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    -1,    76,    -1,    78,    79,    80,    -1,    -1,    83,
      84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    34,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    -1,    -1,    15,    -1,    55,
      56,    57,    20,    59,    -1,    61,    -1,    25,    26,    -1,
      66,    -1,    -1,    -1,    70,    71,    72,    35,    74,    -1,
      76,    -1,    78,    -1,    80,    -1,    -1,    83,    84,    -1,
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
      -1,    -1,    92,    15,    -1,    55,    56,    57,    20,    59,
      -1,    61,    -1,    25,    26,    -1,    66,    -1,    -1,    -1,
      70,    71,    72,    35,    74,    -1,    76,    77,    78,    -1,
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
      74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,    83,
      84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,
      56,    57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,
      66,    -1,    -1,    -1,    70,    71,    72,    -1,    74,    -1,
      76,    -1,    78,    -1,    80,    -1,    -1,    83,    84,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    56,    57,
      -1,    59,    -1,    61,    -1,    -1,    -1,    -1,    66,    -1,
      -1,    -1,    70,    71,    72,    -1,    74,    75,    76,    -1,
      78,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    42,    -1,    -1,    -1,    46,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    55,    56,    57,    -1,    59,
      -1,    61,    -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,
      70,    71,    72,    -1,    74,    -1,    76,    -1,    78,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      42,    -1,    -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    56,    57,    -1,    59,    -1,    61,
      -1,    -1,    -1,    -1,    66,    -1,    -1,    -1,    70,    71,
      72,    -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      92,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,
      -1,    -1,    46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    56,    57,    -1,    59,    -1,    61,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    -1,
      74,    75,    76,    -1,    78,    -1,    80,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    42,    -1,    -1,    -1,
      46,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      56,    57,    -1,    59,    -1,    61,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    -1,    74,    -1,
      76,    -1,    78,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    56,    57,
      -1,    59,    -1,    61,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    -1,    74,    -1,    76,    -1,
      78,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    46,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,
      -1,    -1,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    -1,    -1,    74,    -1,    76,    -1,    78,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       8,    -1,    -1,    -1,    46,    -1,    -1,    15,    16,    17,
      18,    19,    20,    21,    22,    57,    24,    25,    26,    27,
      28,    29,    30,    -1,    -1,    33,    -1,    -1,    70,    71,
      -1,    -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,
      -1,    -1,    15,    16,    17,    18,    19,    20,    21,    22,
      92,    24,    25,    26,    27,    28,    29,    30,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    74,    -1,    76,    -1,
      78,    -1,    80,    46,    15,    16,    17,    18,    19,    20,
      21,    22,    -1,    24,    25,    26,    27,    28,    29,    30,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    74,    -1,    76,    -1,    78,    -1,    80,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    74,    -1,    76,    -1,    78,    -1,    80
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
      38,   123
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    98,    99,    99,    99,    99,   100,   100,   100,   101,
     101,   101,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   102,   102,
     102,   103,   104,   105,   105,   106,   106,   107,   108,   109,
     109,   109,   109,   109,   109,   109,   109,   110,   110,   111,
     112,   112,   113,   114,   114,   114,   114,   115,   115,   116,
     117,   117,   118,   118,   119,   119,   120,   120,   120,   120,
     120,   120,   120,   120,   120,   120,   120,   120,   120,   120,
     120,   120,   120,   120,   120,   121,   121,   122,   122,   123,
     123,   123,   123,   124,   124,   124,   124,   124,   124,   125,
     125,   126,   126,   126,   126,   126,   126,   126,   126,   126,
     127,   127,   127,   127,   127,   128,   128,   128,   128,   129,
     129,   129,   129,   129,   129,   129,   129,   129,   130,   130,
     131,   132,   132,   133,   133,   134,   134,   135,   135,   136,
     136,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   138,   138,
     139,   140,   140,   141,   142,   142,   143,   143,   144,   144,
     144,   145,   145,   146,   146,   147,   147,   148,   148,   149,
     149,   150,   150,   150,   150,   150,   150,   150,   150,   150,
     150,   150,   150,   150,   150,   150,   150,   150,   150,   150,
     150,   150,   150,   150,   150,   151,   151,   151,   151,   151,
     152,   153,   153,   154,   154,   155,   155,   156,   157,   157,
     157,   158,   158,   158,   158,   158,   158,   158,   158,   158,
     158,   158,   158,   158,   158,   158,   159,   159,   159,   159,
     160,   160,   161,   161,   161,   162,   162,   162,   163,   163,
     164,   165,   165,   166,   166,   166,   166,   166,   166,   166,
     166,   166,   166,   166,   166,   166,   166,   166,   166,   166,
     166,   166,   166,   166,   167,   167,   168,   168,   169,   169,
     169,   170,   170,   170,   170,   170,   170,   170,   170,   170,
     170,   170,   170,   170,   170,   170,   170,   170,   170,   171,
     171,   172,   172,   173,   173,   174,   174,   175,   175,   176,
     176,   177,   177,   178,   178,   178,   178,   179
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     2,     4,     2,     2,     3,     4,     1,     0,
       1,     2,     1,     1,     1,     1,     1,     1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       1,     2,     3,     2,     2,     4,     4,     3,     3,     5,
       7,     7,     9,     3,     5,     5,     7,     1,     3,     3,
       1,     2,     2,     3,     5,     5,     7,     1,     2,     2,
       1,     3,     1,     2,     1,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     1,     2,     4,
       4,     3,     1,     2,     3,     3,     3,     3,     1,     6,
       1,     3,     3,     3,     3,     3,     3,     3,     3,     1,
       3,     3,     3,     2,     1,     3,     3,     3,     1,     1,
       4,     5,     4,     3,     4,     4,     6,     3,     3,     1,
       3,     2,     1,     4,     2,     3,     1,     5,     3,     3,
       1,     4,     1,     5,     4,     5,     3,     4,     4,     6,
       5,     5,     5,     5,     3,     6,     8,     3,     4,     2,
       1,     1,     2,     6,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     2,     1,
       3,     3,     1,     4,     2,     0,     3,     1,     1,     1,
       1,     1,     2,     2,     1,     2,     1,     4,     6,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     3,     4,     3,     5,     5,
       3,     4,     3,     4,     1,     1,     3,     4,     3,     4,
       1,     1,     0,     3,     1,     3,     1,     3,     0,     3,
       5,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     2,     2,     1,
       1,     3,     3,     5,     5,     0,     1,     3,     3,     1,
       3,     1,     3,     2,     3,     3,     3,     7,     9,     7,
       7,     9,     7,     5,     5,     5,     5,     7,     7,     9,
       9,     7,     7,     5,     1,     2,     2,     1,     3,     1,
       1,     1,     3,     2,     3,     7,     3,     3,     3,     3,
       2,     1,     1,     4,     3,     3,     4,     1,     3,     1,
       1,     1,     3,     1,     5,     1,     3,     1,     3,     3,
       3,     5,     3,     5,     3,     3,     1,     1
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
  YYUSE (yyoutput);
  YYUSE (yylocationp);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yykind < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yykind], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yykind);
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
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yykind);
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
#line 2844 "hexpr.parse.C"
    break;

  case 3: /* s: "dodefn" id "=" l0expr  */
#line 489 "hexpr.y"
                            { yyParsedVar    = *(yyvsp[-2].string); yyParsedExpr = (yyvsp[0].exp); }
#line 2850 "hexpr.parse.C"
    break;

  case 4: /* s: "dodefn" l0expr  */
#line 490 "hexpr.y"
                            { yyParsedVar    = "";  yyParsedExpr = (yyvsp[0].exp); }
#line 2856 "hexpr.parse.C"
    break;

  case 5: /* s: "doexpr" l0expr  */
#line 491 "hexpr.y"
                            { yyParsedExpr   = (yyvsp[0].exp);                     }
#line 2862 "hexpr.parse.C"
    break;

  case 6: /* module: "option" id module  */
#line 494 "hexpr.y"
                                 { (yyval.module) = (yyvsp[0].module); (yyval.module)->setOption(*(yyvsp[-1].string), m((yylsp[-2]))); }
#line 2868 "hexpr.parse.C"
    break;

  case 7: /* module: "module" id "where" defs  */
#line 495 "hexpr.y"
                                 { (yyval.module) = new Module(*(yyvsp[-2].string), *(yyvsp[0].mdefs)); }
#line 2874 "hexpr.parse.C"
    break;

  case 8: /* module: defs  */
#line 496 "hexpr.y"
                                 { (yyval.module) = new Module(freshName(), *(yyvsp[0].mdefs)); }
#line 2880 "hexpr.parse.C"
    break;

  case 9: /* defs: %empty  */
#line 498 "hexpr.y"
                    { (yyval.mdefs) = autorelease(new ModuleDefs()); }
#line 2886 "hexpr.parse.C"
    break;

  case 10: /* defs: def  */
#line 499 "hexpr.y"
                    { (yyval.mdefs) = autorelease(new ModuleDefs()); (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2892 "hexpr.parse.C"
    break;

  case 11: /* defs: defs def  */
#line 500 "hexpr.y"
                    { (yyval.mdefs) = (yyvsp[-1].mdefs);                            (yyval.mdefs)->push_back(ModuleDefPtr((yyvsp[0].mdef))); }
#line 2898 "hexpr.parse.C"
    break;

  case 12: /* def: importdef  */
#line 502 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2904 "hexpr.parse.C"
    break;

  case 13: /* def: tydef  */
#line 503 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2910 "hexpr.parse.C"
    break;

  case 14: /* def: vartybind  */
#line 504 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mvtydef); }
#line 2916 "hexpr.parse.C"
    break;

  case 15: /* def: classdef  */
#line 505 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2922 "hexpr.parse.C"
    break;

  case 16: /* def: instdef  */
#line 506 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2928 "hexpr.parse.C"
    break;

  case 17: /* def: pragmadef  */
#line 507 "hexpr.y"
               { (yyval.mdef) = (yyvsp[0].mdef); }
#line 2934 "hexpr.parse.C"
    break;

  case 18: /* def: id "=" l0expr  */
#line 509 "hexpr.y"
                   { (yyval.mdef) = new MVarDef(list(*(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 2940 "hexpr.parse.C"
    break;

  case 19: /* def: id id "=" l0expr  */
#line 510 "hexpr.y"
                      { (yyval.mdef) = new MVarDef(list(*(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 2946 "hexpr.parse.C"
    break;

  case 20: /* def: id id id "=" l0expr  */
#line 511 "hexpr.y"
                         { (yyval.mdef) = new MVarDef(list(*(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 2952 "hexpr.parse.C"
    break;

  case 21: /* def: id id id id "=" l0expr  */
#line 512 "hexpr.y"
                            { (yyval.mdef) = new MVarDef(list(*(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 2958 "hexpr.parse.C"
    break;

  case 22: /* def: id id id id id "=" l0expr  */
#line 513 "hexpr.y"
                               { (yyval.mdef) = new MVarDef(list(*(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-6]), (yylsp[0]))); }
#line 2964 "hexpr.parse.C"
    break;

  case 23: /* def: id id id id id id "=" l0expr  */
#line 514 "hexpr.y"
                                  { (yyval.mdef) = new MVarDef(list(*(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 2970 "hexpr.parse.C"
    break;

  case 24: /* def: id id id id id id id "=" l0expr  */
#line 515 "hexpr.y"
                                     { (yyval.mdef) = new MVarDef(list(*(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-8]), (yylsp[0]))); }
#line 2976 "hexpr.parse.C"
    break;

  case 25: /* def: id id id id id id id id "=" l0expr  */
#line 516 "hexpr.y"
                                        { (yyval.mdef) = new MVarDef(list(*(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-9]), (yylsp[0]))); }
#line 2982 "hexpr.parse.C"
    break;

  case 26: /* def: id id id id id id id id id "=" l0expr  */
#line 517 "hexpr.y"
                                           { (yyval.mdef) = new MVarDef(list(*(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-10]), (yylsp[0]))); }
#line 2988 "hexpr.parse.C"
    break;

  case 27: /* def: id id id id id id id id id id "=" l0expr  */
#line 518 "hexpr.y"
                                              { (yyval.mdef) = new MVarDef(list(*(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-11]), (yylsp[0]))); }
#line 2994 "hexpr.parse.C"
    break;

  case 28: /* def: id id id id id id id id id id id "=" l0expr  */
#line 519 "hexpr.y"
                                                 { (yyval.mdef) = new MVarDef(list(*(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-12]), (yylsp[0]))); }
#line 3000 "hexpr.parse.C"
    break;

  case 29: /* def: id id id id id id id id id id id id "=" l0expr  */
#line 520 "hexpr.y"
                                                    { (yyval.mdef) = new MVarDef(list(*(yyvsp[-13].string), *(yyvsp[-12].string), *(yyvsp[-11].string), *(yyvsp[-10].string), *(yyvsp[-9].string), *(yyvsp[-8].string), *(yyvsp[-7].string), *(yyvsp[-6].string), *(yyvsp[-5].string), *(yyvsp[-4].string), *(yyvsp[-3].string), *(yyvsp[-2].string)), ExprPtr((yyvsp[0].exp)), m((yylsp[-13]), (yylsp[0]))); }
#line 3006 "hexpr.parse.C"
    break;

  case 30: /* def: l5expr  */
#line 523 "hexpr.y"
            { (yyval.mdef) = new MVarDef(list(freshName()), let(freshName(), ExprPtr((yyvsp[0].exp)), mktunit(m((yylsp[0]))), m((yylsp[0]))), m((yylsp[0]))); }
#line 3012 "hexpr.parse.C"
    break;

  case 31: /* importdef: "import" cppid  */
#line 526 "hexpr.y"
                          { (yyval.mdef) = new MImport(yyModulePath, *(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3018 "hexpr.parse.C"
    break;

  case 32: /* pragmadef: "{-#" pragmaty "#-}"  */
#line 529 "hexpr.y"
                                { (yyval.mdef) = (yyvsp[-1].mdef); }
#line 3024 "hexpr.parse.C"
    break;

  case 33: /* pragmaty: "UNSAFE" id  */
#line 530 "hexpr.y"
                      { (yyval.mdef) = new MUnsafePragmaDef(*(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3030 "hexpr.parse.C"
    break;

  case 34: /* pragmaty: "SAFE" id  */
#line 531 "hexpr.y"
                    { (yyval.mdef) = new MSafePragmaDef(*(yyvsp[0].string), m((yylsp[-1]), (yylsp[0]))); }
#line 3036 "hexpr.parse.C"
    break;

  case 35: /* tydef: "type" nameseq "=" qtype  */
#line 534 "hexpr.y"
                                { (yyval.mdef) = new MTypeDef(MTypeDef::Transparent, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 3042 "hexpr.parse.C"
    break;

  case 36: /* tydef: "data" nameseq "=" qtype  */
#line 535 "hexpr.y"
                                { (yyval.mdef) = new MTypeDef(MTypeDef::Opaque, hobbes::select(*(yyvsp[-2].strings), 0), hobbes::select(*(yyvsp[-2].strings), 1, (int)(yyvsp[-2].strings)->size()), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-3]), (yylsp[0]))); }
#line 3048 "hexpr.parse.C"
    break;

  case 37: /* vartybind: name "::" qtype  */
#line 538 "hexpr.y"
                           { (yyval.mvtydef) = new MVarTypeDef(*(yyvsp[-2].string), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]), (yylsp[0]))); }
#line 3054 "hexpr.parse.C"
    break;

  case 38: /* vardef: names "=" l0expr  */
#line 540 "hexpr.y"
                         { (yyval.mvdef) = new MVarDef(*(yyvsp[-2].strings), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3060 "hexpr.parse.C"
    break;

  case 39: /* classdef: "class" cst "=>" id names  */
#line 543 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-3].tconstraints), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3066 "hexpr.parse.C"
    break;

  case 40: /* classdef: "class" cst "=>" id names "|" fundeps  */
#line 544 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-6]), (yylsp[0]))); wantIndent(false); }
#line 3072 "hexpr.parse.C"
    break;

  case 41: /* classdef: "class" cst "=>" id names "where" cmembers  */
#line 545 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-5].tconstraints), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 3078 "hexpr.parse.C"
    break;

  case 42: /* classdef: "class" cst "=>" id names "|" fundeps "where" cmembers  */
#line 546 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(*(yyvsp[-7].tconstraints), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-8]), (yylsp[0])));            wantIndent(false); }
#line 3084 "hexpr.parse.C"
    break;

  case 43: /* classdef: "class" id names  */
#line 547 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].strings), CFunDepDefs(), MVarTypeDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 3090 "hexpr.parse.C"
    break;

  case 44: /* classdef: "class" id names "|" fundeps  */
#line 548 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), *(yyvsp[0].fundeps),           MVarTypeDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3096 "hexpr.parse.C"
    break;

  case 45: /* classdef: "class" id names "where" cmembers  */
#line 549 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].strings), CFunDepDefs(), *(yyvsp[0].mvtydefs), m((yylsp[-4]), (yylsp[0])));            wantIndent(false); }
#line 3102 "hexpr.parse.C"
    break;

  case 46: /* classdef: "class" id names "|" fundeps "where" cmembers  */
#line 550 "hexpr.y"
                                                                 { (yyval.mdef) = new ClassDef(Constraints(), *(yyvsp[-5].string), *(yyvsp[-4].strings), *(yyvsp[-2].fundeps),           *(yyvsp[0].mvtydefs), m((yylsp[-6]), (yylsp[0])));            wantIndent(false); }
#line 3108 "hexpr.parse.C"
    break;

  case 47: /* fundeps: fundep  */
#line 552 "hexpr.y"
                            { (yyval.fundeps) = autorelease(new CFunDepDefs()); (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 3114 "hexpr.parse.C"
    break;

  case 48: /* fundeps: fundeps "," fundep  */
#line 553 "hexpr.y"
                            { (yyval.fundeps) = (yyvsp[-2].fundeps);                             (yyval.fundeps)->push_back(*(yyvsp[0].fundep)); }
#line 3120 "hexpr.parse.C"
    break;

  case 49: /* fundep: idseq "->" idseq  */
#line 555 "hexpr.y"
                         { (yyval.fundep) = autorelease(new CFunDepDef(*(yyvsp[-2].strings), *(yyvsp[0].strings))); }
#line 3126 "hexpr.parse.C"
    break;

  case 50: /* cmembers: cmember  */
#line 557 "hexpr.y"
                           { (yyval.mvtydefs) = autorelease(new MVarTypeDefs()); (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 3132 "hexpr.parse.C"
    break;

  case 51: /* cmembers: cmembers cmember  */
#line 558 "hexpr.y"
                           { (yyval.mvtydefs) = (yyvsp[-1].mvtydefs);                              (yyval.mvtydefs)->push_back(MVarTypeDefPtr((yyvsp[0].mvtydef))); }
#line 3138 "hexpr.parse.C"
    break;

  case 52: /* cmember: "indent" vartybind  */
#line 560 "hexpr.y"
                            { (yyval.mvtydef) = (yyvsp[0].mvtydef); }
#line 3144 "hexpr.parse.C"
    break;

  case 53: /* instdef: "instance" id types  */
#line 563 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-2]), (yylsp[0]))); wantIndent(false); }
#line 3150 "hexpr.parse.C"
    break;

  case 54: /* instdef: "instance" cst "=>" id types  */
#line 564 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(*(yyvsp[-3].tconstraints),           *(yyvsp[-1].string), *(yyvsp[0].mtypes), MVarDefs(), m((yylsp[-4]), (yylsp[0]))); wantIndent(false); }
#line 3156 "hexpr.parse.C"
    break;

  case 55: /* instdef: "instance" id types "where" imembers  */
#line 565 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(Constraints(), *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-4]), (yylsp[0])));        wantIndent(false); }
#line 3162 "hexpr.parse.C"
    break;

  case 56: /* instdef: "instance" cst "=>" id types "where" imembers  */
#line 566 "hexpr.y"
                                                       { (yyval.mdef) = new InstanceDef(*(yyvsp[-5].tconstraints),           *(yyvsp[-3].string), *(yyvsp[-2].mtypes), *(yyvsp[0].mvdefs), m((yylsp[-6]), (yylsp[0])));        wantIndent(false); }
#line 3168 "hexpr.parse.C"
    break;

  case 57: /* imembers: imember  */
#line 568 "hexpr.y"
                           { (yyval.mvdefs) = autorelease(new MVarDefs()); (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 3174 "hexpr.parse.C"
    break;

  case 58: /* imembers: imembers imember  */
#line 569 "hexpr.y"
                           { (yyval.mvdefs) = (yyvsp[-1].mvdefs);                          (yyval.mvdefs)->push_back(MVarDefPtr((yyvsp[0].mvdef))); }
#line 3180 "hexpr.parse.C"
    break;

  case 59: /* imember: "indent" vardef  */
#line 571 "hexpr.y"
                         { (yyval.mvdef) = (yyvsp[0].mvdef); }
#line 3186 "hexpr.parse.C"
    break;

  case 60: /* names: nameseq  */
#line 574 "hexpr.y"
               { (yyval.strings) = (yyvsp[0].strings); }
#line 3192 "hexpr.parse.C"
    break;

  case 61: /* names: id opname id  */
#line 576 "hexpr.y"
                    { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[-1].string)); (yyval.strings)->push_back(*(yyvsp[-2].string)); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3198 "hexpr.parse.C"
    break;

  case 62: /* nameseq: name  */
#line 578 "hexpr.y"
                      { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3204 "hexpr.parse.C"
    break;

  case 63: /* nameseq: nameseq name  */
#line 579 "hexpr.y"
                      { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3210 "hexpr.parse.C"
    break;

  case 64: /* name: id  */
#line 581 "hexpr.y"
         { (yyval.string) = (yyvsp[0].string); }
#line 3216 "hexpr.parse.C"
    break;

  case 65: /* name: "(" opname ")"  */
#line 583 "hexpr.y"
                     { (yyval.string) = (yyvsp[-1].string); }
#line 3222 "hexpr.parse.C"
    break;

  case 66: /* opname: "and"  */
#line 585 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("and")); }
#line 3228 "hexpr.parse.C"
    break;

  case 67: /* opname: "or"  */
#line 586 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("or")); }
#line 3234 "hexpr.parse.C"
    break;

  case 68: /* opname: "o"  */
#line 587 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("compose")); }
#line 3240 "hexpr.parse.C"
    break;

  case 69: /* opname: "."  */
#line 588 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("compose")); }
#line 3246 "hexpr.parse.C"
    break;

  case 70: /* opname: "~"  */
#line 589 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("~")); }
#line 3252 "hexpr.parse.C"
    break;

  case 71: /* opname: "=~"  */
#line 590 "hexpr.y"
               { (yyval.string) = autorelease(new std::string("=~")); }
#line 3258 "hexpr.parse.C"
    break;

  case 72: /* opname: "==="  */
#line 591 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("===")); }
#line 3264 "hexpr.parse.C"
    break;

  case 73: /* opname: "=="  */
#line 592 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("==")); }
#line 3270 "hexpr.parse.C"
    break;

  case 74: /* opname: "<"  */
#line 593 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("<")); }
#line 3276 "hexpr.parse.C"
    break;

  case 75: /* opname: "<="  */
#line 594 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("<=")); }
#line 3282 "hexpr.parse.C"
    break;

  case 76: /* opname: ">"  */
#line 595 "hexpr.y"
              { (yyval.string) = autorelease(new std::string(">")); }
#line 3288 "hexpr.parse.C"
    break;

  case 77: /* opname: ">="  */
#line 596 "hexpr.y"
              { (yyval.string) = autorelease(new std::string(">=")); }
#line 3294 "hexpr.parse.C"
    break;

  case 78: /* opname: "in"  */
#line 597 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("in")); }
#line 3300 "hexpr.parse.C"
    break;

  case 79: /* opname: "++"  */
#line 598 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("append")); }
#line 3306 "hexpr.parse.C"
    break;

  case 80: /* opname: "+"  */
#line 599 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("+")); }
#line 3312 "hexpr.parse.C"
    break;

  case 81: /* opname: "-"  */
#line 600 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("-")); }
#line 3318 "hexpr.parse.C"
    break;

  case 82: /* opname: "*"  */
#line 601 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("*")); }
#line 3324 "hexpr.parse.C"
    break;

  case 83: /* opname: "/"  */
#line 602 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("/")); }
#line 3330 "hexpr.parse.C"
    break;

  case 84: /* opname: "%"  */
#line 603 "hexpr.y"
              { (yyval.string) = autorelease(new std::string("%")); }
#line 3336 "hexpr.parse.C"
    break;

  case 85: /* idseq: id  */
#line 605 "hexpr.y"
                { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3342 "hexpr.parse.C"
    break;

  case 86: /* idseq: idseq id  */
#line 606 "hexpr.y"
                { (yyval.strings) = (yyvsp[-1].strings);                          (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 3348 "hexpr.parse.C"
    break;

  case 87: /* types: l0mtype  */
#line 608 "hexpr.y"
                     { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3354 "hexpr.parse.C"
    break;

  case 88: /* types: types l0mtype  */
#line 609 "hexpr.y"
                     { (yyval.mtypes) = (yyvsp[-1].mtypes);                           (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 3360 "hexpr.parse.C"
    break;

  case 89: /* l0expr: "\\" patterns "." l0expr  */
#line 612 "hexpr.y"
                                 { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3366 "hexpr.parse.C"
    break;

  case 90: /* l0expr: "fn" patterns "." l0expr  */
#line 613 "hexpr.y"
                                 { (yyval.exp) = makePatternFn(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3372 "hexpr.parse.C"
    break;

  case 91: /* l0expr: lhexpr "<-" lhexpr  */
#line 614 "hexpr.y"
                                 { (yyval.exp) = new Assign(ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-2]), (yylsp[0]))); }
#line 3378 "hexpr.parse.C"
    break;

  case 92: /* l0expr: lhexpr  */
#line 615 "hexpr.y"
                                 { (yyval.exp) = (yyvsp[0].exp); }
#line 3384 "hexpr.parse.C"
    break;

  case 93: /* lhexpr: "!" l1expr  */
#line 617 "hexpr.y"
                                 { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), (yyvsp[0].exp), m((yylsp[-1]),(yylsp[0]))); }
#line 3390 "hexpr.parse.C"
    break;

  case 94: /* lhexpr: lhexpr "and" lhexpr  */
#line 618 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("and",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3396 "hexpr.parse.C"
    break;

  case 95: /* lhexpr: lhexpr "or" lhexpr  */
#line 619 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("or",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3402 "hexpr.parse.C"
    break;

  case 96: /* lhexpr: lhexpr "o" lhexpr  */
#line 620 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("compose",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3408 "hexpr.parse.C"
    break;

  case 97: /* lhexpr: l1expr "in" l1expr  */
#line 621 "hexpr.y"
                                 { (yyval.exp) = TAPP2(var("in",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3414 "hexpr.parse.C"
    break;

  case 98: /* lhexpr: l1expr  */
#line 622 "hexpr.y"
                                 { (yyval.exp) = (yyvsp[0].exp); }
#line 3420 "hexpr.parse.C"
    break;

  case 99: /* l1expr: "if" l0expr "then" l0expr "else" l0expr  */
#line 624 "hexpr.y"
                                                { (yyval.exp) = TAPP3(var("if",m((yylsp[-5]))), (yyvsp[-4].exp), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-5]), (yylsp[0]))); }
#line 3426 "hexpr.parse.C"
    break;

  case 100: /* l1expr: l2expr  */
#line 625 "hexpr.y"
                                                { (yyval.exp) = (yyvsp[0].exp); }
#line 3432 "hexpr.parse.C"
    break;

  case 101: /* l2expr: l2expr "~" l2expr  */
#line 627 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("~",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3438 "hexpr.parse.C"
    break;

  case 102: /* l2expr: l2expr "===" l2expr  */
#line 628 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("===",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3444 "hexpr.parse.C"
    break;

  case 103: /* l2expr: l2expr "==" l2expr  */
#line 629 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3450 "hexpr.parse.C"
    break;

  case 104: /* l2expr: l2expr "!=" l2expr  */
#line 630 "hexpr.y"
                            { (yyval.exp) = TAPP1(var("not",m((yylsp[-1]))), TAPP2(var("==",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))), m((yylsp[-2]),(yylsp[0]))); }
#line 3456 "hexpr.parse.C"
    break;

  case 105: /* l2expr: l2expr "<" l2expr  */
#line 631 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("<",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3462 "hexpr.parse.C"
    break;

  case 106: /* l2expr: l2expr "<=" l2expr  */
#line 632 "hexpr.y"
                            { (yyval.exp) = TAPP2(var("<=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3468 "hexpr.parse.C"
    break;

  case 107: /* l2expr: l2expr ">" l2expr  */
#line 633 "hexpr.y"
                            { (yyval.exp) = TAPP2(var(">",m((yylsp[-1]))),  (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3474 "hexpr.parse.C"
    break;

  case 108: /* l2expr: l2expr ">=" l2expr  */
#line 634 "hexpr.y"
                            { (yyval.exp) = TAPP2(var(">=",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3480 "hexpr.parse.C"
    break;

  case 109: /* l2expr: l3expr  */
#line 635 "hexpr.y"
                            { (yyval.exp) = (yyvsp[0].exp); }
#line 3486 "hexpr.parse.C"
    break;

  case 110: /* l3expr: l3expr "+" l3expr  */
#line 637 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("+",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3492 "hexpr.parse.C"
    break;

  case 111: /* l3expr: l3expr "-" l3expr  */
#line 638 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("-",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3498 "hexpr.parse.C"
    break;

  case 112: /* l3expr: l3expr "++" l3expr  */
#line 639 "hexpr.y"
                           { (yyval.exp) = TAPP2(var("append",m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]),(yylsp[0]))); }
#line 3504 "hexpr.parse.C"
    break;

  case 113: /* l3expr: "-" l3expr  */
#line 640 "hexpr.y"
                           { (yyval.exp) = TAPP1(var("neg",m((yylsp[-1]))), ExprPtr((yyvsp[0].exp)), m((yylsp[-1]),(yylsp[0]))); }
#line 3510 "hexpr.parse.C"
    break;

  case 114: /* l3expr: l4expr  */
#line 641 "hexpr.y"
                           { (yyval.exp) = (yyvsp[0].exp); }
#line 3516 "hexpr.parse.C"
    break;

  case 115: /* l4expr: l4expr "*" l4expr  */
#line 643 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("*", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3522 "hexpr.parse.C"
    break;

  case 116: /* l4expr: l4expr "/" l4expr  */
#line 644 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("/", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3528 "hexpr.parse.C"
    break;

  case 117: /* l4expr: l4expr "%" l4expr  */
#line 645 "hexpr.y"
                          { (yyval.exp) = TAPP2(var("%", m((yylsp[-1]))), (yyvsp[-2].exp), (yyvsp[0].exp), m((yylsp[-2]), (yylsp[0]))); }
#line 3534 "hexpr.parse.C"
    break;

  case 118: /* l4expr: l5expr  */
#line 646 "hexpr.y"
                          { (yyval.exp) = (yyvsp[0].exp); }
#line 3540 "hexpr.parse.C"
    break;

  case 119: /* l5expr: l6expr  */
#line 648 "hexpr.y"
               { (yyval.exp) = (yyvsp[0].exp); }
#line 3546 "hexpr.parse.C"
    break;

  case 120: /* l5expr: "let" letbindings "in" l0expr  */
#line 651 "hexpr.y"
                                      { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-2].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3552 "hexpr.parse.C"
    break;

  case 121: /* l5expr: "let" letbindings ";" "in" l0expr  */
#line 652 "hexpr.y"
                                          { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[0].exp)), m((yylsp[-4]),(yylsp[0])))->clone(); }
#line 3558 "hexpr.parse.C"
    break;

  case 122: /* l5expr: "match" l6exprs "with" patternexps  */
#line 655 "hexpr.y"
                                           { (yyval.exp) = compileMatch(yyParseCC, *(yyvsp[-2].exps), normPatternRules(*(yyvsp[0].patternexps), m((yylsp[-3]),(yylsp[0]))), m((yylsp[-3]),(yylsp[0])))->clone(); }
#line 3564 "hexpr.parse.C"
    break;

  case 123: /* l5expr: l6expr "matches" pattern  */
#line 658 "hexpr.y"
                                 { (yyval.exp) = compileMatchTest(yyParseCC, ExprPtr((yyvsp[-2].exp)), PatternPtr((yyvsp[0].pattern)), m((yylsp[-2]),(yylsp[0])))->clone(); }
#line 3570 "hexpr.parse.C"
    break;

  case 124: /* l5expr: "parse" "{" prules "}"  */
#line 661 "hexpr.y"
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
#line 3585 "hexpr.parse.C"
    break;

  case 125: /* l5expr: "do" "{" dobindings "}"  */
#line 673 "hexpr.y"
                                                { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-1].letbindings), ExprPtr(new Unit(m((yylsp[-3]),(yylsp[0])))), m((yylsp[-3]),(yylsp[0]))); }
#line 3591 "hexpr.parse.C"
    break;

  case 126: /* l5expr: "do" "{" dobindings "return" l0expr "}"  */
#line 674 "hexpr.y"
                                                { (yyval.exp) = compileNestedLetMatch(*(yyvsp[-3].letbindings), ExprPtr((yyvsp[-1].exp)), m((yylsp[-5]),(yylsp[0]))); }
#line 3597 "hexpr.parse.C"
    break;

  case 127: /* l5expr: l6expr "::" qtype  */
#line 677 "hexpr.y"
                                { (yyval.exp) = new Assump(ExprPtr((yyvsp[-2].exp)), QualTypePtr((yyvsp[0].qualtype)), m((yylsp[-2]),(yylsp[0]))); }
#line 3603 "hexpr.parse.C"
    break;

  case 128: /* letbindings: letbindings ";" letbinding  */
#line 679 "hexpr.y"
                                        { (yyvsp[-2].letbindings)->push_back(*(yyvsp[0].letbinding)); (yyval.letbindings) = (yyvsp[-2].letbindings); }
#line 3609 "hexpr.parse.C"
    break;

  case 129: /* letbindings: letbinding  */
#line 680 "hexpr.y"
                                        { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3615 "hexpr.parse.C"
    break;

  case 130: /* letbinding: irrefutablep "=" l1expr  */
#line 682 "hexpr.y"
                                    { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-2].pattern)), ExprPtr((yyvsp[0].exp)))); }
#line 3621 "hexpr.parse.C"
    break;

  case 131: /* dobindings: dobindings dobinding  */
#line 684 "hexpr.y"
                                 { (yyval.letbindings) = (yyvsp[-1].letbindings); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3627 "hexpr.parse.C"
    break;

  case 132: /* dobindings: dobinding  */
#line 685 "hexpr.y"
                                 { (yyval.letbindings) = autorelease(new LetBindings()); (yyval.letbindings)->push_back(*(yyvsp[0].letbinding)); }
#line 3633 "hexpr.parse.C"
    break;

  case 133: /* dobinding: irrefutablep "=" l0expr ";"  */
#line 687 "hexpr.y"
                                       { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr((yyvsp[-3].pattern)), ExprPtr((yyvsp[-1].exp)))); }
#line 3639 "hexpr.parse.C"
    break;

  case 134: /* dobinding: l0expr ";"  */
#line 688 "hexpr.y"
                                       { (yyval.letbinding) = autorelease(new LetBinding(PatternPtr(new MatchAny("_",m((yylsp[-1])))), ExprPtr((yyvsp[-1].exp)))); }
#line 3645 "hexpr.parse.C"
    break;

  case 135: /* cselconds: cselconds "," lhexpr  */
#line 690 "hexpr.y"
                                { (yyval.exps) = (yyvsp[-2].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3651 "hexpr.parse.C"
    break;

  case 136: /* cselconds: lhexpr  */
#line 691 "hexpr.y"
                                { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 3657 "hexpr.parse.C"
    break;

  case 137: /* cselection: pattern "<-" l0expr "," cselconds  */
#line 693 "hexpr.y"
                                              { (yyval.cselection) = new CSelection(); (yyval.cselection)->pat = PatternPtr((yyvsp[-4].pattern)); (yyval.cselection)->seq = ExprPtr((yyvsp[-2].exp)); (yyval.cselection)->conds = *(yyvsp[0].exps); }
#line 3663 "hexpr.parse.C"
    break;

  case 138: /* cselection: pattern "<-" l0expr  */
#line 694 "hexpr.y"
                                              { (yyval.cselection) = new CSelection(); (yyval.cselection)->pat = PatternPtr((yyvsp[-2].pattern)); (yyval.cselection)->seq = ExprPtr((yyvsp[0].exp)); }
#line 3669 "hexpr.parse.C"
    break;

  case 139: /* cselections: cselections "|" cselection  */
#line 696 "hexpr.y"
                                        { (yyval.cselections) = (yyvsp[-2].cselections); (yyval.cselections)->push_back(CSelectionPtr((yyvsp[0].cselection))); }
#line 3675 "hexpr.parse.C"
    break;

  case 140: /* cselections: cselection  */
#line 697 "hexpr.y"
                                        { (yyval.cselections) = autorelease(new CSelections()); (yyval.cselections)->push_back(CSelectionPtr((yyvsp[0].cselection))); }
#line 3681 "hexpr.parse.C"
    break;

  case 141: /* l6expr: l6expr "(" cargs ")"  */
#line 700 "hexpr.y"
                                { (yyval.exp) = new App(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].exps), m((yylsp[-3]), (yylsp[0]))); }
#line 3687 "hexpr.parse.C"
    break;

  case 142: /* l6expr: id  */
#line 701 "hexpr.y"
                                { (yyval.exp) = varCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 3693 "hexpr.parse.C"
    break;

  case 143: /* l6expr: "[" l0expr ".." l0expr "]"  */
#line 704 "hexpr.y"
                                                          { (yyval.exp) = new App(var("range", m((yylsp[-2]))), list(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]), (yylsp[0]))); }
#line 3699 "hexpr.parse.C"
    break;

  case 144: /* l6expr: "[" l0expr ".." "]"  */
#line 705 "hexpr.y"
                                                          { (yyval.exp) = new App(var("iterateS", m((yylsp[-1]))), list(ExprPtr((yyvsp[-2].exp)), fn(str::strings(".x"), fncall(var("+", m((yylsp[-1]))), list(var(".x", m((yylsp[-1]))), ExprPtr(new Int(1, m((yylsp[-1]))))), m((yylsp[-1]))), m((yylsp[-1])))), m((yylsp[-3]), (yylsp[0]))); }
#line 3705 "hexpr.parse.C"
    break;

  case 145: /* l6expr: "[" l0expr "|" cselections "]"  */
#line 706 "hexpr.y"
                                                          { (yyval.exp) = desugarComprehension(yyParseCC, ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].cselections), m((yylsp[-4]), (yylsp[0]))); }
#line 3711 "hexpr.parse.C"
    break;

  case 146: /* l6expr: "[" cargs "]"  */
#line 707 "hexpr.y"
                                                          { (yyval.exp) = new MkArray(*(yyvsp[-1].exps), m((yylsp[-2]), (yylsp[0]))); }
#line 3717 "hexpr.parse.C"
    break;

  case 147: /* l6expr: l6expr "[" "timeV" "]"  */
#line 708 "hexpr.y"
                                                          { (yyval.exp) = maybeArraySliceWithTime(ExprPtr((yyvsp[-3].exp)), *(yyvsp[-1].string), m((yylsp[-3]), (yylsp[0]))); }
#line 3723 "hexpr.parse.C"
    break;

  case 148: /* l6expr: l6expr "[" l0expr "]"  */
#line 709 "hexpr.y"
                                                          { (yyval.exp) = mkAIndex(ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-3]), (yylsp[0]))); }
#line 3729 "hexpr.parse.C"
    break;

  case 149: /* l6expr: l6expr "[" l0expr ":" l0expr "]"  */
#line 710 "hexpr.y"
                                                          { (yyval.exp) = new App(var("slice", m((yylsp[-2]))), list(ExprPtr((yyvsp[-5].exp)), ExprPtr((yyvsp[-3].exp)), ExprPtr((yyvsp[-1].exp))), m((yylsp[-5]), (yylsp[0]))); }
#line 3735 "hexpr.parse.C"
    break;

  case 150: /* l6expr: l6expr "[" l0expr ":" "]"  */
#line 711 "hexpr.y"
                                                          { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-1]))), list(var(vn,m((yylsp[-4]))), ExprPtr((yyvsp[-2].exp)), fncall(var("size",m((yylsp[-1]))), list(var(vn,m((yylsp[-4])))),m((yylsp[-4])))),m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3741 "hexpr.parse.C"
    break;

  case 151: /* l6expr: l6expr "[" ":" l0expr "]"  */
#line 712 "hexpr.y"
                                                          { std::string vn = freshName(); (yyval.exp) = new Let(vn, ExprPtr((yyvsp[-4].exp)), fncall(var("slice",m((yylsp[-2]))), list(var(vn,m((yylsp[-4]))), fncall(var("size",m((yylsp[-2]))), list(var(vn,m((yylsp[-2])))),m((yylsp[-4]))), ExprPtr((yyvsp[-1].exp))), m((yylsp[-4]),(yylsp[0]))), m((yylsp[-4]), (yylsp[0]))); }
#line 3747 "hexpr.parse.C"
    break;

  case 152: /* l6expr: "|" id "=" l0expr "|"  */
#line 715 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(*(yyvsp[-3].string), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3753 "hexpr.parse.C"
    break;

  case 153: /* l6expr: "|" "intV" "=" l0expr "|"  */
#line 716 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(".f" + str::from((yyvsp[-3].intv)), ExprPtr((yyvsp[-1].exp)), m((yylsp[-4]), (yylsp[0]))); }
#line 3759 "hexpr.parse.C"
    break;

  case 154: /* l6expr: "|" id "|"  */
#line 717 "hexpr.y"
                                                              { (yyval.exp) = new MkVariant(*(yyvsp[-1].string), ExprPtr(new Unit(m((yylsp[-1])))), m((yylsp[-2]), (yylsp[0]))); }
#line 3765 "hexpr.parse.C"
    break;

  case 155: /* l6expr: "case" l0expr "of" "|" varfields "|"  */
#line 718 "hexpr.y"
                                                              { (yyval.exp) = new Case(ExprPtr((yyvsp[-4].exp)), *(yyvsp[-1].vfields), m((yylsp[-5]), (yylsp[0]))); }
#line 3771 "hexpr.parse.C"
    break;

  case 156: /* l6expr: "case" l0expr "of" "|" varfields "|" "default" l0expr  */
#line 719 "hexpr.y"
                                                              { (yyval.exp) = new Case(ExprPtr((yyvsp[-6].exp)), *(yyvsp[-3].vfields), ExprPtr((yyvsp[0].exp)), m((yylsp[-7]), (yylsp[0]))); }
#line 3777 "hexpr.parse.C"
    break;

  case 157: /* l6expr: "{" recfields "}"  */
#line 722 "hexpr.y"
                              { if ((yyvsp[-1].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-1].rfields), m((yylsp[-2]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-2]), (yylsp[0]))); } }
#line 3783 "hexpr.parse.C"
    break;

  case 158: /* l6expr: "{" recfields "," "}"  */
#line 723 "hexpr.y"
                              { if ((yyvsp[-2].rfields)->size() > 0) { (yyval.exp) = new MkRecord(*(yyvsp[-2].rfields), m((yylsp[-3]), (yylsp[0]))); } else { (yyval.exp) = new Unit(m((yylsp[-3]), (yylsp[0]))); } }
#line 3789 "hexpr.parse.C"
    break;

  case 159: /* l6expr: l6expr recfieldpath  */
#line 724 "hexpr.y"
                              { (yyval.exp) = makeProjSeq((yyvsp[-1].exp), *(yyvsp[0].strings), m((yylsp[-1]), (yylsp[0]))); }
#line 3795 "hexpr.parse.C"
    break;

  case 160: /* l6expr: recfieldpath  */
#line 727 "hexpr.y"
                     { (yyval.exp) = new Fn(str::strings("x"), proj(var("x", m((yylsp[0]))), *(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 3801 "hexpr.parse.C"
    break;

  case 161: /* l6expr: "regexV"  */
#line 730 "hexpr.y"
                 { (yyval.exp) = compileRegexFn(yyParseCC, std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0])))->clone(); }
#line 3807 "hexpr.parse.C"
    break;

  case 162: /* l6expr: "pack" l6expr  */
#line 733 "hexpr.y"
                                           { (yyval.exp) = new Pack(ExprPtr((yyvsp[0].exp)), m((yylsp[-1]), (yylsp[0]))); }
#line 3813 "hexpr.parse.C"
    break;

  case 163: /* l6expr: "unpack" id "=" l6expr "in" l6expr  */
#line 734 "hexpr.y"
                                           { (yyval.exp) = new Unpack(*(yyvsp[-4].string), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)), m((yylsp[-5]), (yylsp[0]))); }
#line 3819 "hexpr.parse.C"
    break;

  case 164: /* l6expr: "boolV"  */
#line 737 "hexpr.y"
                    { (yyval.exp) = new Bool((yyvsp[0].boolv), m((yylsp[0]))); }
#line 3825 "hexpr.parse.C"
    break;

  case 165: /* l6expr: "charV"  */
#line 738 "hexpr.y"
                    { (yyval.exp) = new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3831 "hexpr.parse.C"
    break;

  case 166: /* l6expr: "byteV"  */
#line 739 "hexpr.y"
                    { (yyval.exp) = new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3837 "hexpr.parse.C"
    break;

  case 167: /* l6expr: "bytesV"  */
#line 740 "hexpr.y"
                    { (yyval.exp) = mkarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 3843 "hexpr.parse.C"
    break;

  case 168: /* l6expr: "shortV"  */
#line 741 "hexpr.y"
                    { (yyval.exp) = new Short((yyvsp[0].shortv), m((yylsp[0]))); }
#line 3849 "hexpr.parse.C"
    break;

  case 169: /* l6expr: "intV"  */
#line 742 "hexpr.y"
                    { (yyval.exp) = new Int((yyvsp[0].intv), m((yylsp[0]))); }
#line 3855 "hexpr.parse.C"
    break;

  case 170: /* l6expr: "longV"  */
#line 743 "hexpr.y"
                    { (yyval.exp) = new Long((yyvsp[0].longv), m((yylsp[0]))); }
#line 3861 "hexpr.parse.C"
    break;

  case 171: /* l6expr: "int128V"  */
#line 744 "hexpr.y"
                    { (yyval.exp) = new Int128((yyvsp[0].int128v), m((yylsp[0]))); }
#line 3867 "hexpr.parse.C"
    break;

  case 172: /* l6expr: "floatV"  */
#line 745 "hexpr.y"
                    { (yyval.exp) = new Float((yyvsp[0].floatv), m((yylsp[0]))); }
#line 3873 "hexpr.parse.C"
    break;

  case 173: /* l6expr: "doubleV"  */
#line 746 "hexpr.y"
                    { (yyval.exp) = new Double((yyvsp[0].doublev), m((yylsp[0]))); }
#line 3879 "hexpr.parse.C"
    break;

  case 174: /* l6expr: "stringV"  */
#line 747 "hexpr.y"
                    { (yyval.exp) = mkarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 3885 "hexpr.parse.C"
    break;

  case 175: /* l6expr: tsseq  */
#line 748 "hexpr.y"
                    { (yyval.exp) = mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0])))->clone(); }
#line 3891 "hexpr.parse.C"
    break;

  case 176: /* l6expr: "timeV"  */
#line 749 "hexpr.y"
                    { (yyval.exp) = mkTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3897 "hexpr.parse.C"
    break;

  case 177: /* l6expr: "dateTimeV"  */
#line 750 "hexpr.y"
                    { (yyval.exp) = mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0])))->clone(); }
#line 3903 "hexpr.parse.C"
    break;

  case 178: /* l6expr: "(" cargs ")"  */
#line 753 "hexpr.y"
                      { (yyval.exp) = pickNestedExp((yyvsp[-1].exps), m((yylsp[-2]),(yylsp[0]))); }
#line 3909 "hexpr.parse.C"
    break;

  case 179: /* l6expr: "(" "++" ")"  */
#line 756 "hexpr.y"
                      { (yyval.exp) = new Var("append", m((yylsp[-1]))); }
#line 3915 "hexpr.parse.C"
    break;

  case 180: /* l6expr: "(" "+" ")"  */
#line 757 "hexpr.y"
                      { (yyval.exp) = new Var("+",      m((yylsp[-1]))); }
#line 3921 "hexpr.parse.C"
    break;

  case 181: /* l6expr: "(" "-" ")"  */
#line 758 "hexpr.y"
                      { (yyval.exp) = new Var("-",      m((yylsp[-1]))); }
#line 3927 "hexpr.parse.C"
    break;

  case 182: /* l6expr: "(" "*" ")"  */
#line 759 "hexpr.y"
                      { (yyval.exp) = new Var("*",      m((yylsp[-1]))); }
#line 3933 "hexpr.parse.C"
    break;

  case 183: /* l6expr: "(" "/" ")"  */
#line 760 "hexpr.y"
                      { (yyval.exp) = new Var("/",      m((yylsp[-1]))); }
#line 3939 "hexpr.parse.C"
    break;

  case 184: /* l6expr: "(" "%" ")"  */
#line 761 "hexpr.y"
                      { (yyval.exp) = new Var("%",      m((yylsp[-1]))); }
#line 3945 "hexpr.parse.C"
    break;

  case 185: /* l6expr: "(" "~" ")"  */
#line 762 "hexpr.y"
                      { (yyval.exp) = new Var("~",      m((yylsp[-1]))); }
#line 3951 "hexpr.parse.C"
    break;

  case 186: /* l6expr: "(" "===" ")"  */
#line 763 "hexpr.y"
                      { (yyval.exp) = new Var("===",    m((yylsp[-1]))); }
#line 3957 "hexpr.parse.C"
    break;

  case 187: /* l6expr: "(" "==" ")"  */
#line 764 "hexpr.y"
                      { (yyval.exp) = new Var("==",     m((yylsp[-1]))); }
#line 3963 "hexpr.parse.C"
    break;

  case 188: /* l6expr: "(" "!=" ")"  */
#line 765 "hexpr.y"
                      { (yyval.exp) = new Var("!=",     m((yylsp[-1]))); }
#line 3969 "hexpr.parse.C"
    break;

  case 189: /* l6expr: "(" "<" ")"  */
#line 766 "hexpr.y"
                      { (yyval.exp) = new Var("<",      m((yylsp[-1]))); }
#line 3975 "hexpr.parse.C"
    break;

  case 190: /* l6expr: "(" ">" ")"  */
#line 767 "hexpr.y"
                      { (yyval.exp) = new Var(">",      m((yylsp[-1]))); }
#line 3981 "hexpr.parse.C"
    break;

  case 191: /* l6expr: "(" ">=" ")"  */
#line 768 "hexpr.y"
                      { (yyval.exp) = new Var(">=",     m((yylsp[-1]))); }
#line 3987 "hexpr.parse.C"
    break;

  case 192: /* l6expr: "(" "<=" ")"  */
#line 769 "hexpr.y"
                      { (yyval.exp) = new Var("<=",     m((yylsp[-1]))); }
#line 3993 "hexpr.parse.C"
    break;

  case 193: /* l6expr: "(" "and" ")"  */
#line 770 "hexpr.y"
                      { (yyval.exp) = new Var("and",    m((yylsp[-1]))); }
#line 3999 "hexpr.parse.C"
    break;

  case 194: /* l6expr: "(" "or" ")"  */
#line 771 "hexpr.y"
                      { (yyval.exp) = new Var("or",     m((yylsp[-1]))); }
#line 4005 "hexpr.parse.C"
    break;

  case 195: /* l6expr: "(" "in" ")"  */
#line 772 "hexpr.y"
                      { (yyval.exp) = new Var("in",     m((yylsp[-1]))); }
#line 4011 "hexpr.parse.C"
    break;

  case 196: /* l6expr: "(" "!" ")"  */
#line 773 "hexpr.y"
                      { (yyval.exp) = new Var("not",    m((yylsp[-1]))); }
#line 4017 "hexpr.parse.C"
    break;

  case 197: /* l6expr: "`" l0expr "`"  */
#line 776 "hexpr.y"
                       { (yyval.exp) = new Assump(fncall(var("unsafeCast", m((yylsp[-1]))), list(mktunit(m((yylsp[-1])))), m((yylsp[-1]))), qualtype(tapp(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp)))))), m((yylsp[-1]))); }
#line 4023 "hexpr.parse.C"
    break;

  case 198: /* prules: prules prule  */
#line 778 "hexpr.y"
                     { (yyval.prules) = (yyvsp[-1].prules); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 4029 "hexpr.parse.C"
    break;

  case 199: /* prules: prule  */
#line 779 "hexpr.y"
                     { (yyval.prules) = autorelease(new Grammar()); (yyval.prules)->push_back(*(yyvsp[0].prule)); }
#line 4035 "hexpr.parse.C"
    break;

  case 200: /* prule: id ":=" prdefs  */
#line 781 "hexpr.y"
                      { (yyval.prule) = autorelease(new Grammar::value_type(*(yyvsp[-2].string), *(yyvsp[0].prdefs))); }
#line 4041 "hexpr.parse.C"
    break;

  case 201: /* prdefs: prdefs "|" prdef  */
#line 783 "hexpr.y"
                         { (yyval.prdefs) = (yyvsp[-2].prdefs); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 4047 "hexpr.parse.C"
    break;

  case 202: /* prdefs: prdef  */
#line 784 "hexpr.y"
                         { (yyval.prdefs) = autorelease(new GrammarRules()); (yyval.prdefs)->push_back(*(yyvsp[0].prdef)); }
#line 4053 "hexpr.parse.C"
    break;

  case 203: /* prdef: pbelems "{" l0expr "}"  */
#line 786 "hexpr.y"
                              { (yyval.prdef) = autorelease(new GrammarRule(*(yyvsp[-3].pbelems), ExprPtr((yyvsp[-1].exp)))); }
#line 4059 "hexpr.parse.C"
    break;

  case 204: /* pbelems: pbelems pbelem  */
#line 788 "hexpr.y"
                        { (yyval.pbelems) = (yyvsp[-1].pbelems); (yyval.pbelems)->push_back(*(yyvsp[0].pbelem)); }
#line 4065 "hexpr.parse.C"
    break;

  case 205: /* pbelems: %empty  */
#line 789 "hexpr.y"
                        { (yyval.pbelems) = autorelease(new BoundGrammarValues()); }
#line 4071 "hexpr.parse.C"
    break;

  case 206: /* pbelem: id ":" pvalue  */
#line 791 "hexpr.y"
                      { (yyval.pbelem) = autorelease(new BoundGrammarValue(*(yyvsp[-2].string), GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 4077 "hexpr.parse.C"
    break;

  case 207: /* pbelem: pvalue  */
#line 792 "hexpr.y"
                      { (yyval.pbelem) = autorelease(new BoundGrammarValue("_", GrammarValuePtr((yyvsp[0].pvalue)))); }
#line 4083 "hexpr.parse.C"
    break;

  case 208: /* pvalue: id  */
#line 794 "hexpr.y"
                      { (yyval.pvalue) = new GSymRef(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4089 "hexpr.parse.C"
    break;

  case 209: /* pvalue: "stringV"  */
#line 795 "hexpr.y"
                      { (yyval.pvalue) = new GStr(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4095 "hexpr.parse.C"
    break;

  case 210: /* pvalue: "charV"  */
#line 796 "hexpr.y"
                      { (yyval.pvalue) = new GStr(std::string(1, str::readCharDef(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4101 "hexpr.parse.C"
    break;

  case 211: /* tsseq: "timespanV"  */
#line 798 "hexpr.y"
                         { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4107 "hexpr.parse.C"
    break;

  case 212: /* tsseq: tsseq "timespanV"  */
#line 799 "hexpr.y"
                         { (yyval.strings) = (yyvsp[-1].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4113 "hexpr.parse.C"
    break;

  case 213: /* l6exprs: l6exprs l6expr  */
#line 801 "hexpr.y"
                        { (yyval.exps) = (yyvsp[-1].exps); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4119 "hexpr.parse.C"
    break;

  case 214: /* l6exprs: l6expr  */
#line 802 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4125 "hexpr.parse.C"
    break;

  case 215: /* patternexps: patternexps patternexp  */
#line 804 "hexpr.y"
                                    { (yyval.patternexps) = (yyvsp[-1].patternexps); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 4131 "hexpr.parse.C"
    break;

  case 216: /* patternexps: patternexp  */
#line 805 "hexpr.y"
                                    { (yyval.patternexps) = autorelease(new PatternRows()); (yyval.patternexps)->push_back(*(yyvsp[0].patternexp)); }
#line 4137 "hexpr.parse.C"
    break;

  case 217: /* patternexp: "|" patterns "->" l0expr  */
#line 807 "hexpr.y"
                                                    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-2].patterns), ExprPtr((yyvsp[0].exp)))); }
#line 4143 "hexpr.parse.C"
    break;

  case 218: /* patternexp: "|" patterns "where" l0expr "->" l0expr  */
#line 808 "hexpr.y"
                                                    { (yyval.patternexp) = autorelease(new PatternRow(*(yyvsp[-4].patterns), ExprPtr((yyvsp[-2].exp)), ExprPtr((yyvsp[0].exp)))); }
#line 4149 "hexpr.parse.C"
    break;

  case 219: /* patterns: patterns pattern  */
#line 811 "hexpr.y"
                           { (yyval.patterns) = (yyvsp[-1].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4155 "hexpr.parse.C"
    break;

  case 220: /* patterns: pattern  */
#line 812 "hexpr.y"
                           { (yyval.patterns) = autorelease(new Patterns()); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4161 "hexpr.parse.C"
    break;

  case 221: /* refutablep: "boolV"  */
#line 814 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Bool((yyvsp[0].boolv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4167 "hexpr.parse.C"
    break;

  case 222: /* refutablep: "charV"  */
#line 815 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Char(str::readCharDef(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 4173 "hexpr.parse.C"
    break;

  case 223: /* refutablep: "byteV"  */
#line 816 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Byte(str::dehex(*(yyvsp[0].string)), m((yylsp[0])))), m((yylsp[0]))); }
#line 4179 "hexpr.parse.C"
    break;

  case 224: /* refutablep: "shortV"  */
#line 817 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Short((yyvsp[0].shortv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4185 "hexpr.parse.C"
    break;

  case 225: /* refutablep: "intV"  */
#line 818 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int((yyvsp[0].intv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4191 "hexpr.parse.C"
    break;

  case 226: /* refutablep: "longV"  */
#line 819 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Long((yyvsp[0].longv), m((yylsp[0])))), m((yylsp[0]))); }
#line 4197 "hexpr.parse.C"
    break;

  case 227: /* refutablep: "int128V"  */
#line 820 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Int128((yyvsp[0].int128v), m((yylsp[0])))), m((yylsp[0]))); }
#line 4203 "hexpr.parse.C"
    break;

  case 228: /* refutablep: "doubleV"  */
#line 821 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(PrimitivePtr(new Double((yyvsp[0].doublev), m((yylsp[0])))), m((yylsp[0]))); }
#line 4209 "hexpr.parse.C"
    break;

  case 229: /* refutablep: "bytesV"  */
#line 822 "hexpr.y"
                                       { (yyval.pattern) = mkpatarray(str::dehexs(*(yyvsp[0].string)), m((yylsp[0]))); }
#line 4215 "hexpr.parse.C"
    break;

  case 230: /* refutablep: "stringV"  */
#line 823 "hexpr.y"
                                       { (yyval.pattern) = mkpatarray(str::unescape(str::trimq(*(yyvsp[0].string))), m((yylsp[0]))); }
#line 4221 "hexpr.parse.C"
    break;

  case 231: /* refutablep: tsseq  */
#line 824 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkTimespanPrim(*(yyvsp[0].strings), m((yylsp[0]))), mkTimespanExpr(*(yyvsp[0].strings), m((yylsp[0]))), m((yylsp[0]))); }
#line 4227 "hexpr.parse.C"
    break;

  case 232: /* refutablep: "timeV"  */
#line 825 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4233 "hexpr.parse.C"
    break;

  case 233: /* refutablep: "dateTimeV"  */
#line 826 "hexpr.y"
                                       { (yyval.pattern) = new MatchLiteral(mkDateTimePrim(*(yyvsp[0].string), m((yylsp[0]))), mkDateTimeExpr(*(yyvsp[0].string), m((yylsp[0]))), m((yylsp[0]))); }
#line 4239 "hexpr.parse.C"
    break;

  case 234: /* refutablep: "regexV"  */
#line 827 "hexpr.y"
                                       { (yyval.pattern) = new MatchRegex(std::string((yyvsp[0].string)->begin() + 1, (yyvsp[0].string)->end() - 1), m((yylsp[0]))); }
#line 4245 "hexpr.parse.C"
    break;

  case 235: /* refutablep: "[" patternseq "]"  */
#line 828 "hexpr.y"
                                       { (yyval.pattern) = new MatchArray(*(yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4251 "hexpr.parse.C"
    break;

  case 236: /* refutablep: "[" patternseq "," "]"  */
#line 829 "hexpr.y"
                                       { (yyval.pattern) = new MatchArray(*(yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4257 "hexpr.parse.C"
    break;

  case 237: /* refutablep: "|" id "|"  */
#line 830 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(*(yyvsp[-1].string), PatternPtr(new MatchLiteral(PrimitivePtr(new Unit(m((yylsp[-1])))), m((yylsp[-1])))), m((yylsp[-2]),(yylsp[0]))); }
#line 4263 "hexpr.parse.C"
    break;

  case 238: /* refutablep: "|" id "=" pattern "|"  */
#line 831 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(*(yyvsp[-3].string), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4269 "hexpr.parse.C"
    break;

  case 239: /* refutablep: "|" "intV" "=" pattern "|"  */
#line 832 "hexpr.y"
                                       { (yyval.pattern) = new MatchVariant(".f" + str::from((yyvsp[-3].intv)), PatternPtr((yyvsp[-1].pattern)), m((yylsp[-4]),(yylsp[0]))); }
#line 4275 "hexpr.parse.C"
    break;

  case 240: /* refutablep: "(" patternseq ")"  */
#line 833 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4281 "hexpr.parse.C"
    break;

  case 241: /* refutablep: "(" patternseq "," ")"  */
#line 834 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4287 "hexpr.parse.C"
    break;

  case 242: /* refutablep: "{" recpatfields "}"  */
#line 835 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4293 "hexpr.parse.C"
    break;

  case 243: /* refutablep: "{" recpatfields "," "}"  */
#line 836 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4299 "hexpr.parse.C"
    break;

  case 244: /* refutablep: id  */
#line 837 "hexpr.y"
                                       { (yyval.pattern) = patVarCtorFn(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4305 "hexpr.parse.C"
    break;

  case 245: /* irrefutablep: id  */
#line 839 "hexpr.y"
                                       { (yyval.pattern) = new MatchAny(*(yyvsp[0].string), m((yylsp[0]))); }
#line 4311 "hexpr.parse.C"
    break;

  case 246: /* irrefutablep: "(" patternseq ")"  */
#line 840 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-1].patterns), m((yylsp[-2]),(yylsp[0]))); }
#line 4317 "hexpr.parse.C"
    break;

  case 247: /* irrefutablep: "(" patternseq "," ")"  */
#line 841 "hexpr.y"
                                       { (yyval.pattern) = pickNestedPat((yyvsp[-2].patterns), m((yylsp[-3]),(yylsp[0]))); }
#line 4323 "hexpr.parse.C"
    break;

  case 248: /* irrefutablep: "{" recpatfields "}"  */
#line 842 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-1].recpatfields), m((yylsp[-2]),(yylsp[0]))); }
#line 4329 "hexpr.parse.C"
    break;

  case 249: /* irrefutablep: "{" recpatfields "," "}"  */
#line 843 "hexpr.y"
                                       { (yyval.pattern) = new MatchRecord(*(yyvsp[-2].recpatfields), m((yylsp[-3]),(yylsp[0]))); }
#line 4335 "hexpr.parse.C"
    break;

  case 250: /* pattern: refutablep  */
#line 845 "hexpr.y"
                    { (yyval.pattern) = (yyvsp[0].pattern); }
#line 4341 "hexpr.parse.C"
    break;

  case 251: /* patternseq: patternseqn  */
#line 847 "hexpr.y"
                          { (yyval.patterns) = (yyvsp[0].patterns); }
#line 4347 "hexpr.parse.C"
    break;

  case 252: /* patternseq: %empty  */
#line 848 "hexpr.y"
                          { (yyval.patterns) = new Patterns(); }
#line 4353 "hexpr.parse.C"
    break;

  case 253: /* patternseqn: patternseqn "," pattern  */
#line 850 "hexpr.y"
                                     { (yyval.patterns) = (yyvsp[-2].patterns); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4359 "hexpr.parse.C"
    break;

  case 254: /* patternseqn: pattern  */
#line 851 "hexpr.y"
                                     { (yyval.patterns) = new Patterns(); (yyval.patterns)->push_back(PatternPtr((yyvsp[0].pattern))); }
#line 4365 "hexpr.parse.C"
    break;

  case 255: /* recpatfields: recpatfields "," recpatfield  */
#line 853 "hexpr.y"
                                           { (yyval.recpatfields) = (yyvsp[-2].recpatfields); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4371 "hexpr.parse.C"
    break;

  case 256: /* recpatfields: recpatfield  */
#line 854 "hexpr.y"
                                           { (yyval.recpatfields) = new MatchRecord::Fields(); (yyval.recpatfields)->push_back(*(yyvsp[0].recpatfield)); }
#line 4377 "hexpr.parse.C"
    break;

  case 257: /* recpatfield: id "=" pattern  */
#line 856 "hexpr.y"
                            { (yyval.recpatfield) = new MatchRecord::Field(*(yyvsp[-2].string), PatternPtr((yyvsp[0].pattern))); }
#line 4383 "hexpr.parse.C"
    break;

  case 258: /* recfields: %empty  */
#line 858 "hexpr.y"
                                                 { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); }
#line 4389 "hexpr.parse.C"
    break;

  case 259: /* recfields: recfieldname "=" l0expr  */
#line 859 "hexpr.y"
                                                 { (yyval.rfields) = autorelease(new MkRecord::FieldDefs()); (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4395 "hexpr.parse.C"
    break;

  case 260: /* recfields: recfields "," recfieldname "=" l0expr  */
#line 860 "hexpr.y"
                                                 { (yyval.rfields) = (yyvsp[-4].rfields);                                     (yyval.rfields)->push_back(MkRecord::FieldDef(*(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4401 "hexpr.parse.C"
    break;

  case 261: /* recfieldname: id  */
#line 862 "hexpr.y"
                         { (yyval.string) = (yyvsp[0].string); }
#line 4407 "hexpr.parse.C"
    break;

  case 262: /* recfieldname: "data"  */
#line 863 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("data")); }
#line 4413 "hexpr.parse.C"
    break;

  case 263: /* recfieldname: "type"  */
#line 864 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("type")); }
#line 4419 "hexpr.parse.C"
    break;

  case 264: /* recfieldname: "where"  */
#line 865 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("where")); }
#line 4425 "hexpr.parse.C"
    break;

  case 265: /* recfieldname: "class"  */
#line 866 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("class")); wantIndent(false); }
#line 4431 "hexpr.parse.C"
    break;

  case 266: /* recfieldname: "instance"  */
#line 867 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("instance")); wantIndent(false); }
#line 4437 "hexpr.parse.C"
    break;

  case 267: /* recfieldname: "exists"  */
#line 868 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("exists")); }
#line 4443 "hexpr.parse.C"
    break;

  case 268: /* recfieldname: "import"  */
#line 869 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("import")); }
#line 4449 "hexpr.parse.C"
    break;

  case 269: /* recfieldname: "module"  */
#line 870 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("module")); }
#line 4455 "hexpr.parse.C"
    break;

  case 270: /* recfieldname: "parse"  */
#line 871 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("parse")); }
#line 4461 "hexpr.parse.C"
    break;

  case 271: /* recfieldname: "do"  */
#line 872 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("do")); }
#line 4467 "hexpr.parse.C"
    break;

  case 272: /* recfieldname: "return"  */
#line 873 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("return")); }
#line 4473 "hexpr.parse.C"
    break;

  case 273: /* recfieldname: "fn"  */
#line 874 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string("fn")); }
#line 4479 "hexpr.parse.C"
    break;

  case 274: /* recfieldname: "intV"  */
#line 875 "hexpr.y"
                         { (yyval.string) = autorelease(new std::string(".f" + str::from((yyvsp[0].intv)))); }
#line 4485 "hexpr.parse.C"
    break;

  case 275: /* recfieldname: "stringV"  */
#line 876 "hexpr.y"
                         { std::string stringField = str::unescape(str::trimq(*(yyvsp[0].string)));
                           if (stringField.size() > 0 && stringField[0] == '.' ) {
                             throw annotated_error(m((yylsp[0])), "Cannot define record string label with leading '.'");
                           }
                           (yyval.string) = autorelease(new std::string(str::unescape(str::trimq(*(yyvsp[0].string))))); }
#line 4495 "hexpr.parse.C"
    break;

  case 276: /* recfieldpath: recfieldpath "." recfieldname  */
#line 882 "hexpr.y"
                                            { (yyval.strings) = (yyvsp[-2].strings); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4501 "hexpr.parse.C"
    break;

  case 277: /* recfieldpath: recfieldpath "tupSection"  */
#line 883 "hexpr.y"
                                            { (yyval.strings) = (yyvsp[-1].strings); str::seq x = tupSectionFields(*(yyvsp[0].string)); (yyval.strings)->insert((yyval.strings)->end(), x.begin(), x.end()); }
#line 4507 "hexpr.parse.C"
    break;

  case 278: /* recfieldpath: "." recfieldname  */
#line 884 "hexpr.y"
                                            { (yyval.strings) = autorelease(new str::seq()); (yyval.strings)->push_back(*(yyvsp[0].string)); }
#line 4513 "hexpr.parse.C"
    break;

  case 279: /* recfieldpath: "tupSection"  */
#line 885 "hexpr.y"
                                            { (yyval.strings) = autorelease(new str::seq()); *(yyval.strings) = tupSectionFields(*(yyvsp[0].string)); }
#line 4519 "hexpr.parse.C"
    break;

  case 280: /* varfields: varbind  */
#line 887 "hexpr.y"
                                 { (yyval.vfields) = autorelease(new Case::Bindings()); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4525 "hexpr.parse.C"
    break;

  case 281: /* varfields: varfields "," varbind  */
#line 888 "hexpr.y"
                                 { (yyval.vfields) = (yyvsp[-2].vfields); (yyval.vfields)->push_back(*(yyvsp[0].vbind)); }
#line 4531 "hexpr.parse.C"
    break;

  case 282: /* varbind: id "=" l0expr  */
#line 890 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-2].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4537 "hexpr.parse.C"
    break;

  case 283: /* varbind: id ":" id "=" l0expr  */
#line 891 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(*(yyvsp[-4].string), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4543 "hexpr.parse.C"
    break;

  case 284: /* varbind: "intV" ":" id "=" l0expr  */
#line 892 "hexpr.y"
                                  { (yyval.vbind) = autorelease(new Case::Binding(".f" + str::from((yyvsp[-4].intv)), *(yyvsp[-2].string), ExprPtr((yyvsp[0].exp)))); }
#line 4549 "hexpr.parse.C"
    break;

  case 285: /* cargs: %empty  */
#line 894 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); }
#line 4555 "hexpr.parse.C"
    break;

  case 286: /* cargs: l0expr  */
#line 895 "hexpr.y"
                        { (yyval.exps) = autorelease(new Exprs()); (yyval.exps)->push_back(ExprPtr((yyvsp[0].exp))); }
#line 4561 "hexpr.parse.C"
    break;

  case 287: /* cargs: cargs "," l0expr  */
#line 896 "hexpr.y"
                        { (yyvsp[-2].exps)->push_back(ExprPtr((yyvsp[0].exp))); (yyval.exps) = (yyvsp[-2].exps); }
#line 4567 "hexpr.parse.C"
    break;

  case 288: /* qtype: cst "=>" l0mtype  */
#line 898 "hexpr.y"
                         { (yyval.qualtype) = new QualType(*(yyvsp[-2].tconstraints), *(yyvsp[0].mtype)); }
#line 4573 "hexpr.parse.C"
    break;

  case 289: /* qtype: l0mtype  */
#line 899 "hexpr.y"
                         { (yyval.qualtype) = new QualType(Constraints(), *(yyvsp[0].mtype)); }
#line 4579 "hexpr.parse.C"
    break;

  case 290: /* cst: "(" tpreds ")"  */
#line 902 "hexpr.y"
                    { (yyval.tconstraints) = (yyvsp[-1].tconstraints); }
#line 4585 "hexpr.parse.C"
    break;

  case 291: /* tpreds: tpred  */
#line 904 "hexpr.y"
                         { (yyval.tconstraints) = autorelease(new Constraints()); (yyval.tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); }
#line 4591 "hexpr.parse.C"
    break;

  case 292: /* tpreds: tpreds "," tpred  */
#line 905 "hexpr.y"
                         { (yyvsp[-2].tconstraints)->push_back(ConstraintPtr((yyvsp[0].tconstraint))); (yyval.tconstraints) = (yyvsp[-2].tconstraints); }
#line 4597 "hexpr.parse.C"
    break;

  case 293: /* tpred: id l1mtargl  */
#line 907 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(*(yyvsp[-1].string), *(yyvsp[0].mtypes)); }
#line 4603 "hexpr.parse.C"
    break;

  case 294: /* tpred: l1mtype "==" l1mtype  */
#line 908 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(EqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4609 "hexpr.parse.C"
    break;

  case 295: /* tpred: l1mtype "!=" l1mtype  */
#line 909 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(NotEqualTypes::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4615 "hexpr.parse.C"
    break;

  case 296: /* tpred: l1mtype "~" l1mtype  */
#line 910 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(FixIsoRecur::constraintName(), list(*(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4621 "hexpr.parse.C"
    break;

  case 297: /* tpred: l1mtype "=" "{" l1mtype "*" l1mtype "}"  */
#line 911 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4627 "hexpr.parse.C"
    break;

  case 298: /* tpred: l1mtype "=" "{" id ":" l1mtype "*" l1mtype "}"  */
#line 912 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4633 "hexpr.parse.C"
    break;

  case 299: /* tpred: l1mtype "=" "(" l1mtype "*" l1mtype ")"  */
#line 913 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4639 "hexpr.parse.C"
    break;

  case 300: /* tpred: "{" l1mtype "*" l1mtype "}" "=" l1mtype  */
#line 914 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4645 "hexpr.parse.C"
    break;

  case 301: /* tpred: "{" id ":" l1mtype "*" l1mtype "}" "=" l1mtype  */
#line 915 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4651 "hexpr.parse.C"
    break;

  case 302: /* tpred: "(" l1mtype "*" l1mtype ")" "=" l1mtype  */
#line 916 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(1), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4657 "hexpr.parse.C"
    break;

  case 303: /* tpred: l1mtype "." recfieldname "::" l1mtype  */
#line 918 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4663 "hexpr.parse.C"
    break;

  case 304: /* tpred: l1mtype "." recfieldname "<-" l1mtype  */
#line 919 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), TString::make(*(yyvsp[-2].string)), *(yyvsp[0].mtype)); }
#line 4669 "hexpr.parse.C"
    break;

  case 305: /* tpred: l1mtype "/" l1mtype "::" l1mtype  */
#line 920 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Read,  *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4675 "hexpr.parse.C"
    break;

  case 306: /* tpred: l1mtype "/" l1mtype "<-" l1mtype  */
#line 921 "hexpr.y"
                                                      { (yyval.tconstraint) = HasField::newConstraint(HasField::Write, *(yyvsp[-4].mtype), *(yyvsp[-2].mtype),                *(yyvsp[0].mtype)); }
#line 4681 "hexpr.parse.C"
    break;

  case 307: /* tpred: l1mtype "=" "|" l1mtype "+" l1mtype "|"  */
#line 923 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-6].mtype), freshTypeVar(),  *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4687 "hexpr.parse.C"
    break;

  case 308: /* tpred: "|" l1mtype "+" l1mtype "|" "=" l1mtype  */
#line 924 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), freshTypeVar(),  *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4693 "hexpr.parse.C"
    break;

  case 309: /* tpred: l1mtype "=" "|" id ":" l1mtype "+" l1mtype "|"  */
#line 925 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *(yyvsp[-8].mtype), TVar::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype), *(yyvsp[-1].mtype))); }
#line 4699 "hexpr.parse.C"
    break;

  case 310: /* tpred: "|" id ":" l1mtype "+" l1mtype "|" "=" l1mtype  */
#line 926 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *(yyvsp[0].mtype), TVar::make(*(yyvsp[-7].string)), *(yyvsp[-5].mtype), *(yyvsp[-3].mtype))); }
#line 4705 "hexpr.parse.C"
    break;

  case 311: /* tpred: "|" id ":" l0mtype "|" "::" l1mtype  */
#line 928 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), TString::make(*(yyvsp[-5].string)), *(yyvsp[-3].mtype))); }
#line 4711 "hexpr.parse.C"
    break;

  case 312: /* tpred: "|" l1mtype "/" l0mtype "|" "::" l1mtype  */
#line 929 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(CtorVerifier::constraintName(), list(*(yyvsp[0].mtype), *(yyvsp[-5].mtype),                *(yyvsp[-3].mtype))); }
#line 4717 "hexpr.parse.C"
    break;

  case 313: /* tpred: l1mtype "++" l1mtype "=" l1mtype  */
#line 930 "hexpr.y"
                                                      { (yyval.tconstraint) = new Constraint(AppendsToUnqualifier::constraintName(), list(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype), *(yyvsp[0].mtype))); }
#line 4723 "hexpr.parse.C"
    break;

  case 314: /* l1mtargl: l1mtype  */
#line 932 "hexpr.y"
                           { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4729 "hexpr.parse.C"
    break;

  case 315: /* l1mtargl: l1mtargl l1mtype  */
#line 933 "hexpr.y"
                           { (yyvsp[-1].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4735 "hexpr.parse.C"
    break;

  case 316: /* ltmtype: ltmtype l0mtype  */
#line 935 "hexpr.y"
                          { (yyval.mtypes) = (yyvsp[-1].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4741 "hexpr.parse.C"
    break;

  case 317: /* ltmtype: l0mtype  */
#line 936 "hexpr.y"
                          { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4747 "hexpr.parse.C"
    break;

  case 318: /* l0mtype: l0mtargl "->" l1mtype  */
#line 938 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(Func::make(tuplety(*(yyvsp[-2].mtypes)), *(yyvsp[0].mtype)))); }
#line 4753 "hexpr.parse.C"
    break;

  case 319: /* l0mtype: mtuplist  */
#line 939 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(makeTupleType(*(yyvsp[0].mtypes)))); }
#line 4759 "hexpr.parse.C"
    break;

  case 320: /* l0mtype: msumlist  */
#line 940 "hexpr.y"
                               { (yyval.mtype) = autorelease(new MonoTypePtr(makeSumType(*(yyvsp[0].mtypes)))); }
#line 4765 "hexpr.parse.C"
    break;

  case 321: /* l1mtype: id  */
#line 942 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(monoTypeByName(*(yyvsp[0].string)))); }
#line 4771 "hexpr.parse.C"
    break;

  case 322: /* l1mtype: "<" cppid ">"  */
#line 943 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(OpaquePtr::make(str::replace<char>(*(yyvsp[-1].string), ".", "::"), 0, false))); }
#line 4777 "hexpr.parse.C"
    break;

  case 323: /* l1mtype: "[" "]"  */
#line 944 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("[]"))); }
#line 4783 "hexpr.parse.C"
    break;

  case 324: /* l1mtype: "[" ltmtype "]"  */
#line 945 "hexpr.y"
                                           { try { (yyval.mtype) = autorelease(new MonoTypePtr(Array::make(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4789 "hexpr.parse.C"
    break;

  case 325: /* l1mtype: "[" ":" l0mtype "|" tyind ":" "]"  */
#line 946 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(FixedArray::make(*(yyvsp[-4].mtype), *(yyvsp[-2].mtype)))); }
#line 4795 "hexpr.parse.C"
    break;

  case 326: /* l1mtype: "(" "->" ")"  */
#line 947 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("->"))); }
#line 4801 "hexpr.parse.C"
    break;

  case 327: /* l1mtype: "(" ltmtype ")"  */
#line 948 "hexpr.y"
                                           { try { (yyval.mtype) = autorelease(new MonoTypePtr(clone(yyParseCC->replaceTypeAliases(accumTApp(*(yyvsp[-1].mtypes)))))); } catch (std::exception& ex) { throw annotated_error(m((yylsp[-1])), ex.what()); } }
#line 4807 "hexpr.parse.C"
    break;

  case 328: /* l1mtype: "{" mreclist "}"  */
#line 949 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(makeRecType(*(yyvsp[-1].mreclist)))); }
#line 4813 "hexpr.parse.C"
    break;

  case 329: /* l1mtype: "|" mvarlist "|"  */
#line 950 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(makeVarType(*(yyvsp[-1].mvarlist)))); }
#line 4819 "hexpr.parse.C"
    break;

  case 330: /* l1mtype: "(" ")"  */
#line 951 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Prim::make("unit"))); }
#line 4825 "hexpr.parse.C"
    break;

  case 331: /* l1mtype: "intV"  */
#line 952 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(((yyvsp[0].intv) == 0) ? Prim::make("void") : TLong::make((yyvsp[0].intv)))); }
#line 4831 "hexpr.parse.C"
    break;

  case 332: /* l1mtype: "boolV"  */
#line 953 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr((yyvsp[0].boolv) ? TLong::make(1) : TLong::make(0))); }
#line 4837 "hexpr.parse.C"
    break;

  case 333: /* l1mtype: "exists" id "." l1mtype  */
#line 954 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Exists::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4843 "hexpr.parse.C"
    break;

  case 334: /* l1mtype: l1mtype "@" l1mtype  */
#line 955 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype), *(yyvsp[0].mtype)))); }
#line 4849 "hexpr.parse.C"
    break;

  case 335: /* l1mtype: l1mtype "@" "?"  */
#line 956 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(fileRefTy(*(yyvsp[-2].mtype)))); }
#line 4855 "hexpr.parse.C"
    break;

  case 336: /* l1mtype: "^" id "." l1mtype  */
#line 957 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(Recursive::make(*(yyvsp[-2].string), *(yyvsp[0].mtype)))); }
#line 4861 "hexpr.parse.C"
    break;

  case 337: /* l1mtype: "stringV"  */
#line 958 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(TString::make(str::unescape(str::trimq(*(yyvsp[0].string)))))); }
#line 4867 "hexpr.parse.C"
    break;

  case 338: /* l1mtype: "`" l0expr "`"  */
#line 959 "hexpr.y"
                                           { (yyval.mtype) = autorelease(new MonoTypePtr(TApp::make(primty("quote"), list(texpr(ExprPtr((yyvsp[-1].exp))))))); }
#line 4873 "hexpr.parse.C"
    break;

  case 339: /* tyind: id  */
#line 961 "hexpr.y"
              { (yyval.mtype) = autorelease(new MonoTypePtr(TVar::make(*(yyvsp[0].string)))); }
#line 4879 "hexpr.parse.C"
    break;

  case 340: /* tyind: "intV"  */
#line 962 "hexpr.y"
              { (yyval.mtype) = autorelease(new MonoTypePtr(TLong::make((yyvsp[0].intv)))); }
#line 4885 "hexpr.parse.C"
    break;

  case 341: /* cppid: id  */
#line 964 "hexpr.y"
                    { (yyval.string) = (yyvsp[0].string); }
#line 4891 "hexpr.parse.C"
    break;

  case 342: /* cppid: cppid "." id  */
#line 965 "hexpr.y"
                    { (yyval.string) = (yyvsp[-2].string); *(yyval.string) += "."; *(yyval.string) += *(yyvsp[0].string); }
#line 4897 "hexpr.parse.C"
    break;

  case 343: /* l0mtargl: l1mtype  */
#line 967 "hexpr.y"
                                        { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4903 "hexpr.parse.C"
    break;

  case 344: /* l0mtargl: "(" l0mtype "," l0mtarglt ")"  */
#line 968 "hexpr.y"
                                        { (yyvsp[-1].mtypes)->insert((yyvsp[-1].mtypes)->begin(), *(yyvsp[-3].mtype)); (yyval.mtypes) = (yyvsp[-1].mtypes); }
#line 4909 "hexpr.parse.C"
    break;

  case 345: /* l0mtarglt: l0mtype  */
#line 970 "hexpr.y"
                                 { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4915 "hexpr.parse.C"
    break;

  case 346: /* l0mtarglt: l0mtarglt "," l0mtype  */
#line 971 "hexpr.y"
                                 { (yyvsp[-2].mtypes)->push_back(*(yyvsp[0].mtype)); (yyval.mtypes) = (yyvsp[-2].mtypes); }
#line 4921 "hexpr.parse.C"
    break;

  case 347: /* mtuplist: l1mtype  */
#line 973 "hexpr.y"
                               { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4927 "hexpr.parse.C"
    break;

  case 348: /* mtuplist: mtuplist "*" l1mtype  */
#line 974 "hexpr.y"
                               { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4933 "hexpr.parse.C"
    break;

  case 349: /* msumlist: l1mtype "+" l1mtype  */
#line 976 "hexpr.y"
                               { (yyval.mtypes) = autorelease(new MonoTypes()); (yyval.mtypes)->push_back(*(yyvsp[-2].mtype)); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4939 "hexpr.parse.C"
    break;

  case 350: /* msumlist: msumlist "+" l1mtype  */
#line 977 "hexpr.y"
                               { (yyval.mtypes) = (yyvsp[-2].mtypes); (yyval.mtypes)->push_back(*(yyvsp[0].mtype)); }
#line 4945 "hexpr.parse.C"
    break;

  case 351: /* mreclist: mreclist "," id ":" l0mtype  */
#line 979 "hexpr.y"
                                      { (yyval.mreclist) = (yyvsp[-4].mreclist);                                 (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4951 "hexpr.parse.C"
    break;

  case 352: /* mreclist: id ":" l0mtype  */
#line 980 "hexpr.y"
                                      { (yyval.mreclist) = autorelease(new Record::Members()); (yyval.mreclist)->push_back(Record::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype))); }
#line 4957 "hexpr.parse.C"
    break;

  case 353: /* mvarlist: mvarlist "," id ":" l0mtype  */
#line 982 "hexpr.y"
                                      { (yyval.mvarlist) = (yyvsp[-4].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4963 "hexpr.parse.C"
    break;

  case 354: /* mvarlist: mvarlist "," id  */
#line 983 "hexpr.y"
                                      { (yyval.mvarlist) = (yyvsp[-2].mvarlist);                                  (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4969 "hexpr.parse.C"
    break;

  case 355: /* mvarlist: id ":" l0mtype  */
#line 984 "hexpr.y"
                                      { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[-2].string), *(yyvsp[0].mtype),                0)); }
#line 4975 "hexpr.parse.C"
    break;

  case 356: /* mvarlist: id  */
#line 985 "hexpr.y"
                                      { (yyval.mvarlist) = autorelease(new Variant::Members()); (yyval.mvarlist)->push_back(Variant::Member(*(yyvsp[0].string), Prim::make("unit"), 0)); }
#line 4981 "hexpr.parse.C"
    break;


#line 4985 "hexpr.parse.C"

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

#line 989 "hexpr.y"

#pragma GCC diagnostic pop


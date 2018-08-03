%{
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
%}

%{
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

%}

%locations
%define parse.error verbose

/* token data types */
%union {
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
};

%token TPARSEMODULE      "domodule"
%token TPARSEDEFN        "dodefn"
%token TPARSEEXPR        "doexpr"

%token TMODULE "module"
%token TWHERE  "where"
%token TIMPORT "import"
%token TTYPE   "type"
%token TDATA   "data"
%token TCLASS  "class"
%token TINST   "instance"
%token TINDENT "indent"

%token <boolv>   TBOOL         "boolV"
%token <string>  TCHAR         "charV"
%token <string>  TBYTE         "byteV"
%token <string>  TBYTES        "bytesV"
%token <shortv>  TSHORT        "shortV"
%token <intv>    TINT          "intV"
%token <longv>   TLONG         "longV"
%token <floatv>  TFLOAT        "floatV"
%token <doublev> TDOUBLE       "doubleV"
%token <string>  TIDENT        "id"
%token <string>  TSTRING       "stringV"
%token <string>  TREGEX        "regexV"
%token <string>  TTIMEINTERVAL "timespanV"
%token <string>  TTIME         "timeV"
%token <string>  TDATETIME     "dateTimeV"
%token <string>  TTUPSECTION   "tupSection"

%token TCSTARROW  "=>"
%token TARROW     "->"
%token TCOLON     ":"
%token TEXISTS    "exists"

%token TASSIGN    "<-"
%token TPARROW    ":="
%token TEQUALS    "="
%token TASSUMP    "::"
%token TAPPEND    "++"
%token TPLUS      "+"
%token TMINUS     "-"
%token TTIMES     "*"
%token TDIVIDE    "/"
%token TREM       "%"
%token TDOT       "."
%token TEQUIV     "=="
%token TEQ        "==="
%token TCIEQ      "~"
%token TNEQ       "!="
%token TLT        "<"
%token TLTE       "<="
%token TGT        ">"
%token TGTE       ">="
%token TNOT       "!"
%token TLET       "let"
%token TCASE      "case"
%token TDEFAULT   "default"
%token TMATCH     "match"
%token TMATCHES   "matches"
%token TPARSE     "parse"
%token TWITH      "with"
%token TOF        "of"
%token TAND       "and"
%token TOR        "or"
%token TIF        "if"
%token TTHEN      "then"
%token TELSE      "else"
%token TIN        "in"
%token TPACK      "pack"
%token TUNPACK    "unpack"
%token TDO        "do"
%token TRETURN    "return"
%token TLPAREN    "("
%token TRPAREN    ")"
%token TLBRACKET  "["
%token TRBRACKET  "]"
%token TLBRACE    "{"
%token TRBRACE    "}"
%token TBAR       "|"
%token TCOMMA     ","
%token TSEMICOLON ";"
%token TFN        "\\"
%token TCOMPOSE   "o"
%token TUPTO      ".."
%token TCARET     "^"
%token TAT        "@"
%token TDOLLAR    "$"
%token TQUESTION  "?"
%token TSQUOTE    "'"
%token TEQUOTE    "`"

/* nonterminal node types */
%type <module>       module
%type <mdefs>        defs
%type <mdef>         def importdef tydef classdef instdef
%type <fundep>       fundep
%type <fundeps>      fundeps
%type <mvtydefs>     cmembers
%type <mvtydef>      vartybind cmember
%type <mvdefs>       imembers
%type <mvdef>        vardef imember
%type <strings>      names nameseq idseq
%type <string>       name opname
%type <mtypes>       types mtuplist msumlist
%type <mreclist>     mreclist
%type <mvarlist>     mvarlist

%type <exp>          l0expr l1expr l2expr l3expr l4expr l5expr l6expr
%type <exps>         l6exprs cargs
%type <rfields>      recfields
%type <vfields>      varfields
%type <vbind>        varbind
%type <string>       id cppid recfieldname
%type <strings>      tsseq recfieldpath
%type <patternexps>  patternexps
%type <patternexp>   patternexp
%type <patterns>     patterns patternseq patternseqn
%type <pattern>      pattern refutablep irrefutablep
%type <recpatfields> recpatfields
%type <recpatfield>  recpatfield

%type <letbinding>   letbinding dobinding
%type <letbindings>  letbindings dobindings

%type <prules>  prules  // Grammar*
%type <prule>   prule   // Grammar::value_type*
%type <prdefs>  prdefs  // GrammarRules*
%type <prdef>   prdef   // GrammarRule*
%type <pbelems> pbelems // BoundGrammarValues*
%type <pbelem>  pbelem  // BoundGrammarValue*
%type <pvalue>  pvalue  // GrammarValue*

%type <qualtype>     qtype
%type <tconstraints> cst tpreds
%type <tconstraint>  tpred
%type <mtype>        l0mtype l1mtype tyind
%type <mtypes>       ltmtype l1mtargl l0mtargl l0mtarglt

/* associativity to give expected operator precedence */
%right "else"
%right "in"
%right "->"
%left "!"
%left "."
%left "++" "+" "-"
%left "*" "/" "%"
%left "~" "===" "==" "!=" "<" "<=" ">" ">="
%left "and" "or"
%left "o"
%right "@"
%right "::"

%start s

%%

s: "domodule" module        { yyParsedModule = $2;                     }
 | "dodefn"   id "=" l0expr { yyParsedVar    = *$2; yyParsedExpr = $4; }
 | "dodefn"   l0expr        { yyParsedVar    = "";  yyParsedExpr = $2; }
 | "doexpr"   l0expr        { yyParsedExpr   = $2;                     }

/* modules */
module: "module" id "where" defs { $$ = new Module(*$2, *$4); }
      | defs                     { $$ = new Module(freshName(), *$1); }

defs: /* nothing */ { $$ = autorelease(new ModuleDefs()); }
    | def           { $$ = autorelease(new ModuleDefs()); $$->push_back(ModuleDefPtr($1)); }
    | defs def      { $$ = $1;                            $$->push_back(ModuleDefPtr($2)); }

def: importdef { $$ = $1; }
   | tydef     { $$ = $1; }
   | vartybind { $$ = $1; }
   | classdef  { $$ = $1; }
   | instdef   { $$ = $1; }

   | id "=" l0expr { $$ = new MVarDef(list(*$1), ExprPtr($3), m(@1, @3)); }
   | id id "=" l0expr { $$ = new MVarDef(list(*$1, *$2), ExprPtr($4), m(@1, @4)); }
   | id id id "=" l0expr { $$ = new MVarDef(list(*$1, *$2, *$3), ExprPtr($5), m(@1, @5)); }
   | id id id id "=" l0expr { $$ = new MVarDef(list(*$1, *$2, *$3, *$4), ExprPtr($6), m(@1, @6)); }
   | id id id id id "=" l0expr { $$ = new MVarDef(list(*$1, *$2, *$3, *$4, *$5), ExprPtr($7), m(@1, @7)); }
   | id id id id id id "=" l0expr { $$ = new MVarDef(list(*$1, *$2, *$3, *$4, *$5, *$6), ExprPtr($8), m(@1, @8)); }
   | id id id id id id id "=" l0expr { $$ = new MVarDef(list(*$1, *$2, *$3, *$4, *$5, *$6, *$7), ExprPtr($9), m(@1, @9)); }
   | id id id id id id id id "=" l0expr { $$ = new MVarDef(list(*$1, *$2, *$3, *$4, *$5, *$6, *$7, *$8), ExprPtr($10), m(@1, @10)); }
   | id id id id id id id id id "=" l0expr { $$ = new MVarDef(list(*$1, *$2, *$3, *$4, *$5, *$6, *$7, *$8, *$9), ExprPtr($11), m(@1, @11)); }
   | id id id id id id id id id id "=" l0expr { $$ = new MVarDef(list(*$1, *$2, *$3, *$4, *$5, *$6, *$7, *$8, *$9, *$10), ExprPtr($12), m(@1, @12)); }
   | id id id id id id id id id id id "=" l0expr { $$ = new MVarDef(list(*$1, *$2, *$3, *$4, *$5, *$6, *$7, *$8, *$9, *$10, *$11), ExprPtr($13), m(@1, @13)); }
   | id id id id id id id id id id id id "=" l0expr { $$ = new MVarDef(list(*$1, *$2, *$3, *$4, *$5, *$6, *$7, *$8, *$9, *$10, *$11, *$12), ExprPtr($14), m(@1, @14)); }

   // evaluate an expression just for side-effects, then discard it
   | l5expr { $$ = new MVarDef(list(freshName()), let(freshName(), ExprPtr($1), mktunit(m(@1)), m(@1)), m(@1)); }

/* import an external 'module' */
importdef: "import" cppid { $$ = new MImport(yyModulePath, *$2, m(@1, @2)); }

/* abbreviate type names */
tydef: "type" nameseq "=" qtype { $$ = new MTypeDef(MTypeDef::Transparent, hobbes::select(*$2, 0), hobbes::select(*$2, 1, (int)$2->size()), QualTypePtr($4), m(@1, @4)); }
     | "data" nameseq "=" qtype { $$ = new MTypeDef(MTypeDef::Opaque, hobbes::select(*$2, 0), hobbes::select(*$2, 1, (int)$2->size()), QualTypePtr($4), m(@1, @4)); }

/* variable bindings by type and expression */
vartybind: name "::" qtype { $$ = new MVarTypeDef(*$1, QualTypePtr($3), m(@1, @3)); }

vardef: names "=" l0expr { $$ = new MVarDef(*$1, ExprPtr($3), m(@1, @3)); }

/* type class definitions */
classdef: "class" cst "=>" id names                              { $$ = new ClassDef(*$2, *$4, *$5, CFunDepDefs(), MVarTypeDefs(), m(@1, @5)); wantIndent(false); }
        | "class" cst "=>" id names "|" fundeps                  { $$ = new ClassDef(*$2, *$4, *$5, *$7,           MVarTypeDefs(), m(@1, @7)); wantIndent(false); }
        | "class" cst "=>" id names "where" cmembers             { $$ = new ClassDef(*$2, *$4, *$5, CFunDepDefs(), *$7, m(@1, @7));            wantIndent(false); }
        | "class" cst "=>" id names "|" fundeps "where" cmembers { $$ = new ClassDef(*$2, *$4, *$5, *$7,           *$9, m(@1, @9));            wantIndent(false); }
        | "class" id names                                       { $$ = new ClassDef(Constraints(), *$2, *$3, CFunDepDefs(), MVarTypeDefs(), m(@1, @3)); wantIndent(false); }
        | "class" id names "|" fundeps                           { $$ = new ClassDef(Constraints(), *$2, *$3, *$5,           MVarTypeDefs(), m(@1, @5)); wantIndent(false); }
        | "class" id names "where" cmembers                      { $$ = new ClassDef(Constraints(), *$2, *$3, CFunDepDefs(), *$5, m(@1, @5));            wantIndent(false); }
        | "class" id names "|" fundeps "where" cmembers          { $$ = new ClassDef(Constraints(), *$2, *$3, *$5,           *$7, m(@1, @7));            wantIndent(false); }

fundeps: fundep             { $$ = autorelease(new CFunDepDefs()); $$->push_back(*$1); }
       | fundeps "," fundep { $$ = $1;                             $$->push_back(*$3); }

fundep: idseq "->" idseq { $$ = autorelease(new CFunDepDef(*$1, *$3)); }

cmembers: cmember          { $$ = autorelease(new MVarTypeDefs()); $$->push_back(MVarTypeDefPtr($1)); }
        | cmembers cmember { $$ = $1;                              $$->push_back(MVarTypeDefPtr($2)); }

cmember: "indent" vartybind { $$ = $2; }

/* type class instance definitions */
instdef: "instance" id types                           { $$ = new InstanceDef(Constraints(), *$2, *$3, MVarDefs(), m(@1, @3)); wantIndent(false); }
       | "instance" cst "=>" id types                  { $$ = new InstanceDef(*$2,           *$4, *$5, MVarDefs(), m(@1, @5)); wantIndent(false); }
       | "instance" id types "where" imembers          { $$ = new InstanceDef(Constraints(), *$2, *$3, *$5, m(@1, @5));        wantIndent(false); }
       | "instance" cst "=>" id types "where" imembers { $$ = new InstanceDef(*$2,           *$4, *$5, *$7, m(@1, @7));        wantIndent(false); }

imembers: imember          { $$ = autorelease(new MVarDefs()); $$->push_back(MVarDefPtr($1)); }
        | imembers imember { $$ = $1;                          $$->push_back(MVarDefPtr($2)); }

imember: "indent" vardef { $$ = $2; }

/* module common */
names: nameseq { $$ = $1; }
     /* used for definitions on known infix symbols */
     | id opname id { $$ = autorelease(new str::seq()); $$->push_back(*$2); $$->push_back(*$1); $$->push_back(*$3); }

nameseq: name         { $$ = autorelease(new str::seq()); $$->push_back(*$1); }
       | nameseq name { $$ = $1;                          $$->push_back(*$2); }

name: id { $$ = $1; }
    /* allow referencing known operators as names */
    | "(" opname ")" { $$ = $2; }

opname: "and" { $$ = autorelease(new std::string("and")); }
      | "or"  { $$ = autorelease(new std::string("or")); }
      | "o"   { $$ = autorelease(new std::string("compose")); }
      | "."   { $$ = autorelease(new std::string("compose")); }
      | "~"   { $$ = autorelease(new std::string("~")); }
      | "=~"   { $$ = autorelease(new std::string("=~")); }
      | "===" { $$ = autorelease(new std::string("===")); }
      | "=="  { $$ = autorelease(new std::string("==")); }
      | "<"   { $$ = autorelease(new std::string("<")); }
      | "<="  { $$ = autorelease(new std::string("<=")); }
      | ">"   { $$ = autorelease(new std::string(">")); }
      | ">="  { $$ = autorelease(new std::string(">=")); }
      | "in"  { $$ = autorelease(new std::string("in")); }
      | "++"  { $$ = autorelease(new std::string("append")); }
      | "+"   { $$ = autorelease(new std::string("+")); }
      | "-"   { $$ = autorelease(new std::string("-")); }
      | "*"   { $$ = autorelease(new std::string("*")); }
      | "/"   { $$ = autorelease(new std::string("/")); }
      | "%"   { $$ = autorelease(new std::string("%")); }

idseq: id       { $$ = autorelease(new str::seq()); $$->push_back(*$1); }
     | idseq id { $$ = $1;                          $$->push_back(*$2); }

types: l0mtype       { $$ = autorelease(new MonoTypes()); $$->push_back(*$1); }
     | types l0mtype { $$ = $1;                           $$->push_back(*$2); }

/* expressions */
l0expr: "\\" patterns "." l0expr { $$ = makePatternFn(*$2, ExprPtr($4), m(@1, @4)); }
      | "!" l1expr               { $$ = TAPP1(var("not",m(@1)), $2, m(@1,@2)); }
      | l0expr "and" l0expr      { $$ = TAPP2(var("and",m(@2)), $1, $3, m(@1,@3)); }
      | l0expr "or"  l0expr      { $$ = TAPP2(var("or",m(@2)),  $1, $3, m(@1,@3)); }
      | l0expr "o"   l0expr      { $$ = TAPP2(var("compose",m(@2)), $1, $3, m(@1,@3)); }
      | l1expr "<-"  l1expr      { $$ = new Assign(ExprPtr($1), ExprPtr($3), m(@1, @3)); }
      | l1expr "in"  l1expr      { $$ = TAPP2(var("in",m(@2)), $1, $3, m(@1,@3)); }
      | l1expr                   { $$ = $1; }

l1expr: "if" l0expr "then" l0expr "else" l0expr { $$ = TAPP3(var("if",m(@1)), $2, $4, $6, m(@1, @6)); }
      | l2expr                                  { $$ = $1; }

l2expr: l2expr "~"   l2expr { $$ = TAPP2(var("~",m(@2)), $1, $3, m(@1,@3)); }
      | l2expr "===" l2expr { $$ = TAPP2(var("===",m(@2)), $1, $3, m(@1,@3)); }
      | l2expr "=="  l2expr { $$ = TAPP2(var("==",m(@2)), $1, $3, m(@1,@3)); }
      | l2expr "!="  l2expr { $$ = TAPP1(var("not",m(@2)), TAPP2(var("==",m(@2)), $1, $3, m(@1,@3)), m(@1,@3)); }
      | l2expr "<"   l2expr { $$ = TAPP2(var("<",m(@2)),  $1, $3, m(@1,@3)); }
      | l2expr "<="  l2expr { $$ = TAPP2(var("<=",m(@2)), $1, $3, m(@1,@3)); }
      | l2expr ">"   l2expr { $$ = TAPP2(var(">",m(@2)),  $1, $3, m(@1,@3)); }
      | l2expr ">="  l2expr { $$ = TAPP2(var(">=",m(@2)), $1, $3, m(@1,@3)); }
      | l3expr              { $$ = $1; }

l3expr: l3expr "+"  l3expr { $$ = TAPP2(var("+",m(@2)), $1, $3, m(@1,@3)); }
      | l3expr "-"  l3expr { $$ = TAPP2(var("-",m(@2)), $1, $3, m(@1,@3)); }
      | l3expr "++" l3expr { $$ = TAPP2(var("append",m(@2)), $1, $3, m(@1,@3)); }
      | "-" l3expr         { $$ = TAPP1(var("neg",m(@1)), ExprPtr($2), m(@1,@2)); }
      | l4expr             { $$ = $1; }

l4expr: l4expr "*" l4expr { $$ = TAPP2(var("*", m(@2)), $1, $3, m(@1, @3)); }
      | l4expr "/" l4expr { $$ = TAPP2(var("/", m(@2)), $1, $3, m(@1, @3)); }
      | l4expr "%" l4expr { $$ = TAPP2(var("%", m(@2)), $1, $3, m(@1, @3)); }
      | l5expr            { $$ = $1; }

l5expr: l6expr { $$ = $1; }

      /* local variable introduction */
      | "let" letbindings "in" l0expr { $$ = compileNestedLetMatch(*$2, ExprPtr($4), m(@1,@4))->clone(); }
      | "let" letbindings ";" "in" l0expr { $$ = compileNestedLetMatch(*$2, ExprPtr($5), m(@1,@5))->clone(); }

      /* pattern matching */
      | "match" l6exprs "with" patternexps { $$ = compileMatch(yyParseCC, *$2, normPatternRules(*$4, m(@1,@4)), m(@1,@4))->clone(); }

      /* match test */
      | l6expr "matches" pattern { $$ = compileMatchTest(yyParseCC, ExprPtr($1), PatternPtr($3), m(@1,@3))->clone(); }

      /* parser generation */
      | "parse" "{" prules "}" {
        try {
          $$ = makeParser(yyParseCC, *$3, m(@1,@4))->clone();
        } catch (hobbes::compile_table_failure& ctf) {
          std::ostringstream ss;
          ss << ctf.what() << std::endl;
          ctf.print(ss);
          throw annotated_error(m(@1,@4), ss.str());
        }
      }

      /* action sequence */
      | "do" "{" dobindings "}"                 { $$ = compileNestedLetMatch(*$3, ExprPtr(new Unit(m(@1,@4))), m(@1,@4)); }
      | "do" "{" dobindings "return" l0expr "}" { $$ = compileNestedLetMatch(*$3, ExprPtr($5), m(@1,@6)); }

      /* forced type assignment */
      | l6expr "::" qtype       { $$ = new Assump(ExprPtr($1), QualTypePtr($3), m(@1,@3)); }

letbindings: letbindings ";" letbinding { $1->push_back(*$3); $$ = $1; }
           | letbinding                 { $$ = autorelease(new LetBindings()); $$->push_back(*$1); }

letbinding: irrefutablep "=" l1expr { $$ = autorelease(new LetBinding(PatternPtr($1), ExprPtr($3))); }

dobindings: dobindings dobinding { $$ = $1; $$->push_back(*$2); }
          | dobinding            { $$ = autorelease(new LetBindings()); $$->push_back(*$1); }

dobinding: irrefutablep "=" l0expr ";" { $$ = autorelease(new LetBinding(PatternPtr($1), ExprPtr($3))); }
         | l0expr ";"                  { $$ = autorelease(new LetBinding(PatternPtr(new MatchAny("_",m(@1))), ExprPtr($1))); }

/* assignment */
l6expr: l6expr "(" cargs ")"    { $$ = new App(ExprPtr($1), *$3, m(@1, @4)); }
      | id                      { $$ = varCtorFn(*$1, m(@1)); }

      /* array construction / elimination */
      | "[" l0expr ".." l0expr "]"                        { $$ = new App(var("range", m(@3)), list(ExprPtr($2), ExprPtr($4)), m(@1, @5)); }
      | "[" l0expr ".." "]"                               { $$ = new App(var("iterateS", m(@3)), list(ExprPtr($2), fn(str::strings(".x"), fncall(var("+", m(@3)), list(var(".x", m(@3)), ExprPtr(new Int(1, m(@3)))), m(@3)), m(@3))), m(@1, @4)); }
      | "[" l0expr "|" pattern "<-" l0expr "]"            { $$ = compileArrayComprehension(ExprPtr($2), PatternPtr($4), ExprPtr($6), m(@1, @7)); }
      | "[" l0expr "|" pattern "<-" l0expr "," l0expr "]" { $$ = compileArrayComprehension(ExprPtr($2), PatternPtr($4), ExprPtr($6), ExprPtr($8), m(@1, @9)); }
      | "[" cargs "]"                                     { $$ = new MkArray(*$2, m(@1, @3)); }
      | l6expr "[" l0expr "]"                             { $$ = new AIndex(ExprPtr($1), ExprPtr($3), m(@1, @4)); }
      | l6expr "[" l0expr ":" l0expr "]"                  { $$ = new App(var("slice", m(@4)), list(ExprPtr($1), ExprPtr($3), ExprPtr($5)), m(@1, @6)); }
      | l6expr "[" l0expr ":" "]"                         { std::string vn = freshName(); $$ = new Let(vn, ExprPtr($1), fncall(var("slice",m(@4)), list(var(vn,m(@1)), ExprPtr($3), fncall(var("size",m(@4)), list(var(vn,m(@1))),m(@1))),m(@1,@5)), m(@1, @5)); }
      | l6expr "[" ":" l0expr "]"                         { std::string vn = freshName(); $$ = new Let(vn, ExprPtr($1), fncall(var("slice",m(@3)), list(var(vn,m(@1)), fncall(var("size",m(@3)), list(var(vn,m(@3))),m(@1)), ExprPtr($4)), m(@1,@5)), m(@1, @5)); }

      /* variant construction / elimination */
      | "|" id "=" l0expr "|"                                 { $$ = new MkVariant(*$2, ExprPtr($4), m(@1, @5)); }
      | "|" "intV" "=" l0expr "|"                             { $$ = new MkVariant(".f" + str::from($2), ExprPtr($4), m(@1, @5)); }
      | "|" id "|"                                            { $$ = new MkVariant(*$2, ExprPtr(new Unit(m(@2))), m(@1, @3)); }
      | "case" l0expr "of" "|" varfields "|"                  { $$ = new Case(ExprPtr($2), *$5, m(@1, @6)); }
      | "case" l0expr "of" "|" varfields "|" "default" l0expr { $$ = new Case(ExprPtr($2), *$5, ExprPtr($8), m(@1, @8)); }

      /* record construction / elimination */
      | "{" recfields "}"     { if ($2->size() > 0) { $$ = new MkRecord(*$2, m(@1, @3)); } else { $$ = new Unit(m(@1, @3)); } }
      | "{" recfields "," "}" { if ($2->size() > 0) { $$ = new MkRecord(*$2, m(@1, @4)); } else { $$ = new Unit(m(@1, @4)); } }
      | l6expr recfieldpath   { $$ = makeProjSeq($1, *$2, m(@1, @2)); }

      /* record sections */
      | recfieldpath { $$ = new Fn(str::strings("x"), proj(var("x", m(@1)), *$1, m(@1)), m(@1)); }

      /* existential construction / elimination */
      | "pack" l6expr                      { $$ = new Pack(ExprPtr($2), m(@1, @2)); }
      | "unpack" id "=" l6expr "in" l6expr { $$ = new Unpack(*$2, ExprPtr($4), ExprPtr($6), m(@1, @6)); }

      /* constant values */
      | "boolV"     { $$ = new Bool($1, m(@1)); }
      | "charV"     { $$ = new Char(str::readCharDef(*$1), m(@1)); }
      | "byteV"     { $$ = new Byte(str::dehex(*$1), m(@1)); }
      | "bytesV"    { $$ = mkarray(str::dehexs(*$1), m(@1)); }
      | "shortV"    { $$ = new Short($1, m(@1)); }
      | "intV"      { $$ = new Int($1, m(@1)); }
      | "longV"     { $$ = new Long($1, m(@1)); }
      | "floatV"    { $$ = new Float($1, m(@1)); }
      | "doubleV"   { $$ = new Double($1, m(@1)); }
      | "stringV"   { $$ = mkarray(str::unescape(str::trimq(*$1)), m(@1)); }
      | tsseq       { $$ = mkTimespanExpr(*$1, m(@1))->clone(); }
      | "timeV"     { $$ = mkTimeExpr(*$1, m(@1))->clone(); }
      | "dateTimeV" { $$ = mkDateTimeExpr(*$1, m(@1))->clone(); }

      /* take care of unit values, nested terms, and tuples */
      | "(" cargs ")" { $$ = pickNestedExp($2, m(@1,@3)); }

      /* escapes for infix functions */
      | "(" "++" ")"  { $$ = new Var("append", m(@2)); }
      | "(" "+" ")"   { $$ = new Var("+",      m(@2)); }
      | "(" "-" ")"   { $$ = new Var("-",      m(@2)); }
      | "(" "*" ")"   { $$ = new Var("*",      m(@2)); }
      | "(" "/" ")"   { $$ = new Var("/",      m(@2)); }
      | "(" "%" ")"   { $$ = new Var("%",      m(@2)); }
      | "(" "~" ")"   { $$ = new Var("~",      m(@2)); }
      | "(" "===" ")" { $$ = new Var("===",    m(@2)); }
      | "(" "==" ")"  { $$ = new Var("==",     m(@2)); }
      | "(" "!=" ")"  { $$ = new Var("!=",     m(@2)); }
      | "(" "<" ")"   { $$ = new Var("<",      m(@2)); }
      | "(" ">" ")"   { $$ = new Var(">",      m(@2)); }
      | "(" ">=" ")"  { $$ = new Var(">=",     m(@2)); }
      | "(" "<=" ")"  { $$ = new Var("<=",     m(@2)); }
      | "(" "and" ")" { $$ = new Var("and",    m(@2)); }
      | "(" "or" ")"  { $$ = new Var("or",     m(@2)); }
      | "(" "in" ")"  { $$ = new Var("in",     m(@2)); }
      | "(" "!" ")"   { $$ = new Var("not",    m(@2)); }

      /* quoted expressions */
      | "`" l0expr "`" { $$ = new Assump(fncall(var("unsafeCast", m(@2)), list(mktunit(m(@2))), m(@2)), qualtype(tapp(primty("quote"), list(texpr(ExprPtr($2))))), m(@2)); }

prules: prules prule { $$ = $1; $$->push_back(*$2); }
      | prule        { $$ = autorelease(new Grammar()); $$->push_back(*$1); }

prule: id ":=" prdefs { $$ = autorelease(new Grammar::value_type(*$1, *$3)); }

prdefs: prdefs "|" prdef { $$ = $1; $$->push_back(*$3); }
      | prdef            { $$ = autorelease(new GrammarRules()); $$->push_back(*$1); }

prdef: pbelems "{" l0expr "}" { $$ = autorelease(new GrammarRule(*$1, ExprPtr($3))); }

pbelems: pbelems pbelem { $$ = $1; $$->push_back(*$2); }
       | /* nothing */  { $$ = autorelease(new BoundGrammarValues()); }

pbelem: id ":" pvalue { $$ = autorelease(new BoundGrammarValue(*$1, GrammarValuePtr($3))); }
      | pvalue        { $$ = autorelease(new BoundGrammarValue("_", GrammarValuePtr($1))); }

pvalue: id            { $$ = new GSymRef(*$1, m(@1)); }
      | "stringV"     { $$ = new GStr(str::unescape(str::trimq(*$1)), m(@1)); }
      | "charV"       { $$ = new GStr(std::string(1, str::readCharDef(*$1)), m(@1)); }

tsseq: "timespanV"       { $$ = autorelease(new str::seq()); $$->push_back(*$1); }
     | tsseq "timespanV" { $$ = $1; $$->push_back(*$2); }

l6exprs: l6exprs l6expr { $$ = $1; $$->push_back(ExprPtr($2)); }
       | l6expr         { $$ = autorelease(new Exprs()); $$->push_back(ExprPtr($1)); }

patternexps: patternexps patternexp { $$ = $1; $$->push_back(*$2); }
           | patternexp             { $$ = autorelease(new PatternRows()); $$->push_back(*$1); }

patternexp: "|" patterns "->" l0expr                { $$ = autorelease(new PatternRow(*$2, ExprPtr($4))); }
          | "|" patterns "where" l0expr "->" l0expr { $$ = autorelease(new PatternRow(*$2, ExprPtr($4), ExprPtr($6))); }
/*        | "|" patterns                            { $$ = autorelease(new PatternRow(*$2, ExprPtr(0))); } // introduces ambiguous syntax, but could be used for fall-through logic in match expressions */

patterns: patterns pattern { $$ = $1; $$->push_back(PatternPtr($2)); }
        | pattern          { $$ = autorelease(new Patterns()); $$->push_back(PatternPtr($1)); }

refutablep: "boolV"                    { $$ = new MatchLiteral(PrimitivePtr(new Bool($1, m(@1))), m(@1)); }
          | "charV"                    { $$ = new MatchLiteral(PrimitivePtr(new Char(str::readCharDef(*$1), m(@1))), m(@1)); }
          | "byteV"                    { $$ = new MatchLiteral(PrimitivePtr(new Byte(str::dehex(*$1), m(@1))), m(@1)); }
          | "shortV"                   { $$ = new MatchLiteral(PrimitivePtr(new Short($1, m(@1))), m(@1)); }
          | "intV"                     { $$ = new MatchLiteral(PrimitivePtr(new Int($1, m(@1))), m(@1)); }
          | "longV"                    { $$ = new MatchLiteral(PrimitivePtr(new Long($1, m(@1))), m(@1)); }
          | "doubleV"                  { $$ = new MatchLiteral(PrimitivePtr(new Double($1, m(@1))), m(@1)); }
          | "bytesV"                   { $$ = mkpatarray(str::dehexs(*$1), m(@1)); }
          | "stringV"                  { $$ = mkpatarray(str::unescape(str::trimq(*$1)), m(@1)); }
          | tsseq                      { $$ = new MatchLiteral(mkTimespanPrim(*$1, m(@1)), mkTimespanExpr(*$1, m(@1)), m(@1)); }
          | "timeV"                    { $$ = new MatchLiteral(mkTimePrim(*$1, m(@1)), mkTimeExpr(*$1, m(@1)), m(@1)); }
          | "dateTimeV"                { $$ = new MatchLiteral(mkDateTimePrim(*$1, m(@1)), mkDateTimeExpr(*$1, m(@1)), m(@1)); }
          | "regexV"                   { $$ = new MatchRegex(std::string($1->begin() + 1, $1->end() - 1), m(@1)); }
          | "[" patternseq "]"         { $$ = new MatchArray(*$2, m(@1,@3)); }
          | "[" patternseq "," "]"     { $$ = new MatchArray(*$2, m(@1,@4)); }
          | "|" id "|"                 { $$ = new MatchVariant(*$2, PatternPtr(new MatchLiteral(PrimitivePtr(new Unit(m(@2))), m(@2))), m(@1,@3)); }
          | "|" id "=" pattern "|"     { $$ = new MatchVariant(*$2, PatternPtr($4), m(@1,@5)); }
          | "|" "intV" "=" pattern "|" { $$ = new MatchVariant(".f" + str::from($2), PatternPtr($4), m(@1,@5)); }
          | "(" patternseq ")"         { $$ = pickNestedPat($2, m(@1,@3)); }
          | "(" patternseq "," ")"     { $$ = pickNestedPat($2, m(@1,@4)); }
          | "{" recpatfields "}"       { $$ = new MatchRecord(*$2, m(@1,@3)); }
          | "{" recpatfields "," "}"   { $$ = new MatchRecord(*$2, m(@1,@4)); }
          | id                         { $$ = patVarCtorFn(*$1, m(@1)); }

irrefutablep: id                       { $$ = new MatchAny(*$1, m(@1)); }
            | "(" patternseq ")"       { $$ = pickNestedPat($2, m(@1,@3)); }
            | "(" patternseq "," ")"   { $$ = pickNestedPat($2, m(@1,@4)); }
            | "{" recpatfields "}"     { $$ = new MatchRecord(*$2, m(@1,@3)); }
            | "{" recpatfields "," "}" { $$ = new MatchRecord(*$2, m(@1,@4)); }

pattern: refutablep { $$ = $1; }

patternseq: patternseqn   { $$ = $1; }
          | /* nothing */ { $$ = new Patterns(); }

patternseqn: patternseqn "," pattern { $$ = $1; $$->push_back(PatternPtr($3)); }
           | pattern                 { $$ = new Patterns(); $$->push_back(PatternPtr($1)); }

recpatfields: recpatfields "," recpatfield { $$ = $1; $$->push_back(*$3); }
            | recpatfield                  { $$ = new MatchRecord::Fields(); $$->push_back(*$1); }

recpatfield: id "=" pattern { $$ = new MatchRecord::Field(*$1, PatternPtr($3)); }

recfields: /* nothing */                         { $$ = autorelease(new MkRecord::FieldDefs()); }
         | recfieldname "=" l0expr               { $$ = autorelease(new MkRecord::FieldDefs()); $$->push_back(MkRecord::FieldDef(*$1, ExprPtr($3))); }
         | recfields "," recfieldname "=" l0expr { $$ = $1;                                     $$->push_back(MkRecord::FieldDef(*$3, ExprPtr($5))); }

recfieldname: id         { $$ = $1; }
            | "data"     { $$ = autorelease(new std::string("data")); }
            | "type"     { $$ = autorelease(new std::string("type")); }
            | "where"    { $$ = autorelease(new std::string("where")); }
            | "class"    { $$ = autorelease(new std::string("class")); wantIndent(false); }
            | "instance" { $$ = autorelease(new std::string("instance")); wantIndent(false); }
            | "exists"   { $$ = autorelease(new std::string("exists")); }
            | "import"   { $$ = autorelease(new std::string("import")); }
            | "module"   { $$ = autorelease(new std::string("module")); }
            | "parse"    { $$ = autorelease(new std::string("parse")); }
            | "do"       { $$ = autorelease(new std::string("do")); }
            | "return"   { $$ = autorelease(new std::string("return")); }
            | "intV"     { $$ = autorelease(new std::string(".f" + str::from($1))); }

recfieldpath: recfieldpath "." recfieldname { $$ = $1; $$->push_back(*$3); }
            | recfieldpath "tupSection"     { $$ = $1; str::seq x = tupSectionFields(*$2); $$->insert($$->end(), x.begin(), x.end()); }
            | "." recfieldname              { $$ = autorelease(new str::seq()); $$->push_back(*$2); }
            | "tupSection"                  { $$ = autorelease(new str::seq()); *$$ = tupSectionFields(*$1); }

varfields: varbind               { $$ = autorelease(new Case::Bindings()); $$->push_back(*$1); }
         | varfields "," varbind { $$ = $1; $$->push_back(*$3); }

varbind: id            "=" l0expr { $$ = autorelease(new Case::Binding(*$1, *$1, ExprPtr($3))); }
       | id     ":" id "=" l0expr { $$ = autorelease(new Case::Binding(*$1, *$3, ExprPtr($5))); }
       | "intV" ":" id "=" l0expr { $$ = autorelease(new Case::Binding(".f" + str::from($1), *$3, ExprPtr($5))); }

cargs: /* nothing */    { $$ = autorelease(new Exprs()); }
     | l0expr           { $$ = autorelease(new Exprs()); $$->push_back(ExprPtr($1)); }
     | cargs "," l0expr { $1->push_back(ExprPtr($3)); $$ = $1; }

qtype : cst "=>" l0mtype { $$ = new QualType(*$1, *$3); }
      | l0mtype          { $$ = new QualType(Constraints(), *$1); }

/* to avoid parsing ambiguity, we require all type constraints to be in parens (this could be solved with a better parser) */
cst: "(" tpreds ")" { $$ = $2; }

tpreds: tpred            { $$ = autorelease(new Constraints()); $$->push_back(ConstraintPtr($1)); }
      | tpreds "," tpred { $1->push_back(ConstraintPtr($3)); $$ = $1; }

tpred: id l1mtargl                                    { $$ = new Constraint(*$1, *$2); }
     | l1mtype "==" l1mtype                           { $$ = new Constraint(EqualTypes::constraintName(), list(*$1, *$3)); }
     | l1mtype "!=" l1mtype                           { $$ = new Constraint(NotEqualTypes::constraintName(), list(*$1, *$3)); }
     | l1mtype "~" l1mtype                            { $$ = new Constraint(FixIsoRecur::constraintName(), list(*$1, *$3)); }
     | l1mtype "=" "{" l1mtype "*" l1mtype "}"        { $$ = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *$1, freshTypeVar(),  *$4, *$6)); }
     | l1mtype "=" "{" id ":" l1mtype "*" l1mtype "}" { $$ = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(0), *$1, TVar::make(*$4), *$6, *$8)); }
     | l1mtype "=" "(" l1mtype "*" l1mtype ")"        { $$ = new Constraint(RecordDeconstructor::constraintName(), list(tlong(1), tlong(1), *$1, freshTypeVar(),  *$4, *$6)); }
     | "{" l1mtype "*" l1mtype "}"        "=" l1mtype { $$ = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *$7, freshTypeVar(),  *$2, *$4)); }
     | "{" id ":" l1mtype "*" l1mtype "}" "=" l1mtype { $$ = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(0), *$9, TVar::make(*$2), *$4, *$6)); }
     | "(" l1mtype "*" l1mtype ")"        "=" l1mtype { $$ = new Constraint(RecordDeconstructor::constraintName(), list(tlong(0), tlong(1), *$7, freshTypeVar(),  *$2, *$4)); }

     | l1mtype "." recfieldname "::" l1mtype          { $$ = HasField::newConstraint(HasField::Read,  *$1, TString::make(*$3), *$5); }
     | l1mtype "." recfieldname "<-" l1mtype          { $$ = HasField::newConstraint(HasField::Write, *$1, TString::make(*$3), *$5); }
     | l1mtype "/" l1mtype "::" l1mtype               { $$ = HasField::newConstraint(HasField::Read,  *$1, *$3,                *$5); }
     | l1mtype "/" l1mtype "<-" l1mtype               { $$ = HasField::newConstraint(HasField::Write, *$1, *$3,                *$5); }

     | l1mtype "=" "|" l1mtype "+" l1mtype "|"        { $$ = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *$1, freshTypeVar(),  *$4, *$6)); }
     | "|" l1mtype "+" l1mtype "|" "=" l1mtype        { $$ = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *$7, freshTypeVar(),  *$2, *$4)); }
     | l1mtype "=" "|" id ":" l1mtype "+" l1mtype "|" { $$ = new Constraint(VariantDeconstructor::constraintName(), list(tlong(1), *$1, TVar::make(*$4), *$6, *$8)); }
     | "|" id ":" l1mtype "+" l1mtype "|" "=" l1mtype { $$ = new Constraint(VariantDeconstructor::constraintName(), list(tlong(0), *$9, TVar::make(*$2), *$4, *$6)); }

     | "|" id ":" l0mtype "|" "::" l1mtype            { $$ = new Constraint(CtorVerifier::constraintName(), list(*$7, TString::make(*$2), *$4)); }
     | "|" l1mtype "/" l0mtype "|" "::" l1mtype       { $$ = new Constraint(CtorVerifier::constraintName(), list(*$7, *$2,                *$4)); }
     | l1mtype "++" l1mtype "=" l1mtype               { $$ = new Constraint(AppendsToUnqualifier::constraintName(), list(*$1, *$3, *$5)); }

l1mtargl: l1mtype          { $$ = autorelease(new MonoTypes()); $$->push_back(*$1); }
        | l1mtargl l1mtype { $1->push_back(*$2); $$ = $1; }

ltmtype : ltmtype l0mtype { $$ = $1; $$->push_back(*$2); }
        | l0mtype         { $$ = autorelease(new MonoTypes()); $$->push_back(*$1); }

l0mtype: l0mtargl "->" l1mtype { $$ = autorelease(new MonoTypePtr(Func::make(tuplety(*$1), *$3))); }
       | mtuplist              { $$ = autorelease(new MonoTypePtr(makeTupleType(*$1))); }
       | msumlist              { $$ = autorelease(new MonoTypePtr(makeSumType(*$1))); }

l1mtype: id                                { $$ = autorelease(new MonoTypePtr(monoTypeByName(*$1))); }
       | "<" cppid ">"                     { $$ = autorelease(new MonoTypePtr(OpaquePtr::make(str::replace<char>(*$2, ".", "::"), 0, false))); }
       | "[" "]"                           { $$ = autorelease(new MonoTypePtr(Prim::make("[]"))); }
       | "[" ltmtype "]"                   { try { $$ = autorelease(new MonoTypePtr(Array::make(yyParseCC->replaceTypeAliases(accumTApp(*$2))))); } catch (std::exception& ex) { throw annotated_error(m(@2), ex.what()); } }
       | "[" ":" l0mtype "|" tyind ":" "]" { $$ = autorelease(new MonoTypePtr(FixedArray::make(*$3, *$5))); }
       | "(" "->" ")"                      { $$ = autorelease(new MonoTypePtr(Prim::make("->"))); }
       | "(" ltmtype ")"                   { try { $$ = autorelease(new MonoTypePtr(clone(yyParseCC->replaceTypeAliases(accumTApp(*$2))))); } catch (std::exception& ex) { throw annotated_error(m(@2), ex.what()); } }
       | "{" mreclist "}"                  { $$ = autorelease(new MonoTypePtr(makeRecType(*$2))); }
       | "|" mvarlist "|"                  { $$ = autorelease(new MonoTypePtr(makeVarType(*$2))); }
       | "(" ")"                           { $$ = autorelease(new MonoTypePtr(Prim::make("unit"))); }
       | "intV"                            { $$ = autorelease(new MonoTypePtr(($1 == 0) ? Prim::make("void") : TLong::make($1))); }
       | "boolV"                           { $$ = autorelease(new MonoTypePtr($1 ? TLong::make(1) : TLong::make(0))); }
       | "exists" id "." l1mtype           { $$ = autorelease(new MonoTypePtr(Exists::make(*$2, *$4))); }
       | l1mtype "@" l1mtype               { $$ = autorelease(new MonoTypePtr(TApp::make(Prim::make("fileref"), list(*$1, *$3)))); }
       | l1mtype "@" "?"                   { $$ = autorelease(new MonoTypePtr(TApp::make(Prim::make("fileref"), list(*$1)))); }
       | "^" id "." l1mtype                { $$ = autorelease(new MonoTypePtr(Recursive::make(*$2, *$4))); }
       | "stringV"                         { $$ = autorelease(new MonoTypePtr(TString::make(str::unescape(str::trimq(*$1))))); }
       | "`" l0expr "`"                    { $$ = autorelease(new MonoTypePtr(TApp::make(primty("quote"), list(texpr(ExprPtr($2)))))); }

tyind: id     { $$ = autorelease(new MonoTypePtr(TVar::make(*$1))); }
     | "intV" { $$ = autorelease(new MonoTypePtr(TLong::make($1))); }

cppid: id           { $$ = $1; }
     | cppid "." id { $$ = $1; *$$ += "."; *$$ += *$3; }

l0mtargl: l1mtype                       { $$ = autorelease(new MonoTypes()); $$->push_back(*$1); }
        | "(" l0mtype "," l0mtarglt ")" { $4->insert($4->begin(), *$2); $$ = $4; }

l0mtarglt: l0mtype               { $$ = autorelease(new MonoTypes()); $$->push_back(*$1); }
         | l0mtarglt "," l0mtype { $1->push_back(*$3); $$ = $1; }

mtuplist: l1mtype              { $$ = autorelease(new MonoTypes()); $$->push_back(*$1); }
        | mtuplist "*" l1mtype { $$ = $1; $$->push_back(*$3); }

msumlist: l1mtype "+" l1mtype  { $$ = autorelease(new MonoTypes()); $$->push_back(*$1); $$->push_back(*$3); }
        | msumlist "+" l1mtype { $$ = $1; $$->push_back(*$3); }

mreclist: mreclist "," id ":" l0mtype { $$ = $1;                                 $$->push_back(Record::Member(*$3, *$5)); }
        | id ":" l0mtype              { $$ = autorelease(new Record::Members()); $$->push_back(Record::Member(*$1, *$3)); }

mvarlist: mvarlist "," id ":" l0mtype { $$ = $1;                                  $$->push_back(Variant::Member(*$3, *$5,                0)); }
        | mvarlist "," id             { $$ = $1;                                  $$->push_back(Variant::Member(*$3, Prim::make("unit"), 0)); }
        | id ":" l0mtype              { $$ = autorelease(new Variant::Members()); $$->push_back(Variant::Member(*$1, *$3,                0)); }
        | id                          { $$ = autorelease(new Variant::Members()); $$->push_back(Variant::Member(*$1, Prim::make("unit"), 0)); }

id: TIDENT;

%%


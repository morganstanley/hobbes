
#include <cstdio>
#include <fstream>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/module.H>
#include <hobbes/lang/pat/pattern.H>
#include <hobbes/parse/grammar.H>
#include <hobbes/read/parser.H>
#include <hobbes/read/pgen/hexpr.parse.H>
#include <hobbes/util/autorelease.H>
#include <iostream>
#include <mutex>
#include <stack>
#include <stdexcept>
#include <string>

// protect access to lexer/parser globals (blech)
static std::recursive_mutex parse_mutex;
struct parse_mutex_lock {
   parse_mutex_lock() { parse_mutex.lock(); }
  ~parse_mutex_lock() { parse_mutex.unlock(); }
};
#define LOCK_PARSER parse_mutex_lock parse_lock

////////////////////////////////////////////////////////
// bison parser defs
extern hobbes::cc*     yyParseCC;
extern std::string     yyVexpLexError;
extern hobbes::Module* yyParsedModule;
extern std::string     yyParsedVar;
extern hobbes::Expr*   yyParsedExpr;
extern std::string     yyMatchDiagram;
extern int             yyInitToken;
extern std::string     yyModulePath;
extern int             yylineno;
extern int             yycolumn;
extern YYLTYPE         yyErrPos;

extern int yyparse();
#define YY_BUF_SIZE 16384
struct yy_buffer_state;
using YY_BUFFER_STATE = yy_buffer_state *;
extern YY_BUFFER_STATE yy_scan_string(const char*);
extern YY_BUFFER_STATE yy_create_buffer(FILE*, int);
extern void yy_switch_to_buffer(YY_BUFFER_STATE);
extern void yy_delete_buffer(YY_BUFFER_STATE);
////////////////////////////////////////////////////////

namespace hobbes {

[[noreturn]] void throwFileError(const std::string& fname, const YYLTYPE& errPos, const std::string& emsg) {
  throw annotated_error::fileError(fname, Pos(errPos.first_line, errPos.first_column), Pos(errPos.last_line, errPos.last_column), emsg);
}

[[noreturn]] void throwBufferError(const char* buffer, const YYLTYPE& errPos, const std::string& emsg) {
  throw annotated_error::bufferError(buffer, Pos(errPos.first_line, errPos.first_column), Pos(errPos.last_line, errPos.last_column), emsg);
}

// (parser may call itself recursively when importing scripts)
std::stack<YY_BUFFER_STATE> activeParseBuffers;

void freeParserData() {
  if (activeParseBuffers.empty()) {
    AutoreleaseSet::reset();
  }
}

template <typename T>
  const T& checkReturn(const T& x) {
    if (!x) {
      throw std::runtime_error("Internal error: parser failed to produce a value");
    } else {
      return x;
    }
  }

// KNOWN LEAK ON THE SYNTAX-ERROR PATH (documented, not fixed)
//
// When yyparse hits a syntax error it recovers by discarding the semantic
// values on its stack. Those values are partly-built AST fragments, and the
// grammar declares no bison %destructor for them, so whichever ones are not
// registered with the AutoreleaseSet are orphaned -- notably the bare `new`
// results from defVarCtor / defPatVarCtor below, and the source-text copy that
// pushLiteralContext attaches to each fragment's LexicalAnnotation. A
// successful parse does not leak: the completed tree is returned and owned by
// the caller's ExprPtr.
//
// Measured at ~200 bytes per rejected parse for the tested short input. Because
// an orphaned fragment's LexicalAnnotation shares ownership of the complete
// source-text buffer created by pushLiteralContext, the retained bytes can
// also grow with input length. LeakSanitizer roots it at the yyparse call below,
// defPatVarCtor, and pushLiteralContext; live-heap bytes are flat across valid
// parses and climb only on rejected ones. The RSS growth visible while fuzzing
// is separate AddressSanitizer quarantine/shadow retention, which release
// builds do not have.
//
// This is left as-is deliberately. In the deployed shape -- an embedded
// compiler parsing source it is given, or an RPC peer sending expressions --
// malformed input is the rare case, so a few hundred bytes per rejection is a
// slow leak that a process restart clears. A real fix means giving the grammar
// %destructor coverage (or autoreleasing every semantic-value allocation) and
// touching the checked-in generated parser, which is not worth the risk for
// the exposure. Revisit if a path ever parses attacker-controlled input in a
// tight loop without restarting.

// Every function that walks an expression recurs through its levels of nesting
// -- type inference, unsweetening, printing, and the expression's own
// destructor -- so how deeply an expression nests is stack depth in everything
// downstream of the parse, and the parser will build a tree as deep as its
// input describes. Source nested this deeply is not written by hand; it is
// generated, or it is hostile: reading source text is on the near side of the
// trust boundary (see SECURITY.md), and `x+x+x...` repeated enough times is all
// it takes. Bound it here, where every expression the parser returns has to
// pass, rather than in each of those walks.
const size_t maxExprNestingDepth = 1000;

// takes the expression by pointer, and must be given the only reference to it:
// an expression past the limit is let go here one level at a time, and a second
// reference left anywhere would run the destructor chain that the limit is
// meant to prevent when it went out of scope
void checkNestingDepth(ExprPtr* e) {
  const size_t d = nestingDepth(*e);
  if (d > maxExprNestingDepth) {
    releaseNesting(*e);

    throw std::runtime_error(
      "Expression nests " + str::from(d) + " levels deep, past the limit of " + str::from(maxExprNestingDepth)
    );
  }
}

void runParserOnBuffer(cc* c, int initTok, YY_BUFFER_STATE bs) {
  yyParseCC   = c;
  yyInitToken = initTok;
  yylineno    = 1;
  yycolumn    = 1;
  yy_switch_to_buffer(bs);
  activeParseBuffers.push(bs);

  yyparse();

  activeParseBuffers.pop();
  freeParserData();
  yy_delete_buffer(bs);
  if (!activeParseBuffers.empty()) { yy_switch_to_buffer(activeParseBuffers.top()); }

  if (!yyVexpLexError.empty()) {
    std::string msg = yyVexpLexError;
    yyVexpLexError = "";
    throw std::runtime_error(msg);
  }
}

void runParserOnFile(cc* c, int initTok, const std::string& fname) {
  FILE* f = fopen(fname.c_str(), "r");
  if (f == nullptr) {
    throw std::runtime_error("Failed to open file for reading, '" + fname + "'");
  }
  try {
    LexicallyAnnotated::pushFileContext(fname);
    runParserOnBuffer(c, initTok, yy_create_buffer(f, YY_BUF_SIZE));
    fclose(f);
    LexicallyAnnotated::popContext();
  } catch (annotated_error&) {
    fclose(f);
    LexicallyAnnotated::popContext();
    throw;
  } catch (std::exception& ex) {
    fclose(f);
    LexicallyAnnotated::popContext();
    throwFileError(fname, yyErrPos, ex.what());
  }
}

void runParserOnString(cc* c, int initTok, const char* s) {
  try {
    LexicallyAnnotated::pushLiteralContext(s);
    runParserOnBuffer(c, initTok, yy_scan_string(s));
    LexicallyAnnotated::popContext();
  } catch (annotated_error&) {
    LexicallyAnnotated::popContext();
    throw;
  } catch (std::exception& ex) {
    LexicallyAnnotated::popContext();
    throwBufferError(s, yyErrPos, ex.what());
  }
}

ModulePtr defReadModuleFile(cc* c, const std::string& file) {
  LOCK_PARSER;

  yyParsedModule = nullptr;
  yyModulePath = str::rsplit(file, "/").first;
  runParserOnFile(c, TPARSEMODULE, file);
  yyModulePath = "";

  return checkReturn(yyParsedModule != nullptr ? ModulePtr(yyParsedModule) : ModulePtr());
}

ModulePtr defReadModule(cc* c, const char* text) {
  LOCK_PARSER;

  yyParsedModule = nullptr;
  runParserOnString(c, TPARSEMODULE, text);

  return checkReturn(yyParsedModule != nullptr ? ModulePtr(yyParsedModule) : ModulePtr());
}

ModulePtr defReadModule(cc* c, const std::string& text) {
  return defReadModule(c, text.c_str());
}

ExprDefn defReadExprDefn(cc* c, const std::string& expr) {
  LOCK_PARSER;

  yyParsedVar  = "";
  yyParsedExpr = nullptr;
  runParserOnString(c, TPARSEDEFN, expr.c_str());

  ExprPtr e = checkReturn(yyParsedExpr != nullptr ? ExprPtr(yyParsedExpr) : ExprPtr());
  checkNestingDepth(&e);
  return ExprDefn(yyParsedVar, e);
}

ExprPtr defReadExpr(cc* c, const std::string& expr) {
  LOCK_PARSER;

  yyParsedExpr = nullptr;
  runParserOnString(c, TPARSEEXPR, expr.c_str());

  ExprPtr e = checkReturn(yyParsedExpr != nullptr ? ExprPtr(yyParsedExpr) : ExprPtr());
  checkNestingDepth(&e);
  return e;
}

// allow variable and pattern variable overloading
// (these bare `new`s are not autoreleased, so a syntax error that discards them
// mid-parse leaks -- see runParserOnBuffer above)
Expr* defVarCtor(const std::string& vn, const LexicalAnnotation& la) { return new Var(vn, la); }
VarCtorFn varCtorFn = &defVarCtor;
void overrideVarCtor(VarCtorFn f) { varCtorFn = f; }

Pattern* defPatVarCtor(const std::string& vn, const LexicalAnnotation& la) { return new MatchAny(vn, la); }
PatVarCtorFn patVarCtorFn = &defPatVarCtor;
void overridePatVarCtor(PatVarCtorFn f) { patVarCtorFn = f; }

}


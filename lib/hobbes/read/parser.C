
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

  return ExprDefn(yyParsedVar, checkReturn(yyParsedExpr != nullptr ? ExprPtr(yyParsedExpr) : ExprPtr()));
}

ExprPtr defReadExpr(cc* c, const std::string& expr) {
  LOCK_PARSER;

  yyParsedExpr = nullptr;
  runParserOnString(c, TPARSEEXPR, expr.c_str());

  return checkReturn(yyParsedExpr != nullptr ? ExprPtr(yyParsedExpr) : ExprPtr());
}

// allow variable and pattern variable overloading
Expr* defVarCtor(const std::string& vn, const LexicalAnnotation& la) { return new Var(vn, la); }
VarCtorFn varCtorFn = &defVarCtor;
void overrideVarCtor(VarCtorFn f) { varCtorFn = f; }

Pattern* defPatVarCtor(const std::string& vn, const LexicalAnnotation& la) { return new MatchAny(vn, la); }
PatVarCtorFn patVarCtorFn = &defPatVarCtor;
void overridePatVarCtor(PatVarCtorFn f) { patVarCtorFn = f; }

}


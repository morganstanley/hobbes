
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
extern void pushLexerParseState();
extern void popLexerParseState();
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

size_t openParseCount() {
  return activeParseBuffers.size();
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

// Every expression the parser returns is bounded in how deeply it nests
// (checkNestingDepth, and the reasoning behind it, in lang/expr.H): the parser
// builds a tree as deep as its input describes, and everything that walks the
// tree afterwards recurs through its levels. The check throws with the tree
// still referenced from here, which is safe: expression teardown is iterative
// whoever lets go of the tree, so unwinding past it costs one frame, not one
// per level.
//
// The same bound is applied a second time, earlier, by the compilers that
// grammar actions run mid-parse on sub-expressions (compileMatch for `\`,
// `match`, `let` with a pattern and comprehensions; makeParser for `parse`
// blocks) -- those walk what they are given before the parse is over and the
// check here can see it (OSS-Fuzz 556791547).

// One parse, from the scanner's point of view: the buffer it reads from, the
// lexer state it starts with, and the undoing of both when it is over.
//
// The undoing is what matters, and it is in a destructor so that it happens
// however the parse ends. yyparse can return -- on success, or on a syntax
// error it has recovered from -- or it can throw, when a grammar action does
// (a regex literal over its term limit, a numeric literal too large for its
// type). Teardown used to follow yyparse as plain statements, so the throwing
// path skipped it: the buffer was never closed, the count of open parses never
// came back down, and since that count is how the parser decides it is
// between parses and may release what the last one autoreleased, nothing was
// released again for the rest of the process (the OSS-Fuzz parse-expr harness
// grew by about 44KB per rejected input that way).
//
// The lexer state is the other half. The scanner keeps the start condition
// and the off-side-rule bookkeeping between tokens, and a parse that ends
// early leaves them wherever the failure found them; see pushLexerParseState
// in hexpr.l for what that did to the next parse. Saved on the way in and put
// back on the way out, every parse starts as a fresh process would, and the
// parse of an imported script -- which runs nested inside its importer's --
// hands the importer's state back when it is done.
class ParseScope {
public:
  ParseScope(cc* c, int initTok, YY_BUFFER_STATE bs) : bs(bs) {
    yyParseCC      = c;
    yyInitToken    = initTok;
    yylineno       = 1;
    yycolumn       = 1;
    yyVexpLexError = ""; // a lexer error left by a parse that threw is not this one's
    pushLexerParseState();
    yy_switch_to_buffer(bs);
    activeParseBuffers.push(bs);
  }

  ~ParseScope() {
    activeParseBuffers.pop();
    freeParserData();
    yy_delete_buffer(this->bs);
    if (!activeParseBuffers.empty()) { yy_switch_to_buffer(activeParseBuffers.top()); }
    popLexerParseState();
  }

  ParseScope(const ParseScope&) = delete;
  ParseScope& operator=(const ParseScope&) = delete;
private:
  YY_BUFFER_STATE bs;
};

void runParserOnBuffer(cc* c, int initTok, YY_BUFFER_STATE bs) {
  {
    ParseScope scope(c, initTok, bs);
    yyparse();
  }

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
  checkNestingDepth(e);
  return ExprDefn(yyParsedVar, e);
}

ExprPtr defReadExpr(cc* c, const std::string& expr) {
  LOCK_PARSER;

  yyParsedExpr = nullptr;
  runParserOnString(c, TPARSEEXPR, expr.c_str());

  ExprPtr e = checkReturn(yyParsedExpr != nullptr ? ExprPtr(yyParsedExpr) : ExprPtr());
  checkNestingDepth(e);
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


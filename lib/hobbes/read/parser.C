
#include <cstdio>
#include <exception>
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
extern std::exception_ptr yyParseThrown;
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

// WHAT A FAILED PARSE FREES
//
// Grammar actions build tree nodes with a bare `new`, and the action that
// consumes a node takes ownership of it. A parse that fails leaves nodes that
// no action consumed, and each of the ways it can leave them has its own
// cleanup:
//
//  * values still on bison's stack at a syntax error are deleted by the
//    %destructor that hexpr.y declares for their types;
//  * strings, lists and the like are registered with the AutoreleaseSet as
//    they are made, and released between parses (see ParseScope, below);
//  * the finished tree, handed over by the start rule in a parse that then
//    fails, is deleted by the reader (see ParsedResult, below).
//
// The first of those only happens on the parser's own ways out, so an
// exception -- from the lexer (a literal out of range) or from an action (a
// match or a `parse` block that doesn't compile) -- is not allowed to unwind
// through yyparse: it is caught, the parse is stopped the parser's way, and
// runParserOnBuffer rethrows it afterwards (see yyParseThrown in hexpr.y).
//
// What is still stranded is the right-hand side of the one rule whose action
// threw, where that action hadn't yet taken ownership of it: bison leaves
// those symbols alone, since it can't tell which ones the action took.

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
    yyParseThrown  = nullptr;
    yyErrPos       = YYLTYPE(); // nor is the position of one (see parseErrorPos)
    yylloc         = YYLTYPE{1, 1, 1, 1}; // the lexer sets this per token, and an empty parse has none: its
                                          // end-of-file error would otherwise sit where the previous parse's last token was
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

// where an error in the parse just run was found. yyerror records the
// position of a syntax error, but an error thrown from a lexer or grammar
// action (an unsupported literal, a duplicate field name) goes past yyerror,
// and until ParseScope cleared it the position reported for one of those was
// whatever the last syntax error in the process left behind. With it cleared,
// such an error is reported at the token the parser was on when it was thrown.
YYLTYPE parseErrorPos() {
  return yyErrPos.first_line != 0 ? yyErrPos : yylloc;
}

void runParserOnBuffer(cc* c, int initTok, YY_BUFFER_STATE bs) {
  {
    ParseScope scope(c, initTok, bs);
    yyparse();
  }

  // what the lexer or a grammar action threw, carried around the parser rather
  // than through it (see yyParseThrown in hexpr.y)
  if (yyParseThrown) {
    std::exception_ptr thrown;
    std::swap(thrown, yyParseThrown);
    yyVexpLexError = "";
    std::rethrow_exception(thrown);
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
    throwFileError(fname, parseErrorPos(), ex.what());
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
    throwBufferError(s, parseErrorPos(), ex.what());
  }
}

// a module's expressions get the same nesting bound as an expression read on
// its own (see defReadExpr below): a definition's body, and the body of each
// member of an instance, are the places a module holds an expression
static void checkNestingDepth(const ModulePtr& m) {
  for (const auto& md : m->definitions()) {
    if (const MVarDef* vd = is<MVarDef>(md)) {
      checkNestingDepth(vd->varExpr());
    } else if (const InstanceDef* id = is<InstanceDef>(md)) {
      for (const auto& mvd : id->members()) {
        checkNestingDepth(mvd->varExpr());
      }
    }
  }
}

// What a parse made is handed over through yyParsedModule and yyParsedExpr,
// set by the action of the grammar's start rule. That action can run in a
// parse that fails: where a state has one thing it can do, bison does it
// without looking at the next token, so an expression followed by something
// that can't follow it is reduced all the way to the start rule -- its tree
// handed over -- before the token is read and reported ("unexpected intV,
// expecting end of file"). The error is thrown from runParserOnBuffer, past
// the statements that would have taken the tree, and since the pointer is
// cleared for the next parse, nothing ever freed it: a string literal of 1MB
// with a digit after its closing quote is a million character constants, and
// each read of it left 145MB behind (OSS-Fuzz 587020633, where a fuzzer that
// keeps reading the same input in one process ran out of memory).
//
// So what was handed over is owned from the moment the parse is over, however
// it ended: taken by the reader if it gets that far, and deleted here if not.
template <typename T>
  class ParsedResult {
  public:
    explicit ParsedResult(T** p) : p(p) { *p = nullptr; }
    ~ParsedResult() { delete *this->p; *this->p = nullptr; }

    std::shared_ptr<T> take() {
      // (out of the global first: making a shared_ptr deletes what it was given
      //  if it can't allocate, and this must not then delete it again)
      T* x = *this->p;
      *this->p = nullptr;
      return std::shared_ptr<T>(x);
    }

    ParsedResult(const ParsedResult&) = delete;
    ParsedResult& operator=(const ParsedResult&) = delete;
  private:
    T** p;
  };

ModulePtr defReadModuleFile(cc* c, const std::string& file) {
  LOCK_PARSER;

  ParsedResult<Module> parsed(&yyParsedModule);
  yyModulePath = str::rsplit(file, "/").first;
  runParserOnFile(c, TPARSEMODULE, file);
  yyModulePath = "";

  ModulePtr m = checkReturn(parsed.take());
  checkNestingDepth(m);
  return m;
}

ModulePtr defReadModule(cc* c, const char* text) {
  LOCK_PARSER;

  ParsedResult<Module> parsed(&yyParsedModule);
  runParserOnString(c, TPARSEMODULE, text);

  ModulePtr m = checkReturn(parsed.take());
  checkNestingDepth(m);
  return m;
}

ModulePtr defReadModule(cc* c, const std::string& text) {
  return defReadModule(c, text.c_str());
}

ExprDefn defReadExprDefn(cc* c, const std::string& expr) {
  LOCK_PARSER;

  yyParsedVar  = "";
  ParsedResult<Expr> parsed(&yyParsedExpr);
  runParserOnString(c, TPARSEDEFN, expr.c_str());

  ExprPtr e = checkReturn(parsed.take());
  checkNestingDepth(e);
  return ExprDefn(yyParsedVar, e);
}

ExprPtr defReadExpr(cc* c, const std::string& expr) {
  LOCK_PARSER;

  ParsedResult<Expr> parsed(&yyParsedExpr);
  runParserOnString(c, TPARSEEXPR, expr.c_str());

  ExprPtr e = checkReturn(parsed.take());
  checkNestingDepth(e);
  return e;
}

// allow variable and pattern variable overloading
// (these bare `new`s are not autoreleased: the consuming grammar action wraps
// them, and a syntax error that discards them mid-parse runs the %destructor
// hexpr.y declares for their types. What still leaks them is an action that
// throws, since the exception unwinds past bison's cleanup -- see
// runParserOnBuffer above for what that path does and does not put back)
Expr* defVarCtor(const std::string& vn, const LexicalAnnotation& la) { return new Var(vn, la); }
VarCtorFn varCtorFn = &defVarCtor;
void overrideVarCtor(VarCtorFn f) { varCtorFn = f; }

Pattern* defPatVarCtor(const std::string& vn, const LexicalAnnotation& la) { return new MatchAny(vn, la); }
PatVarCtorFn patVarCtorFn = &defPatVarCtor;
void overridePatVarCtor(PatVarCtorFn f) { patVarCtorFn = f; }

}


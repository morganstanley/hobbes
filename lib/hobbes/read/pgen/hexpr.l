%{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wold-style-cast"

int yycolumn = 1;

#define YY_NO_INPUT
#define YY_USER_ACTION \
  yylloc.first_line = yylloc.last_line = yylineno; \
  yylloc.first_column = yycolumn; yylloc.last_column = yycolumn+yyleng-1; \
  yycolumn += yyleng;

#include <hobbes/lang/module.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/pat/pattern.H>
#include <hobbes/util/autorelease.H>
#include <hobbes/util/stream.H>
#include <hobbes/parse/grammar.H>
#include <hobbes/read/pgen/hexpr.parse.H>
#include <limits>
#include <string>
#include <iostream>

namespace {
std::out_of_range make_err(const std::string& s) {
  return std::out_of_range(std::string("literal ") + s + " is not supported");
}

int str2int(const std::string& s) {
  try {
    return std::stoi(s);
  } catch (const std::out_of_range&) {
    throw make_err(s);
  }
}

long str2long(const std::string& s) {
  try {
    return std::stol(s);
  } catch (const std::out_of_range&) {
    throw make_err(s);
  }
}

short str2short(const std::string& s) {
  const auto i = str2int(s);
  if (i > std::numeric_limits<short>::max()) {
    throw make_err(s);
  }
  return static_cast<short>(i);
}

__int128 str2int128(const std::string& s) {
  __int128 i = 0;
  if (!readInt128(s, &i)) {
    throw make_err(s);
  }
  return i;
}
}

#define SAVE_STR    yylval.string = hobbes::autorelease(new std::string(yytext, yytext + yyleng))
#define SAVE_IDENT  yylval.string = identifier(yytext, yytext + yyleng)
#define SAVE_BOOL   std::istringstream(std::string(yytext, yytext + yyleng)) >> std::boolalpha >> yylval.boolv
#define SAVE_SHORT  yylval.shortv = str2short(std::string(yytext, yytext + yyleng - 1))
#define SAVE_INT    yylval.intv = str2int(std::string(yytext, yytext + yyleng))
#define SAVE_LONG   yylval.longv = str2long(std::string(yytext, yytext + yyleng - 1))
#define SAVE_INT128 yylval.int128v = str2int128(std::string(yytext, yytext + yyleng - 1))
#define SAVE_FLOAT  yylval.floatv = std::stof(std::string(yytext, yytext + yyleng))
#define SAVE_DOUBLE yylval.doublev = std::stod(std::string(yytext, yytext + yyleng))

std::string yyVexpLexError;
int yyInitToken = 0;

int yyWantIndent = 0; // hack to simulate one level of the "off side" rule
void wantIndent(bool f) {
  if (f) {
    ++yyWantIndent;
  } else if (yyWantIndent > 0) {
    --yyWantIndent;
  }
}
bool wantIndent() { return yyWantIndent > 0; }

std::vector<int> yyIndentStack;
void pushIndent() {
  yyIndentStack.push_back(yyWantIndent);
  yyWantIndent = 0;
}
void popIndent() {
  if(!yyIndentStack.empty()){
    yyWantIndent = *--yyIndentStack.end();
    yyIndentStack.pop_back();
  }
}

// The scanner keeps state from one token to the next: the start condition it
// is in (INITIAL, or inside a block comment), whether an indent is currently
// significant, and the stack of that for each bracket opened. A parse that
// ends early -- a syntax error, an exception out of a grammar action -- leaves
// all of it wherever the failure found it, and nothing used to reset it, so
// the next parse in the process inherited it: an unterminated block comment
// left every later input read as comment, and a class body cut short left
// later inputs with an indent token where none belongs.
//
// So the parser brackets every parse with these. pushLexerParseState saves
// the state and resets it to what a fresh process has; popLexerParseState
// puts the saved state back. Saving rather than just resetting is for the
// parse of an imported script, which runs nested inside its importer's parse
// and must hand the importer's state back when it is done.
struct LexerParseState {
  int              start;
  int              wantIndent;
  std::vector<int> indentStack;
};
std::vector<LexerParseState> yySavedLexerParseStates;

void pushLexerParseState() {
  LexerParseState s;
  s.start       = YY_START;
  s.wantIndent  = yyWantIndent;
  s.indentStack = yyIndentStack;
  yySavedLexerParseStates.push_back(s);

  BEGIN(0); // INITIAL -- which flex defines only after this prologue, as 0
  yyWantIndent = 0;
  yyIndentStack.clear();
}

void popLexerParseState() {
  if (yySavedLexerParseStates.empty()) {
    return;
  }
  const LexerParseState& s = yySavedLexerParseStates.back();
  BEGIN(s.start);
  yyWantIndent  = s.wantIndent;
  yyIndentStack = s.indentStack;
  yySavedLexerParseStates.pop_back();
}

// Give back the last n characters of the current match.
//
// The indentation rules below need one or two characters of lookahead to tell
// an indented line from a comment, and they used to ask flex for it as
// trailing context. Variable trailing context makes flex carry a backtracking
// state stack -- one entry per character scanned while matching a token -- in
// a buffer it allocates once at (YY_BUF_SIZE + 2) states and never grows, so
// scanning a token longer than that wrote past the end of it. Matching the
// lookahead and handing it back leaves no rule with trailing context, and flex
// emits no such buffer at all, so the length of a token is bounded only by the
// input. yyless rewinds yyleng and, for any newline among the characters it
// gives back, yylineno -- both of which YY_USER_ACTION has already used, so
// the token's line and end column are restated here once they are right.
#define pushBackLookahead(n)                                   \
  do {                                                         \
    yyless(yyleng - (n));                                      \
    yylloc.first_line = yylloc.last_line = yylineno;           \
    yylloc.last_column = yylloc.first_column + yyleng - 1;     \
  } while (false)

std::string* identifier(const char* b, const char* e) {
  if ((b+1) == e && *b == '_') {
    return hobbes::autorelease(new std::string(hobbes::freshName()));
  } else {
    return hobbes::autorelease(new std::string(b, e));
  }
}
%}
%x BLOCK_COMMENT

%option yylineno
%option noyywrap

%%

%{
  /* simulate multiple start symbols in our grammar by supporting a distinguished prefix token to switch on pseudo-start symbol productions */
  if (yyInitToken) {
    int r = yyInitToken;
    yyInitToken = 0;
    return r;
  }
%}

\n                              { yycolumn = 1; }
[ \t\r\f]                       { }
"\n"[ \t]+[^ \t\n/]             { pushBackLookahead(1); yycolumn = 2; if (wantIndent()) { return TINDENT; } }
"\n"[ \t]+"/"[^*/]              { pushBackLookahead(2); yycolumn = 2; if (wantIndent()) { return TINDENT; } }
"\n "                           { yycolumn = 2; if (wantIndent()) { return TINDENT; } }
"\r "                           {               if (wantIndent()) { return TINDENT; } }
"\n\t"                          { yycolumn = 2; if (wantIndent()) { return TINDENT; } }
"\r\t"                          {               if (wantIndent()) { return TINDENT; } }
"//"[^\n]*\n                    { yycolumn = 1; }

"/*"                   { BEGIN(BLOCK_COMMENT); }
<BLOCK_COMMENT>"*/"    { BEGIN(INITIAL); }
<BLOCK_COMMENT>\n      { yycolumn = 1; }
<BLOCK_COMMENT>.       { }

"option"               { return TOPTION; }
"module"               { return TMODULE; }
"where"                { return TWHERE; }
"import"               { return TIMPORT; }
"type"                 { return TTYPE; }
"data"                 { return TDATA; }
"class"                { wantIndent(true); return TCLASS; }
"instance"             { wantIndent(true); return TINST; }
"exists"               { return TEXISTS; }
"UNSAFE"               { return TUNSAFE; }
"SAFE"                 { return TSAFE; } 

"<-"                   { return TASSIGN; }
"="                    { return TEQUALS; }
"::"                   { return TASSUMP; }
"=>"                   { return TCSTARROW; }
":="                   { return TPARROW; }
":"                    { return TCOLON; }
"->"                   { return TARROW; }
"=="                   { return TEQUIV; }
"==="                  { return TEQ; }
"~"                    { return TCIEQ; }
"!="                   { return TNEQ; }
"<"                    { return TLT; }
"<="                   { return TLTE; }
">"                    { return TGT; }
">="                   { return TGTE; }
"!"                    { return TNOT; }
"not"                  { return TNOT; }
"let"                  { return TLET; }
"case"                 { return TCASE; }
"default"              { return TDEFAULT; }
"match"                { return TMATCH; }
"matches"              { return TMATCHES; }
"parse"                { return TPARSE; }
"with"                 { return TWITH; }
"of"                   { return TOF; }
"and"                  { return TAND; }
"or"                   { return TOR; }
"if"                   { return TIF; }
"then"                 { return TTHEN; }
"else"                 { return TELSE; }
"true"                 { SAVE_BOOL; return TBOOL; }
"false"                { SAVE_BOOL; return TBOOL; }
"in"                   { return TIN; }
"pack"                 { return TPACK; }
"unpack"               { return TUNPACK; }
"do"                   { return TDO; }
"return"               { return TRETURN; }

"{-#"                  { return TLPRAGMA; }
"#-}"                  { return TRPRAGMA; }   

"("                    { pushIndent(); return TLPAREN; }
")"                    { popIndent();  return TRPAREN; }
"["                    { pushIndent(); return TLBRACKET; }
"]"                    { popIndent();  return TRBRACKET; }
"{"                    { pushIndent(); return TLBRACE; }
"}"                    { popIndent();  return TRBRACE; }

"|"                    { return TBAR; }
","                    { return TCOMMA; }
";"                    { return TSEMICOLON; }
"++"                   { return TAPPEND; }
"+"                    { return TPLUS; }
"-"                    { return TMINUS; }
"*"                    { return TTIMES; }
"/"                    { return TDIVIDE; }
"%"                    { return TREM; }
"\\"                   { return TFN; }
"fn"                   { return TFNL; }
" . "                  { return TCOMPOSE; }
"."                    { return TDOT; }
".."                   { return TUPTO; }
"^"                    { return TCARET; }
"@"                    { return TAT; }
"$"                    { return TDOLLAR; }
"?"                    { return TQUESTION; }
'(\\.|[^'\n\\])+'      { SAVE_STR;    return (hobbes::str::unescape(hobbes::str::trimq(*yylval.string, '\'')).size() <= 1) ? TCHAR : TREGEX; }
"'"                    { return TSQUOTE; }
"`"                    { return TEQUOTE; }
0X[0-9a-fA-F]{2}       { SAVE_STR;    return TBYTE; }
0x([0-9a-fA-F]{2})+    { SAVE_STR;    return TBYTES; }
[a-zA-Z_][a-zA-Z0-9_]* { SAVE_IDENT;  return TIDENT; }
(\.[0-9]+)(\.[0-9]+)   { SAVE_STR;    return TTUPSECTION; }
[0-9]+\.[0-9]+[Ff]     { SAVE_FLOAT;  return TFLOAT; }
[0-9]+\.[0-9]+         { SAVE_DOUBLE; return TDOUBLE; }
[0-9]+\.[0-9]+%        { SAVE_DOUBLE; yylval.doublev *= 0.01; return TDOUBLE; }
[0-9]+H                { SAVE_INT128; return TINT128; }
[0-9]+[Ll]             { SAVE_LONG;   return TLONG; }
[0-9]+S                { SAVE_SHORT;  return TSHORT; }
[0-9]+                 { SAVE_INT;    return TINT; }
\"(\\.|[^"])*\"        { SAVE_STR;    return TSTRING; }

[0-9]+ms               { SAVE_STR;    return TTIMEINTERVAL; }
[0-9]+us               { SAVE_STR;    return TTIMEINTERVAL; }
[0-9]+s                { SAVE_STR;    return TTIMEINTERVAL; }
[0-9]+m                { SAVE_STR;    return TTIMEINTERVAL; }
[0-9]+h                { SAVE_STR;    return TTIMEINTERVAL; }
[0-9]+d                { SAVE_STR;    return TTIMEINTERVAL; }
[0-9]+min              { SAVE_STR;    return TTIMEINTERVAL; }
[0-9]+hour             { SAVE_STR;    return TTIMEINTERVAL; }
[0-9]+day              { SAVE_STR;    return TTIMEINTERVAL; }

[0-9]{2}:[0-9]{2}                    { SAVE_STR; return TTIME; }
[0-9]{2}:[0-9]{2}:[0-9]{2}           { SAVE_STR; return TTIME; }
[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3} { SAVE_STR; return TTIME; }
[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{6} { SAVE_STR; return TTIME; }

[0-9]{4}\-[0-9]{2}\-[0-9]{2}                                      { SAVE_STR; return TDATETIME; }
[0-9]{4}\-[0-9]{2}\-[0-9]{2}T[0-9]{2}:[0-9]{2}                    { SAVE_STR; return TDATETIME; }
[0-9]{4}\-[0-9]{2}\-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}           { SAVE_STR; return TDATETIME; }
[0-9]{4}\-[0-9]{2}\-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3} { SAVE_STR; return TDATETIME; }
[0-9]{4}\-[0-9]{2}\-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{6} { SAVE_STR; return TDATETIME; }

. { yyVexpLexError = "Unknown character: " + std::string(yytext); yyterminate(); if (false) { yyrealloc(0, 0); yyunput(0, 0); } }

%%
#pragma GCC diagnostic pop


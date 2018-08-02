%{
#ifdef __clang__
#pragma clang diagnostic ignored "-Wdeprecated-register"
#endif

int yycolumn = 1;

#define YY_USER_ACTION \
  yylloc.first_line = yylloc.last_line = yylineno; \
  yylloc.first_column = yycolumn; yylloc.last_column = yycolumn+yyleng-1; \
  yycolumn += yyleng;

#include <hobbes/lang/module.H>
#include <hobbes/lang/expr.H>
#include <hobbes/lang/pat/pattern.H>
#include <hobbes/util/autorelease.H>
#include <hobbes/parse/grammar.H>
#include <hobbes/read/pgen/hexpr.parse.H>
#include <string>
#include <iostream>

#define SAVE_STR    yylval.string = hobbes::autorelease(new std::string(yytext, yytext + yyleng))
#define SAVE_IDENT  yylval.string = identifier(yytext, yytext + yyleng)
#define SAVE_BOOL   std::istringstream(std::string(yytext, yytext + yyleng)) >> std::boolalpha >> yylval.boolv
#define SAVE_SHORT  yylval.shortv = (short)std::stoi(std::string(yytext, yytext + yyleng))
#define SAVE_INT    yylval.intv = std::stoi(std::string(yytext, yytext + yyleng))
#define SAVE_LONG   yylval.longv = std::stol(std::string(yytext, yytext + yyleng - 1))
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

\n                     { yycolumn = 1; }
[ \t\r\f]              { }
"\n "                  { yycolumn = 2; if (wantIndent()) { return TINDENT; } }
"\r "                  {               if (wantIndent()) { return TINDENT; } }
"\n\t"                 { yycolumn = 2; if (wantIndent()) { return TINDENT; } }
"\r\t"                 {               if (wantIndent()) { return TINDENT; } }
"//"[^\n]*\n           { yycolumn = 1; }
"/*"                   { BEGIN(BLOCK_COMMENT); }
<BLOCK_COMMENT>"*/"    { BEGIN(INITIAL); }
<BLOCK_COMMENT>\n      { yycolumn = 1; }
<BLOCK_COMMENT>.       { }

"module"               { return TMODULE; }
"where"                { return TWHERE; }
"import"               { return TIMPORT; }
"type"                 { return TTYPE; }
"data"                 { return TDATA; }
"class"                { wantIndent(true); return TCLASS; }
"instance"             { wantIndent(true); return TINST; }
"exists"               { return TEXISTS; }

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
"("                    { return TLPAREN; }
")"                    { return TRPAREN; }
"["                    { return TLBRACKET; }
"]"                    { return TRBRACKET; }
"{"                    { return TLBRACE; }
"}"                    { return TRBRACE; }
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

. { yyVexpLexError = "Unknown character: " + std::string(yytext); yyterminate(); if (false) { yy_flex_realloc(0, 0); yyunput(0, 0); } }

%%

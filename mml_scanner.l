%option c++ prefix="mml_scanner_" outfile="mml_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "mml_parser.tab.h"

// don't change this
#define yyerror LexerError
%}
%x X_STRING X_COMMENT
%%
  yydebug=1; set_debug(1);

/******************************************************************************/
 *  COMMENTS - can start both with // and /* and can be nested
 ******************************************************************************/

"//".*                 ;

"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>"/*"        yy_push_state(X_COMMENT); /* nested comment */
<X_COMMENT>.|\n        ;

/******************************************************************************/
 *  KEYWORDS/OPERATORS
 ******************************************************************************/

">="                   return tGE; 
"<="                   return tLE; 
"=="                   return tEQ; 
"!="                   return tNE;

"&&"                   return tAND;
"||"                   return tOR;

"while"                return tWHILE; 
"input"                return tINPUT; 
"!"                    return tWRITE;
"!!"                   return tWRITELN;
"sizeof"               return tSIZEOF;
"return"               return tRETURN;

"foreign"              return tFOREIGN;
"forward"              return tFORWARD;
"public"               return tPUBLIC;
"auto"                 return tAUTO;

"begin"                return tBEGIN; 
"end"                  return tEND;
"next"                 return tNEXT;
"stop"                 return tSTOP;

"int"                  return tINTEGER_TYPE;
"double"               return tDOUBLE_TYPE;
"string"               return tSTRING_TYPE;
"null"                 return tNULLPTR;

"if"                   return tIF;
"elif"                 return tELIF;
"else"                 return tELSE; 

[A-Za-z][A-Za-z0-9_]*  yylval.s = new std::string(yytext); return tIDENTIFIER;

\'                     yy_push_state(X_STRING); yylval.s = new std::string("");
<X_STRING>\'           yy_pop_state(); return tSTRING;
<X_STRING>\\\'         *yylval.s += yytext + 1;
<X_STRING>.            *yylval.s += yytext;
<X_STRING>\n           yyerror("newline in string");

[0-9]+                 yylval.i = strtol(yytext, nullptr, 10); return tINTEGER;

[-()<>=+*/%;{}.]       return *yytext;

[ \t\n]+               ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%

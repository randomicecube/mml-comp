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

"//".*                 ;

"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"/*"        yy_push_state(X_COMMENT); /* nested comment */
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>.|\n        ;

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

"int"                  return tINT_TYPE;
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

([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)? { /* double */
  try {
    yylval.d = std::stod(yytext);
    return tDOUBLE;
  } catch (const std::out_of_range &e) {
    yyerror("out of range double");
  }
}

0[0-7]+ { /* octal */
  try {
    yylval.i = std::stoi(yytext, NULL, 8);
    return tINTEGER;
  } catch (const std::out_of_range &e) {
    yyerror("out of range integer");
  }
}
0[0-9]+                yyerror("invalid base for number"); /* leading zero is not allowed unless in octal form */
0|[1-9][0-9]* { /* decimal */
  try {
    yylval.i = std::stoi(yytext);
    return tINTEGER;
  } catch (const std::out_of_range &e) {
    yyerror("out of range integer");
  }
}

[-()<>=+*/%;{}.]       return *yytext;

[ \t\n]+               ; /* ignore whitespace */

.                      yyerror("Unknown character");

%%

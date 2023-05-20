%option c++ prefix="mml_scanner_" outfile="mml_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <sstream>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "mml_parser.tab.h"

static std::ostringstream strlit;

// don't change this
#define yyerror LexerError

%}
%x X_STRING X_COMMENT X_BACKSLASH X_STRING_SKIP
%%
  yydebug=1; set_debug(1);

  /* ====================================================================== */
  /* ====[                  1 - Tipos de dados                        ]==== */
  /* ====================================================================== */

"int"                  return tINT_TYPE;
"double"               return tDOUBLE_TYPE;
"string"               return tSTRING_TYPE;
"void"                 return tVOID_TYPE;

  /* ====================================================================== */
  /* ====[                      3.3 - Comentários                     ]==== */
  /* ====================================================================== */

"//".*                 ;

"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"/*"        yy_push_state(X_COMMENT); /* nested comment */
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>.|\n        ;

  /* ====================================================================== */
  /* ====[             3.8.2 - Reais em vírgula flutuante             ]==== */
  /* ====================================================================== */


([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)? {
                                                    try {
                                                      yylval.d = std::stod(yytext);
                                                      return tDOUBLE;
                                                    } catch (const std::out_of_range &e) {
                                                      yyerror("out of range double");
                                                    }
                                                  }

  /* ====================================================================== */
  /* ====[                     3.8.1 - Inteiros                       ]==== */
  /* ====================================================================== */

0[0-7]+                 { /* octal */
                          try {
                            yylval.i = std::stoi(yytext, NULL, 8);
                            return tINTEGER;
                          } catch (const std::out_of_range &e) {
                            yyerror("out of range integer");
                          }
                        }

0[0-9]+                yyerror("invalid base for number"); /* leading zero is not allowed unless in octal form */

0|[1-9][0-9]*           { /* decimal */
                          try {
                            yylval.i = std::stoi(yytext);
                            return tINTEGER;
                          } catch (const std::out_of_range &e) {
                            yyerror("out of range integer");
                          }
                        }

  /* ====================================================================== */
  /* ====[              3.8.3 - Cadeias de caracteres                 ]==== */
  /* ====================================================================== */

"\""                   yy_push_state(X_STRING);
<X_STRING>\\           yy_push_state(X_BACKSLASH);
<X_STRING>"\""        {
                        yylval.s = new std::string(strlit.str());
                        strlit.str("");
                        yy_pop_state();
                        return tSTRING;
                      }

<X_STRING>\0           yyerror("NULL byte in string");
<X_STRING>\n           yyerror("Newline in string");
<X_STRING>.            strlit << *yytext;

<X_BACKSLASH>n         strlit << '\n'; yy_pop_state();
<X_BACKSLASH>r         strlit << '\r'; yy_pop_state();
<X_BACKSLASH>t         strlit << '\t'; yy_pop_state();
<X_BACKSLASH>\"        strlit << '"'; yy_pop_state();
<X_BACKSLASH>\\        strlit << '\\'; yy_pop_state();
<X_BACKSLASH>0         yy_push_state(X_STRING_SKIP);

<X_BACKSLASH>.         strlit << *yytext; yy_pop_state();

<X_BACKSLASH>[0-7]{1,3}   {
                            const int c = std::stoi(yytext, NULL, 8); /* TODO: check use of stoul */
                            if (c > 255) {
                              yyerror("octal escape sequence out of range");
                            }
                            strlit << (char) c; /* TODO: check whether it is also needed to cast to unsigned char, like in OG */
                            yy_pop_state(); 
                          }

<X_STRING_SKIP>"\""        {
                              yylval.s = new std::string(strlit.str());
                              strlit.str("");
                              yy_pop_state(); yy_pop_state(); yy_pop_state(); /* X_STRING_SKIP, X_BACKSLASH, X_STRING */
                              return tSTRING;
                            }
<X_STRING_SKIP>.           ;

  /* ====================================================================== */
  /* ====[                  4.5 - Símbolos Globais                    ]==== */  
  /* ====================================================================== */

"foreign"              return tFOREIGN;
"forward"              return tFORWARD;
"public"               return tPUBLIC;

"auto"                 return tAUTO;

  /* ====================================================================== */
  /* ====[                 5 - Funções                                ]==== */
  /* ====================================================================== */

[@{}<>]		             return *yytext;
"->"                   return tARROW;

"begin"                return tBEGIN; 
"end"                  return tEND;

  /* ====================================================================== */
  /* ====[                 6.2 - Instrução condicional                ]==== */
  /* ====================================================================== */

"if"                   return tIF;
"elif"                 return tELIF;
"else"                 return tELSE; 

  /* ====================================================================== */
  /* ====[                 6.3 - Instrução de iteração                ]==== */
  /* ====[                 6.4 - Instrução de terminação              ]==== */
  /* ====[                 6.5 - Instrução de continuação             ]==== */
  /* ====[                 6.6 - Instrução de retorno                 ]==== */
  /* ====================================================================== */

"while"                return tWHILE; 
"stop"                 return tSTOP;
"next"                 return tNEXT;
"return"               return tRETURN;

  /* ====================================================================== */
  /* ====[                 6.8 - Instruções de Impressão                ]==== */
  /* ====================================================================== */

"!"                    return tWRITE;
"!!"                   return tWRITELN;

  /* ====================================================================== */
  /* ====[                 7 - Operadores de expressões               ]==== */
  /* ====[                 7 - Expressões especiais                   ]==== */
  /* ====================================================================== */

[-+*/%\[\]?~=();,]     return *yytext;

">="                   return tGE; 
"<="                   return tLE; 
"=="                   return tEQ; 
"!="                   return tNE;

"&&"                   return tAND;
"||"                   return tOR;

"sizeof"               return tSIZEOF;
"input"                return tINPUT; 

  /* ====================================================================== */
  /* ====[                         SPECIAL                            ]==== */
  /* ====================================================================== */

"null"                 return tNULLPTR;

  /* ====================================================================== */
  /* ====[                3.7 - Identificadores (nomes)               ]==== */
  /* ====================================================================== */

[A-Za-z][A-Za-z0-9_]*  yylval.s = new std::string(yytext); return tIDENTIFIER;

  /* ====================================================================== */
  /* ====[                    3.1 - Caracteres brancos                ]==== */
  /* ====================================================================== */


[ \t\n\r]+             ; /* ignore whitespace */  /* TODO: check if we can use [[:space:]] instead */

  /* ====================================================================== */
  /* ====[                    Everything else                         ]==== */
  /* ====================================================================== */

.                      yyerror("Unknown character");

%%

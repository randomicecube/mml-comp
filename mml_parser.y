%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;	/* integer value */
  double                d;	/* double value */
  std::string          *s;	/* symbol name or string literal */
  cdk::basic_type      *t;	/* type */

  cdk::basic_node      *node;	/* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;

  mml::block_node      *block;
  mml::declaration_node *declaration;
};

%token <i> tINTEGER
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTRING

%token tINPUT tWRITE tWRITELN tSIZEOF tRETURN
%token tFOREIGN tFORWARD tPUBLIC tPRIVATE tAUTO
%token tBEGIN tEND tARROW tNEXT tSTOP
%token tINT_TYPE tDOUBLE_TYPE tSTRING_TYPE tVOID_TYPE tNULLPTR

%nonassoc tIF tELIF
%nonassoc tELSE tWHILE

%type <sequence> file global_declarations declarations instructions
%type <expression> expr main integer double opt_init init
%type <lvalue> lval
%type <block> block
%type <declaration> global_declaration declaration
%type <t> type auto
%type <s> string

/* Associativity rules */

%right '='
%left tOR
%left tAND
%nonassoc tNOT
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY
%nonassoc '(' '['

/* Associativity rules */

%right '='
%left tOR
%left tAND
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY
%nonassoc '(' '['

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file : /* empty */                                { compiler->ast($$ = new cdk::sequence_node(LINE)); }
     | global_declarations                        { compiler->ast($$ = $1); }
     |                     main                   { compiler->ast($$ = new cdk::sequence_node(LINE, $1)); }
     | global_declarations main                   { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }
     ;

main : tBEGIN inner_block tEND                    { $$ = new mml::function_definition_node(LINE, $2); }
     ;

global_declarations : global_declaration ';'                                    { $$ = new cdk::sequence_node(LINE, $1); }
                    | global_declarations global_declaration ';'                { $$ = new cdk::sequence_node(LINE, $2, $1); }
                    ;

global_declaration : tFOREIGN  fun_type  tIDENTIFIER                            { $$ = new mml::declaration_node(LINE, tFOREIGN, $2, *$3, nullptr); }
                   | tFORWARD  data_type tIDENTIFIER                            { $$ = new mml::declaration_node(LINE, tFORWARD, $2, *$3, nullptr); }
                   | tPUBLIC   data_type tIDENTIFIER opt_init                   { $$ = new mml::declaration_node(LINE, tPUBLIC, $2, *$3, $4); }
                   | tPUBLIC   opt_auto  tIDENTIFIER opt_init                   { $$ = new mml::declaration_node(LINE, tPUBLIC, $2, *$3, $4); }
                   | declaration                                                { $$ = $1; }
                   ;

opt_auto: /* empty */                             { $$ = nullptr; }
        | tAUTO                                   { $$ = nullptr; }
        ;

block : '{' inner_block '}'                       { $$ = $2; }
      ;

inner_block : declarations instrs                 { $$ = new mml::block_node(LINE, $2, $3); }
            | declarations                        { $$ = new mml::block_node(LINE, $2, nullptr); }
            | instrs                              { $$ = new mml::block_node(LINE, nullptr, $2); }
            |                                     { $$ = new mml::block_node(LINE, nullptr, nullptr); }
            ;

data_type : tSTRING_TYPE                          { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
          | tINT_TYPE                             { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
          | tDOUBLE_TYPE                          { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
          | '[' data_type ']'                     { $$ = cdk::reference_type::create(4, $2); }
          | fun_type                              { $$ = $1; }
          | void_type                             { $$ = $1; }
          ;

data_types : data_type                            { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1); }
           | data_types ',' data_type             { $$= $1; $$->push_back($3); }
           ;

fun_type : data_type '<' data_types '>'           { $$ = cdk_function_type::create(*$3, $1); delete $3; }
         | data_type '<'            '>'           { $$ = cdk_function_type::create($1); }
         | void_type '<' data_types '>'           { $$ = cdk_function_type::create(*$3, $1); delete $3; }
         | void_type '<'            '>'           { $$ = cdk_function_type::create($1); }
         ;

void_type : '[' tVOID_TYPE ']'                    { $$ = cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_VOID)); }
          | '[' void_type ']'                     { $$ = $2; }
          ;

opt_init : /* empty */                            { $$ = nullptr; }
         | init                                   { $$ = $1; }
         ;

init : '=' expr                                   { $$ = $2; }
     ;

declarations : declaration ';'	                  { $$ = new cdk::sequence_node(LINE, $1); }
             | declarations declaration ';'       { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

declaration : data_type tIDENTIFIER opt_init      { $$ = new mml::declaration_node(LINE, tPRIVATE, $1, *$2, $3); }
            | tAUTO     tIDENTIFIER init          { $$ = new mml::declaration_node(LINE, tPRIVATE, nullptr, *$2, $3); }
            ;

instructions : instruction                        { $$ = new cdk::sequence_node(LINE, $1); }
             | instructions instruction           { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

instruction : block                               { $$ = $1; }
            | tIF '(' expr ')' instruction        { $$ = new mml::if_node(LINE, $3, $5); }
            | tIF '(' expr ')' instruction else   { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
            | tWHILE '(' expr ')' instruction     { $$ = new mml::while_node(LINE, $3, $5); }
            | tSTOP ';'                           { $$ = new mml::stop_node(LINE);  }
            | tSTOP tINTEGER ';'                  { $$ = new mml::stop_node(LINE, $2);  }
            | tNEXT ';'                           { $$ = new mml::next_node(LINE); }
            | tNEXT tINTEGER ';'                  { $$ = new mml::next_node(LINE, $2); }
            | tRETURN ';'                         { $$ = new mml::return_node(LINE, nullptr); }
            | tRETURN expr ';'                    { $$ = new mml::return_node(LINE, $2);      }
            | expr ';'                            { $$ = new mml::evaluation_node(LINE, $1); }
            | expressions tWRITE                  { $$ = new mml::write_node(LINE, $1, false); }
            | expressions tWRITELN                { $$ = new mml::write_node(LINE, $1, true);  }
            ;

else : tELSE instruction                          { $$ = $2; }
     | tELIF '(' expr ')' instruction             { $$ = new mml::if_node(LINE, $3, $5); }
     | tELIF '(' expr ')' instruction else        { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
     ;

expr : integer                       { $$ = $1; }
     | double                        { $$ = $1; }
     | string                        { $$ = new cdk::string_node(LINE, $1); }
     | tNULLPTR                      { $$ = new mml::nullptr_node(LINE); }
     | '(' expr ')'                  { $$ = $2; }
     | '[' expr ']'                  { $$ = new mml::stack_alloc_node(LINE, $2); }
     | '+' expr %prec tUNARY         { $$ = new mml::identity_node(LINE, $2); }
     | '-' expr %prec tUNARY         { $$ = new cdk::neg_node(LINE, $2); }
     | lval '?'                      { $$ = new mml::address_of_node(LINE, $1); }
     | lval                          { $$ = new cdk::rvalue_node(LINE, $1); }  // FIXME: is this needed/in the right place?
     | expr '*' expr	               { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr	               { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr	               { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '+' expr	               { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr	               { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '<' expr	               { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr	               { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr	               { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr                 { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tEQ expr	               { $$ = new cdk::eq_node(LINE, $1, $3); }
     | expr tNE expr	               { $$ = new cdk::ne_node(LINE, $1, $3); }
     | tNOT expr %prec tUNARY        { $$ = new cdk::not_node(LINE, $2); }
     | expr tAND expr                { $$ = new cdk::and_node(LINE, $1, $3); }
     | expr tOR expr                 { $$ = new cdk::or_node(LINE, $1, $3); }
     | lval '=' expr                 { $$ = new cdk::assignment_node(LINE, $1, $3); }
     ;

integer : tINTEGER                   { $$ = new cdk::integer_node(LINE, $1); }
        ;

double : tDOUBLE                     { $$ = new cdk::double_node(LINE, $1); }
       ;

string : tSTRING                     { $$ = $1; }
       | string tSTRING              { $$ = $1; $$->append(*$2); delete $2; }
       ;

lval : tIDENTIFIER                   { $$ = new cdk::variable_node(LINE, $1); }
     ;

%%

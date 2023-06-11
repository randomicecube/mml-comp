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
  std::vector<std::shared_ptr<cdk::basic_type>> *types;

  cdk::basic_node      *node;	/* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;

  mml::block_node      *block;
};

%token <i> tINTEGER
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTRING

%token tINPUT tWRITE tWRITELN tSIZEOF tRETURN
%token tFOREIGN tFORWARD tPUBLIC tPRIVATE tAUTO
%token tBEGIN tEND tARROW tNEXT tSTOP
%token tINT_TYPE tDOUBLE_TYPE tSTRING_TYPE tVOID_TYPE tNULLPTR

%type <sequence> file global_declarations declarations instructions opt_expressions expressions opt_args args
%type <expression> expression opt_init  literal fun_def init
%type <lvalue> lval
%type <block> inner_block block
%type <node> main global_declaration declaration
%type <node> conditional_instruction instruction else arg 

%type <s> string
%type <type> fun_type data_type void_type void_pointer opt_auto auto return_type var_type
%type <types> var_types

/* Associativity rules */

%nonassoc tIF tWHILE
%nonassoc tELIF tELSE

%right '='
%left tAND tOR
%nonassoc '~'
%left tNE tEQ
%left '<' tLE tGE '>'
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

global_declaration : tFOREIGN  fun_type  tIDENTIFIER                            { $$ = new mml::declaration_node(LINE, tFOREIGN, $2, *$3, nullptr); delete $3; }
                   | tFORWARD  var_type tIDENTIFIER                             { $$ = new mml::declaration_node(LINE, tFORWARD, $2, *$3, nullptr); delete $3; }
                   | tPUBLIC   var_type tIDENTIFIER opt_init                    { $$ = new mml::declaration_node(LINE, tPUBLIC, $2, *$3, $4); delete $3; }
                   | tPUBLIC   opt_auto  tIDENTIFIER opt_init                   { $$ = new mml::declaration_node(LINE, tPUBLIC, $2, *$3, $4); delete $3; }
                   | declaration                                                { $$ = $1; }
                   ;

opt_auto: /* empty */                             { $$ = nullptr; }
        | auto                                    { $$ = $1; }
        ;

auto : tAUTO                                      { $$ = cdk::primitive_type::create(0, cdk::TYPE_UNSPEC); }
     ;

block : '{' inner_block '}'                       { $$ = $2; }
      ;

inner_block : declarations instructions           { $$ = new mml::block_node(LINE, $1, $2); }
            | declarations                        { $$ = new mml::block_node(LINE, $1, nullptr); }
            | instructions                        { $$ = new mml::block_node(LINE, nullptr, $1); }
            |                                     { $$ = new mml::block_node(LINE, nullptr, nullptr); }
            ;

data_type : tSTRING_TYPE                          { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
          | tINT_TYPE                             { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
          | tDOUBLE_TYPE                          { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
          | fun_type                              { $$ = $1; }
          | '[' data_type ']'                     { $$ = cdk::reference_type::create(4, $2); }
          ;

fun_type : return_type '<' var_types '>'          { $$ = cdk::functional_type::create(*$3, $1); delete $3; }
         | return_type '<'            '>'         { $$ = cdk::functional_type::create($1); }
         ;

var_type : data_type                              { $$ = $1; }
          | void_pointer                          { $$ = $1; }
          ;

var_types : var_type                              { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1); }
           | var_types ',' var_type               { $$ = $1; $$->push_back($3); }
           ;

void_pointer : '[' void_pointer ']'               { $$ = $2; } 
             | '[' void_type ']'                  { $$ = cdk::reference_type::create(4, $2); }
             ;

void_type : tVOID_TYPE                            { $$ = cdk::primitive_type::create(4, cdk::TYPE_VOID); }
          ;

return_type : var_type                       { $$ = $1; }
            | void_type                      { $$ = $1; }
            ;

opt_init : /* empty */                            { $$ = nullptr; }
         | init                                   { $$ = $1; }
         ;

init : '=' expression                             { $$ = $2; }
     ;

declarations : declaration ';'	                        { $$ = new cdk::sequence_node(LINE, $1); }
             | declarations declaration ';'             { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

declaration : var_type tIDENTIFIER opt_init             { $$ = new mml::declaration_node(LINE, tPRIVATE, $1, *$2, $3); delete $2; }
            | auto      tIDENTIFIER init                { $$ = new mml::declaration_node(LINE, tPRIVATE, $1, *$2, $3); delete $2; }
            ;

instructions : instruction                              { $$ = new cdk::sequence_node(LINE, $1); }
             | instructions instruction                 { $$ = new cdk::sequence_node(LINE, $2, $1); }
             ;

instruction : block                                     { $$ = $1; }
            | conditional_instruction                   { $$ = $1; }
            | tWHILE '(' expression ')' instruction     { $$ = new mml::while_node(LINE, $3, $5); }
            | tSTOP ';'                                 { $$ = new mml::stop_node(LINE);  }
            | tSTOP tINTEGER ';'                        { $$ = new mml::stop_node(LINE, $2);  }
            | tNEXT ';'                                 { $$ = new mml::next_node(LINE); }
            | tNEXT tINTEGER ';'                        { $$ = new mml::next_node(LINE, $2); }
            | tRETURN ';'                               { $$ = new mml::return_node(LINE); }
            | tRETURN expression ';'                    { $$ = new mml::return_node(LINE, $2);      }
            | expression ';'                            { $$ = new mml::evaluation_node(LINE, $1); }
            | expressions tWRITE                        { $$ = new mml::print_node(LINE, $1, false); }
            | expressions tWRITELN                      { $$ = new mml::print_node(LINE, $1, true);  }
            ;

conditional_instruction : tIF '(' expression ')' instruction %prec tIF          { $$ = new mml::if_node(LINE, $3, $5); }
                        | tIF '(' expression ')' instruction else               { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
                        ;

else : tELSE instruction                                                        { $$ = $2; }
     | tELIF '(' expression ')' instruction  %prec tIF                          { $$ = new mml::if_node(LINE, $3, $5); }
     | tELIF '(' expression ')' instruction else                                { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
     ;

opt_expressions : /* empty */                           { $$ = nullptr; }
                | expressions                           { $$ = $1; }
                ;

expressions : expression                                { $$ = new cdk::sequence_node(LINE, $1); }
            | expressions ',' expression                { $$ = new cdk::sequence_node(LINE, $3, $1); }
            ;

expression : literal                             { $$ = $1; }
           | '(' expression ')'                  { $$ = $2; }
           | '[' expression ']'                  { $$ = new mml::stack_alloc_node(LINE, $2); }
           | '+' expression %prec tUNARY         { $$ = new mml::identity_node(LINE, $2); }
           | '-' expression %prec tUNARY         { $$ = new cdk::neg_node(LINE, $2); }
           | lval '?'                            { $$ = new mml::address_of_node(LINE, $1); }
           | expression '*' expression	         { $$ = new cdk::mul_node(LINE, $1, $3); }
           | expression '/' expression	         { $$ = new cdk::div_node(LINE, $1, $3); }
           | expression '%' expression	         { $$ = new cdk::mod_node(LINE, $1, $3); }
           | expression '+' expression	         { $$ = new cdk::add_node(LINE, $1, $3); }
           | expression '-' expression	         { $$ = new cdk::sub_node(LINE, $1, $3); }
           | expression '<' expression	         { $$ = new cdk::lt_node(LINE, $1, $3); }
           | expression '>' expression	         { $$ = new cdk::gt_node(LINE, $1, $3); }
           | expression tGE expression           { $$ = new cdk::ge_node(LINE, $1, $3); }
           | expression tLE expression           { $$ = new cdk::le_node(LINE, $1, $3); }
           | expression tEQ expression	         { $$ = new cdk::eq_node(LINE, $1, $3); }
           | expression tNE expression	         { $$ = new cdk::ne_node(LINE, $1, $3); }
           | '~' expression                      { $$ = new cdk::not_node(LINE, $2); }
           | expression tAND expression          { $$ = new cdk::and_node(LINE, $1, $3); }
           | expression tOR expression           { $$ = new cdk::or_node(LINE, $1, $3); }
           | tSIZEOF '(' expression ')'          { $$ = new mml::sizeof_node(LINE, $3); }
           | tINPUT                              { $$ = new mml::input_node(LINE); }
           | expression '(' opt_expressions ')'  { $$ = new mml::function_call_node(LINE, $1, $3); }
           | '@' '(' opt_expressions ')'         { $$ = new mml::function_call_node(LINE, nullptr, $3); }
           | lval                                { $$ = new cdk::rvalue_node(LINE, $1); }
           | lval '=' expression                 { $$ = new cdk::assignment_node(LINE, $1, $3); }
           | fun_def                             { $$ = $1; }
           ;

fun_def : '(' opt_args ')' tARROW return_type block { $$ = new mml::function_definition_node(LINE, $5, $2, $6); }
        ;

opt_args : /* empty */                       { $$ = new cdk::sequence_node(LINE); }
         | args                              { $$ = $1; }
         ;

args : arg                                   { $$ = new cdk::sequence_node(LINE, $1); }
     | args ',' arg                          { $$ = new cdk::sequence_node(LINE, $3, $1); }
     ;

arg : var_type tIDENTIFIER                   { $$ = new mml::declaration_node(LINE, tPRIVATE, $1, *$2, NULL); delete $2; }
    ;

literal: tINTEGER                            { $$ = new cdk::integer_node(LINE, $1); }
       | tDOUBLE                             { $$ = new cdk::double_node(LINE, $1); }
       | string                              { $$ = new cdk::string_node(LINE, $1); }
       | tNULLPTR                            { $$ = new mml::nullptr_node(LINE); }
       ;

string : tSTRING                             { $$ = $1; }
       | string tSTRING                      { $$ = $1; $$->append(*$2); delete $2; }
       ;

lval : tIDENTIFIER                           { $$ = new cdk::variable_node(LINE, $1); }
     | expression '[' expression ']'         { $$ = new mml::index_node(LINE, $1, $3); }
     ;

%%

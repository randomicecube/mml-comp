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

/* FIXME: remove tPRINT */
%token tWHILE tPRINT tINPUT tWRITE tWRITELN tSIZEOF tRETURN
%token tFOREIGN tFORWARD tPUBLIC tAUTO
%token tBEGIN tEND tNEXT tSTOP
%token tINT_TYPE tDOUBLE_TYPE tSTRING_TYPE tVOID_TYPE tNULLPTR

%nonassoc tIFX
%nonassoc tIF
%nonassoc tELIF
%nonassoc tELSE

%type <sequence> file global_decls decls instrs
%type <expression> expr main
%type <lvalue> lval
%type <block> blk
%type <declaration> global_decl decl
%type <t> type auto

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

/* TODO: is parsing an empty file needed? */
file : global_decls     { compiler->ast($$ = $1); }
    | main              { compiler->ast($$ = new cdk::sequence_node(LINE, $1)); }
    | global_decls main { compiler->ast(new cdk::sequence_node(LINE, $2, $1)); }
    ;

global_decls : global_decl               { $$ = new cdk::sequence_node(LINE, $1); }
    | global_decls global_decl           { $$ = new cdk::sequence_node(LINE, $2, $1); }
    ;

// TODO: global_decl

main : tBEGIN blk tEND { $$ = new mml::function_definition_node(LINE, $2); }
    ;

blk : '{' decls instrs '}'                { $$ = new mml::block_node(LINE, $2, $3); }
    | '{' decls '}'                       { $$ = new mml::block_node(LINE, $2, nullptr); }
    | '{' instrs '}'                      { $$ = new mml::block_node(LINE, nullptr, $2); }
    | '{' '}'                             { $$ = new mml::block_node(LINE, nullptr, nullptr); }
    ;


decls : decl	          { $$ = new cdk::sequence_node(LINE, $1); }
    | decls decl          { $$ = new cdk::sequence_node(LINE, $2, $1); }
    ;

instrs : instr            { $$ = new cdk::sequence_node(LINE, $1); }
    | instrs instr        { $$ = new cdk::sequence_node(LINE, $2, $1); }
    ;

/* TODO: decl, instr */

/* stmt : expr ';'                         { $$ = new mml::evaluation_node(LINE, $1); }
    | tPRINT expr ';'                   { $$ = new mml::print_node(LINE, $2); }
    | tINPUT                            { $$ = new mml::input_node(LINE); }
    | tWHILE '(' expr ')' stmt          { $$ = new mml::while_node(LINE, $3, $5); }
    | tIF '(' expr ')' stmt %prec tIFX  { $$ = new mml::if_node(LINE, $3, $5); }
    | tIF '(' expr ')' stmt tELSE stmt  { $$ = new mml::if_else_node(LINE, $3, $5, $7); }
    | '{' list '}'                      { $$ = $2; }
    ; */

expr : tINTEGER                    { $$ = new cdk::integer_node(LINE, $1); }
    | tDOUBLE                      { $$ = new cdk::double_node(LINE, $1); }
    | tSTRING                      { $$ = new cdk::string_node(LINE, $1); }
    | tNULLPTR                     { $$ = new mml::nullptr_node(LINE); }
    | '(' expr ')'                 { $$ = $2; }
    | '[' expr ']'                 { $$ = new mml::stack_alloc_node(LINE, $2); }
    | '+' expr %prec tUNARY        { $$ = new mml::identity_node(LINE, $2); }
    | '-' expr %prec tUNARY        { $$ = new cdk::neg_node(LINE, $2); }
    | lval '?'                     { $$ = new mml::address_of_node(LINE, $1); }
    | lval                         { $$ = new cdk::rvalue_node(LINE, $1); }  // FIXME: is this needed/in the right place?
    | expr '*' expr	               { $$ = new cdk::mul_node(LINE, $1, $3); }
    | expr '/' expr	               { $$ = new cdk::div_node(LINE, $1, $3); }
    | expr '%' expr	               { $$ = new cdk::mod_node(LINE, $1, $3); }
    | expr '+' expr	               { $$ = new cdk::add_node(LINE, $1, $3); }
    | expr '-' expr	               { $$ = new cdk::sub_node(LINE, $1, $3); }
    | expr '<' expr	               { $$ = new cdk::lt_node(LINE, $1, $3); }
    | expr '>' expr	               { $$ = new cdk::gt_node(LINE, $1, $3); }
    | expr tGE expr	               { $$ = new cdk::ge_node(LINE, $1, $3); }
    | expr tLE expr                { $$ = new cdk::le_node(LINE, $1, $3); }
    | expr tEQ expr	               { $$ = new cdk::eq_node(LINE, $1, $3); }
    | expr tNE expr	               { $$ = new cdk::ne_node(LINE, $1, $3); }
    | tNOT expr %prec tUNARY       { $$ = new cdk::not_node(LINE, $2); }
    | expr tAND expr               { $$ = new cdk::and_node(LINE, $1, $3); }
    | expr tOR expr                { $$ = new cdk::or_node(LINE, $1, $3); }
    | lval '=' expr                { $$ = new cdk::assignment_node(LINE, $1, $3); }
    ;

lval : tIDENTIFIER             { $$ = new cdk::variable_node(LINE, $1); }
    ;

%%

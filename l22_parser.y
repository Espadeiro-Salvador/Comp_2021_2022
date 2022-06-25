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

#include <vector>
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

  int                                                         i;	/* integer value */
  double                                                      d;    /* double value */
  std::string                                                *s;	/* symbol name or string literal */
  std::vector<std::shared_ptr<cdk::basic_type>>          *types; 
  cdk::basic_node                                         *node;	/* node pointer */
  cdk::sequence_node                                  *sequence;
  cdk::expression_node                                       *expression; /* expression nodes */
  cdk::lvalue_node                                           *lvalue;
  l22::block_node                                     *block;
};

%token <i> tINTEGER
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTRING
%token tPUBLIC tUSE tFOREIGN tTHEN tELIF tDO tSTOP tAGAIN tWRITE tRETURN tWRITELN tSIZEOF tNULL tTYPE_INT tTYPE_DOUBLE tTYPE_VOID tTYPE_STRING tTYPE_VAR tAND tOR tNOT tINPUT tWHILE tIF tREAD tBEGIN tEND tPRIVATE tARROW

%nonassoc tIFX
%nonassoc tELSE

%right '='
%left tAND tOR
%nonassoc tNOT
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY

%type <node> file stmt or_else program decl variable
%type <block> block
%type <sequence> stmts exprs decls variables
%type<type> data_type return_type basic_type void_ptr_type
%type<types> data_types
%type <expression> expr funcexpr funcref
%type <lvalue> lval
%type <s> string

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file :              
                       { compiler->ast($$ = new cdk::sequence_node(LINE)); }
     | decls           { compiler->ast($$ = $1); }
     | program         { compiler->ast($$ = $1); }
     | decls program   { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }
     ;

decls :
        decl              { $$ = new cdk::sequence_node(LINE, $1);     }
      | decls decl        { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

decl : 
       data_type tIDENTIFIER ';'                           { $$ = new l22::variable_declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); delete $2;}
     | data_type tIDENTIFIER '=' expr ';'                  { $$ = new l22::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $4); delete $2;}
     | data_type tIDENTIFIER '=' funcexpr ';'              { $$ = new l22::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $4); delete $2;}
     | tPUBLIC data_type tIDENTIFIER ';'                   { $$ = new l22::variable_declaration_node(LINE, tPUBLIC, $2, *$3, nullptr); delete $3;}
     | tFOREIGN data_type tIDENTIFIER ';'                  { $$ = new l22::variable_declaration_node(LINE, tFOREIGN, $2, *$3, nullptr); delete $3;}
     | tUSE data_type tIDENTIFIER  ';'                     { $$ = new l22::variable_declaration_node(LINE, tUSE, $2, *$3, nullptr); delete $3;}
     | tPUBLIC data_type tIDENTIFIER '=' expr  ';'         { $$ = new l22::variable_declaration_node(LINE, tPUBLIC, $2, *$3, $5); delete $3;}
     | tPUBLIC data_type tIDENTIFIER '=' funcexpr  ';'     { $$ = new l22::variable_declaration_node(LINE, tPUBLIC, $2, *$3, $5); delete $3;}
     | tTYPE_VAR tIDENTIFIER '=' expr  ';'                 { $$ = new l22::variable_declaration_node(LINE, tPRIVATE, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC), *$2, $4); delete $2;}
     | tTYPE_VAR tIDENTIFIER '=' funcexpr  ';'             { $$ = new l22::variable_declaration_node(LINE, tPRIVATE, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC), *$2, $4); delete $2;}
     | tPUBLIC tTYPE_VAR tIDENTIFIER '=' expr ';'          { $$ = new l22::variable_declaration_node(LINE, tPUBLIC, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC), *$3, $5); delete $3;}
     | tPUBLIC tTYPE_VAR tIDENTIFIER '=' funcexpr ';'      { $$ = new l22::variable_declaration_node(LINE, tPUBLIC, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC), *$3, $5); delete $3;}
     | tPUBLIC tIDENTIFIER '=' expr  ';'                   { $$ = new l22::variable_declaration_node(LINE, tPUBLIC, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC), *$2, $4); delete $2;}
     | tPUBLIC tIDENTIFIER '=' funcexpr  ';'               { $$ = new l22::variable_declaration_node(LINE, tPUBLIC, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC), *$2, $4); delete $2;}
     ;

basic_type:
            tTYPE_INT                             { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT);     }
          | tTYPE_STRING                          { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING);  }
          | tTYPE_DOUBLE                          { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE);  }
          | '[' basic_type ']'                    { $$ = cdk::reference_type::create(4, $2); }
          ;

void_ptr_type:
            '[' tTYPE_VOID ']'                    { $$ = cdk::primitive_type::create(1, cdk::TYPE_VOID); }
          | '[' void_ptr_type ']'                 { $$ = $2; }
          ;

data_type : 
            basic_type                            { $$ = $1; }
          | void_ptr_type                         { $$ = $1; }
          | return_type '<' '>'                   { $$ = cdk::functional_type::create($1); }
          | return_type '<' data_types '>'        { $$ = cdk::functional_type::create(*$3, $1); }
          ;

return_type :
              data_type                 { $$ = $1; }
            | tTYPE_VOID                { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
            ;

data_types :
            data_type                   { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1); }
          | data_types ',' data_type    { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(*$1); $$->push_back($3); delete $1; }
          ;

program : 
          tBEGIN block ';' tEND { $$ = new l22::program_node(LINE, $2); }
	     ;

block : 
        '{' decls '}' { $$ = new l22::block_node(LINE, $2, nullptr); }
      | '{' stmts '}' { $$ = new l22::block_node(LINE, nullptr, $2); }
      | '{' decls stmts '}' { $$ = new l22::block_node(LINE, $2, $3); }
     ;

stmts :
        stmt	           { $$ = new cdk::sequence_node(LINE, $1); }
      | stmts stmt 	     { $$ = new cdk::sequence_node(LINE, $2, $1); }
      ;

stmt :
       expr ';'                                             { $$ = new l22::evaluation_node(LINE, $1); }
     | tWRITE exprs ';'                                     { $$ = new l22::print_node(LINE, $2); }
 	   | tWRITELN exprs ';'                                   { $$ = new l22::print_node(LINE, $2, true); }
     | tRETURN expr ';'                                     { $$ = new l22::return_node(LINE, $2); }
     | tRETURN funcexpr ';'                                 { $$ = new l22::return_node(LINE, $2); }
     | tRETURN ';'                                          { $$ = new l22::return_node(LINE); }
     | tAGAIN ';'                                           { $$ = new l22::again_node(LINE); }
     | tSTOP  ';'                                           { $$ = new l22::stop_node(LINE); }
     | tWHILE '(' expr ')' tDO block ';'                    { $$ = new l22::while_node(LINE, $3, $6); }
     | tIF '(' expr ')' tTHEN block ';' %prec tIFX          { $$ = new l22::if_node(LINE, $3, $6); }
     | tIF '(' expr ')' tTHEN block ';' or_else             { $$ = new l22::if_else_node(LINE, $3, $6, $8); }
     | block                                                { $$ = $1; }
     ;

or_else :
          tELSE block ';'                                   { $$ = $2; }
        | tELIF '(' expr ')' tTHEN block ';' %prec tIFX     { $$ = new l22::if_node(LINE, $3, $6); }
        | tELIF '(' expr ')' tTHEN block ';' or_else        { $$ = new l22::if_else_node(LINE, $3, $6, $8); }

exprs : 
       expr                                                 { $$ = new cdk::sequence_node(LINE, $1);     }
     | funcexpr ';'                                         { $$ = new cdk::sequence_node(LINE, $1);     }
     | exprs ',' expr                                       { $$ = new cdk::sequence_node(LINE, $3, $1); }
     | exprs ',' funcexpr ';'                               { $$ = new cdk::sequence_node(LINE, $3, $1); }
     ;

expr : tINTEGER                                { $$ = new cdk::integer_node(LINE, $1); }
     | tDOUBLE                                 { $$ = new cdk::double_node(LINE, $1); }
	   | string                                  { $$ = new cdk::string_node(LINE, $1); }
     | tNULL                                   { $$ = new l22::nullptr_node(LINE); }
     | '-' expr %prec tUNARY                   { $$ = new cdk::neg_node(LINE, $2); }
     | '+' expr %prec tUNARY                   { $$ = new l22::identity_node(LINE, $2); }
     | tNOT expr                               { $$ = new cdk::not_node(LINE, $2); }
     | expr '+' expr                           { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr	                         { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr	                         { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr	                         { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr	                         { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr	                         { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr	                         { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr	                         { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr                           { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tNE expr	                         { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tEQ expr	                         { $$ = new cdk::eq_node(LINE, $1, $3); }
     | expr tAND expr                          { $$ = new cdk::and_node(LINE, $1, $3);}
     | expr tOR expr                           { $$ = new cdk::or_node(LINE, $1, $3);}
     | lval '?'                                { $$ = new l22::address_of_node(LINE, $1); }
     | '(' expr ')'                            { $$ = $2; }
     | '(' funcexpr ')'                        { $$ = $2; }
     | lval                                    { $$ = new cdk::rvalue_node(LINE, $1); }
     | lval '=' expr                           { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | lval '=' funcexpr                       { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | tSIZEOF '(' expr ')'                    { $$ = new l22::sizeof_node(LINE, $3); }
     | funcref '(' ')'                         { $$ = new l22::function_call_node(LINE, $1); }
     | funcref '(' exprs ')'                   { $$ = new l22::function_call_node(LINE, $1, $3); }
     | '[' expr ']'                            { $$ = new l22::stack_alloc_node(LINE, $2); }
     | tREAD                                   { $$ = new l22::read_node(LINE); }
     ;

funcref: 
          '(' funcexpr ';' ')'                 { $$ = $2; }
        | lval                                 { $$ = new cdk::rvalue_node(LINE, $1); }
        | '@'                                  { $$ = new cdk::rvalue_node(LINE, new cdk::variable_node(LINE, "@")); }
        ; 

funcexpr: 
      '(' ')' tARROW return_type ':' block            { $$ = new l22::function_node(LINE, cdk::functional_type::create($4), nullptr, $6); }
     | '(' variables ')' tARROW return_type ':' block  { $$ = new l22::function_node(LINE, cdk::functional_type::create($5), $2, $7); }
     ;

variables :
            variable                   { $$ = new cdk::sequence_node(LINE, $1); }
          | variables ',' variable      { $$ = new cdk::sequence_node(LINE, $3, $1); }
          ;

variable :
            data_type tIDENTIFIER                       { $$ = new l22::variable_declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); delete $2;}
         ;

lval : 
       tIDENTIFIER                        { $$ = new cdk::variable_node(LINE, $1); }
     | lval         '[' expr ']'          { $$ = new l22::index_node(LINE, new cdk::rvalue_node(LINE, $1), $3); }
     | '(' expr ')' '[' expr ']'          { $$ = new l22::index_node(LINE, $2, $5); };

string : 
         tSTRING                       { $$ = $1; }
       | string tSTRING                { $$ = $1; $$->append(*$2); delete $2; }
       ;
      
%%
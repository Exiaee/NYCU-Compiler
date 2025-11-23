%{
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string>
#include <vector>
#include <fstream>
#include "symbol_table.h"

#define YYLTYPE LocType

#define MAX_LINE_LENG      256
extern int line_no, col_no, opt_list;
extern char buffer[MAX_LINE_LENG];
extern FILE *yyin;        /* declared by lex */
extern char *yytext;      /* declared by lex */
extern int yyleng;

int yylex();
int yyparse();
void yyerror(const char *msg);


int pass_error = 0;
char *output = NULL;
Node* root = NULL;
SymbolTable symtab;
%}

%locations

%token PROGRAM VAR ARRAY OF INTEGER REAL STRING FUNCTION PROCEDURE PBEGIN END IF THEN ELSE WHILE DO NOT AND OR

%token LPAREN RPAREN SEMICOLON DOT COMMA COLON LBRACE RBRACE DOTDOT ASSIGNMENT ADDOP SUBOP MULOP DIVOP LTOP GTOP EQOP GETOP LETOP NEQOP

%token IDENTIFIER REALNUMBER INTEGERNUM SCIENTIFIC LITERALSTR
%nonassoc LOWER_SEMICOLON

%union {
  int ival;
  double dval;
  char* sval;
  IDType tval;
  OpType oval;
  Node* nval;
}

%type <ival> INTEGERNUM
%type <dval> REALNUMBER SCIENTIFIC
%type <sval> IDENTIFIER LITERALSTR
%type <tval> standard_type
%type <oval> addop mulop relop
%type <nval> prog identifier_list declarations type subprogram_declarations subprogram_head arguments parameter_list parameter compound_statement statement_list statement variable tail procedure_statement expression_list expression boolexpression simple_expression term factor signed_num num

%%

    /* define your snytax here */
    /* @n return the sturct LocType of "n-th node", ex: @1 return the PROGRAM node's locType
       $n return the $$ result you assigned to the rule, ex: $1 */
prog: PROGRAM IDENTIFIER LPAREN identifier_list RPAREN SEMICOLON
{   symtab.open_scope();
    TypeDescriptor *void_type = new TypeDescriptor{IDType::VOID, 0, 0, nullptr, nullptr};
    symtab.add($2, void_type);

    
}
      declarations
      subprogram_declarations
      compound_statement
      DOT 
{

    //symtab.open_scope();
    root = new Node{NodeType::PROG, @2, {$4, $8, $9, $10}, nullptr};

    //TypeDescriptor *void_type = new TypeDescriptor{IDType::VOID, 0, 0, nullptr, nullptr};
    //symtab.flush();
    symtab.close_scope(); 
    

    symtab.print_symbol_table();

};

identifier_list: IDENTIFIER
{
    $$ = new Node{NodeType::ID_LIST, @1, {}, nullptr};
    $$->metadata.sval = $1;
    
}
               | IDENTIFIER COMMA identifier_list 
{
    $$ = new Node{NodeType::ID_LIST, @1, {}, $3};
    $$->metadata.sval = $1;
}
               ;
               
declarations: declarations VAR identifier_list COLON type SEMICOLON
{
   
    $$ = new Node{NodeType::DECL_LIST, @2, {$3, $5},$1};
    symtab.add_declarations($$);
    symtab.flush();
    
}
            |
{
    $$ = nullptr;
}
            ;
            
type: standard_type
{
    $$ = new Node{NodeType::TYPE, @1, {}, nullptr};
    $$->metadata.tval = $1;
}
    | ARRAY LBRACE INTEGERNUM DOTDOT INTEGERNUM RBRACE OF type
{
    Node *lower_bound = new Node{NodeType::LITERAL_INT, @3, {}, nullptr};
    lower_bound->metadata.ival = $3;
    Node *upper_bound = new Node{NodeType::LITERAL_INT, @5, {}, nullptr};
    upper_bound->metadata.ival = $5;
    $$ = new Node{NodeType::TYPE, @1, {lower_bound, upper_bound, $8}, nullptr};
    $$->metadata.tval = IDType::ARRAY;

}
    ;
    
standard_type: INTEGER
{
    $$ = IDType::INT;
}
             | REAL
{
    $$ = IDType::REAL;
}
             | STRING
{
    $$ = IDType::STRING;
}
             ;

subprogram_declarations: subprogram_declarations subprogram_head 
                         declarations 
                         compound_statement SEMICOLON
                        
{

    $$ = new Node{NodeType::SUBPROG_DECL_LIST, {}, {$1, $2, $3, $4}, $1};
    if(symtab.is_function && !symtab.function_has_return){
          fprintf(stderr, RETURN_VAL,
            $2->loc.first_line, $2->loc.first_column,
            symtab.current_function_name.c_str());
        symtab.is_function=false;
        symtab.function_has_return=false;
    }
    symtab.sub_close_scope();
    symtab.current_function_name="";

}
                       |
{
    $$ = nullptr;
}
                       ;

subprogram_head: FUNCTION IDENTIFIER arguments COLON type SEMICOLON
{   
    symtab.is_function=true;
    $$ = new Node{NodeType::SUBPROG_HEAD, @1, {$3, $5}, nullptr};
    $$->metadata.sval = $2;
    symtab.current_function_name= $2;
    //printf("current_function, %s\n",symtab.current_function_name.c_str());
    // 建立函式 TypeDescriptor
    TypeDescriptor* func_type = new TypeDescriptor{IDType::SUBPROG, 0, 0, nullptr, nullptr};
    TypeDescriptor* return_type = new TypeDescriptor{$5->metadata.tval, 0, 0, nullptr, nullptr};
    std::string name(symtab.id_type_to_str($5->metadata.tval));
    symtab.func_return_type=name;
    // 收集參數型別並鏈結
    TypeDescriptor* param_chain = symtab.collect_function_param_types($3);
    //return_type->next = param_chain; // return -> param1 -> param2 ...
    func_type->next = param_chain;
    func_type->base = return_type;
    

    TypeDescriptor* redefined_func=symtab.lookup($2);
    if(redefined_func && redefined_func->base && redefined_func->base->id_type==return_type->id_type){
        fprintf(stderr,"%d:%d: missing return value of function %s\n",@1.first_line,@1.first_column,$2);
    }
    if(symtab.lookup_in_current_scope($2)){
        fprintf(stderr, REDEF_FUN, @1.first_line, @1.first_column, $2);
        symtab.duplicated_func[$2]++;
    }
    else symtab.add($2, func_type);
    std::vector<std::string> prama_types;
    // 開啟 function scope
    symtab.open_scope();
    symtab.add_function_params($3, prama_types); // 實際加入參數名稱到符號表
    symtab.function_param_count[$2]=prama_types.size();
    prama_types.clear();


}
               | PROCEDURE IDENTIFIER arguments SEMICOLON
{

     $$ = new Node{NodeType::SUBPROG_HEAD, @1, {$3}, nullptr};
    $$->metadata.sval = $2;
      if(symtab.lookup_in_current_scope($2)){
        fprintf(stderr, REDEF_FUN, @1.first_line, @1.first_column, $2);
       
    }
    TypeDescriptor* proc_type = new TypeDescriptor{IDType::SUBPROG,0,0,nullptr,nullptr};
    TypeDescriptor* ret_type = new TypeDescriptor{IDType::VOID,0,0,nullptr,nullptr};

    TypeDescriptor* param_chain = symtab.collect_function_param_types($3);
    proc_type->next = param_chain; // return void -> params
    proc_type->base = ret_type;

    symtab.add($2, proc_type);
    std::vector<std::string> prama_types;
    symtab.open_scope();
    symtab.add_function_params($3, prama_types); // 實際加入參數名稱到符號表

    prama_types.clear();

}
               ;

arguments: LPAREN parameter_list RPAREN
{
    $$ = $2;
}
         |
{
    $$ = nullptr;
}
         ;
         
parameter_list: parameter
{
    $$ = $1;
}
              | parameter_list SEMICOLON parameter %prec LOWER_SEMICOLON
{
    
    Node* last = $1;
    while(last->next){
        last = last -> next;
    }
    last->next = $3;
    $$=$1;
 
}
              ;

parameter: optional_var identifier_list COLON type
{
    $$ = new Node{NodeType::PARAM_LIST, {}, {$2, $4}, nullptr};
    $$->metadata.tval=$4->metadata.tval;
}
              
optional_var: VAR
            |
            ;

compound_statement: PBEGIN
                    statement_list 
                    END
{
    $$ = $2;
    symtab.flush();
    symtab.seen.clear();
    symtab.undeclared_variable=false;
    symtab.is_arry_op=false;
    // close the current scope due to encountering END
    
}|
    PBEGIN END{
        $$ = nullptr;
        if (symtab.is_function) {
            symtab.function_has_return = false;
        }
    }
                  ;
                   
statement_list: 
        statement
{
    $$ = new Node{NodeType::STMT_LIST, {}, {$1}, nullptr};
}
              | statement SEMICOLON statement_list
{
    if($1==nullptr) $$=$3;
    else $$ = new Node{NodeType::STMT_LIST, {}, {$1}, $3};
}
              ;
              
statement: variable ASSIGNMENT expression
{
    LocType loc = { @1.first_line, @1.first_column, @3.last_line, @3.last_column };
    if($1->child[0]!=nullptr) symtab.is_arry_op=true;
    if(symtab.is_arry_op && !symtab.undeclared_variable){
        symtab.check_array($1);
        symtab.check_array_index($1);
    }
    //symtab.undeclared_variable=true;
    TypeDescriptor* lhs_type=symtab.lookup($1->metadata.sval);
    //TypeDescriptor* rhs_type=nullptr;
    TypeDescriptor* rhs_type = symtab.get_exp_str($3);
    if(lhs_type && lhs_type->id_type==IDType::SUBPROG ){
        symtab.is_function=true;
        if(symtab.duplicated_func.count($1->metadata.sval)){
            fprintf(stderr, ASSIG_TYPE,
                @1.first_line, @1.first_column);
        }
    }
    std::string name($1->metadata.sval);

    
    
    if(lhs_type && rhs_type &&!symtab.undeclared_variable){
        if(symtab.is_function &&lhs_type){          
            if(std::string($1->metadata.sval)==symtab.current_function_name){
                symtab.function_has_return = true;
            }
            TypeDescriptor* lhs_type_func=symtab.lookup($1->metadata.sval);
            if(symtab.duplicated_func.count($1->metadata.sval)==0 && lhs_type_func->base && rhs_type){
                symtab.check_type_compatibility(lhs_type_func->base, rhs_type, loc);
            }
            else if(lhs_type_func->base && lhs_type_func->base->id_type==IDType::REAL){
                symtab.check_type_compatibility(lhs_type_func->base, rhs_type, loc);
            }
            else{
                symtab.check_type_compatibility(lhs_type, rhs_type, loc);
            }
         
            TypeDescriptor *lhs_base=symtab.cloneType(lhs_type);
        
            if(lhs_type->id_type==IDType::ARRAY && lhs_type->base){
                lhs_base=symtab.get_base_type(lhs_type);
            }
    
            if(lhs_type->id_type==IDType::ARRAY){
                symtab.check_type_compatibility(lhs_base->base, rhs_type, loc);
            }

            if(lhs_type && lhs_type->id_type == IDType::SUBPROG && lhs_type->base){
                lhs_type=lhs_type->base;
                std::string lhs_type_name(symtab.id_type_to_str(lhs_type->id_type));
                if(symtab.func_return_type!=lhs_type_name){
                    symtab.function_has_return=false;
                    }
            
                }
            }
            else{
                TypeDescriptor *lhs_base=lhs_type;
                if(lhs_type->id_type==IDType::ARRAY && lhs_type->base){
                    lhs_base=symtab.get_base_type(lhs_type);
                    if(lhs_type->base->id_type==IDType::ARRAY && lhs_base){
                        symtab.check_type_compatibility(lhs_base->base, rhs_type, loc);
                    }
                }
                if( lhs_type->id_type!=IDType::ARRAY || rhs_type->id_type!=IDType::ARRAY){
        
                    if(lhs_type->id_type==IDType::ARRAY){
                        symtab.check_type_compatibility(lhs_base, rhs_type, loc);
                    }
                    else{
                        symtab.check_type_compatibility(lhs_type, rhs_type, loc);
                    }
                }
            
        }
    }

 
    $$ = new Node{NodeType::ASSIGN, {}, {$1, $3}, nullptr};

}
         | procedure_statement
{
    $$ = $1;
}
         | compound_statement
{
    $$ = $1;
}
         | IF expression THEN statement ELSE statement
{
    $$ = new Node{NodeType::IF, {}, {$2, $4, $6}, nullptr};
}
         | WHILE expression DO statement
{
    $$ = new Node{NodeType::WHILE, {}, {$2, $4}, nullptr};
}
         |
{
    $$ = nullptr;
}
         ;

variable: IDENTIFIER tail
{
   
    $$ = new Node{NodeType::VAR, @1, {$2}, nullptr};
    $$->metadata.sval = $1;
    TypeDescriptor* type=symtab.lookup($1);
    
    std::string name = $1;
    if(!type){
         fprintf(stderr, "%d:%d: undeclared variable %s\n",
                @1.first_line, @1.first_column, $1);
        symtab.undeclared_table[name]={@1.first_line,@1.first_column};
        symtab.undeclared_variable=true;
    }
    if(type && type->id_type==IDType::REAL && symtab.is_arry_op){
        fprintf(stderr, INDEX_TYPE, @1.first_line, @1.first_column);
    }
    if(type && (type->id_type==IDType::INT) && $$->child[0]){
        fprintf(stderr, INDEX_MANY, @1.first_line, @1.first_column,$1);
    }
 
}
        ;

tail: LBRACE expression RBRACE tail
{
    $$ = new Node{NodeType::EXPR_LIST, {}, {$2}, $4};
    
    
}
    |
{
    $$ = nullptr;
}
    ;

procedure_statement: IDENTIFIER
{
    TypeDescriptor* type = symtab.lookup($1);
    if(!type || type->id_type!=IDType::SUBPROG){
         fprintf(stderr, UNDEC_FUN, @1.first_line, @1.first_column, $1);
    }
    $$ = new Node{NodeType::PROCEDURE, @1, {nullptr}, nullptr};
    $$->metadata.sval = $1;
 
}
                   | IDENTIFIER LPAREN expression_list RPAREN
{
    

    Node* proc_node = new Node{NodeType::PROCEDURE, @1, {$3}, nullptr};
    proc_node->metadata.sval = $1;
    symtab.check_procedure_type(proc_node,$3);
    $$=proc_node;

}
                   ;
                   
expression_list: expression
{
    $$ = new Node{NodeType::EXPR_LIST, {}, {$1}, nullptr};
}
/*               | expression COMMA expression_list
{
    $$ = new Node{NodeType::EXPR_LIST, {}, {$1}, $3};
}*/
                | expression COMMA expression_list{
    //int c=1;
    //$$ = new Node{NodeType::EXPR_LIST, {}, {$1}, $3};
    Node* last = $1;
    while(last->next){ 
        last = last->next; // find the tail node
    
    }
          last->next = $3;                     // append the new expression to the list
          $$ = $1;    

}

               ;
               
expression: boolexpression
{
    $$ = $1;
}
          | boolexpression AND boolexpression
{
    $$ = new Node{NodeType::OP, @2, {$1, $3}, nullptr};
    $$->metadata.oval = OpType::AND;
}
          | boolexpression OR boolexpression
{
    $$ = new Node{NodeType::OP, @2, {$1, $3}, nullptr};
    $$->metadata.oval = OpType::OR;
}
          ;
          
boolexpression: simple_expression
{
    $$ = $1;
}
              | simple_expression relop simple_expression
{
    $$ = new Node{NodeType::OP, @2, {$1, $3}, nullptr};
    $$->metadata.oval = $2;
    TypeDescriptor *lhs=symtab.get_exp_str($$->child[0]);
    TypeDescriptor *rhs=symtab.get_exp_str($$->child[1]);
    if(lhs && rhs &&(lhs->id_type!=rhs->id_type)){
        fprintf(stderr,ARITH_TYPE, @2.first_line, @2.first_column,
                    ($2==OpType::LET?"<=":
                    $2==OpType::GET?">=":"?"));
    }

}
              ;
              
simple_expression: term
{
    $$ = $1;
}
                 | simple_expression addop term
{
    $$ = new Node{NodeType::OP, @2, {$1, $3}, nullptr};
    $$->metadata.oval = $2;
    LocType loc = {@2.first_line,@2.first_column,@2.last_line,@2.last_column};
    OpType op=$2;
    TypeDescriptor* lhs_type=symtab.get_exp_str($1);
    TypeDescriptor* rhs_type=symtab.get_exp_str($3);
    if(lhs_type && rhs_type && (lhs_type->id_type==IDType::SUBPROG || (rhs_type->id_type==IDType::SUBPROG))){ 
        symtab.is_function=true;
    }
    if((lhs_type && lhs_type->id_type==IDType::ARRAY) || (rhs_type && rhs_type->id_type==IDType::ARRAY)){
        symtab.is_arry_op=true;
    }
    if(lhs_type && lhs_type->id_type==IDType::ARRAY){
        TypeDescriptor* lhs_base=symtab.cloneType(lhs_type);
        if(lhs_base->base){
            TypeDescriptor* f_lhs_base=symtab.get_base_type(lhs_base);
            if(f_lhs_base){
                symtab.check_arithmetic_type(f_lhs_base->base, rhs_type, op, loc);
            }
        }
        if(!$1->child[0] &&!symtab.undeclared_variable ){
            fprintf(stderr,ARITH_TYPE, @2.first_line, @2.first_column,
                    ($2==OpType::ADD?"+":
                    $2==OpType::SUB?"-":"?"));

        }
        if(!symtab.undeclared_variable && lhs_base->base){
                symtab.check_arithmetic_type(lhs_base->base, rhs_type, op, loc);
        }

    }
    else if(rhs_type && rhs_type->id_type==IDType::ARRAY && !symtab.undeclared_variable){
        if(!$3->child[0] && !symtab.undeclared_variable){
            fprintf(stderr,ARITH_TYPE,@2.first_line, @2.first_column,
                    ($2==OpType::ADD?"+":
                    $2==OpType::SUB?"-":"?"));
        }

    }
   
   
   if((!lhs_type || !rhs_type) && !symtab.is_arry_op && !symtab.undeclared_variable ){
        fprintf(stderr,ARITH_TYPE,@2.first_line, @2.first_column,
                    ($2==OpType::ADD?"+":
                    $2==OpType::SUB?"-":"?"));           
    }
    
    if(!symtab.is_arry_op && !symtab.undeclared_variable &&(lhs_type && rhs_type)){
        symtab.check_non_arithmetic_type(lhs_type, rhs_type, op, loc);

    }
}
                 ;
                 
term: factor
{
    $$ = $1;
}
    | term mulop factor
{
    $$ = new Node{NodeType::OP, @2, {$1, $3}, nullptr};
    $$->metadata.oval = $2;
}
    ;
    
factor: variable
{
    $$ = $1;
}
      | IDENTIFIER LPAREN expression_list RPAREN
{
    //symtab.is_arry_op=true;
    $$ = new Node{NodeType::VAR, @1, {$3}, nullptr};
    Node *find_func=symtab.find_func($3);
    if(find_func) symtab.is_function=true;
    int actual_arg=symtab.actual_args($3);
    int expected_arg=symtab.function_param_count[$1];
    TypeDescriptor* func=symtab.lookup($1);
    if(!func){
        fprintf(stderr,UNDEC_FUN, @1.first_line, @1.first_column, $1);
    }
    else if(actual_arg!=expected_arg){
        fprintf(stderr, WRONG_ARGS, @1.first_line, @1.first_column, $1);
    }
    $$->metadata.sval = $1;
}
      | signed_num
{
    $$ = $1;
}
      | LITERALSTR
{
    $$ = new Node{NodeType::LITERAL_STR, @1, {}, nullptr};
    $$->metadata.sval = $1;
}
      | LPAREN expression RPAREN
{
    $$ = $2;
}
      | NOT factor
{
    $$ = new Node{NodeType::NOT, {}, {$2}, nullptr};
}
      ;
      
addop: ADDOP
{
    $$ = OpType::ADD;
}
     | SUBOP
{
    $$ = OpType::SUB;
}
     ;

mulop: MULOP
{
    $$ = OpType::MUL;
}
     | DIVOP
{
    $$ = OpType::DIV;
}
     ;
     
relop: LTOP
{
    $$ = OpType::LT;
}
     | GTOP
{
    $$ = OpType::GT;
}
     | EQOP
{
    $$ = OpType::EQ;
}
     | LETOP
{
    $$ = OpType::LET;
}
     | GETOP
{
    $$ = OpType::GET;
}
     | NEQOP
{
    $$ = OpType::NEQ;
}
     ;

signed_num: num
{
    $$ = $1;
}
          | SUBOP signed_num
{
    $$ = new Node{NodeType::NEGATE, {}, $2, nullptr};
}
          ;
     
num: REALNUMBER
{
    $$ = new Node{NodeType::LITERAL_DBL, @1, {}, nullptr};
    $$->metadata.dval = $1;
}
   | INTEGERNUM
{
    $$ = new Node{NodeType::LITERAL_INT, @1, {}, nullptr};
    $$->metadata.ival = $1;
}
   | SCIENTIFIC
{
    $$ = new Node{NodeType::LITERAL_DBL, @1, {}, nullptr};
    $$->metadata.dval = $1;
}
   ;

%%

void yyerror(const char *msg) {
    fprintf(stderr,
            "[ERROR] line %4d:%3d %s, Unmatched token: %s\n",
            line_no, col_no - yyleng, buffer, yytext);
    pass_error = 1;
}

int main(int argc, const char *argv[]) {
    if(argc > 2)
        fprintf(stderr, "Usage: ./parser [filename]\n"), exit(0);

    FILE *fp = argc == 1 ? stdin : fopen(argv[1], "r");
    if(fp == NULL)
        fprintf(stderr, "Open file error\n"), exit(-1);

    yyin = fp;
    yyparse();
    
    return 0;
}

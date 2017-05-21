/**
 * File: parser.mly
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * Definition of parser for the Snick compiler. The parser reads 
 * a stream of tokens from the tokenizer and constructs a Snick 
 * Ast.
 */

%{
open Ast
open Lexing

let parse_error msg =
  let start_pos = symbol_start_pos () in
  let end_pos = symbol_end_pos () in
  Printf.eprintf 
    "Syntax error at line %d, characters %d-%d: %s\n"
    start_pos.pos_lnum
    (start_pos.pos_cnum - start_pos.pos_bol)
    (end_pos.pos_cnum - start_pos.pos_bol)
    msg ;
  raise Parse_error
%}

%token <string> BOOL_CONST
%token <string> FLOAT_CONST
%token <string> INT_CONST
%token <string> STRING_CONST
%token <string> IDENT
%token BOOL FLOAT INT
%token PROC END
%token VAL REF
%token WHILE DO OD
%token IF THEN ELSE FI
%token READ WRITE
%token ASSIGN
%token EQ NE LTE GTE LT GT
%token ADD MUL DIV
%token AND OR NOT
%token MINUS
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token SEMICOLON COMMA
%token UPTO
%token EOF

%left OR /* lowest precedence */
%left AND
%nonassoc NOT
%nonassoc EQ NE LT LTE GT GTE
%left ADD MINUS
%left MUL DIV
%nonassoc UMINUS /* highest precendence */

%type <Ast.program> program

%start program
%%

program:
  procedure_list { { procdefs = List.rev $1 } }

procedure_list:
  | procedure_definition { [$1] }
  | procedure_list procedure_definition {
    $2 :: $1
  }

procedure_definition:
  PROC procedure_header procedure_body END { 
    { header = $2; body = $3 } 
  }

procedure_header:
  identifier LPAREN parameter_list RPAREN {
    { proc_id = $1; params = List.rev $3 }
  }

parameter_list:
  | { [] }
  | parameter_definition { [$1] }
  | parameter_list COMMA parameter_definition { $3 :: $1 }

parameter_definition:
  reference_specifier type_specifier identifier {
    { mode = $1; dtype = $2; id = $3 }
  }

reference_specifier:
  | VAL { Val }
  | REF { Ref }

type_specifier:
  | BOOL { BoolType }
  | FLOAT { FloatType }
  | INT { IntType }

identifier:
  IDENT { $1 }

procedure_body:
  declaration_list statement_list {
    { decls = List.rev $1; stmts = List.rev $2 }
  }

declaration_list:
  | { [] }
  | declaration_list declaration SEMICOLON { $2 :: $1 }

declaration:
  | variable_declaration { $1 }
  | array_declaration { $1 }

variable_declaration:
  type_specifier identifier { VarDecl ($1, $2) }

array_declaration:
  | type_specifier identifier LBRACKET interval_list RBRACKET {
    ArrDecl ($1, $2, List.rev $4)
  }

interval_list:
  | interval { [$1] }
  | interval_list COMMA interval { $3 :: $1 }

interval:
  INT_CONST UPTO INT_CONST { 
    (int_of_string $1, int_of_string $3) 
  }

statement_list:
  | statement { [$1] }
  | statement_list statement { $2 :: $1 }

statement:
  | assignment_statement { AtomStmt $1 }
  | read_statement { AtomStmt $1 }
  | write_statement { AtomStmt $1 }
  | procedure_call_statement { AtomStmt $1 }
  | selection_statement { CompStmt $1 }
  | iteration_statement { CompStmt $1 }

assignment_statement:
  | lvalue ASSIGN expression SEMICOLON { Assign ($1, $3) }

read_statement:
  READ lvalue SEMICOLON { Read $2 }

write_statement:
  WRITE expression SEMICOLON { Write $2 }

procedure_call_statement:
  | identifier LPAREN expression_list RPAREN SEMICOLON { 
    Call ($1, List.rev $3) 
  }

selection_statement:
  | IF expression THEN statement_list FI { 
    IfThen ($2, List.rev $4) 
  }
  | IF expression THEN statement_list ELSE statement_list FI {
    IfThenElse ($2, List.rev $4, List.rev $6)
  }

iteration_statement:
  WHILE expression DO statement_list OD { 
    While ($2, List.rev $4) 
  }

lvalue:
  | identifier LBRACKET expression_list RBRACKET { 
    ArrAccess ($1, List.rev $3) 
  }
  | identifier { Id $1 }

expression_list:
  | { [] }
  | expression { [$1] }
  | expression_list COMMA expression { $3 :: $1 }

expression:
  | expression_ { { expr = $1 ; inferred_type = UnknownType } }
  
expression_:
  | lvalue { LvalueExpr $1 }
  | constant { ConstExpr $1 }
  | expression OR expression { BinopExpr ($1, OrBinop, $3) }
  | expression AND expression { BinopExpr ($1, AndBinop, $3) }
  | expression EQ expression { BinopExpr ($1, EqBinop, $3) }
  | expression NE expression { BinopExpr ($1, NeBinop, $3) }
  | expression LTE expression { BinopExpr ($1, LteBinop, $3) }
  | expression GTE expression { BinopExpr ($1, GteBinop, $3) }
  | expression LT expression { BinopExpr ($1, LtBinop, $3) }
  | expression GT expression { BinopExpr ($1, GtBinop, $3) }
  | expression ADD expression { BinopExpr ($1, AddBinop, $3) }
  | expression MINUS expression { BinopExpr ($1, SubBinop, $3) }
  | expression MUL expression { BinopExpr ($1, MulBinop, $3) }
  | expression DIV expression { BinopExpr ($1, DivBinop, $3) }
  | NOT expression { UnopExpr (NotUnop, $2) }
  | MINUS expression %prec UMINUS { UnopExpr (MinusUnop, $2) }
  | LPAREN expression_ RPAREN { $2 }

constant:
  | BOOL_CONST {
    { value = Boolean (bool_of_string $1); raw = $1 }
  }
  | FLOAT_CONST {
    { value = Float (float_of_string $1); raw = $1 }
  }
  | INT_CONST {
    { value = Integer (int_of_string $1); raw = $1 }
  }
  | STRING_CONST {
    { value = String $1; raw = Printf.sprintf "\"%s\"" $1 }
  }

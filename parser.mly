/* ocamlyacc parser for bean */
%{
open Ast
%}

%token <bool> BOOL_CONST
%token <float> FLOAT_CONST
%token <int> INT_CONST
%token <string> IDENT
%token BOOL FLOAT INT
%token PROC END
%token VAL REF
%token WHILE DO OD
%token IF THEN ELSE FI
%token READ WRITE
%token ASSIGN
%token EQ LTE GTE LT GT
%token ADD SUB MUL DIV
%token AND OR NOT
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token SEMICOLON
%token UPTO
%token EOF

%nonassoc EQ LT GT LTE GTE
%left ADD SUB MUL DIV AND OR
%nonassoc UMINUS NOT

%type <Snack_ast.program> program

%start program
%%

program:
	procdefs { { procdefs = $1 } }

procdefs:
	| procdef procdefs { $1 :: $2 }
	| { [] }

procdef:
	PROC procheader procbody END { { header = $1; body = $2 } }

procheader:
	IDENT LPAREN params RPAREN { { id = $1; params = $3 } }

params:
	| param params { $1 :: $2 }
	| { [] }

param:
  passind dtype ident { { mode = $1; dtype = $2; id = $3 } }

passind:
	| VAL { Val }
	| REF { Ref }

dtype:
  | BOOL { Bool }
	| FLOAT { Float }
  | INT { Int }

ident:
	IDENT { $1 }

procbody:
	decls stmts { { decls = $1; stmts = $2 } }

decls:
  | decl decls { $1 :: $2 }
  | { [] }

decl:
	| vardecl { $1 }
	| arrdecl { $1 }

vardecl:
	dtype IDENT SEMICOLON { VarDecl ($2, $1) }

vardecl:
	dtype IDENT LBRACKET intervals RBRACKET SEMICOLON { ArrDecl ($1, $2, $4) }

intervals:
	| interval COMMA intervals { $1 :: $3 }
	| interval { [$1] }

interval:
	INT_CONST UPTO INT_CONST { ($1, $3) }

stmts:
  | stmt stmts { $1 :: $2 }
  | { [] }

stmt:
	| atomic_stmt { $1 }
	| composite_stmt { $1 }

atomic_stmt:
	stmt_body SEMICOLON { $1 }

stmt_body:
	| lvalue ASSIGN expr { Assign ($1, $3) }
  | READ lvalue { Read $2 }
  | WRITE expr { Write $2 }
  | IDENT LPAREN exprs RPAREN { Call ($1, $3) }

lvalue:
	| IDENT LBRACKET exprs RBRACKET { SubscrLval ($1, $3) }
  | IDENT { IdLval $1 }

exprs:
	| expr COMMA exprs { $1 :: $2 }
	| expr { [$1] }

expr:
	| const { ConstExpr $1 }
	| subscr { SubscrExpr $1 }
	| id { IdExpr $1 }
	| binop_expr { $1 }
	| unop_expr { $1 }
	| LPAREN expr RPAREN { $2 }

const:
	| BOOL_CONST { $1 }
	| FLOAT_CONST { $1 }
	| INT_CONST { $1 }

binop_expr:
	| expr binop expr { BinopExpr ($1, $2, $3) }

binop:
	| OR { OrBinop }
	| AND { AndBinop }
	| EQ { EqBinop }
	| NE { NeBinop }
	| LTE { LteBinop }
	| GTE { GteBinop }
	| LT { LtBinop }
	| GT { GtBinop }
	| ADD { AddBinop }
	| SUB { SubBinop }
	| MUL { MulBinop }
	| DIV { DivBinop }

unop_expr:
	| unop expr { UnopExpr ($1, $2) }

unop:
	| NOT { Not }
	| MINUS { Minus }

composite_stmt:
	| ifthen { $1 }
	| ifthenelse { $1 }
	| whiledo { $1 }

ifthen:
	IF expr THEN stmts FI { IfThen ($2, $4) }

ifthenelse:
	IF expr THEN stmts ELSE stmts FI { IfThenElse ($2, $4, $6) }

whiledo:
	WHILE expr DO stmts OD { While ($2, $4) }

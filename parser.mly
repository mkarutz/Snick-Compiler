/* ocamlyacc parser for bean */
%{
open Ast
%}

%token <bool> BOOL_CONST
%token <float> FLOAT_CONST
%token <int> INT_CONST
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
	procdefs { { procdefs = $1 } }

procdefs:
	| procdef procdefs { $1 :: $2 }
	| { [] }

procdef:
	PROC procheader procbody END { { header = $2; body = $3 } }

procheader:
	IDENT LPAREN params RPAREN { { id = $1; params = $3 } }

params:
	| param COMMA params { $1 :: $3 }
	| param { [$1] }
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
	dtype IDENT SEMICOLON { VarDecl ($1, $2) }

arrdecl:
	dtype IDENT LBRACKET intervals RBRACKET SEMICOLON { ArrDecl ($1, $2, $4) }

intervals:
	| interval COMMA intervals { $1 :: $3 }
	| interval { [$1] }

interval:
	INT_CONST UPTO INT_CONST { ($1, $3) }

stmts:
  | stmt stmts { $1 :: $2 }
  | stmt { [$1] }

stmt:
	| atomic_stmt { $1 }
	| comp_stmt { $1 }

atomic_stmt:
	stmt_body SEMICOLON { AtomStmt $1 }

stmt_body:
	| lvalue ASSIGN expr { Assign ($1, $3) }
  | READ lvalue { Read $2 }
  | WRITE expr { Write $2 }
  | IDENT LPAREN exprs RPAREN { Call ($1, $3) }

lvalue:
	| IDENT LBRACKET exprs RBRACKET { ArrAccess ($1, $3) }
  | IDENT { Id $1 }

exprs:
	| expr COMMA exprs { $1 :: $3 }
	| expr { [$1] }

expr:
	| const { ConstExpr $1 }
	| lvalue { LvalueExpr $1 }
	| expr OR expr { BinopExpr ($1, OrBinop, $3) }
	| expr AND expr { BinopExpr ($1, AndBinop, $3) }
	| expr EQ expr { BinopExpr ($1, EqBinop, $3) }
	| expr NE expr { BinopExpr ($1, NeBinop, $3) }
	| expr LTE expr { BinopExpr ($1, LteBinop, $3) }
	| expr GTE expr { BinopExpr ($1, GteBinop, $3) }
	| expr LT expr { BinopExpr ($1, LtBinop, $3) }
	| expr GT expr { BinopExpr ($1, GtBinop, $3) }
	| expr ADD expr { BinopExpr ($1, AddBinop, $3) }
	| expr MINUS expr { BinopExpr ($1, SubBinop, $3) }
	| expr MUL expr { BinopExpr ($1, MulBinop, $3) }
	| expr DIV expr { BinopExpr ($1, DivBinop, $3) }
	| NOT expr { UnopExpr (NotUnop, $2) }
	| MINUS expr %prec UMINUS { UnopExpr (MinusUnop, $2) }
	| LPAREN expr RPAREN { $2 }

const:
	| BOOL_CONST { BoolConst $1 }
	| FLOAT_CONST { FloatConst $1 }
	| INT_CONST { IntConst $1 }
	| STRING_CONST { StringConst $1 }

comp_stmt:
	| ifthen { CompStmt $1 }
	| ifthenelse { CompStmt $1 }
	| whiledo { CompStmt $1 }

ifthen:
	IF expr THEN stmts FI { IfThen ($2, $4) }

ifthenelse:
	IF expr THEN stmts ELSE stmts FI { IfThenElse ($2, $4, $6) }

whiledo:
	WHILE expr DO stmts OD { While ($2, $4) }

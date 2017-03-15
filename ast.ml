type ident = string

type dtype =
  | Bool
	| Float
  | Int

type interval = (int * int)

type decl =
	| VarDecl of (dtype * ident)
	| ArrDecl of (dtype * ident * (interval list))

type binop =
  | OrBinop
	| AndBinop
	| EqBinop
	| NeBinop
	| LtBinop
	| GtBinop
	| LteBinop
	| GteBinop
	| AddBinop
	| SubBinop
	| MulBinop
	| DivBinop

type unop =
	| NotUnop
  | MinusUnop

type const =
	| BoolConst of bool
	| FloatConst of float
	| IntConst of int
	| StringConst of string

type lvalue =
  | Id of ident
  | ArrAccess of (ident * (expr list))
and expr =
	| ConstExpr of const
	| LvalueExpr of lvalue
	| BinopExpr of (expr * binop * expr)
	| UnopExpr of (unop * expr)

type stmt =
	| AtomStmt of atomstmt
	| CompStmt of compstmt
and atomstmt =
  | Assign of (lvalue * expr)
  | Read of lvalue
  | Write of expr
	| Call of (ident * (expr list))
and compstmt =
	| IfThenElse of (expr * (stmt list) * (stmt list))
	| IfThen of (expr * (stmt list))
	| While of (expr * (stmt list))

type passind =
	| Val
	| Ref

type param = {
	mode : passind ;
	dtype : dtype ;
	id : ident
}

type procheader = {
	id : ident ;
	params : param list
}

type procbody = {
  decls : decl list ;
  stmts : stmt list
}

type procdef = {
	header : procheader ;
	body : procbody ;
}

type program = {
	procdefs : procdef list ;
}

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

type expr =
  | ConstExpr of const
	| IdExpr of ident
  | SubscrExpr of (ident * (expr list))
  | BinopExpr of (expr * binop * expr)
  | UnopExpr of (unop * expr)

type lvalue =
  | IdLval of ident
  | SubscrLval of (ident * (expr list))

type stmt =
  | Assign of (lvalue * expr)
  | Read of lvalue
  | Write of expr
	| Call of (ident * (expr list))
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

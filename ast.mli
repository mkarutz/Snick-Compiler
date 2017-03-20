type identifier = string

type type_spec =
  | Bool
  | Float
  | Int

type interval = (int * int)

type decl =
  | VarDecl of (type_spec * identifier)
  | ArrDecl of (type_spec * identifier * (interval list))

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
  | Id of identifier
  | ArrAccess of (identifier * (expr list))
and expr =
  | ConstExpr of const
  | LvalueExpr of lvalue
  | BinopExpr of (expr * binop * expr)
  | UnopExpr of (unop * expr)

type stmt =
  | AtomStmt of atom_stmt
  | CompStmt of comp_stmt
and atom_stmt =
  | Assign of (lvalue * expr)
  | Read of lvalue
  | Write of expr
  | Call of (identifier * (expr list))
and comp_stmt =
  | IfThenElse of (expr * (stmt list) * (stmt list))
  | IfThen of (expr * (stmt list))
  | While of (expr * (stmt list))

type ref_spec =
  | Val
  | Ref

type param_def = {
  mode : ref_spec ;
  type_spec : type_spec ;
  id : identifier
}

type proc_header = {
  id : identifier ;
  params : param_def list
}

type proc_body = {
  decls : decl list ;
  stmts : stmt list
}

type proc_def = {
  header : proc_header ;
  body : proc_body ;
}

type program = {
  procdefs : proc_def list ;
}

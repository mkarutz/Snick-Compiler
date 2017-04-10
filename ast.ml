(**
 * File: ast.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * Implementation of the Snick abstract syntax tree.
 *)

type identifier = string
type lexeme = string

type type_spec =
  | BoolType
  | FloatType
  | IntType

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

type const = {
  value: value;
  raw: string;
}
and value =
  | Boolean of bool
  | Float of float
  | Integer of int
  | String of string

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
  proc_id : identifier ;
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

(**
 * File: ast.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * Implementation of the Snick abstract syntax tree.
 *)

type identifier = string
type lexeme = string

type dtype =
  | BoolType
  | FloatType
  | IntType
  | StringType
  | UnknownType

type interval = int * int

type decl =
  | VarDecl of dtype * identifier
  | ArrDecl of dtype * identifier * interval list

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

(* Constants carry their raw lexeme. *)
type const = {
  value: value;
  raw: string;
}
and value =
  | Boolean of bool
  | Float of float
  | Integer of int
  | String of string

type expr = {
  expr : expr';
  mutable inferred_type : dtype;
}
and expr' =
  | ConstExpr of const
  | LvalueExpr of lvalue
  | BinopExpr of expr * binop * expr
  | UnopExpr of unop * expr
and lvalue =
  | Id of identifier
  | ArrAccess of identifier * expr list

type stmt =
  | AtomStmt of atom_stmt
  | CompStmt of comp_stmt
and atom_stmt =
  | Assign of lvalue * expr
  | Read of lvalue
  | Write of expr
  | Call of identifier * expr list
and comp_stmt =
  | IfThenElse of expr * stmt list * stmt list
  | IfThen of expr * stmt list
  | While of expr * stmt list

type ref_spec =
  | Val
  | Ref

type param_def = {
  mode : ref_spec ;
  dtype : dtype ;
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

type t = program 

open Ast
open Format

let print_const const =
	match const with
	| BoolConst x -> print_bool x
	| FloatConst x -> print_float x
	| IntConst x -> print_int x
	| StringConst x -> print_string x

let print_binop binop =
  match binop with
	| OrBinop -> print_string "or"
	| AndBinop -> print_string "and"
	| EqBinop -> print_string "="
	| NeBinop -> print_string "!="
	| LtBinop -> print_string "<"
	| GtBinop -> print_string ">"
	| LteBinop -> print_string "<="
	| GteBinop -> print_string ">="
	| AddBinop -> print_string "+"
	| SubBinop -> print_string "-"
	| MulBinop -> print_string "*"
	| DivBinop -> print_string "/"

let print_unop unop =
	match unop with
	| NotUnop -> print_string "not"
  | MinusUnop -> print_string "-"

let rec print_indent n =
	if n <= 0 then ()
	else begin
		print_string "    " ;
		print_indent (n-1)
	end

let unop_precedence unop =
	match unop with
	| MinusUnop -> 7
	| NotUnop -> 3

let binop_precedence binop =
	match binop with
	| OrBinop -> 1
	| AndBinop -> 2
	| EqBinop -> 4
	| NeBinop -> 4
	| LtBinop -> 4
	| GtBinop -> 4
	| LteBinop -> 4
	| GteBinop -> 4
	| AddBinop -> 5
	| SubBinop -> 5
	| MulBinop -> 6
	| DivBinop -> 6

let rec print_expr expr =
	match expr with
	| ConstExpr const ->
		print_const const
	| LvalueExpr lvalue ->
		print_lvalue lvalue
	| BinopExpr (lhs, binop, rhs) ->
		print_lhs binop lhs ;
		print_string " " ;
		print_binop binop ;
		print_string " " ;
		print_rhs binop rhs ;
	| UnopExpr (unop, expr) ->
		print_unop unop ;
		print_string " " ;
		print_expr expr

and print_lhs parent_binop lhs =
	match lhs with
	| ConstExpr _ ->
		print_expr lhs
	| LvalueExpr _ ->
		print_expr lhs
	| BinopExpr (_, binop, _) ->
		if binop_precedence(binop) < binop_precedence(parent_binop) then
		begin
			print_char '(' ;
			print_expr lhs ;
			print_char ')'
		end
		else print_expr lhs
	| UnopExpr (unop, _) ->
		if unop_precedence(unop) < binop_precedence(parent_binop) then
		begin
			print_char '(' ;
			print_expr lhs ;
			print_char ')'
		end
		else print_expr lhs

and print_rhs parent_binop rhs =
	match rhs with
	| ConstExpr _ ->
		print_expr rhs
	| LvalueExpr _ ->
		print_expr rhs
	| BinopExpr (_, binop, _) ->
		if binop_precedence(binop) <= binop_precedence(parent_binop) then
		begin
			print_char '(' ;
			print_expr rhs ;
			print_char ')'
		end
		else print_expr rhs
	| UnopExpr (unop, _) ->
		print_expr rhs

and print_lvalue lvalue =
	match lvalue with
	| Id id ->
		print_string id
	| ArrAccess (id, exprs) ->
		print_string id ;
		print_string "[" ;
		print_exprlist exprs ;
		print_string "]"

and print_exprlist exprs =
	match exprs with
	| [] -> ()
	| x::[] ->
		print_expr x
	| x::xs ->
		print_expr x ;
		print_string ", " ;
		print_exprlist xs

let rec print_stmts stmts indent_lvl =
	match stmts with
	| [] -> ()
	| x::xs ->
		print_stmt x indent_lvl;
		print_stmts xs indent_lvl

and print_stmt stmt indent_lvl =
	match stmt with
	| AtomStmt atom ->
		print_indent indent_lvl ;
		print_atom_stmt atom ;
		print_string ";" ;
		print_newline ()
	| CompStmt comp ->
		print_comp_stmt comp indent_lvl ;
		print_newline ()

and print_atom_stmt stmt =
  match stmt with
	| Assign (lvalue, expr) ->
		print_lvalue lvalue ;
		print_string " := " ;
		print_expr expr ;
  | Read lvalue ->
		print_string "read " ;
		print_lvalue lvalue ;
  | Write expr ->
		print_string "write " ;
		print_expr expr ;
	| Call (id, exprs) ->
		print_string id ;
		print_string " (" ;
		print_exprlist exprs ;
		print_string ")"

and print_comp_stmt stmt indent_lvl =
	match stmt with
	| IfThenElse (expr, a, b) ->
		print_indent indent_lvl ;
		print_string "if " ;
		print_expr expr ;
		print_string " then" ;
		print_newline () ;
		print_stmts a (indent_lvl + 1) ;
		print_indent indent_lvl ;
		print_string "else" ;
		print_newline () ;
		print_stmts b (indent_lvl + 1) ;
		print_indent indent_lvl ;
		print_string "fi" ;
	| IfThen (expr, stmts) ->
		print_indent indent_lvl ;
		print_string "if " ;
		print_expr expr ;
		print_string " then" ;
		print_newline () ;
		print_stmts stmts (indent_lvl + 1) ;
		print_indent indent_lvl ;
		print_string "fi" ;
	| While (expr, stmts) ->
		print_indent indent_lvl ;
		print_string "while " ;
		print_expr expr ;
		print_string " do" ;
		print_newline () ;
		print_stmts stmts (indent_lvl + 1) ;
		print_indent indent_lvl ;
		print_string "od"

let print_id id =
  print_string id

let print_dtype dtype =
	match dtype with
	| Bool -> print_string "bool"
	| Float -> print_string "float"
	| Int -> print_string "int"

let print_intervals intervals =
	print_string "[]"

let print_decl decl =
	print_indent 1 ;
	match decl with
	| VarDecl (dtype, id) ->
		print_dtype dtype ;
		print_string " " ;
		print_id id
	| ArrDecl (dtype, id, intervals) ->
		print_dtype dtype ;
		print_string " " ;
		print_id id ;
		print_intervals intervals

let rec print_decls decls =
  match decls with
	| [] -> ()
	| x::[] ->
		print_decl x ;
		print_string ";" ;
		print_newline () ;
		print_newline () ;
	| x::xs ->
		print_decl x ;
		print_string ";" ;
		print_newline () ;
		print_decls xs

let print_procbody body =
	print_decls body.decls ;
	print_stmts body.stmts 1

let print_parammode mode =
	match mode with
	| Val -> print_string "val"
	| Ref -> print_string "ref"

let print_param param =
	print_parammode param.mode ;
	print_string " " ;
	print_dtype param.dtype ;
	print_string " " ;
	print_id param.id

let rec print_paramslist params =
	match params with
	| [] -> ()
	| x::[] ->
	  print_param x
	| x::xs ->
		print_param x ;
		print_string ", " ;
		print_paramslist xs

let print_procparams params =
	print_string "(" ;
	print_paramslist params ;
	print_string ")"

let print_procheader header =
	print_string "proc " ;
	print_string header.id ;
	print_string " " ;
	print_procparams header.params

let print_proc proc =
	print_procheader proc.header ;
	print_newline () ;
	print_procbody proc.body ;
	print_string "end" ;
	print_newline ()

let rec print_procs procs =
	match procs with
	| [] -> ()
	| x::[] ->
		print_proc x
	| x::xs ->
		print_proc x ;
		print_newline () ;
		print_procs xs

let print_program fmt prog = print_procs (prog.procdefs)

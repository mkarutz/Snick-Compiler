open Ast
open Symbol

let rec assert_type dtype valid_types =
  match valid_types with
  | [] -> failwith "Invalid type."
  | t::ts ->
    if dtype = t then () else assert_type dtype ts

let rec analyse prog =
  build_symtbls prog ;
  check_main_exists () ;
  check_procs prog.procdefs

and check_main_exists () =
  let binding = lookup_proc "main" in
  match binding with
  | Procedure proc ->
    if proc.num_params == 0 then ()
    else failwith "Main procedure must be parameterless."
  | UnboundProc ->
    failwith "No main procedure definition."

and check_procs procs =
  check_procs' procs

and check_procs' procs =
  match procs with
  | [] -> ()
  | curr::rest ->
    check_proc curr ;
    check_procs rest

and check_proc proc =
  check_header proc.header ;
  let proc_id = proc.header.proc_id in
  check_body proc.body proc_id

and check_header header = () 
  (* TODO: Check proc header. 
     - all arguments are distinct *)

and check_body body proc_id =
  check_decls body.decls proc_id ;
  check_stmts body.stmts proc_id
  
and check_decls decls proc_id =
  match decls with
  | [] -> ()
  | curr::rest ->
    check_decl curr proc_id ;
    check_decls rest proc_id
    
and check_decl decl proc_id = 
  () (* TODO: Check declarations. *)

and check_stmts stmts proc_id =
  match stmts with
  | [] -> ()
  | curr::rest ->
    check_stmt curr proc_id ;
    check_stmts rest proc_id

and check_stmt stmt proc_id =
  match stmt with
  | AtomStmt atom_stmt ->
    check_atom_stmt atom_stmt proc_id
  | CompStmt comp_stmt ->
    check_comp_stmt comp_stmt proc_id

and check_atom_stmt stmt proc_id =
  match stmt with
  | Assign (lvalue, expr) ->
    let t1 = check_expr expr proc_id in
    let t2 = check_lvalue lvalue proc_id in
    check_assign t1 t2
  | Read lvalue -> 
    let _ = check_lvalue lvalue proc_id in ()
  | Write expr -> 
    let _ = check_expr expr proc_id in ()
  | Call (id, exprs) -> 
    let _ = check_exprs exprs proc_id in ()

and check_exprs exprs proc_id =
  match exprs with
  | [] -> ()
  | curr::rest ->
    let _ = check_expr curr proc_id in
    check_exprs rest proc_id

and check_assign t1 t2 = 
  match t1, t2 with
  | IntType, FloatType
  | FloatType, IntType -> ()
  | _, _->
    if t1 != t2 
    then failwith "Incompatible types"

and check_comp_stmt stmt proc_id =
  check_guard stmt proc_id ;
  check_substmts stmt proc_id
  
and check_guard stmt proc_id = 
  match stmt with
  | IfThenElse (expr, _, _)
  | IfThen (expr, _)
  | While (expr, _) ->
    let t = check_expr expr proc_id in
    assert_type t [ BoolType ]

and check_substmts stmt proc_id =
  match stmt with
  | IfThen (_, stmts)
  | While (_, stmts) ->
    check_stmts stmts proc_id
  | IfThenElse (_, a, b) ->
    check_stmts a proc_id;
    check_stmts b proc_id

and check_expr expr proc_id =
  match expr.expr with
  | ConstExpr const ->
    let t = check_const const in
    expr.inferred_type <- t ; t
  | LvalueExpr lvalue ->
    let t = check_lvalue lvalue proc_id in
    expr.inferred_type <- t ; t
  | BinopExpr (lhs, binop, rhs) ->
    let ltype = check_expr lhs proc_id in
    let rtype = check_expr rhs proc_id in
    let t = check_binop binop ltype rtype in
    expr.inferred_type <- t ; t
  | UnopExpr (unop, arg) ->
    let argtype = check_expr arg proc_id in
    let t = check_unop unop argtype in
    expr.inferred_type <- t ; t
    
and check_unop unop argtype = 
  match argtype with
  | IntType | FloatType ->
    begin match unop with
    | MinusUnop -> argtype
    | NotUnop -> failwith "Incompatible type"
    end
  | BoolType ->
    begin match unop with
    | NotUnop -> BoolType
    | MinusUnop -> failwith "Incompatible type"
    end
  | _ -> 
    failwith "Incompatible type"

and check_const const =
  match const.value with 
  | Boolean _ -> BoolType
  | Float _ -> FloatType
  | Integer _ -> IntType
  | String _ -> StringType

and check_lvalue lvalue proc_id =
  match lvalue with
  | Id id ->
    check_id_expr id proc_id
  | ArrAccess (id, exprs) ->
    check_arr_expr id exprs proc_id
    
and check_arr_expr id exprs proc_id =
  let _ = check_exprs exprs proc_id in
  let binding = lookup_var proc_id id in
  match binding with
  | Array (dtype, _, _)->
    dtype
  | ScalarVal _
  | ScalarRef _
  | UnboundVar -> 
    failwith (Printf.sprintf "Use of undeclared variable: %s" id)
    
and check_id_expr id proc_id = 
  let binding = lookup_var proc_id id in
  match binding with
  | ScalarVal (dtype, _)
  | ScalarRef (dtype, _) ->
    dtype
  | _ -> 
    failwith (Printf.sprintf "Use of undeclared variable: %s" id)

and check_binop binop ltype rtype =
  match binop with
  | OrBinop
  | AndBinop -> 
    check_logical_binop ltype rtype
  | EqBinop
  | NeBinop ->
    check_equality_binop ltype rtype
  | LtBinop
  | GtBinop
  | LteBinop
  | GteBinop ->
    check_relational_binop ltype rtype
  | AddBinop
  | SubBinop
  | MulBinop
  | DivBinop ->
    check_arithmetic_binop ltype rtype

and check_logical_binop ltype rtype =
  assert_type ltype [ BoolType ] ;
  assert_type rtype [ BoolType ] ;
  BoolType

and check_equality_binop ltype rtype =
  if ltype = rtype 
  then BoolType 
  else failwith "Incompatible types"

and check_relational_binop ltype rtype =
  match ltype, rtype with
  | IntType, IntType
  | IntType, FloatType
  | FloatType, IntType
  | FloatType, FloatType ->
    BoolType
  | _ ->
    failwith "Incompatible types"

and check_arithmetic_binop ltype rtype =
  match ltype, rtype with
  | IntType, IntType ->
    IntType
  | IntType, FloatType
  | FloatType, IntType
  | FloatType, FloatType ->
    FloatType
  | _ ->
    failwith "Incompatible types"

open Ast
open Symbol

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
  match decl with
  | VarDecl (dtype, id) -> ()
  | ArrDecl (dtype, id, intervals) ->
    check_intervals intervals

and check_intervals intervals =
  match intervals with
  | [] -> ()
  | curr::rest ->
    let m,n = curr in
    if m > n
    then failwith "Lower bound of interval must be less than or equal to upper bound."
    else check_intervals rest

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
    let ltype = check_lvalue lvalue proc_id in
    let rtype = check_expr expr proc_id in
    check_assign_types ltype rtype
  | Read lvalue -> 
    let _ = check_lvalue lvalue proc_id in ()
  | Write expr -> 
    let _ = check_expr expr proc_id in ()
  | Call (id, exprs) -> 
    let _ = check_exprs exprs proc_id in
    check_call_stmt id exprs proc_id

and check_call_stmt id exprs proc_id =
  let binding = lookup_proc id in
  match binding with
  | Procedure proc ->
    let params = proc.params in
    check_args exprs params proc_id
  | UnboundProc ->
    failwith (Printf.sprintf "Call to undeclared procedure with id %s." id)

and check_args exprs params proc_id =
  match exprs, params with
  | [], [] -> ()
  | _, []
  | [], _ ->
    failwith "Number of actual parameters does not match expected number of arguments."
  | curr::rest, curr'::rest' ->
    check_arg curr curr' proc_id; 
    check_args rest rest' proc_id

and check_arg expr param proc_id =
  let _ = check_expr expr proc_id in
  match param.mode with
  | Val ->
    let paramtype = param.dtype in
    let exprtype = expr.inferred_type in
    check_parameter_types paramtype exprtype
  | Ref ->
    match expr.expr with
    | LvalueExpr lvalue ->
      begin match lvalue with
      | Id id ->
        let paramtype = param.dtype in
        let exprtype = expr.inferred_type in
        if paramtype != exprtype
        then failwith "Type mismatch passing by reference."
      | ArrAccess _ ->
        failwith "Cannot pass arrays by reference"
      end
    | _ ->
      failwith "Cannot pass rvalue expression by reference."

and check_exprs exprs proc_id =
  match exprs with
  | [] -> ()
  | curr::rest ->
    let _ = check_expr curr proc_id in
    check_exprs rest proc_id

and check_parameter_types expected actual = 
  match expected, actual with
  | FloatType, IntType -> ()
  | _, _->
    if expected != actual 
    then failwith "Incompatible types in procedure call statement."

and check_assign_types ltype rtype = 
  match ltype, rtype with
  | FloatType, IntType -> ()
  | _, _->
    if ltype != rtype 
    then failwith "Incompatible types in assignment statement."

and check_comp_stmt stmt proc_id =
  check_guard stmt proc_id ;
  check_substmts stmt proc_id
  
and check_guard stmt proc_id = 
  match stmt with
  | IfThenElse (expr, _, _)
  | IfThen (expr, _)
  | While (expr, _) ->
    let t = check_expr expr proc_id in
    if t != BoolType
    then failwith "Guard expression in if and while statements must have type bool."

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
  match unop with
  | MinusUnop ->
    begin match argtype with
    | IntType 
    | FloatType -> 
      argtype
    | _ -> 
      failwith "Argument to unary minus expression must be number type."
    end
  | NotUnop ->
    begin match argtype with
    | BoolType -> 
      BoolType
    | _ -> 
      failwith "Argument to negation expression must be of bool type."
    end

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
  | Array (dtype, _, intervals) ->
    check_indices exprs intervals proc_id ;
    dtype
  | ScalarVal _
  | ScalarRef _
  | UnboundVar -> 
    failwith (Printf.sprintf "Use of undeclared variable: %s" id)
  
and check_indices exprs intervals proc_id =
  match exprs, intervals with
  | [], [] -> ()
  | _, [] | [], _ ->
    failwith "Wrong number of indices in array subscript expression."
  | curr::rest, _::rest' ->
    let t = curr.inferred_type in
    if t == IntType
    then check_indices rest rest' proc_id
    else failwith "Expressions in array subscripts must have type int."

and check_id_expr id proc_id = 
  let binding = lookup_var proc_id id in
  match binding with
  | ScalarVal (dtype, _)
  | ScalarRef (dtype, _) ->
    dtype
  | Array _ ->
    failwith (Printf.sprintf "Expected subscript following array identifier: %s" id)
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
  if ltype != BoolType || rtype != BoolType
  then failwith "Arguments of logical operators must have type bool."
  else BoolType

and check_equality_binop ltype rtype =
  if ltype = rtype 
  then BoolType 
  else failwith "Arguments of equality operators must have the same type."

and check_relational_binop ltype rtype =
  match ltype, rtype with
  | IntType, IntType
  | IntType, FloatType
  | FloatType, IntType
  | FloatType, FloatType ->
    BoolType
  | _ ->
    failwith "Incompatible types in relational expression."

and check_arithmetic_binop ltype rtype =
  match ltype, rtype with
  | IntType, IntType ->
    IntType
  | IntType, FloatType
  | FloatType, IntType
  | FloatType, FloatType ->
    FloatType
  | _ ->
    failwith "Incompatible types in arithmetic expression."

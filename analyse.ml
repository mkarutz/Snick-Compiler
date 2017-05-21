(**
 * File: analyse.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * This module provides services for static analysis of a Snick AST.
 *
 * Static analysis is performed by a set of mutually-recursive functions for 
 * traversing the Snick AST. The functions are modelled using an L-attributed 
 * attribute grammar. Each function takes the inherited attributes it requires 
 * in as parameters, and provides synthesized attributes as return values.
 *)

open Ast
open Symbol

exception SemanticError of string

let error msg =
  SemanticError msg |> raise

(** 
 * Traverses a Snick AST and performs semantic analysis checks.
 *
 * <p>Throws an exception if any semantic errors are detected.
 *)
let rec analyse prog =
  build_symtbls prog ;
  check_main_exists () ;
  check_procs prog.procdefs

(** Checks that a parameterless main procedure is defined. *)
and check_main_exists () =
  let binding = lookup_proc "main" in
  match binding with
  | Procedure proc ->
    if proc.num_params == 0 then ()
    else error "Main procedure must be parameterless."
  | UnboundProc ->
    error "No main procedure definition."

and check_procs procs =
  match procs with
  | [] -> ()
  | curr::rest ->
    check_proc curr ;
    check_procs rest

and check_proc proc =
  let proc_id = proc.header.proc_id in
  let body = proc.body in
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

(** Checks that for each interval, m..n, m <= n *)
and check_intervals intervals =
  match intervals with
  | [] -> ()
  | curr::rest ->
    let m,n = curr in
    if m > n
    then error "Lower bound of interval must be <= upper bound."
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

and check_assign_types ltype rtype = 
  match ltype, rtype with
  | FloatType, IntType -> ()
  | _, _->
    if ltype != rtype 
    then error "Incompatible types in assignment statement."

(**
 * Checks a procdure call statement. Checks that:
 * <ul>
 *   <li>The identifier is bound to a procedure definition in the symbol table.
 *   <li>The number of actual parameters is equal to the declared number of 
 * formal paramters.
 *   <li>Each actual parameter is a valid expression and has a valid type.
 * </ul>
 *)
and check_call_stmt id exprs proc_id =
  let binding = lookup_proc id in
  match binding with
  | Procedure proc ->
    let params = proc.params in
    check_args exprs params proc_id
  | UnboundProc ->
    Printf.sprintf "Call to undeclared procedure with id %s." id |> error

and check_args exprs params proc_id =
  match exprs, params with
  | [], [] -> ()
  | _, []
  | [], _ ->
    error "Wrong number of actual parameters."
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
        then error "Type mismatch passing by reference."
      | ArrAccess _ ->
        error "Cannot pass arrays by reference"
      end
    | _ ->
      error "Cannot pass rvalue expression by reference."

and check_parameter_types expected actual = 
  match expected, actual with
  | FloatType, IntType -> ()
  | _, _->
    if expected != actual 
    then error "Incompatible types in procedure call statement."

(**
 * Checks if-then, if-then-else, and while statements. Checks that the guard
 * expression has type bool, then recursively checks sub-statements.
 *)
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
    then error "Conditions must have type bool."

and check_substmts stmt proc_id =
  match stmt with
  | IfThen (_, stmts)
  | While (_, stmts) ->
    check_stmts stmts proc_id
  | IfThenElse (_, a, b) ->
    check_stmts a proc_id;
    check_stmts b proc_id

(**
 * Checks an expression for semantic errors and performs type inference.
 *
 * <p>Annotates the AST node with the inferred type.
 *)
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

(** Helper for checking a list of expression *)
and check_exprs exprs proc_id =
  match exprs with
  | [] -> ()
  | curr::rest ->
    let _ = check_expr curr proc_id in
    check_exprs rest proc_id

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

(** 
 * Checks that the identifier is bound to a local scalar variable in the current 
 * scope. 
 *)
and check_id_expr id proc_id = 
  let binding = lookup_var proc_id id in
  match binding with
  | ScalarVal (dtype, _)
  | ScalarRef (dtype, _) ->
    dtype
  | Array _ ->
    Printf.sprintf "Expected subscript following identifier: %s" id |> error
  | _ -> 
    Printf.sprintf "Use of undeclared variable: %s" id |> error

(** 
 * Checks an array access expression. Checks that:
 * <ul>
 *  <li>The identifier is bound to a local array in the current scope.
 *  <li>The number of indices in the subscript list is equal to the number of
 * dimensions.
 *  <li>Each index expression has type int.
 * </ul>
 *)
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
    Printf.sprintf "Use of undeclared variable: %s" id |> error
  
and check_indices exprs intervals proc_id =
  match exprs, intervals with
  | [], [] -> ()
  | _, [] | [], _ ->
    error "Wrong number of indices in array subscript expression."
  | curr::rest, _::rest' ->
    let t = curr.inferred_type in
    if t == IntType
    then check_indices rest rest' proc_id
    else error "Expressions in array subscripts must have type int."

(** 
 * Checks a binary operator expression.
 * <ul>
 *  <li>The identifier is bound to a local array in the current scope.
 *  <li>The number of indices in the subscript list is equal to the number of
 * dimensions.
 *  <li>Each index expression has type int.
 * </ul>
 *)
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
  then error "Arguments of logical operators must have type bool."
  else BoolType

and check_equality_binop ltype rtype =
  if ltype = rtype 
  then BoolType 
  else error "Arguments of equality operators must have the same type."

and check_relational_binop ltype rtype =
  match ltype, rtype with
  | IntType, IntType
  | IntType, FloatType
  | FloatType, IntType
  | FloatType, FloatType ->
    BoolType
  | _ ->
    error "Incompatible types in relational expression."

and check_arithmetic_binop ltype rtype =
  match ltype, rtype with
  | IntType, IntType ->
    IntType
  | IntType, FloatType
  | FloatType, IntType
  | FloatType, FloatType ->
    FloatType
  | _ ->
    error "Incompatible types in arithmetic expression."

(** Checks a unary operator expression *)
and check_unop unop argtype = 
  match unop with
  | MinusUnop ->
    begin match argtype with
    | IntType 
    | FloatType -> 
      argtype
    | _ -> 
      error "Argument to unary minus expression must be number type."
    end
  | NotUnop ->
    begin match argtype with
    | BoolType -> 
      BoolType
    | _ -> 
      error "Argument to negation expression must be of bool type."
    end

(**
 * File: translate.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * This module provides services for translating a Snick abstract-syntax tree
 * into a Brill program.
 *)

open Ast
open Brill
open Symbol

module AST = Ast
module IR = Brill

let rec translate prog = 
  let prelude = [ Call "main"; Halt ] in
  prelude @ trans_procs prog.procdefs

and trans_procs procs =
  match procs with
  | [] -> []
  | curr::rest ->
    trans_proc curr @ trans_procs rest

and trans_proc proc =
  let proc_id = proc.header.proc_id in
  let stack_slots = get_stack_slots proc in
  let prologue = proc_prologue proc_id stack_slots in
  let epilogue = proc_epilogue stack_slots in
  let decls = trans_decls proc.body.decls in
  let stmts = trans_stmts proc.body.stmts proc.header.proc_id in
  prologue @ decls @ stmts @ epilogue

and get_stack_slots proc =
  let proc_id = proc.header.proc_id in
  let binding = lookup_proc proc_id in
  match binding with
  | Procedure proc -> 
    proc.stack_slots
  | UnboundProc ->
    failwith "Fatal error"

and proc_prologue proc_id stack_slots =
  [ Label proc_id ; PushStackFrame stack_slots ]

and proc_epilogue stack_slots = 
  [ PopStackFrame stack_slots ; Return ]

and trans_decls decls = []

and trans_decl decl = []

and trans_stmts stmts proc_id =
  match stmts with
  | [] -> []
  | curr::rest ->
    trans_stmt curr proc_id @ trans_stmts rest proc_id

and trans_stmt stmt proc_id =
  match stmt with
  | AtomStmt atom_stmt ->
    trans_atom_stmt atom_stmt proc_id
  | CompStmt comp_stmt ->
    trans_comp_stmt comp_stmt proc_id
    
and trans_atom_stmt stmt proc_id =
  match stmt with
  | Assign (lvalue, expr) ->
    let reg = 0 in
    let id = get_id lvalue in
    let t = lookup_type proc_id id in
    trans_expr expr proc_id reg @
    trans_cast t expr.inferred_type reg @
    trans_store lvalue proc_id reg
  | Read lvalue -> 
    (* Infer type of lvalue *)
    let id = Ast.get_id lvalue in
    let t = Symbol.lookup_type proc_id id in
    (* Read value into r0 *)
    trans_read_stmt t @
    (* Store value into lvalue *)
    trans_store lvalue proc_id 0
  | Write expr ->
    let reg = 0 in
    trans_expr expr proc_id reg @
    trans_write_stmt expr.inferred_type
  | Call (id, exprs) -> []

and trans_store lvalue proc_id reg =
  let id = Ast.get_id lvalue in
  let binding = Symbol.lookup_var proc_id id in
  match binding with
  | ScalarVal (_, slotnum) ->
    [ Store (slotnum, reg) ]
  | ScalarRef (_, slotnum) ->
    let reg' = reg + 1 in
    [ LoadAddress (reg', slotnum) ; StoreIndirect (reg', reg) ]
  | Array
  | UnboundVar -> []

and trans_read_stmt t =
  match t with
  | BoolType ->
    [ CallBuiltin ReadBool ]
  | IntType ->
    [ CallBuiltin ReadInt ]
  | FloatType ->
    [ CallBuiltin ReadReal ]
  | StringType
  | UnknownType ->
    failwith "Fatal error."

and trans_write_stmt t =
  match t with
  | BoolType ->
    [ CallBuiltin PrintBool ]
  | IntType ->
    [ CallBuiltin PrintInt ]
  | FloatType ->
    [ CallBuiltin PrintReal ]
  | StringType ->
    [ CallBuiltin PrintString ]
  | UnknownType ->
    failwith "Fatal error."

and trans_comp_stmt stmt proc_id = []

and trans_exprs exprs = []

and trans_expr expr proc_id place =
  match expr.expr with
  | ConstExpr const ->
    trans_const_expr const place
  | LvalueExpr lvalue ->
    trans_load lvalue proc_id place
  | BinopExpr (lhs, binop, rhs) ->
    trans_expr lhs proc_id place @
    trans_expr rhs proc_id (place + 1) @
    trans_cast expr.inferred_type lhs.inferred_type place @
    trans_cast expr.inferred_type rhs.inferred_type (place + 1) @
    trans_binop expr.inferred_type binop place place (place + 1)
  | UnopExpr (unop, operand) -> []

and trans_load lvalue proc_id reg =
  let id = Ast.get_id lvalue in
  let binding = Symbol.lookup_var proc_id id in
  match binding with
  | ScalarVal (_, slotnum) ->
    [ Load (reg, slotnum) ]
  | ScalarRef (_, slotnum) ->
    [ LoadAddress (reg, slotnum) ; LoadIndirect (reg, reg) ]
  | Array
  | UnboundVar -> []

and trans_cast expr_type arg_type place =
  if expr_type == FloatType && arg_type == IntType
  then [ IntToReal (place, place) ]
  else []

and trans_binop t binop dest lhs rhs =
  match binop with
  | OrBinop ->
    trans_logical_or dest lhs rhs
  | AndBinop ->
    trans_logical_and dest lhs rhs
  | EqBinop ->
    trans_eq t dest lhs rhs
  | NeBinop ->
    trans_ne t dest lhs rhs
  | LtBinop -> 
    trans_lt t dest lhs rhs
  | GtBinop ->
    trans_gt t dest lhs rhs
  | LteBinop -> 
    trans_le t dest lhs rhs
  | GteBinop ->
    trans_ge t dest lhs rhs
  | AddBinop ->
    trans_add t dest lhs rhs
  | SubBinop ->
    trans_sub t dest lhs rhs
  | MulBinop -> 
    trans_mul t dest lhs rhs
  | DivBinop ->
    trans_div t dest lhs rhs
    
and trans_logical_or dest lhs rhs =
  [ Or (dest, lhs, rhs) ]
  
and trans_logical_and dest lhs rhs =
  [ And (dest, lhs, rhs) ]
  
and trans_eq t dest lhs rhs =
  match t with
  | IntType ->
    [ CmpEqInt (dest, lhs, rhs) ]
  | FloatType ->
    [ CmpEqInt (dest, lhs, rhs) ]
  | BoolType ->
    [ CmpEqInt (dest, lhs, rhs) ]
  | _ -> 
    failwith "Error."

and trans_ne t dest lhs rhs =
  match t with
  | IntType ->
    [ CmpNeInt (dest, lhs, rhs) ]
  | FloatType ->
    [ CmpNeInt (dest, lhs, rhs) ]
  | BoolType ->
    [ CmpNeInt (dest, lhs, rhs) ]
  | _ -> 
    failwith "Error."
    
and trans_lt t dest lhs rhs =
  match t with
  | IntType ->
    [ CmpLtInt (dest, lhs, rhs) ]
  | FloatType ->
    [ CmpLtInt (dest, lhs, rhs) ]
  | _ -> 
    failwith "Error."

and trans_le t dest lhs rhs =
  match t with
  | IntType ->
    [ CmpLeInt (dest, lhs, rhs) ]
  | FloatType ->
    [ CmpLeInt (dest, lhs, rhs) ]
  | _ -> 
    failwith "Error."
    
and trans_gt t dest lhs rhs =
  match t with
  | IntType ->
    [ CmpGtInt (dest, lhs, rhs) ]
  | FloatType ->
    [ CmpGtInt (dest, lhs, rhs) ]
  | _ -> 
    failwith "Error."

and trans_ge t dest lhs rhs =
  match t with
  | IntType ->
    [ CmpGeInt (dest, lhs, rhs) ]
  | FloatType ->
    [ CmpGeInt (dest, lhs, rhs) ]
  | _ -> 
    failwith "Error."
    
and trans_add t dest lhs rhs =
  match t with
  | IntType ->
    [ AddInt (dest, lhs, rhs) ]
  | FloatType ->
    [ AddReal (dest, lhs, rhs) ]
  | _ -> 
    failwith "Error."
    
and trans_sub t dest lhs rhs =
  match t with
  | IntType ->
    [ SubInt (dest, lhs, rhs) ]
  | FloatType ->
    [ SubReal (dest, lhs, rhs) ]
  | _ -> 
    failwith "Error."
    
and trans_mul t dest lhs rhs =
  match t with
  | IntType ->
    [ MulInt (dest, lhs, rhs) ]
  | FloatType ->
    [ MulReal (dest, lhs, rhs) ]
  | _ -> 
    failwith "Error."
    
and trans_div t dest lhs rhs =
  match t with
  | IntType ->
    [ DivInt (dest, lhs, rhs) ]
  | FloatType ->
    [ DivReal (dest, lhs, rhs) ]
  | _ -> 
    failwith "Error."

and trans_const_expr const place =
  match const.value with
  | Boolean _ ->
    [ IntConst (place, "1") ]
  | Float _ ->
    [ RealConst (place, const.raw) ]
  | Integer _ ->
    [ IntConst (place, const.raw) ]
  | String _ ->
    [ StringConst (place, const.raw) ]

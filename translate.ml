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

let label_counter = ref 0

let new_label () = 
  let label = !label_counter in
  label_counter := !label_counter + 1 ;
  Printf.sprintf "label%d" label

(**
 * Translates a Snick AST to a Brill program.
 *
 * <p>Expects that static analysis has been performed and will fail if any
 * unexpected errors are encountered.
 *)
let rec translate prog = 
  let prelude = [ Call "main" ; Halt ] in
  let epilogue = [ 
    Label "error" ; 
    StringConst (0, "\"Runtime error!\"") ; 
    CallBuiltin PrintString ; 
    Halt 
  ] in
  prelude @ trans_procs prog.procdefs @ epilogue

and trans_procs procs =
  match procs with
  | [] -> []
  | curr::rest ->
    trans_proc curr @ trans_procs rest

(**
 * Translates a Snick procedure into Brill code.
 *)
and trans_proc proc =
  let proc_id = proc.header.proc_id in
  let num_params = get_num_params proc in
  let num_stack_slots = get_num_stack_slots proc in
  let prologue = proc_prologue proc_id num_params num_stack_slots in
  let epilogue = proc_epilogue num_stack_slots in
  let decls = trans_decls proc.body.decls proc.header.proc_id in
  let stmts = trans_stmts proc.body.stmts proc.header.proc_id in
  prologue @ decls @ stmts @ epilogue

and get_num_stack_slots proc =
  let proc_id = proc.header.proc_id in
  let binding = lookup_proc proc_id in
  match binding with
  | Procedure proc -> 
    proc.stack_slots
  | UnboundProc ->
    failwith "Fatal compiler error."
    
and get_num_params proc =
  let proc_id = proc.header.proc_id in
  let binding = lookup_proc proc_id in
  match binding with
  | Procedure proc -> 
    proc.num_params
  | UnboundProc ->
    failwith "Fatal compiler error."

and proc_prologue proc_id num_params stack_slots =
  [ Label proc_id ; PushStackFrame stack_slots ] @
  store_params num_params

and store_params num_params = 
  if num_params == 0 then []
  else Store (num_params - 1, num_params - 1)::store_params (num_params - 1)

and proc_epilogue stack_slots = 
  [ PopStackFrame stack_slots ; Return ]

(*==============*)
(* Declarations *)
(*==============*)

and trans_decls decls proc_id =
  match decls with
  | [] -> []
  | curr::rest ->
    trans_decl curr proc_id @
    trans_decls rest proc_id
    
and trans_decl decl proc_id =
  match decl with
  | VarDecl (dtype, id) ->
    trans_var_decl id dtype proc_id
  | ArrDecl (dtype, id, _) ->
    trans_arr_decl id dtype proc_id
    
and trans_var_decl id dtype proc_id =
  let var = lookup_var proc_id id in
  match var with
  | ScalarVal (_, slotnum) ->
    init_slot slotnum dtype
  | _ ->
    failwith "Fatal compiler error."

and trans_arr_decl id dtype proc_id =
  let binding = lookup_var proc_id id in
  match binding with
  | Array (_, slotnum, intervals) ->
    let size = intervals_size intervals in
    init_slots slotnum size dtype
  | _ ->
    failwith "Fatal compiler error."
    
and init_slots slotnum size dtype =
  if size == 0 then 
    []
  else 
    init_slot slotnum dtype @
    init_slots (slotnum + 1) (size - 1) dtype
  
and init_slot slotnum dtype =
  let reg = 0 in
  match dtype with
  | IntType
  | BoolType ->
    [ IntConst (reg, "0") ; Store (slotnum, reg) ]
  | FloatType ->
    [ RealConst (reg, "0.0") ; Store (slotnum, reg) ]
  | _ ->
    failwith "Fatal compiler error."

(*============*)
(* Statements *)
(*============*)

and trans_stmt stmt proc_id =
  match stmt with
  | AtomStmt atom_stmt ->
    trans_atom_stmt atom_stmt proc_id
  | CompStmt comp_stmt ->
    trans_comp_stmt comp_stmt proc_id

and trans_stmts stmts proc_id =
  match stmts with
  | [] -> []
  | curr::rest ->
    trans_stmt curr proc_id @ trans_stmts rest proc_id
    
and trans_atom_stmt stmt proc_id =
  match stmt with
  | Assign (lvalue, expr) ->
    let reg = 0 in
    let id = get_id lvalue in
    let t = lookup_type proc_id id in
    trans_expr expr proc_id reg @
    trans_maybe_type_cast t expr.inferred_type reg @
    trans_store lvalue proc_id reg
  | Read lvalue -> 
    let id = Ast.get_id lvalue in
    let t = Symbol.lookup_type proc_id id in
    let reg = 0 in
    trans_read_stmt t @
    trans_store lvalue proc_id reg
  | Write expr ->
    let reg = 0 in
    trans_expr expr proc_id reg @
    trans_write_stmt expr.inferred_type
  | Call (label, exprs) ->
    let params = get_params label in
    trans_args exprs proc_id params 0 @
    [ Call label ]
    
and get_params proc_id = 
  let binding = lookup_proc proc_id in
  match binding with
  | Procedure proc -> 
    proc.params
  | UnboundProc ->
    failwith "Fatal compiler error."

and trans_store lvalue proc_id reg =
  match lvalue with
  | Id id ->
    trans_store_scalar id proc_id reg
  | ArrAccess (id, exprs) ->
    trans_store_array id exprs proc_id reg
  
and trans_store_scalar id proc_id reg =
  let binding = Symbol.lookup_var proc_id id in
  match binding with
  | ScalarVal (_, slotnum) ->
    [ Store (slotnum, reg) ]
  | ScalarRef (_, slotnum) ->
    let reg' = reg + 1 in
    [ Load (reg', slotnum) ; StoreIndirect (reg', reg) ]
  | _ ->
    failwith "Fatal compiler error."
    
and trans_store_array id exprs proc_id reg =
  let binding = Symbol.lookup_var proc_id id in
  match binding with
  | Array (_, slotnum, intervals) ->
    let reg' = reg + 1 in
    trans_array_addr slotnum exprs intervals proc_id reg' @
    [ StoreIndirect (reg', reg) ]
  | _ ->
    failwith "Fatal compiler error."

(**
 * Generates brill code for accessing arrays. The code performs the following
 * operations:
 * <ol>
 *   <li>Loads address of first element (greatest address) into a register.
 *   <li>Computes offset.
 *   <li>Checks for out of bound errors at runtime.
 *   <li>Subtracts the offset from the array address.
 * </ol>
 *)
and trans_array_addr slotnum exprs intervals proc_id reg =
  let reg' = reg + 1 in
  [ LoadAddress (reg, slotnum) ] @ 
  trans_array_exprs exprs intervals proc_id reg' @
  array_bounds_check reg' intervals @
  [ SubOffset (reg, reg, reg') ]

(**
 * Generates brill code for computing an array offset.
 *
 * <p>For each expression in the expression list, generates code to:
 * <ol>
 *   <li>Evaluate the expression into a register.
 *   <li>Subtract the interval lower bound to get an offset.
 *   <li>Multiply by the size of the elements in the dimension.
 *   <li>Add the value of the next offset.
 * </ol>
 *)
and trans_array_exprs exprs intervals proc_id reg = 
  match exprs, intervals with
  | [],[] ->
    []
  | curr::[], curr'::[] ->
    (* avoid multiplying by 1 for a 1-D array *)
    trans_expr curr proc_id reg @
    sub_interval_offset curr curr' reg
  | curr::rest, curr'::rest' ->
    let reg' = reg + 1 in
    trans_expr curr proc_id reg @
    sub_interval_offset curr curr' reg @
    mul_intervals_size curr rest' reg @
    trans_array_exprs rest rest' proc_id reg' @
    [ AddInt (reg, reg, reg') ]
  | _ ->
    failwith "Fatal compiler error."

and array_bounds_check reg intervals =
  let size = intervals_size intervals in
  let reg' = reg + 1 in
  let reg'' = reg' + 1 in
  [ 
    IntConst (reg', string_of_int size) ; 
    IntConst (reg'', "0") ;
    CmpGeInt (reg', reg, reg') ; 
    CmpLtInt (reg'', reg, reg'') ; 
    Or (reg', reg', reg'') ;
    BranchOnTrue (reg', "error") 
  ]

and sub_interval_offset expr interval reg =
  let reg' = reg + 1 in
  let (lower, _) = interval in
  [ IntConst (reg', string_of_int lower) ; SubInt (reg, reg, reg') ]
  
and mul_intervals_size expr intervals reg =
  let reg' = reg + 1 in
  let size = intervals_size intervals in
  [ IntConst (reg', string_of_int size) ; MulInt (reg, reg, reg') ]

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
    failwith "Fatal compiler error."

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
    failwith "Fatal compiler error."

and trans_comp_stmt stmt proc_id =
  match stmt with
  | IfThen (expr, stmts) ->
    trans_ifthen expr stmts proc_id
  | IfThenElse (expr, a, b) ->
    trans_ifthenelse expr a b proc_id
  | While (expr, stmts) ->
    trans_while expr stmts proc_id

and trans_ifthenelse expr a b proc_id =
  let mid_label = new_label () in
  let end_label = new_label () in
  trans_expr expr proc_id 0 @
  [ BranchOnFalse (0, mid_label) ] @
  trans_stmts a proc_id @
  [ BranchUncond (end_label) ] @
  [ Label (mid_label) ] @
  trans_stmts b proc_id @
  [ Label (end_label) ]

and trans_ifthen expr stmts proc_id =
  let end_label = new_label () in
  trans_expr expr proc_id 0 @
  [ BranchOnFalse (0, end_label) ] @
  trans_stmts stmts proc_id @
  [ Label (end_label) ]

and trans_while expr stmts proc_id = 
  let guard_label = new_label () in
  let body_label = new_label () in
  [ BranchUncond guard_label ] @
  [ Label (body_label) ] @ 
  trans_stmts stmts proc_id @ 
  [ Label (guard_label) ] @
  trans_expr expr proc_id 0 @
  [ BranchOnTrue (0, body_label) ] 

and trans_args args proc_id params place =
  match args, params with
  | [], [] -> []
  | arg::args, param::params ->
    trans_arg arg proc_id param place @
    trans_args args proc_id params (place + 1)
  | _ -> failwith "Fatal compiler error."
  
and trans_arg arg proc_id param place =
  match param.mode with
  | Val ->
    let t = param.dtype in
    trans_expr arg proc_id place @
    trans_maybe_type_cast t arg.inferred_type place
  | Ref ->
    match arg.expr with
    | LvalueExpr lvalue -> 
      trans_load_address lvalue proc_id place
    | _ -> failwith "Fatal compiler error."

(*=============*)
(* Expressions *)
(*=============*)

and trans_expr expr proc_id place =
  match expr.expr with
  | ConstExpr const ->
    trans_const_expr const place
  | LvalueExpr lvalue ->
    trans_load lvalue proc_id place
  | BinopExpr (lhs, binop, rhs) ->
    let lt = lhs.inferred_type in
    let rt = rhs.inferred_type in
    let t = reconcile_types lt rt in
    let place' = place + 1 in
    trans_expr lhs proc_id place @
    trans_expr rhs proc_id place' @
    maybe_type_cast binop lt rt place @
    maybe_type_cast binop rt lt place' @
    trans_binop binop t place place place'
  | UnopExpr (unop, arg) ->
    match unop with
    | NotUnop ->
      trans_logical_not arg proc_id place
    | MinusUnop ->
      trans_minus arg proc_id place

and reconcile_types t1 t2 =
  match t1, t2 with
  | IntType, FloatType
  | FloatType, IntType ->
    FloatType
  | _ ->
    t1 (* t1 and t2 are equal *)

and trans_load lvalue proc_id reg =
  match lvalue with
  | Id id ->
    trans_scalar_expr id proc_id reg
  | ArrAccess (id, exprs) ->
    trans_array_expr id exprs proc_id reg

and trans_scalar_expr id proc_id reg = 
  let binding = Symbol.lookup_var proc_id id in
  match binding with
  | ScalarVal (_, slotnum) ->
    [ Load (reg, slotnum) ]
  | ScalarRef (_, slotnum) ->
    [ Load (reg, slotnum) ; LoadIndirect (reg, reg) ]
  | Array _
  | UnboundVar ->
    failwith "Fatal compiler error."

and trans_array_expr id exprs proc_id reg =
  let binding = Symbol.lookup_var proc_id id in
  match binding with
  | Array (_, slotnum, intervals) ->
    trans_array_addr slotnum exprs intervals proc_id reg @
    [ LoadIndirect (reg, reg) ]
  | ScalarVal _
  | ScalarRef _
  | UnboundVar ->
    failwith "Fatal compiler error."
  
and trans_load_address lvalue proc_id reg =
  let id = Ast.get_id lvalue in
  let binding = Symbol.lookup_var proc_id id in
  match binding with
  | ScalarVal (_, slotnum) ->
    [ LoadAddress (reg, slotnum) ]
  | ScalarRef (_, slotnum) ->
    [ Load (reg, slotnum) ]
  | Array _
  | UnboundVar -> 
    failwith "Fatal compiler error."

and trans_maybe_type_cast var_t expr_t place =
  match var_t, expr_t with
  | FloatType, IntType ->
    [ IntToReal (place, place) ]
  | _ -> []

and maybe_type_cast binop t t' place =
  match binop with
  | LtBinop
  | GtBinop
  | LteBinop
  | GteBinop
  | AddBinop
  | SubBinop
  | MulBinop
  | DivBinop ->
    if t' == FloatType && t == IntType
    then [ IntToReal (place, place) ]
    else []
  | _ -> []

and trans_binop binop t dest lhs rhs =
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
    [ CmpEqReal (dest, lhs, rhs) ]
  | BoolType ->
    [ CmpEqInt (dest, lhs, rhs) ]
  | _ -> 
    failwith "Fatal compiler error."

and trans_ne t dest lhs rhs =
  match t with
  | IntType ->
    [ CmpNeInt (dest, lhs, rhs) ]
  | FloatType ->
    [ CmpNeReal (dest, lhs, rhs) ]
  | BoolType ->
    [ CmpNeInt (dest, lhs, rhs) ]
  | _ -> 
    failwith "Fatal compiler error."
    
and trans_lt t dest lhs rhs =
  match t with
  | IntType ->
    [ CmpLtInt (dest, lhs, rhs) ]
  | FloatType ->
    [ CmpLtReal (dest, lhs, rhs) ]
  | _ -> 
    failwith "Fatal compiler error."

and trans_le t dest lhs rhs =
  match t with
  | IntType ->
    [ CmpLeInt (dest, lhs, rhs) ]
  | FloatType ->
    [ CmpLeReal (dest, lhs, rhs) ]
  | _ -> 
    failwith "Fatal compiler error."
    
and trans_gt t dest lhs rhs =
  match t with
  | IntType ->
    [ CmpGtInt (dest, lhs, rhs) ]
  | FloatType ->
    [ CmpGtReal (dest, lhs, rhs) ]
  | _ -> 
    failwith "Fatal compiler error."

and trans_ge t dest lhs rhs =
  match t with
  | IntType ->
    [ CmpGeInt (dest, lhs, rhs) ]
  | FloatType ->
    [ CmpGeReal (dest, lhs, rhs) ]
  | _ -> 
    failwith "Fatal compiler error."
    
and trans_add t dest lhs rhs =
  match t with
  | IntType ->
    [ AddInt (dest, lhs, rhs) ]
  | FloatType ->
    [ AddReal (dest, lhs, rhs) ]
  | _ -> 
    failwith "Fatal compiler error."
    
and trans_sub t dest lhs rhs =
  match t with
  | IntType ->
    [ SubInt (dest, lhs, rhs) ]
  | FloatType ->
    [ SubReal (dest, lhs, rhs) ]
  | _ ->
    failwith "Fatal compiler error."
    
and trans_mul t dest lhs rhs =
  match t with
  | IntType ->
    [ MulInt (dest, lhs, rhs) ]
  | FloatType ->
    [ MulReal (dest, lhs, rhs) ]
  | _ -> 
    failwith "Fatal compiler error."
    
and trans_div t dest lhs rhs =
  match t with
  | IntType ->
    [ DivInt (dest, lhs, rhs) ]
  | FloatType ->
    [ DivReal (dest, lhs, rhs) ]
  | _ -> 
    failwith "Fatal compiler error."

and trans_const_expr const place =
  match const.value with
  | Boolean x ->
    [ IntConst (place, if x then "1" else "0") ]
  | Float _ ->
    [ RealConst (place, const.raw) ]
  | Integer _ ->
    [ IntConst (place, const.raw) ]
  | String _ ->
    [ StringConst (place, const.raw) ]

and trans_logical_not arg proc_id place =
  trans_expr arg proc_id place @
  [ Not (place, place) ]
  
and trans_minus arg proc_id place =
  trans_expr arg proc_id place @
  begin
    (* Unary minus is simulated by subtraction from zero. *)
    let t = arg.inferred_type in
    let place' = place + 1 in
    match t with
    | IntType ->
      [ IntConst (place', "0") ; SubInt (place, place', place) ]
    | FloatType ->
      [ RealConst (place', "0.0") ; SubReal (place, place', place) ]
    | _ ->
      failwith "Fatal compiler error."
  end

(* Debugging util *)
let print_type t = 
  match t with
  | IntType ->
    Printf.printf("int\n");
  | FloatType ->
    Printf.printf("float\n");
  | BoolType ->
    Printf.printf("bool\n");
  | UnknownType ->
    Printf.printf("unknown\n")
  | StringType ->
    Printf.printf("string\n")

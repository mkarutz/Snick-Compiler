(**
 * File: translate.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * This module provides services for translating a Snick abstract-syntax tree
 * into a Brill program.
 *)

open Ast
module AST = Ast
module IR = Brill

(* Tranlate expressions. *)
let rec trans_expr expr vtable ftable place =
  match expr with
  | AST.ConstExpr const ->
    trans_const_expr const place
  | _ ->
    (* TODO *)
    ([], AST.IntType)

and trans_const_expr const place =
  (* Since Brill *)
  let raw = const.raw in
  let value = const.value in
  match value with
  | Boolean x ->
    ([ IR.Int_const (place, if x then "1" else "0") ], BoolType)
  | Float _ ->
    ([ IR.Real_const (place, raw) ], FloatType)
  | Integer _ ->
    ([ IR.Int_const (place, raw) ], IntType)
  | String _ ->
    ([ IR.StringConst (place, raw) ], StringType)

and trans_binop_expr lhs binop rhs vtable ftable place =
  let (code1, type1) = trans_expr lhs vtable ftable place in
  let (code2, type2) = trans_expr rhs vtable ftable (place + 1) in
  let inferred_type = infer_type type1 type2 in
  let ca = 
    if type1 != inferred_type 
    then [ IR.Int_to_real (place, place) ] 
    else [] 
  in
  let cb = 
    if type2 != inferred_type 
    then [ IR.Int_to_real (place + 1, place + 1) ] 
    else [] 
  in
  code1 @ code2 @ ca @ cb @ trans_binop binop inferred_type place (place + 1)

and trans_binop op dtype lhs_place rhs_place =
  match op with
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
  | DivBinop -> []

and infer_type t1 t2 =
  match t1 with
  | IR.RealT -> IR.RealT
  | _ ->
  match t2 with
  | IR.RealT -> IR.RealT
  | _ -> IR.IntT

and gen_binop_code binop lhs_type rhs_type place = ([], IR.RealT)
  (* let (cast_code, t) = gen_cast_code  *)
  
(* Translate Statements. *)
let rec trans_stmts stmts =
  List.map trans_stmt stmts
  
and trans_stmt stmt =
  match stmt with
  | AST.AtomStmt atom_stmt -> 
    trans_atom_stmt atom_stmt
  | AST.CompStmt comp_stmt -> 
    trans_comp_stmt comp_stmt

and trans_comp_stmt stmt =
  failwith "Not Implemented."
  
and trans_atom_stmt stmt =
  match stmt with
  | AST.Write expr ->
    []
  | AST.Assign _
  | AST.Read _
  | AST.Call _ -> 
    []


(* Translate declarations. *)
let trans_arr_decl type_spec identifier intervals = ()

let trans_var_decl type_spec identifier = ()

let trans_decl decl old_env =
  match decl with
  | AST.VarDecl (type_spec, identifier) ->
    trans_var_decl type_spec identifier
  | AST.ArrDecl (type_spec, identifier, intervals) ->
    trans_arr_decl type_spec identifier intervals

let trans_prog prog =
  [ IR.Label "proc_main" ; IR.Return ]

let prologue () = [IR.Call "proc_main" ; IR.Halt]

let translate prog = prologue () @ (trans_prog prog)

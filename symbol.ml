(**
 * File: symbol.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * This module provides services for building and querying the global symbol table.
 *
 * Building the symbol table uses a set of mutually-recursive functions for traversing
 * the Snick AST. THe functions are modelled using an L-attributed attribute grammar.
 * Each function takes the inherited attributes it requires in as parameters, and returns
 * provides synthesized attributes as return values.
 *)

open Ast

type dtype = Ast.dtype
type slotnum = int

(* An entry in the variable symbol table. *)
type vbinding =
  | ScalarVal of (dtype * slotnum)
  | ScalarRef of (dtype * slotnum)
  | Array of (dtype * slotnum * interval list)
  | UnboundVar

(* A record type holding the details of a procedure definition. *)
type proc = {
  id : string;
  params : param_def list;
  num_params : int;
  stack_slots : int;
}

(* An entry in the procedure symbol table. *)
type pbinding =
  | Procedure of proc
  | UnboundProc

(** 
 * Hashtables for holding symbol bindings. 
 *
 * <p>Since procedures and variables have separate namespaces, we simply use seperate
 * hashtables.
 *)
let vtable = Hashtbl.create 1024
let ptable = Hashtbl.create 1024

(** Looks-up a variable in the symbol table. *)
let lookup_var proc_id id =
  try Hashtbl.find vtable (proc_id, id)
  with _ -> UnboundVar
  
(** Helper function for retrieving the type of a variable from the symbol table. *)
let lookup_type proc_id id =
  let record = lookup_var proc_id id in
  match record with
  | ScalarVal (dtype, _)
  | ScalarRef (dtype, _)
  | Array (dtype, _, _) ->
    dtype
  | UnboundVar ->
    failwith "error"

(** Lookup a procedure in the symbol table. *)
let lookup_proc proc_id =
  try Hashtbl.find ptable proc_id
  with _ -> UnboundProc

(** 
 * Traverses a Snick AST and builds the global symbol table. 
 *
 * <p>Performs basic checking to ensure that all symbols in shared scope and namespaces
 * are distinct.
 *
 * <p>Throws an exception if any declaration shares an identifier with another symbol
 * in the same scope.
 *)
let rec build_symtbls program =
  index_procs program.procdefs

(** Creates symbol table entries for a list of procedure definitions. *)
and index_procs procs =
  match procs with
  | [] -> ()
  | curr::rest ->
    index_proc curr ;
    index_procs rest

(**
 * Creates a symbol-table entry for a procedure defintion.
 * 
 * <p>First creates symbol-table entries for formal parameters and local variable 
 * declarations, which synthesize attributes about stack allocation.
 *)
and index_proc proc =
  let proc_id = proc.header.proc_id in
  let num_slots = index_params proc.header.params proc_id 0 in
  let num_slots' = index_decls proc.body.decls proc_id num_slots in
  let binding = Procedure {
    id = proc_id;
    params = proc.header.params;
    num_params = List.length proc.header.params;
    stack_slots = num_slots + num_slots';
  } in
  let bindings = Hashtbl.find_all ptable proc_id in
  match bindings with
  | [] ->
    Hashtbl.add ptable proc_id binding
  | _ ->
    failwith (Printf.sprintf "Procedure with name %s redeclared." proc_id)

(** Indexes a list of formal paramters. *)
and index_params params proc_id slots_used =
  match params with
  | [] -> slots_used
  | curr::rest ->
    let slots_used' = index_param curr proc_id slots_used in
    index_params rest proc_id slots_used'

(** 
 * Indexes a formal paramters declaration. 
 *
 * <p>Inherited attributes:
 * <ul>
 *   <li>The name of the enclosing procedure.
 *   <li>The number of stack slots already allocated to left-siblings.
 * </ul>
 *
 * <p>Returns the new number of stack slots used so far.
 *)
and index_param param proc_id slots_used =
  let id = param.id in
  let dtype = param.dtype in
  let slot_num = slots_used in
  let slots_used' = slots_used + 1 in
  let bindings = Hashtbl.find_all ptable proc_id in
  match bindings with
  | [] ->
    begin match param.mode with
    | Val ->
      let binding = ScalarVal (dtype, slot_num) in
      Hashtbl.add vtable (proc_id, id) binding ; slots_used'
    | Ref -> 
      let binding = ScalarRef (dtype, slot_num) in
      Hashtbl.add vtable (proc_id, id) binding ; slots_used'
    end
  | _ ->
    failwith (Printf.sprintf "Parameter with name %s redeclared." proc_id)

(** Indexes a list of local variable declarations. *)
and index_decls decls proc_id slots_used =
  match decls with
  | [] -> slots_used
  | curr::rest ->
    let slots_used' = index_decl curr proc_id slots_used in
    index_decls rest proc_id slots_used'

(** 
 * Creates a symbol table entry for a local variable declarations. 
 *
 * <p>Inherited attributes:
 * <ul>
 *   <li>The name of the enclosing procedure.
 *   <li>The number of stack slots already allocated to left-siblings.
 * </ul>
 *
 * <p>Returns the total number of stack slots allocated to parameters and local variables.
 *)
and index_decl decl proc_id slots_used =
  let bindings = Hashtbl.find_all ptable proc_id in
  match bindings with
  | [] ->
    begin match decl with
    | VarDecl (dtype, id) ->
      let slot_num = slots_used in
      let slots_used' = slots_used + 1 in
      let binding = ScalarVal (dtype, slot_num) in
      Hashtbl.add vtable (proc_id, id) binding ; slots_used'
    | ArrDecl (dtype, id, intervals) ->
      let slot_num = slots_used in
      let slots_used' = slots_used + intervals_size intervals in
      let binding = Array (dtype, slot_num, intervals) in
      Hashtbl.add vtable (proc_id, id) binding ; slots_used'
    end
  | _ ->
    failwith (Printf.sprintf "Local variable with name %s redeclared." proc_id)

(**
 * File: symbol.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *)

open Ast

type dtype = Ast.dtype
type slotnum = int

(* An entry in the variable symbol table. *)
type vbinding =
  | ScalarVal of (dtype * slotnum)
  | ScalarRef of (dtype * slotnum)
  | Array
  | UnboundVar
  
type proc = {
  id : string;
  params : param_def list;
  num_params : int;
  stack_slots : int;
}

type pbinding =
  | Procedure of proc
  | UnboundProc

(* Module-local hashtable *)
let vtable = Hashtbl.create 1024
let ptable = Hashtbl.create 1024

(* Lookup a variable in the symbol table. *)
let lookup_var proc_id id =
  try Hashtbl.find vtable (proc_id, id)
  with _ -> UnboundVar
  
let lookup_type proc_id id =
  let record = lookup_var proc_id id in
  match record with
  | ScalarVal (dtype, _)
  | ScalarRef (dtype, _) ->
    dtype
  | Array ->
    failwith "TODO"
  | UnboundVar ->
    failwith "error"

(* Lookup a procedure in the symbol table. *)
let lookup_proc proc_id =
  try Hashtbl.find ptable proc_id
  with _ -> UnboundProc

(* Traverses a Snick AST and builds the global symbol tables. *)
let rec build_symtbls program =
  index_procs program.procdefs

(* Builds the table binding symbols to procedure information. *)
and index_procs procs =
  match procs with
  | [] -> ()
  | curr::rest ->
    index_proc curr ;
    index_procs rest
    
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
  Hashtbl.add ptable proc_id binding
  
and index_params params proc_id slots_used =
  match params with
  | [] -> slots_used
  | curr::rest ->
    let slots_used' = index_param curr proc_id slots_used in
    index_params rest proc_id slots_used'

and index_param param proc_id slots_used =
  let id = param.id in
  let dtype = param.dtype in
  let slot_num = slots_used in
  let slots_used' = slots_used + 1 in
  match param.mode with
  | Val ->
    let binding = ScalarVal (dtype, slot_num) in
    Hashtbl.add vtable (proc_id, id) binding ; slots_used'
  | Ref -> 
    let binding = ScalarRef (dtype, slot_num) in
    Hashtbl.add vtable (proc_id, id) binding ; slots_used'

and index_decls decls proc_id slots_used =
  match decls with
  | [] -> slots_used
  | curr::rest ->
    let slots_used' = index_decl curr proc_id slots_used in
    index_decls rest proc_id slots_used'

and index_decl decl proc_id slots_used =
  match decl with
  | VarDecl (dtype, id) ->
    let slot_num = slots_used in
    let slots_used' = slots_used + 1 in
    let binding = ScalarVal (dtype, slot_num) in
    Hashtbl.add vtable (proc_id, id) binding ; slots_used'
  | ArrDecl _ -> 
    slots_used
    (* TODO *)

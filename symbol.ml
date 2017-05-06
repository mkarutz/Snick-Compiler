(**
 * File: symbol.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *)

open Ast

type dtype = Ast.type_spec
type slotnum = int

(* An entry in the variable symbol table. *)
type vbinding =
  | ScalarVal of (dtype * slotnum)
  | ScalarRef
  | Array
  | UnboundVar
  
type pbinding =
  | UnboundProc

(* Module-local hashtable *)
let vtable = Hashtbl.create 1024
(* let ptable = Hashtbl.create 1024 *)

(* Lookup a variable in the symbol table. *)
let lookup_var proc_id id =
  try Hashtbl.find vtable (proc_id, id)
  with _ -> UnboundVar

(* Lookup a procedure in the symbol table. *)
(* let lookup_proc proc_id =
  try Hashtbl.find ptable proc_id
  with _ -> UnboundProc *)

(* Traverses a Snick AST and builds the global symbol tables. *)
let rec build_symtbls program =
  build_ptable program.procdefs ;
  build_vtable program.procdefs ;

(* Builds the table binding symbols to procedure information. *)
and build_ptable procs = () (* TODO *)

(* Builds the table binding symbols to variable information. *)
and build_vtable procs =
  match procs with
  | [] -> ()
  | curr::rest ->
    build_scope curr
    
and build_scope proc =
  let header = proc.Ast.header in
  let body = proc.Ast.body in
  let proc_id = header.Ast.proc_id in
  let params = header.Ast.params in
  let decls = body.Ast.decls in
  let place = 1 in
  (* Add symbol table entries for formal parameters. *)
  let place' = add_params_entries params proc_id place in
  (* Add symbol table entries for local decls. *)
  add_decls_entries decls proc_id place'

(** 
 * Adds symbol table entries for formal parameters. 
 * <p>
 * Inherited attributed:
 * <ul>
 *   <li> procedure id
 *   <li> next available stack slot 
 * </ul>
 * <p>
 * Returns the next available stack slot after all the formal parameters.
 *)
and add_params_entries params proc_id place =
  match params with
  | [] -> place
  | curr::rest ->
    let place' = add_param_entry curr proc_id place in
    add_params_entries rest proc_id place'

(* Adds a symbol table entry for a formal parameter. *)
and add_param_entry param proc_id place = 
  place (* TODO *)

(* Adds symbol table entries for local variables. *)
and add_decls_entries decls proc_id place =
  match decls with
  | [] -> ()
  | curr::rest ->
    let place' = add_decl_entry curr proc_id place in
    add_decls_entries rest proc_id place'

(* Adds a symbol table entry for a local variable. *)
and add_decl_entry decl proc_id place =
  match decl with
  | VarDecl (dtype, id) -> 
    let sym = (proc_id, id) in
    let binding = ScalarVal (dtype, place) in
    (* Add binding to table. *)
    Hashtbl.add vtable sym binding ;
    (* Return next place. *)
    place + 1
  | ArrDecl (dtype, id, intervals) -> 
    place (* TODO *)

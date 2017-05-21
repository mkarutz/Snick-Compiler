(**
 * File: symbol.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * This module provides services for building and querying the global symbol 
 * table.
 *)

type dtype = Ast.dtype
type slotnum = int

(* An entry in the variable symbol table. *)
type vbinding =
  | ScalarVal of (dtype * slotnum)
  | ScalarRef of (dtype * slotnum)
  | Array of (dtype * slotnum * Ast.interval list)
  | UnboundVar

(* A record type holding the details of a procedure definition. *)
type proc = {
  id : string;
  params : Ast.param_def list;
  num_params : int;
  stack_slots : int;
}

(* An entry in the procedure symbol table. *)
type pbinding =
  | Procedure of proc
  | UnboundProc

(** Looks-up a variable in the symbol table. *)
val lookup_var : string -> string -> vbinding
  
(** Gets the type of a variable from the symbol table. *)
val lookup_type : string -> string -> dtype

(** Looks-up a procedure in the symbol table. *)
val lookup_proc : string -> pbinding

(** 
 * Traverses a Snick AST and builds the global symbol table. 
 *
 * <p>Performs basic checking to ensure that all symbols in shared scope and 
 * namespaces are distinct.
 *
 * <p>Throws an exception if any declaration shares an identifier with another 
 * symbol in the same scope.
 *)
val build_symtbls : Ast.t -> unit

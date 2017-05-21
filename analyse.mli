(**
 * File: analyse.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * This module provides services for static analysis of a Snick AST.
 *)

open Ast
open Symbol

exception SemanticError of string

(** 
 * Traverses a Snick AST and performs semantic analysis checks.
 *
 * <p>Throws an exception if any semantic errors are detected.
 *)
val analyse : Ast.t -> unit

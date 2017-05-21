(**
 * File: translate.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * This module provides services for translating a Snick abstract-syntax tree
 * into a Brill program.
 *)

(**
 * Translates a Snick AST to a Brill program.
 *
 * <p>Expects that static analysis has been performed and will fail if any
 * unexpected errors are encountered.
 *)
val translate : Ast.t -> Brill.t

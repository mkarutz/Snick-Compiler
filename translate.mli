(**
 * File: translate.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * This module provides services for translating a Snick abstract-syntax tree
 * into a Brill program.
 *)

val translate : Ast.t -> Brill.t

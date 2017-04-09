(**
 * File: pprinter.mli
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * Interface of the pretty printer module for the Snick compiler. This module
 * exports one function, <code>print_program</code>, that pretty-prints a
 * parsed Snick program using the given Formatter.
 *)

val print_program : Format.formatter -> Ast.program -> unit

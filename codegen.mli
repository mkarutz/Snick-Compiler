(**
 * File: codegen.mli
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * This module exports one function, <code>print_brill_code</code>, that 
 * prints Brill instructions for the given Brill program and with the given 
 * Formatter.
 *)

val print_code : Brill.t -> unit

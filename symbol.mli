(**
 * File: symbol_table.mli
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *)

type foo = int
type scope = foo list
type t = scope list

val enter_scope : t -> t

val exit_scope : t -> t

val bind_var : t -> string -> foo -> t

val lookup : t -> string -> foo
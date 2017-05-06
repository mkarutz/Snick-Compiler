(**
 * File: codegen.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * This module provides a function that emits code for a given Brill program.
 *)

let printf = Printf.printf

let print_builtin builtin =
  match builtin with
  | Brill.ReadInt -> printf "read_int"
  | Brill.ReadReal -> printf "read_real"
  | Brill.ReadBool -> printf "read_bool"
  | Brill.PrintInt -> printf "print_int"
  | Brill.PrintReal -> printf "print_real"
  | Brill.PrintBool -> printf "print_bool"
  | Brill.PrintString -> printf "print_string"

let print_call_builtin builtin =
  printf "call_builtin " ;
  print_builtin builtin ;
  printf "\n"

let print_instr instr = 
  match instr with
  | Brill.PushStackFrame framesize ->
    printf "push_stack_frame %d\n" framesize
  | Brill.PopStackFrame framesize ->
    printf "pop_stack_frame %d\n" framesize
  | Brill.StringConst (reg, value) ->
    printf "string_const r%d, \"%s\"\n" reg value
  | Brill.Call label ->
    printf "call %s\n" label
  | Brill.CallBuiltin builtin ->
    print_call_builtin builtin
  | Brill.Return ->
    printf "return\n"
  | Brill.Halt ->
    printf "halt\n"
  | Brill.Label label ->
    printf "%s:\n" label
  | _ ->
    failwith "Not implemented."

let rec print_instrs instrs =
  match instrs with
  | [] -> ()
  | x::xs -> 
    print_instr x ;
    print_instrs xs
    
let print_code program = print_instrs program

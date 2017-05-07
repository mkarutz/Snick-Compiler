(**
 * File: codegen.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * This module provides a function that emits code for a given Brill program.
 *)
open Brill
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
  
  (* Push/pop stack frames *)
  | Brill.PushStackFrame framesize ->
    printf "push_stack_frame %d\n" framesize
  | Brill.PopStackFrame framesize ->
    printf "pop_stack_frame %d\n" framesize
    
  (* Load/Store/Move/Dereference *)
  | Brill.Load (reg, slotnum) ->
    printf "load r%d, %d\n" reg slotnum
  | Brill.Store (slotnum, reg) ->
    printf "store %d, r%d\n" slotnum reg
  | LoadAddress (reg, slotnum) ->
    printf "load_address r%d, %d\n" reg slotnum
  | LoadIndirect (dest, src) ->
    printf "load_indirect r%d, r%d\n" dest src
  | StoreIndirect (dest, src) ->
    printf "store_indirect r%d, r%d\n" dest src
    
  (* Constants *)
  | Brill.IntConst (reg, value) ->
    printf "int_const r%d, %s\n" reg value
  | Brill.RealConst (reg, value) ->
    printf "real_const r%d, %s\n" reg value
  | Brill.StringConst (reg, value) ->
    printf "string_const r%d, %s\n" reg value
  
  (* Arithmetic operatiosn *)
  | AddInt (dest, lhs, rhs) ->
    printf "add_int r%d, r%d, r%d\n" dest lhs rhs
  | AddReal (dest, lhs, rhs) ->
    printf "add_real r%d, r%d, r%d\n" dest lhs rhs
  | AddOffset (dest, lhs, rhs) ->
    printf "add_offset r%d, r%d, r%d\n" dest lhs rhs
  | SubInt (dest, lhs, rhs) ->
    printf "sub_int r%d, r%d, r%d\n" dest lhs rhs
  | SubReal (dest, lhs, rhs) ->
    printf "sub_real r%d, r%d, r%d\n" dest lhs rhs
  | SubOffset (dest, lhs, rhs) ->
    printf "sub_offset r%d, r%d, r%d\n" dest lhs rhs
  | MulInt (dest, lhs, rhs) ->
    printf "mul_int r%d, r%d, r%d\n" dest lhs rhs
  | MulReal (dest, lhs, rhs) ->
    printf "mul_real r%d, r%d, r%d\n" dest lhs rhs
  | DivInt (dest, lhs, rhs) ->
    printf "div_int r%d, r%d, r%d\n" dest lhs rhs
  | DivReal (dest, lhs, rhs) ->
    printf "div_real r%d, r%d, r%d\n" dest lhs rhs
    
  (* Relational operations *)
  | CmpEqInt (dest, lhs, rhs) ->
    printf "cmp_eq_int r%d, r%d, r%d\n" dest lhs rhs
  | CmpNeInt (dest, lhs, rhs) ->
    printf "cmp_ne_int r%d, r%d, r%d\n" dest lhs rhs
  | CmpGtInt (dest, lhs, rhs) ->
    printf "cmp_gt_int r%d, r%d, r%d\n" dest lhs rhs
  | CmpGeInt (dest, lhs, rhs) ->
    printf "cmp_ge_int r%d, r%d, r%d\n" dest lhs rhs
  | CmpLtInt (dest, lhs, rhs) ->
    printf "cmp_lt_int r%d, r%d, r%d\n" dest lhs rhs
  | CmpLeInt (dest, lhs, rhs) ->
    printf "cmp_le_int r%d, r%d, r%d\n" dest lhs rhs
  | CmpEqReal (dest, lhs, rhs) ->
    printf "cmp_eq_real r%d, r%d, r%d\n" dest lhs rhs
  | CmpNeReal (dest, lhs, rhs) ->
    printf "cmp_ne_real r%d, r%d, r%d\n" dest lhs rhs
  | CmpGtReal (dest, lhs, rhs) ->
    printf "cmp_gt_real r%d, r%d, r%d\n" dest lhs rhs
  | CmpGeReal (dest, lhs, rhs) ->
    printf "cmp_ge_real r%d, r%d, r%d\n" dest lhs rhs
  | CmpLtReal (dest, lhs, rhs) ->
    printf "cmp_lt_real r%d, r%d, r%d\n" dest lhs rhs
  | CmpLeReal (dest, lhs, rhs) ->
    printf "cmp_le_real r%d, r%d, r%d\n" dest lhs rhs
    
  (* Logical operations *)
  | And (dest, lhs, rhs) ->
    printf "and r%d, r%d, r%d\n" dest lhs rhs
  | Or (dest, lhs, rhs) ->
    printf "or r%d, r%d, r%d\n" dest lhs rhs
  | Not (dest, src) ->
    printf "not r%d, r%d\n" dest src
  
  (* Typecasting *)
  | IntToReal (dest, src) ->
    printf "int_to_real r%d, r%d\n" dest src
    
  (* Pseudo-instruction label *)
  | Brill.Label label ->
    printf "%s:\n" label
    
  (* Brancing *)
  | BranchOnTrue (reg, label) ->
    printf "branch_on_true r%d, %s\n" reg label
  | BranchOnFalse (reg, label) ->
    printf "branch_on_false r%d, %s\n" reg label
  | BranchUncond label ->
    printf "branch_uncond %s\n" label
  | Brill.Call label ->
    printf "call %s\n" label
  | Brill.CallBuiltin builtin ->
    print_call_builtin builtin
  | Brill.Return ->
    printf "return\n"
    
  (* Debugging *)
  (* TODO *)
    
  (* Termination *)
  | Brill.Halt ->
    printf "halt\n"
  
  | _ ->
    failwith "Not implemented."

let rec print_instrs instrs =
  match instrs with
  | [] -> ()
  | x::xs -> 
    print_instr x ;
    print_instrs xs
    
let print_code program = 
  print_instrs program

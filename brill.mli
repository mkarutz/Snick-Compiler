val translate : Ast.t -> Brill.t 

(* module Ast = Ast

type label = string
type builtin_function_name = string
type reg = int
type slotnum = int
type framesize = int

type instr = 
  | Push_stack_frame of framesize
  | Pop_stack_frame of framesize
  (* C analogues: *)
  | Load of (reg * slotnum) (* reg = x *)
  | Store of (slotnum * reg) (* x = reg *)
  | Load_address of (reg * slotnum) (* reg = &x *)
  | Load_indirect of (reg * reg) (* reg = *reg *)
  | Store_indirect of (reg * reg) (* *reg = reg *)
  | Int_const of (reg * int)
  | Real_const of (reg * float)
  | String_const of (reg * string)
  | Add_int of (reg * reg * reg) (* reg = reg + reg *)
  | Add_real of (reg * reg * reg) (* reg = reg + reg *)
  | Add_offset of (reg * reg * reg) (* reg = reg + reg *)
  | Sub_int of (reg * reg * reg) (* reg = reg - reg *)
  | Sub_real of (reg * reg * reg) (* reg = reg - reg *)
  | Sub_offset of (reg * reg * reg) (* reg = reg - reg *)
  | Mul_int of (reg * reg * reg) (* reg = reg * reg *)
  | Mul_real of (reg * reg * reg) (* reg = reg * reg *)
  | Div_int of (reg * reg * reg) (* reg = reg / reg *)
  | Div_real of (reg * reg * reg) (* reg = reg / reg *)
  | Cmp_eq_int of (reg * reg * reg) (* reg = reg == reg *)
  | Cmp_ne_int of (reg * reg * reg)
  | Cmp_gt_int of (reg * reg * reg) (* etc. *)
  | Cmp_ge_int of (reg * reg * reg)
  | Cmp_lt_int of (reg * reg * reg)
  | Cmp_le_int of (reg * reg * reg)
  | Cmp_eq_real of (reg * reg * reg)
  | Cmp_ne_real of (reg * reg * reg)
  | Cmp_gt_real of (reg * reg * reg)
  | Cmp_ge_real of (reg * reg * reg)
  | Cmp_lt_real of (reg * reg * reg)
  | Cmp_le_real of (reg * reg * reg)
  | And of (reg * reg * reg) (* reg = reg && reg *)
  | Or of (reg * reg * reg) (* reg = reg || reg *)
  | Not of (reg * reg) (* reg = !reg *)
  | Int_to_real of (reg * reg) (* reg = (float) reg *)
  | Move of (reg * reg) (* reg = reg *)
  | Branch_on_true of (reg * label) (* if (reg) goto label *)
  | Branch_on_false of (reg * label) (* if (!reg) goto label *)
  | Branch_uncond of label (* goto label *)
  | Call of label
  | Call_builtin of builtin_function_name
  | Return
  | Debug_reg of reg
  | Debug_slot of slotnum
  | Debug_stack
  | Halt

type t = instr list

val generate_code : Format.formatter -> Ast.program -> Bregll.t *)

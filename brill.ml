(**
 * File: brill.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * This module defines the structure of a Brill program.
 *)

type label = string
type reg = int
type slotnum = int
type framesize = int
type raw_const = string

type dtype =
  | IntT
  | RealT
  | StringT

type builtin =
  | ReadInt
  | ReadReal
  | ReadBool
  | PrintInt
  | PrintReal
  | PrintBool
  | PrintString

type instr = 
  (* Push/pop stack frames *)
  | PushStackFrame of framesize
  | PopStackFrame of framesize
  
  (* Load/Store/Move/Dereference *)
  | Load of (reg * slotnum) (* reg = x *)
  | Store of (slotnum * reg) (* x = reg *)
  | Move of (reg * reg) (* reg = reg *)
  | LoadAddress of (reg * slotnum) (* reg = &x *)
  | LoadIndirect of (reg * reg) (* reg = *reg *)
  | StoreIndirect of (reg * reg) (* *reg = reg *)
  
  (* Constants *)
  | IntConst of (reg * raw_const)
  | RealConst of (reg * raw_const)
  | StringConst of (reg * raw_const)
  
  (* Arithmetic operations *)
  | AddInt of (reg * reg * reg) (* reg = reg + reg *)
  | AddReal of (reg * reg * reg) (* reg = reg + reg *)
  | AddOffset of (reg * reg * reg) (* reg = reg + reg *)
  | SubInt of (reg * reg * reg) (* reg = reg - reg *)
  | SubReal of (reg * reg * reg) (* reg = reg - reg *)
  | SubOffset of (reg * reg * reg) (* reg = reg - reg *)
  | MulInt of (reg * reg * reg) (* reg = reg * reg *)
  | MulReal of (reg * reg * reg) (* reg = reg * reg *)
  | DivInt of (reg * reg * reg) (* reg = reg / reg *)
  | DivReal of (reg * reg * reg) (* reg = reg / reg *)
  
  (* Relational operations *)
  | CmpEqInt of (reg * reg * reg) (* reg = reg == reg *)
  | CmpNeInt of (reg * reg * reg) (* etc. *)
  | CmpGtInt of (reg * reg * reg)
  | CmpGeInt of (reg * reg * reg)
  | CmpLtInt of (reg * reg * reg)
  | CmpLeInt of (reg * reg * reg)
  | CmpEqReal of (reg * reg * reg)
  | CmpNeReal of (reg * reg * reg)
  | CmpGtReal of (reg * reg * reg)
  | CmpGeReal of (reg * reg * reg)
  | CmpLtReal of (reg * reg * reg)
  | CmpLeReal of (reg * reg * reg)
  
  (* Logical operations *)
  | And of (reg * reg * reg) (* reg = reg ^ reg *)
  | Or of (reg * reg * reg) (* reg = reg v reg *)
  | Not of (reg * reg) (* reg = !reg *)
  
  (* Typecasting *)
  | IntToReal of (reg * reg) (* reg = (float) reg *)
  
  (* Pseudo-instruction label *)
  | Label of label
  
  (* Branching *)
  | BranchOnTrue of (reg * label) (* if (reg) goto label *)
  | BranchOnFalse of (reg * label) (* if (!reg) goto label *)
  | BranchUncond of label (* goto label *)
  | Call of label
  | CallBuiltin of builtin
  | Return
  
  (* Debugging *)
  | DebugReg of reg
  | DebugSlot of slotnum
  | DebugStack
  
  (* Termination *)
  | Halt

type t = instr list

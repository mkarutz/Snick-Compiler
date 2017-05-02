(**
 * File: translate.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * This module provides services for translating a Snick abstract-syntax tree
 * into a Brill program.
 *)

let translate_program _ = 
  [
    Brill.Call "proc_main" ; 
    Brill.Halt ;
    Brill.Label "proc_main" ;
    Brill.PushStackFrame 0 ;
    Brill.StringConst (0, "Hello, world!\n") ;
    Brill.CallBuiltin Brill.PrintString ;
    Brill.PopStackFrame 0 ;
    Brill.Return
  ]

let translate program = translate_program program

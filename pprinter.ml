(**
 * File: pprinter.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * Implementation of pretty printer module for the Snick compiler. This module
 * exports one function, <code>print_program</code>, that pretty-prints a
 * parsed Snick program using the given Formatter.
 *)

open Ast
open Format

(* Some aliases for pretty-printing functions in the Formatter module. *)
let open_vbox fmt () = pp_open_vbox fmt 0
let open_hbox fmt () = pp_open_hbox fmt ()
let close_box fmt () = pp_close_box fmt ()
let print_space fmt () = pp_print_space fmt ()
let print_break fmt indent = pp_print_break fmt 0 indent
let print_string fmt x = pp_print_string fmt x
let print_char fmt x = pp_print_char fmt x
let print_int fmt x = pp_print_int fmt x

(* Prints a Snick constant. *)
let print_const fmt const = print_string fmt const.raw

(* Prints a Snick binary operator. *)
let print_binop fmt binop =
  match binop with
  | OrBinop -> print_string fmt "or"
  | AndBinop -> print_string fmt "and"
  | EqBinop -> print_char fmt '='
  | NeBinop -> print_string fmt "!="
  | LtBinop -> print_char fmt '<'
  | GtBinop -> print_char fmt '>'
  | LteBinop -> print_string fmt "<="
  | GteBinop -> print_string fmt ">="
  | AddBinop -> print_char fmt '+'
  | SubBinop -> print_char fmt '-'
  | MulBinop -> print_char fmt '*'
  | DivBinop -> print_char fmt '/'

(* Prints a Snick unary operator. *)
let print_unop fmt unop =
  match unop with
  | NotUnop -> print_string fmt "not"
  | MinusUnop -> print_char fmt '-'

(* Returns the precedence level of a Snick binary operator *)
let binop_precedence binop =
  match binop with
  | OrBinop -> 1
  | AndBinop -> 2
  | EqBinop | NeBinop | LtBinop | GtBinop | LteBinop | GteBinop -> 4
  | AddBinop | SubBinop -> 5
  | MulBinop | DivBinop -> 6

(* Returns the precedence level of a Snick unary operator *)
let unop_precedence unop =
  match unop with
  | NotUnop -> 3
  | MinusUnop -> 7

(* Pretty prints a Snick expression. *)
let rec print_expr fmt expr =
  print_expr' fmt expr.expr
  
and print_expr' fmt expr =
  match expr with
  | ConstExpr const ->
    print_const fmt const
  | LvalueExpr lvalue ->
    print_lvalue fmt lvalue
  | BinopExpr (lhs, binop, rhs) ->
    open_hbox fmt () ;
    print_lhs fmt binop lhs ;
    print_space fmt () ;
    print_binop fmt binop ;
    print_space fmt () ;
    print_rhs fmt binop rhs ;
    close_box fmt ()
  | UnopExpr (unop, expr) ->
    open_hbox fmt () ;
    print_unop fmt unop ;
    print_space fmt () ;
    print_unary_operand fmt unop expr ;
    close_box fmt ()

(* Prints the left operand of a binary expression, adding parentheses if
 * needed. *)
and print_lhs fmt parent_binop lhs =
  match lhs.expr with
  | ConstExpr _ | LvalueExpr _ | UnopExpr _ ->
    print_expr fmt lhs
  | BinopExpr (_, binop, _) ->
    if binop_precedence(binop) < binop_precedence(parent_binop) then
      print_parens fmt lhs
    else
      print_expr fmt lhs

(* Prints the right operand of a binary expression, adding parentheses if
 * needed. *)
and print_rhs fmt parent_binop rhs =
  match rhs.expr with
  | ConstExpr _ | LvalueExpr _ | UnopExpr _ ->
    print_expr fmt rhs
  | BinopExpr (_, binop, _) ->
    if binop_precedence(binop) <= binop_precedence(parent_binop) then
      print_parens fmt rhs
    else
      print_expr fmt rhs

(* Prints the operand of a unary expression, adding parentheses if needed. *)
and print_unary_operand fmt unop expr =
  match expr.expr with
  | ConstExpr _ | LvalueExpr _ | UnopExpr _ ->
    print_expr fmt expr
  | BinopExpr (_, binop, _) ->
    if binop_precedence(binop) <= unop_precedence(unop) then
      print_parens fmt expr
    else
      print_expr fmt expr

(* Prints an expression wrapped in parentheses. *)
and print_parens fmt expr =
  open_hbox fmt () ;
  print_char fmt '(' ;
  print_expr fmt expr ;
  print_char fmt ')' ;
  close_box fmt ()

and print_lvalue fmt lvalue =
  match lvalue with
  | Id id ->
    print_string fmt id
  | ArrAccess (id, exprs) ->
    open_hbox fmt () ;
    print_string fmt id ;
    print_char fmt '[' ;
    print_expr_list fmt exprs ;
    print_char fmt ']' ;
    close_box fmt ()

(* Pretty prints a comma-separated list of arguments in a Snick procedure call
 * statement. *)
and print_expr_list fmt exprs =
  match exprs with
  | [] -> ()
  | x::[] ->
    print_expr fmt x
  | x::xs ->
    open_hbox fmt () ;
    print_expr fmt x ;
    print_string fmt ", " ;
    print_expr_list fmt xs ;
    close_box fmt ()

(* Pretty prints a list of Snick statements. *)
let rec print_stmts fmt stmts =
  match stmts with
  | [] -> ()
  | x::[] ->
    print_stmt fmt x;
  | x::xs ->
    open_vbox fmt () ;
    print_stmt fmt x;
    print_break fmt 0 ;
    print_stmts fmt xs ;
    close_box fmt ()

(* Pretty prints a Snick statement. *)
and print_stmt fmt stmt =
  match stmt with
  | AtomStmt atom ->
    open_hbox fmt () ;
    print_atom_stmt fmt atom ;
    print_char fmt ';' ;
    close_box fmt ()
  | CompStmt comp ->
    print_comp_stmt fmt comp

(* Pretty prints an atomic Snick statement. *)
and print_atom_stmt fmt stmt =
  match stmt with
  | Assign (lvalue, expr) ->
    open_hbox fmt () ;
    print_lvalue fmt lvalue ;
    print_string fmt " := " ;
    print_expr fmt expr ;
    close_box fmt ()
  | Read lvalue ->
    open_hbox fmt () ;
    print_string fmt "read " ;
    print_lvalue fmt lvalue ;
    close_box fmt ()
  | Write expr ->
    open_hbox fmt () ;
    print_string fmt "write " ;
    print_expr fmt expr ;
    close_box fmt ()
  | Call (id, exprs) ->
    open_hbox fmt () ;
    print_string fmt id ;
    print_char fmt '(' ;
    print_expr_list fmt exprs ;
    print_char fmt ')' ;
    close_box fmt ()

(* Pretty prints a Snick iteration or selection statement. *)
and print_comp_stmt fmt stmt =
  match stmt with
  | IfThenElse (expr, a, b) ->
    open_vbox fmt () ;
    open_hbox fmt () ;
    print_string fmt "if" ;
    print_space fmt () ;
    print_expr fmt expr ;
    print_space fmt () ;
    print_string fmt "then" ;
    close_box fmt () ;
    print_break fmt 4 ;
    print_stmts fmt a ;
    print_break fmt 0 ;
    print_string fmt "else" ;
    print_break fmt 4 ;
    print_stmts fmt b ;
    print_break fmt 0 ;
    print_string fmt "fi" ;
    close_box fmt ()
  | IfThen (expr, stmts) ->
    open_vbox fmt () ;
    open_hbox fmt () ;
    print_string fmt "if" ;
    print_space fmt () ;
    print_expr fmt expr ;
    print_space fmt () ;
    print_string fmt "then" ;
    close_box fmt () ;
    print_break fmt 4 ;
    print_stmts fmt stmts ;
    print_break fmt 0 ;
    print_string fmt "fi" ;
    close_box fmt ()
  | While (expr, stmts) ->
    open_vbox fmt () ;
    open_hbox fmt () ;
    print_string fmt "while" ;
    print_space fmt () ;
    print_expr fmt expr ;
    print_space fmt () ;
    print_string fmt "do" ;
    close_box fmt () ;
    print_break fmt 4 ;
    print_stmts fmt stmts ;
    print_break fmt 0 ;
    print_string fmt "od" ;
    close_box fmt ()

let print_id fmt id =
  print_string fmt id

let print_dtype fmt dtype =
  match dtype with
  | BoolType -> print_string fmt "bool"
  | FloatType -> print_string fmt "float"
  | IntType -> print_string fmt "int"
  | _ -> failwith "Impossible type for variable declaration."

(* Pretty prints an interval in a Snick array declaration. *)
let print_interval fmt interval =
  let (a, b) = interval in
    open_hbox fmt () ;
    print_int fmt a ;
    print_string fmt ".." ;
    print_int fmt b ;
    close_box fmt ()

(* Pretty prints the list of intervals in a Snick array declaration. *)
let rec print_interval_list fmt intervals =
  match intervals with
  | [] -> ()
  | x::[] ->
    print_interval fmt x
  | x::xs ->
    open_hbox fmt () ;
    print_interval fmt x ;
    print_string fmt "," ;
    print_space fmt () ;
    print_interval_list fmt xs ;
    close_box fmt ()

(* Prints a Snick variable or array declaration. *)
let print_decl fmt decl =
  match decl with
  | VarDecl (dtype, id) ->
    open_hbox fmt () ;
    print_dtype fmt dtype ;
    print_space fmt () ;
    print_id fmt id ;
    print_string fmt ";" ;
    close_box fmt ()
  | ArrDecl (dtype, id, intervals) ->
    open_hbox fmt () ;
    print_dtype fmt dtype ;
    print_space fmt () ;
    print_id fmt id ;
    print_char fmt '[' ;
    print_interval_list fmt intervals ;
    print_char fmt ']' ;
    print_string fmt ";" ;
    close_box fmt ()

(* Prints a list of Snick variable and array declaration. *)
let rec print_decls fmt decls =
  match decls with
  | [] -> ()
  | x::[] ->
    print_decl fmt x
  | x::xs ->
    open_vbox fmt () ;
    print_decl fmt x ;
    print_break fmt 0 ;
    print_decls fmt xs ;
    close_box fmt ()

(* Prints the mode qualifier of a paramter. *)
let print_parammode fmt mode =
  match mode with
  | Val -> print_string fmt "val"
  | Ref -> print_string fmt "ref"

(* Prints a parameter definition. *)
let print_param fmt param =
  open_hbox fmt () ;
  print_parammode fmt param.mode ;
  print_space fmt () ;
  print_dtype fmt param.dtype ;
  print_space fmt () ;
  print_id fmt param.id ;
  close_box fmt ()

(* Pretty prints the parameter definitions for a Snick procedures definition. *)
let rec print_procparams fmt params =
  open_hbox fmt () ;
  print_char fmt '(' ;
  print_procparams' fmt params ;
  print_char fmt ')' ;
  close_box fmt ()
and print_procparams' fmt params =
  match params with
  | [] -> ()
  | x::[] ->
    print_param fmt x ;
  | x::xs ->
    open_hbox fmt () ;
    print_param fmt x ;
    print_string fmt "," ;
    print_space fmt () ;
    print_procparams' fmt xs ;
    close_box fmt ()

(* Pretty prints the header of a Snick procedure definitions. *)
let print_procheader fmt header =
  open_hbox fmt () ;
  print_string fmt "proc" ;
  print_space fmt () ;
  print_string fmt header.proc_id ;
  print_space fmt () ;
  print_procparams fmt header.params ;
  close_box fmt ()

(* Pretty prints a Snick procedures definition. *)
let print_proc fmt proc =
  open_vbox fmt () ;
  print_procheader fmt proc.header ;
  begin match proc.body.decls with
    | [] ->
      print_break fmt 0
    | x::xs ->
      print_break fmt 4 ;
      print_decls fmt proc.body.decls ;
      print_break fmt 0 ;
  end ;
  print_break fmt 4 ;
  print_stmts fmt proc.body.stmts ;
  print_break fmt 0 ;
  print_string fmt "end" ;
  close_box fmt ()

(* Pretty prints a list of Snick procedures definitions. *)
let rec print_procs fmt procs =
  match procs with
  | [] -> ()
  | x::xs ->
    open_vbox fmt () ;
    print_proc fmt x ;
    print_break fmt 0 ;
    print_break fmt 0 ;
    print_procs fmt xs ;
    close_box fmt ()

(* Pretty prints the given Snick program. *)
let print_program fmt prog = print_procs fmt prog.procdefs

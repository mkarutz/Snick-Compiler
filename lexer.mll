(**
 * File: lexer.mll
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * Definition of lexer (tokenizer) for the Snick compiler. This lexer reads a
 * Snick source program and:
 * <ul>
 *   <li> creates tokens for the Parser module
 *   <li> discards comments and whitespace
 *   <li> tracks line numbers for error reporting
 * </ul>
 *)

{
open Parser
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | '_' | '\'' | digit
let digits = digit+
let float = digits '.' digits
let ident = (alpha | '_') alnum*
let newline = '\r'?'\n'
let whitespace = [' ' '\t']+
let string = '"' ([^ '"' '\t' '\r' '\n']* as str) '"'
let comment = '#' [^ '\r' '\n']*

rule token = parse
  whitespace { token lexbuf (* discard and continue lexing *) }
  | comment { token lexbuf (* discard and continue lexing *) }
  | newline { Lexing.new_line lexbuf ; token lexbuf (* track line number *) }
  | "and" { AND }
  | "bool" { BOOL }
  | "do" { DO }
  | "else" { ELSE }
  | "end" { END }
  | "false" as lxm { BOOL_CONST lxm }
  | "fi" { FI }
  | "float" { FLOAT }
  | "if" { IF }
  | "int" { INT }
  | "not" { NOT }
  | "od" { OD }
  | "or" { OR }
  | "proc" { PROC }
  | "read" { READ }
  | "ref" { REF }
  | "then" { THEN }
  | "true" as lxm { BOOL_CONST lxm }
  | "val" { VAL }
  | "while" { WHILE }
  | "write" { WRITE }
  | "bool" { BOOL }
  | "int" { INT }
  | ":=" { ASSIGN }
  | "<=" { LTE }
  | ">=" { GTE }
  | ".." { UPTO }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '=' { EQ }
  | '<' { LT }
  | '>' { GT }
  | '+' { ADD }
  | '-' { MINUS }
  | '*' { MUL }
  | '/' { DIV }
  | ';' { SEMICOLON }
  | ',' { COMMA }
  | '-'? float as lxm { FLOAT_CONST lxm }
  | '-'? digits as lxm { INT_CONST lxm }
  | string { STRING_CONST str }
  | ident as lxm { IDENT lxm }
  | eof { EOF }

{
open Parser
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | '_' | '\'' | digit
let digits = digit+
let float = digits '.' digits
let ident = (alpha | '_') alnum*
let newline = '\r'?'\n''
let whitespace = [' ' '\t']+
let string = '"' ([^ '"' '\t' '\r' '\n']* as str) '"'

rule token = parse
  whitespace { token lexbuf }
  | newline { Lexing.new_line lexbuf ; token lexbuf }
	| "and" { AND }
	| "bool" { BOOL }
	| "do" { DO }
	| "else" { ELSE }
	| "end" { END }
	| "false" { BOOL_CONST(true) }
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
	| "true" { BOOL_CONST(true) }
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
  | '-' { SUB }
  | '*' { MUL }
	| '/' { DIV }
  | ';' { SEMICOLON }
	| '-'? float as lxm { FLOAT_CONST(float_of_string lxm) }
  | '-'? digits as lxm { INT_CONST(int_of_string lxm) }
	| string { STRING_CONST(str) }
  | ident as lxm { IDENT lxm }
	| _ { syntax_error }
  | eof { EOF }

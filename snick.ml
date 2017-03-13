module P = Parser

(* Argument parsing code *)
let infile_name = ref None

type compiler_mode = PrettyPrint | Compile
let mode = ref Compile

(* --------------------------------------------- *)
(*  Specification for command-line options       *)
(* --------------------------------------------- *)
let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
  [
			"-p",
    	Arg.Unit(fun () -> mode := PrettyPrint),
    	" Run the compiler in pretty-printer mode"
  ]

let main () =
  (* Parse the command-line arguments *)
  Arg.parse speclist
    (begin fun fname -> infile_name := Some fname end)
    "snick [-p] [source-code file path]" ;

  (* Open the input file *)
  let infile = match !infile_name with
		  | None -> stdin
		  | Some fname -> open_in fname in
	  (* Initialize lexing buffer *)
	  let lexbuf = Lexing.from_channel infile in
		  (* Call the parser *)
		  let prog = Parser.program Lexer.token lexbuf in
			  match !mode with
				  | PrettyPrint ->
							Pprinter.print_program Format.std_formatter prog
				  | Compile -> ()

let _ = main ()

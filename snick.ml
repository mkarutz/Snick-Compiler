(**
 * File: snick.ml
 * Author: Malcolm Karutz (mkarutz@student.unimelb.edu.au)
 *
 * This is the main program for the Snick compiler. It opens an input source
 * file and calls the lexing, parsing, and pretty printing modules.
 * <p>
 * The Snick compiler has two modes:
 * <ol>
 *   <li> Pretty print: outputs the parsed input program in a canonical format.
 *   <li> Compile: ouputs a program in the target language (not implemented).
 * </ol>
 *)

module P = Parser
open Analyse
open Printexc
open Lexing
open Utils

(*
 * The Snick compiler has two modes:
 * <ol>
 *   <li> Pretty print: outputs the parsed input program in a canonical format.
 *   <li> Compile: ouputs a program in the target language (not implemented).
 * </ol>
 *)
type compiler_mode =
  | PrettyPrint
  | Compile

(* Module for parsing the command line arguments. *)
module Args = struct
  type t = {
    infile_path: string option;
    mode: compiler_mode;
  }

  let usage = Printf.sprintf
    "Usage: %s [-p] [source-code file path]\n"
    Sys.argv.(0)

  let default_mode = Compile

  let parse () =
    let infile_path = ref None in
    let mode = ref default_mode in
    let options = [
      "-p", Arg.Unit(fun () -> mode := PrettyPrint),
      "Run the compiler in pretty-printer mode"
    ] in
    let () = Arg.parse options (fun f -> infile_path := (Some f)) usage in
    {
      infile_path = !infile_path;
      mode = !mode;
    }

  let mode args = args.mode
  let infile_path args = args.infile_path
end

(* Main function of the Snick compiler program. *)
let _ =
  let args = Args.parse () in
  let infile = match Args.infile_path args with
    | None -> stdin
    | Some path -> open_in path in
  let lexbuf = Lexing.from_channel infile in
  try
    let prog = Parser.program Lexer.token lexbuf in
    match Args.mode args with
    | PrettyPrint ->
      Pprinter.print_program Format.std_formatter prog
    | Compile ->
      Analyse.analyse prog ;
      Translate.translate prog |> Codegen.print_code 
  with
    | Failure msg
    | SemanticError msg ->
      Printf.eprintf "Failure: %s\n" msg ;
      Printf.eprintf "Compilation Failed\n" ;
      exit 1
    | e ->
      Printf.eprintf "Compilation Failed\n" ;
      exit 1

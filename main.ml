open Core.Std
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stdout "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stdout "%a: syntax error\n" print_position lexbuf;
    None

(* part 1 *)
let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    Bash.output_commands value;
    parse_and_print lexbuf
  | None -> ()

(* part 2 *)
let () =
  let rec loop () =
    Printf.printf "%s:=> " (Sys.getcwd());
    let line = read_line () in
    let lexbuf = Lexing.from_string (line^";") in
    begin match parse_with_error lexbuf with
    | Some v ->
        Exec.run_commands v
    | None -> ()
    end;
    loop ()
  in
  loop ();;

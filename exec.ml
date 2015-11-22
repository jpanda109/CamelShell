open Char
open Unix

open Bash

let ls args =
  Array.iter (fun v -> print_string (v^" ")) (Sys.readdir (Sys.getcwd ()));
  print_newline ();
  true;;

let cd args =
  try
    Sys.chdir (List.nth args 1);
    true
  with Sys_error s -> print_endline s; false;;

let sleep args =
  if List.length args < 2
  then (print_endline "sleep needs time param"; false)
  else
    try
      let t = int_of_string (List.nth args 1) in
      (Unix.sleep t; true)
    with Failure _ -> print_endline "sleep needs int param"; false;;

let echo args =
  List.iter (fun v -> print_string (v^" ")) (List.tl args); 
  print_newline (); 
  true;;

let cat args =
  List.iter (fun v ->
    let ic = open_in v in
    try while true; do
      print_endline (input_line ic)
    done;
    with End_of_file -> close_in ic) (List.tl args);
  true;;


let get_command_name args =
  match args with
  | [] -> ""
  | hd::_ -> hd;;

let exec_command args bg =
  match Unix.fork() with
  | 0 -> execvp (List.nth args 0) (Array.of_list args)
  | pid ->
      if bg then true
      else let (_, status) = Unix.waitpid [] pid in
      match status with
      | WEXITED num -> num = 0
      | _ -> false;;

let rec run_command comm bg =
  match comm with
  | `Command args ->
      let name = get_command_name args in
      if name = "" then true
      else if name = "exit" then true
      else if name = "cd" then cd args
      else exec_command args bg
  | `Background comm' -> 
      run_command comm' true
  | `And (c1, c2) ->
      run_command c1 bg && run_command c2 bg
  | `Or (c1, c2) ->
      run_command c1 bg || run_command c2 bg
  | _ -> false;;


let rec run_commands commands =
  match commands with
  | [] -> ()
  | hd::tl -> ignore (run_command hd false); run_commands tl;;


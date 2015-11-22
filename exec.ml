open Char
open Unix

open Bash

let cd args =
  try
    Sys.chdir (List.nth args 1);
    true
  with Sys_error s -> print_endline s; false;;

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
      else if name = "exit" then exit 0
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


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

let exec_command args bg outc =
  match Unix.fork() with
  | 0 -> begin
    Unix.dup2 (Unix.descr_of_out_channel outc) Unix.stdout;
    execvp (List.nth args 0) (Array.of_list args)
  end
  | pid ->
      if bg then true
      else let (_, status) = Unix.waitpid [] pid in
      match status with
      | WEXITED num -> num = 0
      | _ -> false;;

let rec run_command comm bg outc =
  match comm with
  | `Command args ->
      let name = get_command_name args in
      if name = "" then true
      else if name = "exit" then exit 0
      else if name = "cd" then cd args
      else exec_command args bg outc
  | `Background comm' -> 
      run_command comm' true outc
  | `Redirect (c, arg) ->
      run_command c bg (open_out arg)
  | `And (c1, c2) ->
      run_command c1 bg outc && run_command c2 bg outc
  | `Or (c1, c2) ->
      run_command c1 bg outc || run_command c2 bg outc
  | _ -> false;;


let rec run_commands commands =
  match commands with
  | [] -> ()
  | hd::tl -> ignore (run_command hd false (Unix.out_channel_of_descr Unix.stdout)); run_commands tl;;


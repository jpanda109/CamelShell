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

let exec_command args bg ind outd =
  match Unix.fork() with
  | 0 -> begin
    Unix.dup2 outd Unix.stdout;
    Unix.dup2 ind Unix.stdin;
    execvp (List.nth args 0) (Array.of_list args)
  end
  | pid ->
      if bg then true
      else let (_, status) = Unix.waitpid [] pid in
      match status with
      | WEXITED num -> num = 0
      | _ -> false;;

let rec run_command comm bg ind outd =
  match comm with
  | `Command args ->
      let name = get_command_name args in
      if name = "" then true
      else if name = "exit" then exit 0
      else if name = "cd" then cd args
      else exec_command args bg ind outd
  | `Background comm' -> 
      run_command comm' true ind outd
  | `Redirect (c, arg) ->
      run_command c bg ind (Unix.descr_of_out_channel (open_out arg))
  | `And (c1, c2) ->
      run_command c1 bg ind outd && run_command c2 bg ind outd
  | `Or (c1, c2) ->
      run_command c1 bg ind outd || run_command c2 bg ind outd
  | `Pipe (c1, c2) ->
      let (pipe_in, pipe_out) = Unix.pipe () in
      ignore(run_command c1 true ind pipe_out); run_command c2 true pipe_in outd
  | _ -> false;;


let rec run_commands commands =
  match commands with
  | [] -> ()
  | hd::tl -> ignore (run_command hd false Unix.stdin Unix.stdout); run_commands tl;;


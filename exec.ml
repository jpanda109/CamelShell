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

let rec run_command comm =
  match comm with
  | `Command args ->
      let name = get_command_name args in
      if name = "" then true
      else if name = "ls" then ls args
      else if name = "cd" then cd args
      else if name = "sleep" then sleep args
      else if name = "echo" then echo args
      else if name = "cat" then cat args
      else (Printf.printf "not a valid command %s\n" name; false)
  | `Background comm' -> 
      run_command comm'
  | `And (c1, c2) ->
      run_command c1 && run_command c2
  | `Or (c1, c2) ->
      run_command c1 || run_command c2
  | _ -> false;;

let rec run_commands commands =
  match commands with
  | [] -> ()
  | hd::tl -> 
      begin match Unix.fork() with
      | 0 -> ignore (run_command hd); exit 0
      | pid ->
          begin match hd with
          | `Background _ -> run_commands tl;
          | _ -> ignore (Unix.waitpid [] pid); run_commands tl; ()
          end 
      end;;

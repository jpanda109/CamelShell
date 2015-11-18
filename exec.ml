open Char
open Unix

open Tokens
open Commands

exception Unexpected_token of token;;

let parse_tokens toks = 
  let rec aux cur toks =
    match toks with
    | [] -> []
    | hd::tl ->
        begin match hd with
        | EOF -> [cur]
        | Background -> 
            begin match cur with
            | Backgrounded _ -> raise (Unexpected_token hd)
            | _ -> aux (Backgrounded cur) tl
            end
        | Arg s ->
            begin match cur with
            | Command args -> aux (Command (s::args)) tl
            | Backgrounded _ -> cur :: (aux (Command [s]) tl)
            (* | _ -> raise (Unexpected_token hd) *)
            end
        | _ -> []  (* THIS CASE SHOULD NOT REMAIN, FOR DEBUGGING *)
        end
  in
  let rec reverse_args comm =
    match comm with
    | Command args -> Command (List.rev args)
    | Backgrounded c -> Backgrounded (reverse_args c) in
  List.map reverse_args (aux (Command []) toks);;

let ls args =
  Utils.print_iter Array.iter (Sys.readdir (Sys.getcwd ()));
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
  (Utils.print_iter List.iter (List.tl args)); print_newline (); true;;

let get_command_name args =
  match args with
  | [] -> ""
  | hd::_ -> hd;;

let rec run_command comm =
  match comm with
  | Command args ->
      let name = get_command_name args in
      if name = "" then true
      else if name = "ls" then ls args
      else if name = "cd" then cd args
      else if name = "sleep" then sleep args
      else if name = "echo" then echo args
      else (Printf.printf "not a valid command %s\n" name; false)
  | Backgrounded comm' -> 
      begin match Unix.fork() with
      | 0 -> ignore (run_command comm'); exit 0  (* if bg, exit after command *)
      | _ -> true
      end;;

let rec run_commands commands =
  match commands with
  | [] -> ()
  | hd::tl -> 
      begin match Unix.fork() with
      | 0 -> ignore (run_command hd); exit 0
      | _ ->
          begin match hd with
          | Backgrounded _ -> run_commands tl;
          | _ -> ignore (Unix.wait ()); run_commands tl; ()
          end 
      end;;

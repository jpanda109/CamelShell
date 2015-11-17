let print_iter iter lst =
  let f e =
    Printf.printf "%s " e in
  iter f lst;;

type token = 
  | Background 
  | Arg of string
  | EOF

let is_alpha c =
  let code = Char.code c in
  (code >= 65 && code <= 90) || (code >= 97 && code <= 122);;

let is_num c =
  let code = Char.code c in
  code >= 48 && code <= 57;;

exception Unrecognized_token of char;;

(* return tokens *)
let scan s =
  let rec scan_iden acc i =
    if i >= String.length s
    then (Arg acc, i)
    else
      let c = s.[i] in
      if is_alpha c || is_num c || c = '_' || c = '-'
      then scan_iden (acc ^ (Char.escaped c)) (i + 1)
      else (Arg acc, i) in
  let rec scan_quoted acc i =
    let c = s.[i] in
    print_char c;
    if c = '"'
    then (Arg acc, i + 1)
    else scan_quoted (acc ^ (Char.escaped c)) (i + 1) in
  let rec aux i =
    if i >= String.length s
    then [EOF]
    else
      let c = s.[i] in
      if c = '"' then 
        let (tok, j) = scan_quoted "" (i + 1) in
        tok::aux j
      else if c = '&' then Background::aux (i + 1)
      else if is_alpha c || is_num c then
        let (tok, j) = scan_iden (Char.escaped c) (i + 1) in
        tok::aux j
      else if c = ' ' || c = '\t' then aux (i + 1)
      else raise (Unrecognized_token c) in
  aux 0;;

type command =
  | Command of string list
  | Backgrounded of command
;;

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
            | _ -> raise (Unexpected_token hd)
            end
        end
  in
  aux (Command []) toks;;

let ls args =
  print_iter Array.iter (Sys.readdir (Sys.getcwd ()));
  print_newline ();;

let get_command_name args =
  match args with
  | [] -> ""
  | hd::_ -> hd;;

let rec run_command comm =
  match comm with
  | Command args ->
      let name = get_command_name args in
      if name = "" then ()
      else if name = "ls" then ls args
      else print_endline "not a valid command"
  | Backgrounded comm' -> run_command comm';;

let rec run_commands commands =
  match commands with
  | [] -> ()
  | hd::tl -> run_command hd; run_commands tl;;

let () =
  let rec loop () =
    print_string ":=> ";
    let line = read_line () in
      if line = "exit"
      then ()
      else begin
        run_commands (parse_tokens (scan line));
        loop()
      end in
  loop();;

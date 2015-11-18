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


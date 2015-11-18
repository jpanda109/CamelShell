open Tokens

let print_iter iter lst =
  let f e =
    Printf.printf "%s " e in
  iter f lst;;

let print_tokens lst =
  let f e =
    let s =
      begin match e with
      | Background -> "Background"
      | Arg s' -> "Arg(" ^ s' ^ ")"
      | OrThen -> "OrThen"
      | AndThen -> "AndThen"
      | Pipe -> "Pipe"
      | EOF -> "EOF"
      end
    in
    Printf.printf "%s " s in
  List.iter f lst;;

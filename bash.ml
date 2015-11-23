type command = [
  | `Command of string list
  | `Compound of command list
  | `Background of command
  | `Redirect of command * string
  | `Pipe of command * command
  | `And of command * command
  | `Or of command * command
]

open Core.Std

let rec output_command = function
  | `Command args -> print_command args
  | `Compound cs -> print_list cs
  | `Background c -> print_string "Background("; output_command c; print_string ")"
  | `Redirect (c, arg) -> print_string "Redirect("; output_command c; print_string arg; print_string ")"
  | `Pipe (c1, c2) -> print_string "Pipe("; output_command c1; print_string ", "; output_command c2; print_string ")"
  | `And (c1, c2) -> print_string "And("; output_command c1; print_string ", "; output_command c2; print_string ")"
  | `Or (c1, c2) -> print_string "Or("; output_command c1; print_string ", "; output_command c2; print_string ")"

and print_command args =
  print_string "Command([";
  List.iteri ~f:(fun i v ->
    if i > 0 then
      print_string ", ";
    print_string v) args;
  print_string "])"

and print_list lst =
  print_string "[";
  List.iteri ~f:(fun i v ->
    if i > 0 then
      print_string ", ";
    output_command v) lst;
  print_string "]"


let rec output_commands = function
  | hd::tl -> (output_command hd); print_string "\n"; output_commands tl
  | [] -> ()


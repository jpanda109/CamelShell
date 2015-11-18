type command =
  | Command of string list
  | Backgrounded of command
;;

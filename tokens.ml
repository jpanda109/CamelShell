type token = 
  | Background 
  | Arg of string
  | OrThen
  | AndThen
  | Pipe
  | EOF


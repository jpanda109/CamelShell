{
open Lexing
open Parser

exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let arg = ['a'-'z' 'A'-'Z' '_' '-' '0'-'9' '.' '/']+
let quoted_arg = '"'_*'"'
let semicolon = ';'
let andthen = "&&"
let orthen = "||"
let pipe = "|"
let bg = '&'
let lparen = '('
let rparen = ')'
let rarrow = '>'

rule read =
  parse
  | white    { read lexbuf }
  | newline { read lexbuf }
  | quoted_arg { 
      let s = Lexing.lexeme lexbuf in 
      ARG (String.sub s 1 ((String.length s) - 2)) 
    }
  | arg      { ARG (Lexing.lexeme lexbuf)}
  | semicolon { SEMICOLON }
  | andthen { AND }
  | orthen { OR }
  | bg { BACKGROUND }
  | pipe {PIPE }
  | lparen { LPAREN }
  | rparen { RPAREN }
  | rarrow { RARROW }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}
  | eof { EOF }

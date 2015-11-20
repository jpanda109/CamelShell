{
open Lexing
open Parser

exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let arg = ['a'-'z' 'A'-'Z' '_' '-' '0'-'9']+
let semicolon = ';'
let andthen = "&&"
let orthen = "||"
let bg = '&'
let lparen = '('
let rparen = ')'

rule read =
  parse
  | white    { read lexbuf }
  | newline { read lexbuf }
  | arg      { ARG (Lexing.lexeme lexbuf)}
  | semicolon { SEMICOLON }
  | andthen { AND }
  | orthen { OR }
  | bg { BACKGROUND }
  | lparen { LPAREN }
  | rparen { RPAREN }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}
  | eof { EOF}

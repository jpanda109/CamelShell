open Char
open Unix

open Tokens

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
      if (is_alpha c || is_num c || c = '_' || c = '-' || c = '.' || 
          c = '\\' || c = '/')
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
      else if c = '&' then
        if (String.length s) > (i + 1) && s.[i+1] = '&' 
        then AndThen::aux (i + 2)
        else Background::aux (i + 1)
      else if c = '|' then
        if (String.length s) > (i + 1) && s.[i+1] = '|'
        then OrThen::aux (i + 2)
        else Pipe::aux (i + 1)
      else if (is_alpha c || is_num c || c = '_' || c = '-' || c = '.' || 
               c = '\\' || c = '/') then
        let (tok, j) = scan_iden (Char.escaped c) (i + 1) in
        tok::aux j
      else if c = ' ' || c = '\t' then aux (i + 1)
      else raise (Unrecognized_token c) in
  aux 0;;


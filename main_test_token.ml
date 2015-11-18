let () =
  let rec loop () =
    Printf.printf "%s:=> " (Sys.getcwd ());
    let line = read_line () in
    if line = "exit"
    then ()
    else begin
      Utils.print_tokens (Lexer.scan line); print_newline ();
      loop ()
    end in
  loop ();;

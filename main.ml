let () =
  let rec loop () =
    Printf.printf "%s:=> " (Sys.getcwd ());
    let line = read_line () in
      if line = "exit"
      then ()
      else begin
        Exec.run_commands (Parser.parse_tokens (Lexer.scan line));
        loop()
      end in
  loop();;

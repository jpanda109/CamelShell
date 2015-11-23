%token <string> ARG
%token AND OR
%token BACKGROUND PIPE
%token LPAREN RPAREN
%token RARROW
%token SEMICOLON
%token EOF

%left AND OR
%left RARROW PIPE

%start <Bash.command list option> prog

%%
prog:
  | EOF { None }
  | stmt = statement; p = prog { 
    match p with
    | Some p' -> Some (stmt :: p')
    | None -> Some ([stmt])
  };

statement:
  | c = command; SEMICOLON { c }
  | c = command; BACKGROUND { `Background c }
  | LPAREN; cl = list(statement); RPAREN { `Compound cl };

command:
  | al = list(ARG) { `Command al }
  | c1 = command; RARROW; a = ARG { `Redirect (c1, a) }
  | c1 = command; PIPE; c2 = command { `Pipe (c1, c2) }
  | c1 = command; AND; c2 = command { `And (c1, c2) } 
  | c1 = command; OR; c2 = command { `Or (c1, c2) };

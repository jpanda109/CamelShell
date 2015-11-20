%token <string> ARG
%token AND OR
%token BACKGROUND
%token LPAREN RPAREN
%token SEMICOLON
%token EOF

%left AND OR

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
  | c = command; BACKGROUND { `Background c };

command:
  | al = list(ARG) { `Command al }
  | LPAREN c = command RPAREN { c }
  | c1 = command; AND; c2 = command { `And (c1, c2) } 
  | c1 = command; OR; c2 = command { `Or (c1, c2) };

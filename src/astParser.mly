%{ open Prog %}

%token           PURE
%token <Prog.id> NOREAD
%token <Prog.id> NOWRITE
%token <Prog.id> NOAFFECT
%token <Prog.id> COMMUTES

%token NOT
%token OR
%token AND
%token EQ
%token NEQ
%token LT
%token LTE
%token GT
%token GTE
%token ADD
%token SUB
%token MUL
%token DIV

%token <bool> BOOLLIT
%token <int>  INTLIT

%token NOP
%token ASSIGN
%token ASSUME

%token SEMI
%token IF
%token ELSE
%token WHILE
%token FOR

%token <Prog.id> ID

%token LPAREN
%token RPAREN
%token LCURL
%token RCURL

%start ast
%type <Prog.ast> ast

%%

ast:
  | stmt
     { {root = $1} }

stmt:
  | basic_stmt
      { $1 }
  | basic_stmt stmt
      { Seq ($1, $2) }

basic_stmt:
  | instr SEMI
      { Instr $1 }
  | IF LPAREN expr RPAREN LCURL stmt RCURL
      { If ($3, $6) }
  | IF LPAREN expr RPAREN LCURL stmt RCURL ELSE LCURL stmt RCURL
      { IfElse ($3, $6, $10) }
  | WHILE LPAREN expr RPAREN LCURL stmt RCURL
      { While ($3, $6) }
  | FOR LPAREN stmt SEMI expr SEMI stmt RPAREN LCURL stmt RCURL
      { let h =
          { init   = $3
          ; guard  = $5
          ; update = $7
          }
        in
        For (h, $10)
      }

instr:
  | NOP
      { Nop }

expr:
  | BOOLLIT
      { BoolLit $1 }
  | INTLIT
      { IntLit $1 }

%%


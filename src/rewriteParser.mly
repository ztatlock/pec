%{

open Common.ZPervasives
open Prog

type decl =
  | OrigDecl
  | TempDecl
  | ExprDecl
  | StmtDecl
  | Unknown

let env =
  ref []

let add_decl id decl =
  env := (id, decl) :: !env

let lkup_decl id =
  try
    List.assoc id !env
  with Not_found ->
    Unknown

let parse_error s =
  failwith (mkstr "AstParser: error on line %d" !line)

%}

%token FIND
%token REPLACE

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

%token <int>  INTLIT
%token <bool> BOOLLIT

%token NOP
%token ASSIGN
%token ASSUME
%token WHERE

%token           PURE
%token <Prog.id> NOREAD
%token <Prog.id> NOWRITE
%token <Prog.id> NOAFFECT
%token <Prog.id> COMMUTES

%token SEMI
%token IF
%token ELSE
%token WHILE
%token FOR

%token ORIG_DECL
%token TEMP_DECL
%token EXPR_DECL
%token STMT_DECL

%token COMMA
%token LPAREN
%token RPAREN
%token LCURL
%token RCURL
%token EOF

%token <Prog.id> ID

%start rewrite
%type <Prog.ast * Prog.ast> rewrite

%left OR
%left AND
%nonassoc EQ NEQ
%nonassoc LT LTE GT GTE
%left ADD SUB
%left MUL DIV
%nonassoc NOT

%%

rewrite:
  | decls FIND stmt REPLACE stmt EOF
      { let l = { root = $3 } in
        let r = { root = $5 } in
        (l, r)
      }

decls:
  |            { }
  | decl decls { }

decl:
  | ORIG_DECL ID
      { add_decl $2 OrigDecl }
  | TEMP_DECL ID
      { add_decl $2 TempDecl }
  | EXPR_DECL ID
      { add_decl $2 ExprDecl }
  | STMT_DECL ID
      { add_decl $2 StmtDecl }

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
  | FOR LPAREN instr SEMI expr SEMI instr RPAREN LCURL stmt RCURL
      { let h =
          { init   = Instr $3
          ; guard  = $5
          ; update = Instr $7
          }
        in
        For (h, $10)
      }

instr:
  | NOP
      { Nop }
  | ID ASSIGN expr
      { match lkup_decl $1 with
        | OrigDecl -> Assign (Orig $1, $3) 
        | TempDecl -> Assign (Temp $1, $3) 
        | _ ->
            failwith (mkstr "'%s' not declared as var." $1)
      }
  | ASSUME LPAREN expr RPAREN
      { Assume $3 }
  | ID
      { StmtParam ($1, []) }
  | ID WHERE side_conds
      { StmtParam ($1, $3) }

expr:
  | INTLIT
      { IntLit $1 }
  | BOOLLIT
      { BoolLit $1 }
  | NOT expr
      { Unop (Not, $2) }
  | binop
      { $1 }
  | ID
      { match lkup_decl $1 with
        | OrigDecl -> Var (Orig $1)
        | TempDecl -> Var (Temp $1)
        | ExprDecl -> ExprParam ($1)
        | _ ->
            failwith (mkstr "'%s' not declared as var or expr." $1)
      }
  | LPAREN expr RPAREN
      { $2 }

binop:
  | expr AND expr
      { Binop (And, $1, $3) }
  | expr OR expr
      { Binop (Or, $1, $3) }
  | expr EQ expr
      { Binop (Eq, $1, $3) }
  | expr NEQ expr
      { Binop (Neq, $1, $3) }
  | expr LT expr
      { Binop (Lt, $1, $3) }
  | expr LTE expr
      { Binop (Lte, $1, $3) }
  | expr GT expr
      { Binop (Gt, $1, $3) }
  | expr GTE expr
      { Binop (Gte, $1, $3) }
  | expr ADD expr
      { Binop (Add, $1, $3) }
  | expr SUB expr
      { Binop (Sub, $1, $3) }
  | expr MUL expr
      { Binop (Mul, $1, $3) }
  | expr DIV expr
      { Binop (Div, $1, $3) }

side_conds:
  | side_cond
      { $1 :: [] }
  | side_cond COMMA side_conds
      { $1 :: $3 }

side_cond:
  | NOREAD 
      { match lkup_decl $1 with
        | OrigDecl -> NoRead (Orig $1)
        | TempDecl -> NoRead (Temp $1)
        | _ ->
            failwith (mkstr "noread: '%s' not declared as var." $1)
      }
  | NOWRITE
      { match lkup_decl $1 with
        | OrigDecl -> NoWrite (Orig $1)
        | TempDecl -> NoWrite (Temp $1)
        | _ ->
            failwith (mkstr "nowrite: '%s' not declared as var." $1)
      }
  | NOAFFECT
      { match lkup_decl $1 with
        | ExprDecl -> NoAffect (ExprParam $1)
        | _ ->
            failwith (mkstr "noaffect: '%s' not declared as expr." $1)
      }
  | COMMUTES
      { match lkup_decl $1 with
        | StmtDecl -> Commutes (StmtParam ($1, []))
        | _ ->
            failwith (mkstr "commutes: '%s' not declared as stmt." $1)
      }

%%


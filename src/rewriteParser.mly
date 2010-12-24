%{

open ZPervasives
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
%token OR  AND EQ  NEQ
%token LT  LTE GT  GTE
%token ADD SUB MUL DIV

%token <int> INTLIT
%token TRUE
%token FALSE

%token NOP
%token ASSIGN
%token ASSUME
%token WHERE

%token NOREAD
%token NOWRITE
%token NOAFFECT

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

%token <string> ID

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
      {mkast $3, mkast $5}

decls:
  |            { }
  | decl decls { }

decl:
  | ORIG_DECL ID { add_decl $2 OrigDecl }
  | TEMP_DECL ID { add_decl $2 TempDecl }
  | EXPR_DECL ID { add_decl $2 ExprDecl }
  | STMT_DECL ID { add_decl $2 StmtDecl }

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
  | var ASSIGN expr
      { Assign ($1, $3) }
  | var ADD ADD
      { Assign ($1, Binop (Add, Var $1, IntLit 1)) }
  | ASSUME LPAREN expr RPAREN
      { Assume $3 }
  | ID
      { match lkup_decl $1 with
        | StmtDecl -> Code ($1, [])
        | _ -> failwith (mkstr "'%s' not declared as stmt." $1)
      }
  | ID WHERE side_conds
      { match lkup_decl $1 with
        | StmtDecl -> Code ($1, $3)
        | _ -> failwith (mkstr "'%s' not declared as stmt." $1)
      }

var:
  | ID { match lkup_decl $1 with
         | OrigDecl -> Orig $1
         | TempDecl -> Temp $1
         | _ -> failwith (mkstr "'%s' not declared as var." $1)
       }

expr:
  | INTLIT
      { IntLit $1 }
  | TRUE
      { IntLit 1 }
  | FALSE
      { IntLit 0 }
  | NOT expr
      { Unop (Not, $2) }
  | binop
      { $1 }
  | ID
      { match lkup_decl $1 with
        | OrigDecl -> Var (Orig $1)
        | TempDecl -> Var (Temp $1)
        | ExprDecl -> Expr ($1, [])
        | _ -> failwith (mkstr "'%s' not declared as var or expr." $1)
      }
  | ID WHERE side_conds
      { match lkup_decl $1 with
        | ExprDecl -> Expr ($1, $3)
        | _ -> failwith (mkstr "'%s' not declared as expr." $1)
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
  | NOREAD LPAREN var RPAREN
      { NoRead $3 }
  | NOWRITE LPAREN var RPAREN
      { NoWrite $3 }
  | NOAFFECT LPAREN ID RPAREN
      { NoAffect $3 }

%%


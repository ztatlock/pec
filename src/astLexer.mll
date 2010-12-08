{

open Common.ZPervasives
open Prog
open AstParser 

}

let id =
  ['a'-'z''A'-'Z']+

let intlit =
  "0" | '-'?['1'-'9']['0'-'9']*

let comment =
  "#"[^'\n']*

let whitespace =
  [' ' '\t']

let line =
  '\n'

rule token = parse
  (* side conditions *)
  | "pure"
      { PURE }
  | "noread(" (id as x) ")"
      { NOREAD x }
  | "nowrite(" (id as x) ")"
      { NOWRITE x }
  | "noaffect(" (id as x) ")"
      { NOAFFECT x }
  | "commutes(" (id as x) ")"
      { COMMUTES x }

  (* operators *)
  | "!"  { NOT }
  | "||" { OR  }
  | "&&" { AND }
  | "==" { EQ  }
  | "!=" { NEQ }
  | "<"  { LT  }
  | "<=" { LTE }
  | ">"  { GT  }
  | ">=" { GTE }
  | "+"  { ADD }
  | "-"  { SUB }
  | "*"  { MUL }
  | "/"  { DIV }

  (* literals *)
  | intlit as x
      { INTLIT (int_of_string x) }
  | "true"
      { BOOLLIT true }
  | "false"
      { BOOLLIT false }

  (* instructions *)
  | "nop"    { NOP    }
  | "="      { ASSIGN }
  | "assume" { ASSUME }
  | "where"  { WHERE  }

  (* control flow *)
  | ";"     { SEMI  }
  | "if"    { IF    }
  | "else"  { ELSE  }
  | "while" { WHILE }
  | "for"   { FOR   }

  (* declarations *)
  | "orig" { ORIG_DECL }
  | "temp" { TEMP_DECL }
  | "epxr" { EXPR_DECL }
  | "stmt" { STMT_DECL }

  (* misc *)
  | "," { COMMA  }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LCURL  }
  | "}" { RCURL  }
  | eof { EOF    }

  (* variables *)
  | id as x
      { ID x }
            
  (* ignore *)
  | comment    { token lexbuf }
  | whitespace { token lexbuf }
  | line       { incr line; token lexbuf }

  (* error *)
  | _ as c
      { failwith (mkstr "AstLexer: char %c on line %d" c !line) }


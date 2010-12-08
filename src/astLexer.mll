{ open AstParser }

let id = ['a'-'z''A'-'Z']+

let intlit = "0" | '-'?['1'-'9']['0'-'9']*

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

  (* unary operators *)
  | "!"  { NOT }

  (* binary operators *)
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
  | "true"
      { BOOLLIT true }
  | "false"
      { BOOLLIT false }
  | intlit as x
      { INTLIT (int_of_string x) }

  (* instructions *)
  | "nop"    { NOP    }
  | "="      { ASSIGN }
  | "assume" { ASSUME }

  (* control flow *)
  | ";"     { SEMI  }
  | "if"    { IF    }
  | "else"  { ELSE  }
  | "while" { WHILE }
  | "for"   { FOR   }

  (* variables *)
  | id as x
      { ID x }

  (* misc *)
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LCURL  }
  | "}" { RCURL  }


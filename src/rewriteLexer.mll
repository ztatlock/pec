{

open ZPervasives
open Prog
open RewriteParser 

}

let id =
  ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']*

let intlit =
  "0" | '-'?['1'-'9']['0'-'9']*

let comment =
  "#"[^'\n']*

let whitespace =
  [' ' '\t']

let line =
  '\n'

rule token = parse
  (* pattern delimeters *)
  | "---" { FIND    }
  | "+++" { REPLACE }

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
  | intlit as x { INTLIT (int_of_string x) }
  | "true"      { TRUE                     }
  | "false"     { FALSE                    }

  (* instructions *)
  | "nop"    { NOP    }
  | "="      { ASSIGN }
  | "assume" { ASSUME }
  | "where"  { WHERE  }

  (* side conditions *)
  | "noread"    { NOREAD    }
  | "nowrite"   { NOWRITE   }
  | "noaffect"  { NOAFFECT  }
  | "nodisturb" { NODISTURB }

  (* control flow *)
  | ";"     { SEMI  }
  | "if"    { IF    }
  | "else"  { ELSE  }
  | "while" { WHILE }
  | "for"   { FOR   }

  (* declarations *)
  | "orig" { ORIG_DECL }
  | "temp" { TEMP_DECL }
  | "expr" { EXPR_DECL }
  | "stmt" { STMT_DECL }

  (* misc *)
  | "," { COMMA  }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LCURL  }
  | "}" { RCURL  }
  | eof { EOF    }

  (* variables *)
  | id as x { ID x }
            
  (* ignore *)
  | comment    { token lexbuf }
  | whitespace { token lexbuf }
  | line       { incr line; token lexbuf }

  (* error *)
  | _ as c
      { failwith (mkstr "AstLexer: char %c on line %d" c !line) }


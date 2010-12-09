(* rewrite.ml : Rewrite Rule Representation *)

open Common.ZPervasives

(* common representation for all inference and checking passes
 * should live in Prog but causes circular module dependencies
 *)

type rewrite =
  { cfgl   : Prog.cfg
  ; cfgr   : Prog.cfg
  ; paths  : (Prog.path * Prog.path) list
  ; simrel : Logic.simrel 
  }

let mk_rewrite (l, r) =
  { cfgl   = l
  ; cfgr   = r
  ; paths  = []
  ; simrel = []
  }

let parse file =
  file |> Common.file_str
       |> Lexing.from_string
       |> RewriteParser.rewrite RewriteLexer.token
       |> pair_map Prog.ast_cfg
       |> mk_rewrite


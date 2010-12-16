(* rewrite.ml : Rewrite Rule Representation *)

open Common.ZPervasives

(* common representation for all inference and checking passes
 * should live in Prog but causes circular module dependencies
 *)

type rewrite =
  { cfgl  : Prog.cfg
  ; cfgr  : Prog.cfg
  ; paths : (Prog.path * Prog.path) list
  ; mutable simrel : Logic.simrel
  }

let mk_rewrite (l, r) =
  { cfgl   = l
  ; cfgr   = r
  ; paths  = []
  ; simrel = []
  }

let set_paths rwr ps =
  { rwr with paths = ps }

let set_simrel rwr sr =
  { rwr with simrel = sr }

let update_simrel rwr np sp =
  rwr.simrel <- (np, sp) :: rwr.simrel

(* logging *)

let log_cfgs rwr =
  Common.log ">>> CFG Dot Reprs";
  rwr.cfgl
    |> Prog.cfg_dot ~nm:"left"
    |> Common.log;
  rwr.cfgr
    |> Prog.cfg_dot ~nm:"right"
    |> Common.log

let log_paths rwr =
  Common.log ">>> Synched Path Progs";
  rwr.paths
    |> List.map Prog.path_pair_str
    |> String.concat "\n:::\n\n"
    |> Common.log

let simrel_entry_str ((l, r), f) =
  mkstr "  %2d %2d : %s"
    l.Prog.nid r.Prog.nid (Logic.form_simp f)

let log_simrel rwr =
  Common.log ">>> Init Simrel";
  rwr.simrel
    |> List.map simrel_entry_str
    |> String.concat "\n"
    |> Common.log

(* utilities *)

let simrel_entry rwr np =
  try
    List.assoc np rwr.simrel
  with Not_found ->
    failwith
      (mkstr "no simrel entry at: (%d, %d)"
        (fst np).Prog.nid
        (snd np).Prog.nid)

let parse file =
  file |> Common.file_str
       |> Lexing.from_string
       |> RewriteParser.rewrite RewriteLexer.token
       |> pair_map Prog.ast_cfg
       |> mk_rewrite


(* rewrite.ml : Rewrite Rule Representation *)

open ZPervasives

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

let paths rwr =
  rwr.paths

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

(* logging *)

let log_cfgs rwr =
  Common.log ">>> CFG Dot Reprs";
  rwr.cfgl
    |> Prog.cfg_dot ~nm:"left"
    |> Common.log;
  rwr.cfgr
    |> Prog.cfg_dot ~nm:"right"
    |> Common.log

let path_pair_str (l, r) =
  let sl, sr =
    Prog.path_str l,
    Prog.path_str r
  in
  Common.side_by_side sl sr

let log_paths rwr =
  rwr.paths
    |> List.length
    |> mkstr ">>> Synched Path Progs (%d)"
    |> Common.log;
  rwr.paths
    |> List.map path_pair_str
    |> String.concat "\n\n"
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

(* dumping dot repr *)

let color_ctr =
  ref 0

let colors =
  [ "red"
  ; "green"
  ; "blue"
  ; "purple"
  ; "orange"
  ]

let next_color () =
  incr color_ctr;
  colors |> List.length
         |> fun n -> !color_ctr mod n
         |> List.nth colors

let simrel_edge_dot (n1, n2) =
  mkstr "  %2d -> %2d [%s];"
    n1.Prog.nid
    n2.Prog.nid
    (String.concat ", "
      [ "color=" ^ (next_color ())
      ; "arrowhead=none"
      ; "constraint=false"
      ])

let dot_str rwr =
  let ens =
    Prog.entries rwr.cfgl @
    Prog.entries rwr.cfgr
      |> List.map Prog.nid
      |> List.map (mkstr "%2d")
      |> String.concat "; "
  in
  let exs =
    Prog.exits rwr.cfgl @
    Prog.exits rwr.cfgr
      |> List.map Prog.nid
      |> List.map (mkstr "%2d")
      |> String.concat "; "
  in
  let edges =
    Prog.cfg_dot_body rwr.cfgl ^
    Prog.cfg_dot_body rwr.cfgr
  in
  let xs =
    rwr.simrel
      |> List.map fst
      |> List.map simrel_edge_dot
      |> String.concat "\n"
  in
  let ndprops =
    String.concat ", "
      [ "fontsize=10"
      ; "fontname=\"Courier\""
      ; "shape=box"
      ; "fixedsize=true"
      ; "height=0.4"
      ; "width=0.4"
      ]
  in
  let edprops =
    String.concat ", "
      [ "fontsize=10"
      ; "fontname=\"Courier\""
      ; "minlen=2"
      ]
  in
  String.concat "\n"
    [ "digraph {"
    ; "  node [" ^ ndprops ^ "];"
    ; "  edge [" ^ edprops ^ "];"
    ; "  {rank=source; " ^ ens ^ "}"
    ; "  {rank=sink;   " ^ exs ^ "}"
    ; edges
    ; xs
    ; "}\n"
    ]

let write_dot rwr =
  let f = Flags.get "dot" in
  if f <> "" then
    rwr |> dot_str
        |> Common.str_file f

let log rwr =
  rwr |> ff log_cfgs
      |> ff log_paths
      |> ff log_simrel
      |> ff write_dot


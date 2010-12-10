
open Common.ZPervasives

let step s0 p sN =
  let code =
    p |> Prog.path_edges
      |> List.map Prog.edge_instr
      |> List.map Prog.instr_str
      |> String.concat "; "
  in
  mkstr "%s { %s } %s"
    s0 code sN

let ck_paths rwr pp =
  let en =
    pp |> pair_map List.hd
       |> Rewrite.simrel_entry rwr
  in
  let ex =
    pp |> pair_map Common.last
       |> Rewrite.simrel_entry rwr
  in
  let l0 = Logic.mk_state "l0" in
  let lN = Logic.mk_state "lN" in
  let r0 = Logic.mk_state "r0" in
  let rN = Logic.mk_state "rN" in
  let query =
    Logic.imply
      (Logic.conj
        [ en l0 r0
        ; step l0 (fst pp) lN
        ; step r0 (snd pp) rN
        ])
      (ex lN rN)
  in
  Logic.is_valid query

let ck_rule rwr =
  List.for_all
    (ck_paths rwr)
    rwr.Rewrite.paths

let check rwr =
  ck_rule rwr


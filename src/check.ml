
open Common.ZPervasives

let ck_paths rwr pp =
  let en =
    pp |> pair_map List.hd
       |> Rewrite.simrel_entry rwr
  in
  let ex =
    pp |> pair_map Common.last
       |> Rewrite.simrel_entry rwr
  in
  let l0, r0 = Logic.start_states in
  let (lN, lsteps) = Semantics.step_path (fst pp) l0 in
  let (rN, rsteps) = Semantics.step_path (snd pp) r0 in
  let assumes =
    [ en l0 r0 ] @ lsteps @ rsteps
  in
  let query =
    Logic.imply
      (Logic.conj assumes)
      (ex lN rN)
  in
  Logic.is_valid query

let ck_rule rwr =
  List.for_all
    (ck_paths rwr)
    rwr.Rewrite.paths

let check rwr =
  ck_rule rwr


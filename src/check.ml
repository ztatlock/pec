
open Common.ZPervasives

let obligation rwr pp =
  let en, ex =
    pair_map List.hd     pp,
    pair_map Common.last pp
  in
  let top, bot =
    Rewrite.simrel_entry rwr en,
    Rewrite.simrel_entry rwr ex
  in
  let (lN, lsteps), (rN, rsteps) =
    Semantics.step_path (fst pp) Logic.start_l,
    Semantics.step_path (snd pp) Logic.start_r
  in
  let hyps =
    top :: Semantics.vars_distinct pp
        :: lsteps @ rsteps
  in
  let bot =
    bot |> Logic.replace_start_l lN
        |> Logic.replace_start_r rN
  in
  Logic.imply (Logic.conj hyps) bot

let strengthen rwr pp f =
  let en = pair_map List.hd pp in
  if not (SimRel.entry en) then begin
    Rewrite.update_simrel rwr en f
  end

let log pp obl =
  Common.log ">>> Checking Path Pair";
  Common.log (Rewrite.path_pair_str pp);
  Common.log (Logic.form_simp obl)

let ck_paths rwr pp =
  let obl = obligation rwr pp in
  log pp obl;
  if Logic.valid Semantics.axioms obl then begin
    Common.log "Passed.";
    true
  end else begin
    Common.log "Failed.";
    strengthen rwr pp obl;
    false
  end

let ck_rule rwr =
  List.for_all
    (ck_paths rwr)
    rwr.Rewrite.paths

let check rwr =
     ck_rule rwr
  || ck_rule rwr


open ZPervasives

(* compute formula's forall firing pattern *)
let pat f =
  let ss = Logic.form_states_nostart f in
  Logic.func
    (mkstr "pec_pat_%d" (List.length ss))
    (List.map Logic.state ss)

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
  let pat =
    Logic.eq
      (Logic.Int 1)
      (pat (Logic.conj (lsteps @ rsteps)))
  in
  let hyps =
    top :: Semantics.vars_distinct pp
        :: pat
        :: lsteps @ rsteps
  in
  let bot =
    bot |> Logic.replace_start_l lN
        |> Logic.replace_start_r rN
  in
  Logic.imply (Logic.conj hyps) bot

let strengthen rwr pp obl =
  let en = pair_map List.hd pp in
  if SimRel.entry en then
    ()
  else
    (* universally quantify over old obl states *)
    let vars =
      obl |> Logic.form_states_nostart
          |> List.map Logic.state_simp
    in
    let str =
      Logic.forallp
        vars
        (Some (pat obl))
        obl
    in
    en |> Rewrite.simrel_entry rwr
       |> Common.snoc [str]
       |> Logic.conj
       |> Logic.simplify
       |> Rewrite.update_simrel rwr en

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


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

(* track # of strengthenings and terminate upon reaching limit *)

exception GiveUp

let nstrength =
  ref 0

let reset_nstrength () =
  nstrength := 0

let too_strong () =
  let n =
    "strength" |> Flags.get
               |> int_of_string
  in
  !nstrength > n

let strengthen rwr pp obl =
  incr nstrength;
  let en = pair_map List.hd pp in
  if SimRel.entry en || too_strong () then
    raise GiveUp
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
       |> snoc [str]
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
    Common.log "Valid.";
    true
  end else begin
    Common.log "Invalid.";
    strengthen rwr pp obl;
    false
  end

(* does path pair pp2 precede pp1 ? *)
let precedes pp1 pp2 =
  let ens =
    pp1 |> pair_map List.hd
        |> pair_map Prog.nid
  in
  let exs =
    pp2 |> pair_map Common.last
        |> pair_map Prog.nid
  in
  ens = exs

let ck_entry rwr (pp, q) =
  if ck_paths rwr pp then
    q
  else
    (* put pp back on the queue *)
    let q = UniQueue.push q pp in
    (* push all path pairs that precede pp *)
    rwr |> Rewrite.paths
        |> List.filter (precedes pp)
        |> List.fold_left UniQueue.push q

let ck_rule rwr =
  let rec loop q =
    if UniQueue.is_empty q then
      true
    else
      q |> UniQueue.pop
        |> ck_entry rwr
        |> loop
  in
  rwr |> Rewrite.paths
      |> List.fold_left UniQueue.push UniQueue.empty
      |> loop

let check rwr =
  reset_nstrength ();
  try
    ck_rule rwr
  with GiveUp ->
    false


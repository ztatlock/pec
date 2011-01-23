(* simRel.ml :  Infer Simulation Relation Candidate *)

open ZPervasives

module type GUESSER = sig
  val guess : Prog.node * Prog.node -> Logic.form
end

module GuessTrue = struct
  let guess np =
    Logic.True
end

module GuessStateEq = struct
  let guess np =
    Logic.state_eq
end

module GuessOrigEquiv = struct
  let guess np =
    Logic.orig_equiv
end

(* analyses *)

let exec_neq_code (l, r) =
  match
    Prog.succ_instrs l,
    Prog.succ_instrs r
  with
  | [Prog.Code (sl, _)],
    [Prog.Code (sr, _)] ->
      sl <> sr
  | _ ->
      false

(* if n's sole predecessor is Assume C then *)
(* we know C holds in any state reaching n  *)
let pred_assume s n =
  match Prog.pred_instrs n with
  | [Prog.Assume c] ->
      c |> Semantics.eval_expr s
        |> fst (* project out value *)
        |> Logic.neq (Logic.Int 0)
  | _ ->
      Logic.True

let pred_assumes (l, r) =
  Logic.conj
    [ pred_assume Logic.start_l l
    ; pred_assume Logic.start_r r
    ]

let strong_post_path sN p =
  let s0 =
    Logic.next_state sN
  in
  let s, links =
    Semantics.step_path p s0
  in
  let f =
    links
      |> Logic.conj
      |> Logic.replace_state s sN
  in
  if s = s0 then
    f
  else
    let vars =
      f |> Logic.form_states
        |> List.filter (fun s -> s <> sN)
        |> List.map Logic.state_simp
    in
    Logic.exists vars f

let log_path p =
  p |> Prog.path_str
    |> Common.log

let log_form f =
  Common.log "# initial formula";
  f |> Logic.form_simp
    |> Common.log;
  Common.log "# simplified";
  f |> Logic.simplify
    |> Logic.form_simp
    |> Common.log

let strong_post sN n =
  let sticky x =
    Prog.is_entry  x ||
    Prog.is_branch x
  in
  n |> Prog.nid
    |> mkstr ">>> Strongest Post Condition for Node %d"
    |> Common.log;
  n |> Prog.paths_near_ancs_st sticky
    |> ff (List.iter log_path)
    |> List.map (strong_post_path sN)
    |> Logic.disj
    |> ff log_form

let strong_posts (l, r) =
  Logic.conj
    [ strong_post Logic.start_l l
    ; strong_post Logic.start_r r
    ]

let ancestor_assigns_temp n =
  let assigns_temp_i = function
    | Prog.Assign (Prog.Temp _, _) -> true
    | _ -> false
  in
  let assigns_temp n =
    n |> Prog.succ_instrs
      |> List.exists assigns_temp_i
  in
  n |> Prog.ancestors
    |> List.exists assigns_temp

let guess_eq (l, r) =
  if ancestor_assigns_temp l
  || ancestor_assigns_temp r then
    Logic.orig_equiv
  else
    Logic.state_eq

module Guess1 = struct
  let guess np =
    if exec_neq_code np then
      Logic.False
    else
      Logic.state_eq
end

module Guess2 = struct
  let guess np =
    if exec_neq_code np then
      Logic.False
    else
      Logic.conj
        [ Logic.state_eq
        ; pred_assumes np
        ]
end

module Guess3 = struct
  let guess np =
    if exec_neq_code np then
      Logic.False
    else
      Logic.conj
        [ guess_eq np
        ; pred_assumes np
        ]
end

module Guess4 = struct
  let guess np =
    if exec_neq_code np then
      Logic.False
    else
      Logic.conj
        [ guess_eq np
        ; strong_posts np
        ]
end

module GenRel(Guesser: GUESSER) = struct
  let synch_points paths =
    let ens, exs =
      List.map (pair_map List.hd)     paths,
      List.map (pair_map Common.last) paths
    in
    Common.uniq (ens @ exs)

  let invalid (l, r) =
    Prog.is_entry l <> Prog.is_entry r ||
    Prog.is_exit  l <> Prog.is_exit  r

  let guess_invariant np =
    if pair_forall Prog.is_entry np then
      Logic.state_eq
    else if pair_forall Prog.is_exit np then
      Logic.orig_equiv
    else if invalid np then
      Logic.False
    else
      Guesser.guess np

  let guess_entry np =
    let i =
      np |> guess_invariant
         |> Logic.simplify
    in
    (np, i)

  let generate rwr =
    rwr |> Rewrite.paths
        |> synch_points
        |> List.map guess_entry
end

module GR = GenRel(Guess4)

let infer rwr =
  rwr |> GR.generate
      |> Rewrite.set_simrel rwr


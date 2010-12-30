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

module Guess0 = struct
  let neq_code_params (l, r) =
    let il, ir =
      Prog.succ_instrs l,
      Prog.succ_instrs r
    in
    match il, ir with
    | [Prog.Code (sl, _)],
      [Prog.Code (sr, _)] ->
        sl <> sr
    | _ ->
        false

  let recent_assume s n =
    match Prog.pred_instrs n with
    | [Prog.Assume c] ->
        c |> Semantics.eval_expr s
          |> fst
          |> Logic.neq (Logic.Int 0)
    | _ ->
        Logic.True

  let recent_assumes (l, r) =
    Logic.conj
      [ Logic.state_eq
      ; recent_assume Logic.start_l l
      ; recent_assume Logic.start_r r
      ]

  let guess np =
    if neq_code_params np then
      Logic.False
    else
      recent_assumes np
end

module GenRel(Guesser: GUESSER) = struct

  let synch_points paths =
    let ens, exs =
      List.map (pair_map List.hd)     paths,
      List.map (pair_map Common.last) paths
    in
    Common.uniq (ens @ exs)

  let invalid (l, r) =
    Prog.entry l <> Prog.entry r ||
    Prog.exit  l <> Prog.exit  r

  let guess_invariant np =
    if pair_for_all Prog.entry np then
      Logic.state_eq
    else if pair_for_all Prog.exit np then
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

module GR = GenRel(Guess0)

let infer rwr =
  rwr |> GR.generate
      |> Rewrite.set_simrel rwr


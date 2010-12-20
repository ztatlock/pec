
open Common.ZPervasives

let synch_points paths =
  let ens =
    List.map
      (pair_map List.hd)
      paths
  in
  let exs =
    List.map
      (pair_map Common.last)
      paths
  in
  Common.uniq (ens @ exs)

let entry (l, r) =
  Prog.entry l &&
  Prog.entry r

let exit (l, r) =
  Prog.exit l &&
  Prog.exit r

let invalid (l, r) =
  Prog.entry l <> Prog.entry r ||
  Prog.exit  l <> Prog.exit  r

let recent_assume s n =
  let ins =
    List.map
      Prog.edge_instr
      n.Prog.in_edges
  in
  match ins with
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

let guess_invariant np =
  if entry np then
    Logic.state_eq
  else if exit np then
    Logic.state_eq
  else if invalid np then
    Logic.False
  else
    recent_assumes np

let guess_entry np =
  let i =
    np |> guess_invariant
       |> Logic.simplify
  in
  (np, i)

let infer rwr =
  let sr =
    rwr.Rewrite.paths
      |> synch_points
      |> List.map guess_entry
  in
  Rewrite.set_simrel rwr sr


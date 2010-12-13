
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
  Prog.is_entry l &&
  Prog.is_entry r

let exit (l, r) =
  Prog.is_exit l &&
  Prog.is_exit r

let invalid (l, r) =
  Prog.is_exit l <> Prog.is_exit r

let guess_invariant np =
  if entry np then
    Logic.state_eq
  else if exit np then
    Logic.state_eq
  else if invalid np then
    Logic.state_false
  else
    Logic.state_eq

let guess_entry np =
  (np, guess_invariant np)

let infer rwr =
  let sr =
    rwr.Rewrite.paths
      |> synch_points
      |> List.map guess_entry
  in
  Rewrite.set_simrel rwr sr


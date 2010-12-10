
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

let guess_invariant np =
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


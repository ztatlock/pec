
open Common.ZPervasives

type state = string

type formula = string

type state_pred =
  state -> state -> formula

type simrel =
  ((Prog.node * Prog.node) * state_pred) list

let imply a b =
  mkstr "(IMPLIES\n  %s\n  %s)" a b

let conj fs =
  fs |> String.concat "\n    "
     |> mkstr "(AND\n    %s)"

let mk_state s =
  mkstr "state_%s" s

let state_eq s1 s2 =
  mkstr "%s = %s" s1 s2

let is_valid f =
  print "\n\n%s\n\nvalid : " f;
  read_line () = "y"


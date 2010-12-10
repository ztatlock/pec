
open Common.ZPervasives

type state = string

type formula = string

type state_pred =
  state -> state -> formula

type simrel =
  ((Prog.node * Prog.node) * state_pred) list


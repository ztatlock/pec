
type state = string

type formula = string

type state_pred =
  state -> state -> formula

type simrel =
  (NodePair.t * state_pred) list


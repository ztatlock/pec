
open Common.ZPervasives
open Logic

let pvar = function
  | Prog.Orig v ->
      func "Orig" [var v]
  | Prog.Temp v ->
      func "Temp" [var v]

let assign v e =
  func "Assign" [pvar v; e]

let lkup s v =
  func "lkup" [state s; pvar v]

let eval s id =
  func "eval" [state s; var id]

let step s i =
  func "step" [s; i]

let rec eval_expr s = function
  | Prog.IntLit i ->
      Int i
  | Prog.BoolLit b ->
      if b then
        Int 1
      else
        Int 0
  | Prog.Var v ->
      lkup s v
  | Prog.Unop (o, e) ->
      func (Prog.unop_str o)
        [eval_expr s e]
  | Prog.Binop (o, l, r) ->
      func (Prog.binop_str o)
        [ eval_expr s l
        ; eval_expr s r
        ]
  | Prog.ExprParam id ->
      eval s id

let apply_side_cond s0 s1 = function
  | Prog.NoRead v ->
      (* TODO *)
      True
  | Prog.NoWrite v ->
      (eq (lkup s0 v)
          (lkup s1 v))
  | Prog.NoAffect e ->
      (eq (eval_expr s0 e)
          (eval_expr s1 e))
  | Prog.Commutes e ->
      (* TODO *)
      True

let step_instr (s0, lns) = function
  | Prog.Nop ->
      (s0, lns)
  | Prog.Assign (v, e) ->
      let s1 = next_state s0 in
      let ln =
        (eq (state s1)
            (step (state s0)
                  (assign v (eval_expr s0 e))))
      in
      (s1, ln::lns)
  | Prog.Assume e ->
      let ln =
        (neq (Int 0)
             (eval_expr s0 e))
      in
      (s0, ln::lns)
  | Prog.StmtParam (id, side_conds) ->
      let s1 = next_state s0 in
      let ln =
        (eq (state s1)
            (step (state s0)
                  (var id)))
      in
      let side_lns =
        List.map
          (apply_side_cond s0 s1)
          side_conds
      in
      (s1, side_lns @ ln::lns)

let step_path p s =
  p |> Prog.path_edges
    |> List.map Prog.edge_instr
    |> List.fold_left step_instr (s, [])
    |> fun (s, lns) -> (s, List.rev lns)


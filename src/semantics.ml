
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

let path_vars_distinct pp =
  pp |> Prog.path_pair_vars
     |> List.map pvar
     |> pred "DISTINCT"


(* TODO encode axioms below in the logic, not strings *)

(* step axioms *)

let step_assign_var_eq =
  [ "(FORALL (state var expr)"
  ; "  (EQ (lkup (step state (Assign var expr)) var)"
  ; "      expr))"
  ]

let step_assign_var_neq =
  [ "(FORALL (state var1 var2 expr)"
  ; "  (IMPLIES (NEQ var1 var2)"
  ; "           (EQ (lkup (step state (Assign var1 expr)) var2)"
  ; "               (lkup state var2))))"
  ]

(* binop axioms *)

let ax_lt =
  [ "(FORALL (expr1 expr2)"
  ; "  (IMPLIES (NEQ 0 (Lt expr1 expr2))"
  ; "           (< expr1 expr2)))"
  ]

let ax_lte =
  [ "(FORALL (expr1 expr2)"
  ; "  (IMPLIES (NEQ 0 (Lte expr1 expr2))"
  ; "           (<= expr1 expr2)))"
  ]

let ax_gt =
  [ "(FORALL (expr1 expr2)"
  ; "  (IMPLIES (NEQ 0 (Gt expr1 expr2))"
  ; "           (> expr1 expr2)))"
  ]

let ax_gte =
  [ "(FORALL (expr1 expr2)"
  ; "  (IMPLIES (NEQ 0 (Gte expr1 expr2))"
  ; "           (>= expr1 expr2)))"
  ]

let ax_not_lt_gte =
  [ "(FORALL (expr1 expr2)"
  ; "  (EQ (Not (Lt expr1 expr2))"
  ; "      (Gte expr1 expr2)))"
  ]

let ax_not_lte_gt =
  [ "(FORALL (expr1 expr2)"
  ; "  (EQ (Not (Lte expr1 expr2))"
  ; "      (Gt expr1 expr2)))"
  ]

let ax_not_gt_lte =
  [ "(FORALL (expr1 expr2)"
  ; "  (EQ (Not (Gt expr1 expr2))"
  ; "      (Lte expr1 expr2)))"
  ]

let ax_not_gte_lt =
  [ "(FORALL (expr1 expr2)"
  ; "  (EQ (Not (Gte expr1 expr2))"
  ; "      (Lt expr1 expr2)))"
  ]

(* all our assumptions *)

let axioms =
  [ step_assign_var_eq
  ; step_assign_var_neq
  ; ax_lt
  ; ax_lte
  ; ax_gt
  ; ax_gte
  ; ax_not_lt_gte
  ; ax_not_lte_gt
  ; ax_not_gt_lte
  ; ax_not_gte_lt
  ]
  |> List.map (String.concat "\n  ")
  |> List.map (mkstr "(BG_PUSH\n  %s\n)")
  |> String.concat "\n\n"
  |> mkstr ";; begin axioms\n\n%s\n\n;; end axioms\n\n"


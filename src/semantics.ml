
open Common.ZPervasives
open Logic

let pvar = function
  | Prog.Orig v -> func "Orig" [var v]
  | Prog.Temp v -> func "Temp" [var v]

let lkup s v   = func "lkup" [s; v]
let eval s e   = func "eval" [s; e]
let step s i   = func "step" [s; i]
let assign v e = func "Assign" [v; e]

(* evaluating expressions
 *
 * An expression E evaluates in state S.
 *
 * The result is a pair (V, SCS) where V represents the value E evalutates to in
 * S and SCS is a list of formulas encoding the side conditions encountered while
 * evaluating E in S.
 *
 *)

(* handle expression parameter side conditions *)
let apply_esc s e = function
  | Prog.NoRead v ->
      forall ["val"]
        (eq (eval (state s) (var e))
            (eval (step (state s) (assign (pvar v) (var "val"))) (var e)))

let rec eval_expr s = function
  | Prog.IntLit i ->
      ( Int i
      , []
      )
  | Prog.Var v ->
      ( lkup (state s) (pvar v)
      , []
      )
  | Prog.Unop (o, e) ->
      let (v, scs) =
        eval_expr s e
      in
      ( func (Prog.unop_str o) [v]
      , scs
      )
  | Prog.Binop (o, l, r) ->
      let (vl, scsl), (vr, scsr) =
        eval_expr s l,
        eval_expr s r
      in
      ( func (Prog.binop_str o) [vl; vr]
      , scsl @ scsr
      )
  | Prog.Expr (Prog.EP (e, escs)) ->
      ( eval (state s) (var e)
      , List.map (apply_esc s e) escs
      )

(* stepping instructions 
 *
 * An instruction I steps state S1 to the next state, S2.
 *
 * The result is a pair (S2, LINKS) where S2 represents the result state of
 * stepping I in S1 and LINKS is a list of formulas encoding relationship of S1
 * to S2 and the side conditions encountered while stepping S1 to S2.
 *
 *)

(* handle code parameter side conditions *)
let apply_csc s1 s2 c = function
  | Prog.NoWrite v ->
      eq (lkup (state s1) (pvar v))
         (lkup (state s2) (pvar v))
  | Prog.NoAffect (Prog.EP (e, _)) ->
      eq (eval (state s1) (var e))
         (eval (state s2) (var e))

let step_instr (s1, links) = function
  | Prog.Nop ->
      ( s1
      , links
      )
  | Prog.Assign (pv, e) ->
      let (v, scs) =
        eval_expr s1 e
      in
      let s2 =
        next_state s1
      in
      let ln =
        eq (state s2)
           (step (state s1)
                 (assign (pvar pv) v))
      in
      ( s2
      , scs @ ln :: links
      )
  | Prog.Assume e ->
      let (v, scs) =
        eval_expr s1 e
      in
      let ln =
        neq (Int 0) v
      in
      ( s1
      , scs @ ln :: links
      )
  | Prog.Code (Prog.CP (c, cscs)) ->
      let s2 = next_state s1 in
      let ln =
        eq (state s2)
           (step (state s1) (var c))
      in
      let scs =
        List.map (apply_csc s1 s2 c) cscs
      in
      ( s2
      , scs @ ln :: links
      )

let step_path p s =
  p |> Prog.path_edges
    |> List.map Prog.edge_instr
    |> List.fold_left step_instr (s, [])
    |> fun (s, lns) -> (s, List.rev lns)

let vars_distinct (l, r) =
  let vl, vr =
    Prog.path_vars l,
    Prog.path_vars r
  in
  (vl @ vr) |> Common.uniq
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

let ax_add =
  [ "(FORALL (expr1 expr2)"
  ; "  (EQ (Add expr1 expr2)"
  ; "      (+ expr1 expr2)))"
  ]

let ax_sub =
  [ "(FORALL (expr1 expr2)"
  ; "  (EQ (Sub expr1 expr2)"
  ; "      (- expr1 expr2)))"
  ]

let ax_mul =
  [ "(FORALL (expr1 expr2)"
  ; "  (EQ (Mul expr1 expr2)"
  ; "      (* expr1 expr2)))"
  ]

let ax_div =
  [ "(FORALL (expr1 expr2)"
  ; "  (EQ (Div expr1 expr2)"
  ; "      (/ expr1 expr2)))"
  ]

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
  ; ax_add
  ; ax_sub
  ; ax_mul
  ; ax_div
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


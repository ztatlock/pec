open ZPervasives
open Logic

let lkup s v          = func "lkup" [state s; pvar v]
let eval s e          = func "eval" [state s; pexpr e]
let step s i          = func "step" [state s; i]
let assign v e        = func "Assign" [pvar v; e]
let noread s e v      = pred "noread" [state s; pexpr e; pvar v]
let nowrite s c v     = pred "nowrite" [state s; pcode c []; pvar v]
let noaffect s c e    = pred "noaffect" [state s; pcode c []; pexpr e]
let nodisturb s c1 c2 = pred "nodisturb" [state s; pcode c1 []; pcode c2 []]

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
      noread s e v
  | _ as sc ->
      sc |> Prog.side_cond_pretty
         |> mkstr "Bogus expression side cond '%s'"
         |> failwith 

let rec eval_expr s = function
  | Prog.IntLit i ->
      (Int i, [])
  | Prog.Var v ->
      (lkup s v, [])
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
  | Prog.Expr (e, scs) ->
      ( eval s e
      , List.map (apply_esc s e) scs
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
let apply_csc s c = function
  | Prog.Skip ->
      True
  | Prog.NoWrite v ->
      nowrite s c v
  | Prog.NoAffect e ->
      noaffect s c e
  | Prog.NoDisturb c' ->
      nodisturb s c c'
  | _ as sc ->
      sc |> Prog.side_cond_pretty
         |> mkstr "Bogus code side cond '%s'"
         |> failwith 

let rec step_instr (s1, links) = function
  | Prog.Nop ->
      (s1, links)
  | Prog.Assign (pv, e) ->
      let s2     = next_state s1 in
      let v, scs = eval_expr s1 e in
      let ln     = eq (state s2) (step s1 (assign pv v)) in
      (s2, scs @ ln :: links)
  | Prog.Assume e ->
      let v, scs = eval_expr s1 e in
      let ln     = neq (Int 0) v in
      (s1, scs @ ln :: links)

  | Prog.Code (c, scs) ->
      let s2 = next_state s1 in
      let ln = eq (state s2) (step s1 (pcode c [])) in
      let ss = List.map (apply_csc s1 c) scs in
      (s2, ss @ ln :: links)

(*
  | Prog.Code (c, (v,e)::eps, scs) ->
      let t  = Prog.fresh_temp () in
      List.fold_left step_instr (s1, links)
        [ Prog.Assign (t, Prog.Var v)
        ; Prog.Assign (v, e)
        ; Prog.Code (c, eps, Prog.NoWrite(v) :: scs)
        ; Prog.Assign (v, Prog.Var t)
        ]
  | Prog.Code (c, [], scs) ->
      let s2   = next_state s1 in
      let ln   = eq (state s2) (step s1 (pcode c [])) in
      let sscs = List.map (apply_csc s1 c) scs in
      (s2, sscs @ ln :: links)
*)

(*
  | Prog.Code (c, eps, scs) ->
      let es, escs =
        eps |> List.map (eval_expr s1)
            |> List.split
      in
      let escs = List.flatten escs in
      let s2   = next_state s1 in
      let ln   = eq (state s2) (step s1 (pcode c es)) in
      let sscs = List.map (apply_csc s1 c) scs in
      (s2, escs @ sscs @ ln :: links)
*)


let step_path p s =
  p |> Prog.path_edges
    |> List.map Prog.edge_instr
    |> List.fold_left step_instr (s, [])
    |> fun (s, links) -> (s, List.rev links)

let vars_distinct (l, r) =
  Prog.path_vars l @
  Prog.path_vars r
    |> Common.uniq
    |> List.map pvar
    |> pred "DISTINCT"

(* background for reasoning about program executions *)

let background = "

;; ARRAYS

(BG_PUSH
  (FORALL (arr id val)
    (EQ (select (store arr id val) id) val))
)

(BG_PUSH
  (FORALL (id1 id2 arr val)
    (IMPLIES (NEQ id1 id2)
             (EQ (select (store arr id1 val) id2)
                 (select arr id2))))
)

;; LOCATIONS

(BG_PUSH
  (FORALL (x y)
    (NEQ (Orig x) (Temp y)))
)

;; STATES

(BG_PUSH
  (FORALL (o t)
    (EQ (orig (state o t)) o))
)

(BG_PUSH
  (FORALL (o t)
    (EQ (temp (state o t)) t))
)

(DEFPRED (orig_eq state1 state2)
  (EQ (orig state1)
      (orig state2))
)

(DEFPRED (orig_equiv state1 state2)
  (FORALL (id)
    (EQ (lkup state1 (Orig id))
        (lkup state2 (Orig id))))
)

(BG_PUSH
  (FORALL (state1 state2)
    (IMPLIES (orig_eq state1 state2)
             (orig_equiv state2 state2)))
)

;; LKUP

(BG_PUSH
  (FORALL (state1 id)
    (EQ (lkup state1 (Orig id))
        (select (orig state1) id)))
)

(BG_PUSH
  (FORALL (state1 id)
    (EQ (lkup state1 (Temp id))
        (select (temp state1) id)))
)

;; STEP

(BG_PUSH
  (FORALL (state1 id expr)
    (EQ (step state1 (Assign (Orig id) expr))
        (state (store (orig state1) id expr)
               (temp state1))))
)

(BG_PUSH
  (FORALL (state1 id expr)
    (EQ (step state1 (Assign (Temp id) expr))
        (state (orig state1)
               (store (temp state1) id expr))))
)

(BG_PUSH
  (FORALL (state1 state2 stmt)
    (IMPLIES
      (orig_equiv state1 state2)
      (orig_equiv (step state1 stmt)
                  (step state2 stmt))))
)

;; CODE PARAMS

(BG_PUSH
  (FORALL (state1 code)
    (EQ (temp (step state1 (PCode code)))
        (temp state1)))
)

(BG_PUSH
  (FORALL (state1 code id)
    (EQ (lkup (step state1 (PCode code)) (Temp id))
        (lkup state1 (Temp id))))
)

;; EXPR PARAMS
   
(BG_PUSH
  (FORALL (state1 state2 expr)
    (IMPLIES (orig_equiv state1 state2)
             (EQ (eval state1 (PExpr expr))
                 (eval state2 (PExpr expr)))))
)

(BG_PUSH
  (FORALL (state1 expr x)
    (EQ (eval state1 (PExpr expr))
        (eval (state (orig state1) x) (PExpr expr))))
)

;; SIDE CONDITIONS

(DEFPRED (noread state1 expr var)
  (FORALL (val)
      (EQ (eval state1 expr)
          (eval (step state1 (Assign var val)) expr)))
)

(BG_PUSH
  (FORALL (state1 state2 expr var val)
    (IMPLIES
      (AND (EQ (step state1 (Assign var val)) state2)
           (noread state2 expr var))
      (EQ (eval state1 expr)
          (eval state2 expr))))
)

(DEFPRED (nowrite state1 stmt var)
  (EQ (lkup state1 var)
      (lkup (step state1 stmt) var))
)

(DEFPRED (noaffect state1 stmt expr)
  (EQ (eval state1 expr)
      (eval (step state1 stmt) expr))
)

(DEFPRED (nodisturb stateCur stmt1 stmt2)
  (FORALL (statePrev)
    (IMPLIES (EQ stateCur (step statePrev stmt2))
             (EQ (step (step stateCur stmt1) stmt2)
                 (step stateCur stmt1))))
)

;; OPERATORS

(BG_PUSH
  (FORALL (expr)
    (IMPLIES (NEQ 0 (Not expr))
             (EQ 0 expr)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Add expr1 expr2)
        (+ expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Sub expr1 expr2)
        (- expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Mul expr1 expr2)
        (* expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Div expr1 expr2)
        (/ expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (IMPLIES (NEQ 0 (Lt expr1 expr2))
             (< expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (IMPLIES (NEQ 0 (Lte expr1 expr2))
             (<= expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (IMPLIES (NEQ 0 (Gt expr1 expr2))
             (> expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (IMPLIES (NEQ 0 (Gte expr1 expr2))
             (>= expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Not (Lt expr1 expr2))
        (Gte expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Not (Lte expr1 expr2))
        (Gt expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Not (Gt expr1 expr2))
        (Lte expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Not (Gte expr1 expr2))
        (Lt expr1 expr2)))
)

;; END BACKGROUND

"

let old_background = "
;; STATE EQUIVALENCE

(DEFPRED (orig_equiv state1 state2)
  (FORALL (id)
    (EQ (lkup state1 (Orig id))
        (lkup state2 (Orig id))))
)

;; SIDE CONDITIONS

(DEFPRED (noread state expr var)
  (FORALL (val)
      (EQ (eval state expr)
          (eval (step state (Assign var val)) expr)))
)

(DEFPRED (nowrite state stmt var)
  (EQ (lkup state var)
      (lkup (step state stmt) var))
)

(DEFPRED (noaffect state stmt expr)
  (EQ (eval state expr)
      (eval (step state stmt) expr))
)

(DEFPRED (nodisturb stateCur stmt1 stmt2)
  (FORALL (statePrev)
    (IMPLIES (EQ stateCur (step statePrev stmt2))
             (EQ (step (step stateCur stmt1) stmt2)
                 (step stateCur stmt1))))
)

;; SEMANTICS

; orig and temp locations never overlap
(BG_PUSH
  (FORALL (x y) (NEQ (Orig x) (Temp y)))
)

; if you assign V = E and then look up V, you get E
(BG_PUSH
  (FORALL (state var expr)
    (EQ (lkup (step state (Assign var expr)) var) expr))
)

; if you assign V1 = E, nothing changes for V2 <> V1
(BG_PUSH
  (FORALL (state var1 var2 expr)
    (IMPLIES (NEQ var1 var2)
             (EQ (lkup (step state (Assign var1 expr)) var2)
                 (lkup state var2))))
)

; stepping the same statement preserves orig_equiv
(BG_PUSH
  (FORALL (state1 state2 stmt)
    (IMPLIES
      (orig_equiv state1 state2)
      (orig_equiv (step state1 stmt)
                  (step state2 stmt))))
)

; code params never write to temp locations
(BG_PUSH
  (FORALL (state stmt var)
    (EQ (lkup state (Temp var))
        (lkup (step state (PCode stmt)) (Temp var))))
)

; expr params only read orig locations
(BG_PUSH
  (FORALL (state1 state2 expr)
    (IMPLIES (orig_equiv state1 state2)
             (EQ (eval state1 (PExpr expr))
                 (eval state2 (PExpr expr)))))
)

; expr params never read temp locations
(BG_PUSH
  (FORALL (state expr var val)
    (EQ (eval state (PExpr expr))
        (eval (step state (Assign (Temp var) val)) (PExpr expr))))
)

; help z3 understand noread
(BG_PUSH
  (FORALL (state1 state2 expr var val)
    (IMPLIES
      (AND (EQ state2 (step state1 (Assign var val)))
           (noread state2 expr var))
      (EQ (eval state1 expr)
          (eval state2 expr))))
)

;; OPERATORS

(BG_PUSH
  (FORALL (expr)
    (IMPLIES (NEQ 0 (Not expr))
             (EQ 0 expr)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Add expr1 expr2)
        (+ expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Sub expr1 expr2)
        (- expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Mul expr1 expr2)
        (* expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Div expr1 expr2)
        (/ expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (IMPLIES (NEQ 0 (Lt expr1 expr2))
             (< expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (IMPLIES (NEQ 0 (Lte expr1 expr2))
             (<= expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (IMPLIES (NEQ 0 (Gt expr1 expr2))
             (> expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (IMPLIES (NEQ 0 (Gte expr1 expr2))
             (>= expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Not (Lt expr1 expr2))
        (Gte expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Not (Lte expr1 expr2))
        (Gt expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Not (Gt expr1 expr2))
        (Lte expr1 expr2)))
)

(BG_PUSH
  (FORALL (expr1 expr2)
    (EQ (Not (Gte expr1 expr2))
        (Lt expr1 expr2)))
)

;; END BACKGROUND

"


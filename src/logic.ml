
open ZPervasives

type state =
  | L of int
  | R of int

type term =
  | Int   of int
  | Var   of string
  | PVar  of Prog.var
  | PExpr of string
  | PCode of string * term list
  | State of state
  | Func  of string * term list

type form =
  | True
  | False
  | Eq     of term * term
  | Neq    of term * term
  | Pred   of string * term list
  | Conj   of form list
  | Disj   of form list
  | Imply  of form * form
  (* forall term option represents firing pattern *)
  | Forall of string list * term option * form
  | Exists of string list * form

(* curried constructors *)
let var v         = Var v
let pvar v        = PVar v
let pexpr e       = PExpr e
let pcode c eps   = PCode (c, eps)
let state s       = State s
let func f args   = Func (f, args)
let eq a b        = Eq (a, b)
let neq a b       = Neq (a, b)
let pred p args   = Pred (p, args)
let conj fs       = Conj fs
let disj fs       = Disj fs
let imply a b     = Imply (a, b)
let forall v f    = Forall (v, None, f)
let forallp v p f = Forall (v, p, f)
let exists v f    = Exists (v, f)

let start_l = L 0
let start_r = R 0

let next_state = function
  | L _ -> L (tock ())
  | R _ -> R (tock ())

let state_eq =
  eq (state start_l)
     (state start_r)

let orig_equiv =
  pred "orig_equiv"
    [ state start_l
    ; state start_r
    ]

(* list all the states mentioned in a formula *)

let rec term_states = function
  | Int _
  | Var _
  | PVar _
  | PExpr _
  | PCode _ ->
      []
  | State s ->
      [s]
  | Func (_, args) ->
      args |> List.map term_states
           |> List.flatten

let term_states t =
  t |> term_states
    |> Common.uniq

let rec form_states = function
  | True
  | False ->
      []
  | Eq (a, b)
  | Neq (a, b) ->
      (term_states a) @
      (term_states b)
  | Pred (_, args) ->
      args |> List.map term_states
           |> List.flatten
  | Conj fs
  | Disj fs ->
      fs |> List.map form_states
         |> List.flatten
  | Imply (a, b) ->
      (form_states a) @
      (form_states b)
  | Forall (_, _, f)
  | Exists (_, f) ->
      form_states f

let form_states f =
  f |> form_states
    |> Common.uniq

let form_states_nostart f =
  f |> form_states
    |> List.filter (fun s -> s <> start_l)
    |> List.filter (fun s -> s <> start_r)

(* Simplify repr, essentially sexprs *)

let state_simp = function
  | L i -> mkstr "l%d" i
  | R i -> mkstr "r%d" i

let rec term_simp = function
  | Int i ->
      mkstr "%d" i
  | Var v ->
      v
  | PVar (Prog.Orig v) ->
      mkstr "(Orig %s)" v
  | PVar (Prog.Temp v) ->
      mkstr "(Temp %s)" v
  | PExpr e ->
      mkstr "(PExpr %s)" e
  | PCode (c, []) ->
      mkstr "(PCode %s)" c
  | PCode (c, args) ->
      args |> List.map term_simp
           |> String.concat " "
           |> mkstr "(PCode (%s %s))" c
  | State s ->
      state_simp s
  | Func (f, args) ->
      args |> List.map term_simp
           |> String.concat " "
           |> mkstr "(%s %s)" f

let rec form_simp = function
  | True ->
      "TRUE"
  | False ->
      "FALSE"
  | Eq (a, b) ->
      mkstr "(EQ %s %s)"
        (term_simp a)
        (term_simp b)
  | Neq (a, b) ->
      mkstr "(NEQ %s %s)"
        (term_simp a)
        (term_simp b)
  | Pred (p, args) ->
      args |> List.map term_simp
           |> String.concat " "
           |> mkstr "(%s %s)" p
  | Conj fs ->
      fs |> List.map form_simp
         |> String.concat "\n\n"
         |> mkstr "(AND\n\n%s\n\n)"
  | Disj fs ->
      fs |> List.map form_simp
         |> String.concat "\n\n"
         |> mkstr "(OR\n\n%s\n\n)"
  | Imply (a, b) ->
      String.concat "\n\n"
        [ "(IMPLIES"
        ; form_simp a
        ; form_simp b
        ; ")"
        ]
  | Forall (vs, None, f) ->
      let v =
        String.concat " " vs
      in
      String.concat "\n\n"
        [ mkstr "(FORALL (%s)" v
        ; form_simp f
        ; ")"
        ]
  | Forall (vs, Some p, f) ->
      let v =
        String.concat " " vs
      in
      let pat =
        p |> term_simp
          |> mkstr "(PATS %s)" 
      in
      String.concat "\n\n"
        [ mkstr "(FORALL (%s)" v
        ; pat
        ; form_simp f
        ; ")"
        ]
  | Exists (vs, f) ->
      let v =
        String.concat " " vs
      in
      String.concat "\n\n"
        [ mkstr "(EXISTS (%s)" v
        ; form_simp f
        ; ")"
        ]

(* term replacement *)

let rec replace_term t1 t2 x =
  if x = t1 then
    t2
  else
    match x with
    | Int _
    | Var _
    | PVar _
    | PExpr _
    | PCode _
    | State _ ->
        x
    | Func (f, args) ->
        args |> List.map (replace_term t1 t2)
             |> func f

let rec replace t1 t2 = function
  | True ->
      True
  | False ->
      False
  | Eq (a, b) ->
      eq (replace_term t1 t2 a)
         (replace_term t1 t2 b)
  | Neq (a, b) ->
      neq (replace_term t1 t2 a)
          (replace_term t1 t2 b)
  | Pred (p, args) ->
      args |> List.map (replace_term t1 t2)
           |> pred p
  | Conj fs ->
      fs |> List.map (replace t1 t2)
         |> conj
  | Disj fs ->
      fs |> List.map (replace t1 t2)
         |> disj
  | Imply (a, b) ->
      imply (replace t1 t2 a)
            (replace t1 t2 b)
  | Forall (vs, po, f) ->
      forallp vs po (replace t1 t2 f)
  | Exists (vs, f) ->
      exists vs (replace t1 t2 f)

let replace_state s1 s2 =
  replace (state s1) (state s2)

let replace_start_l lN =
  replace_state start_l lN

let replace_start_r rN =
  replace_state start_r rN

(* simplify formula by simple axioms *)

let flatten_ands conjs =
  conjs |> List.map
             (function Conj fs -> fs
                     | _ as f  -> [f])
        |> List.flatten

let flatten_ors disjs =
  disjs |> List.map
             (function Disj fs -> fs
                     | _ as f  -> [f])
        |> List.flatten

let rec simp = function
  | True
  | False
  | Eq _
  | Neq _
  | Pred _ as f ->
      f
  | Conj [] ->
      True
  | Conj [f] ->
      f
  | Conj fs ->
      if List.mem False fs then
        False
      else
        fs |> List.filter (fun f -> f <> True)
           |> flatten_ands
           |> List.map simp
           |> conj
  | Disj [] ->
      False
  | Disj [f] ->
      f
  | Disj fs ->
      if List.mem True fs then
        True
      else
        fs |> List.filter (fun f -> f <> False)
           |> flatten_ors
           |> List.map simp
           |> disj
  | Imply (True, f) ->
      f
  | Imply (False, _) ->
      True
  | Imply (a, b) ->
      imply (simp a) (simp b)
  | Forall (vs, po, f) ->
      forallp vs po (simp f)
  | Exists (vs, f) ->
      exists vs (simp f)

let rec simplify f1 =
  let f2 = simp f1 in
  if f2 = f1 then
    f2
  else
    simplify f2

(* dispatch atp query *)

let nQueries = ref 0

let log_query () =
  incr nQueries;
  !nQueries
    |> mkstr ">>> Query # %d"
    |> Common.log

let z3 query =
  log_query ();
  let f0, f1 =
    mkstr "%s/z3-input.simplify"  (Flags.get "scratch"),
    mkstr "%s/z3-output" (Flags.get "scratch")
  in
  (* run z3 on input f0, send output to f1, check result *)
  Common.str_file f0 query;
  let repo = Unix.getenv "PEC" in
  mkstr "%s/script/z3-linux-x86 -s %s > %s" repo f0 f1
    |> Unix.system
    |> ignore;
  Common.file_str f1 = "1: Valid."

let valid prelude form =
  form |> simplify
       |> form_simp
       |> mkstr "%s\n%s\n" prelude
       |> z3



open Common.ZPervasives

type state =
  | L of int
  | R of int

type term =
  | Int   of int
  | Var   of string
  | State of state
  | Func  of string * term list

type form =
  | True
  | False
  | Eq     of term * term
  | Neq    of term * term
  | Conj   of form list
  | Imply  of form * form
  | Pred   of string * term list
  | Forall of string * form

type simrel =
  ((Prog.node * Prog.node) * form) list

(* curried constructors *)
let var v       = Var v
let state s     = State s
let func f args = Func (f, args)
let eq a b      = Eq (a, b)
let neq a b     = Neq (a, b)
let conj fs     = Conj fs
let imply a b   = Imply (a, b) 
let pred p args = Pred (p, args)
let forall v f  = Forall (v, f)

let start_l = L 0
let start_r = R 0

let next_state = function
  | L _ -> L (tock ())
  | R _ -> R (tock ())

let state_eq =
  eq (state start_l)
     (state start_r)

(* simplify repr, essentially sexprs *)

let state_simp = function
  | L i -> mkstr "l%d" i
  | R i -> mkstr "r%d" i

let rec term_simp = function
  | Int i ->
      mkstr "%d" i
  | Var v ->
      v
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
  | Conj fs ->
      fs |> List.map form_simp
         |> String.concat "\n\n"
         |> mkstr "(AND\n\n%s\n\n)"
  | Imply (a, b) ->
      mkstr "(IMPLIES\n\n%s\n\n%s\n\n)"
        (form_simp a)
        (form_simp b)
  | Pred (p, args) ->
      args |> List.map term_simp
           |> String.concat " "
           |> mkstr "(%s %s)" p
  | Forall (v, f) ->
      mkstr "(FORALL (%s) %s)" v
        (form_simp f)

(* term replacement *)

let rec replace_term t1 t2 x =
  if x = t1 then
    t2
  else
    match x with
    | Int _
    | Var _
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
  | Conj fs ->
      fs |> List.map (replace t1 t2)
         |> conj
  | Imply (a, b) ->
      imply (replace t1 t2 a)
            (replace t1 t2 b)
  | Pred (p, args) ->
      args |> List.map (replace_term t1 t2)
           |> pred p
  | Forall (v, f) ->
      forall v (replace t1 t2 f)

let replace_start_l lN =
  replace (state start_l)
          (state lN)

let replace_start_r rN =
  replace (state start_r)
          (state rN)

(* dispatch atp query *)

(* TODO : tighten this interface        *)
(*   use fresh I/O files for each query *)
(*   check process result               *)
let z3 axioms query =
  let f0, f1 =
    "/tmp/z3-input",
    "/tmp/z3-output"
  in
  Common.str_file f0 (axioms ^ "\n\n" ^ query);
  (* run z3 on input f0 and send output to f1 *)
  mkstr "z3 -s %s > %s" f0 f1
    |> Unix.system
    |> ignore;
  (* check result *)
  Common.readlines f1 = [ "1: Valid." ]

let valid axioms form =
  let v = z3 axioms (form_simp form) in
  if Flags.get "interactive" = "" then begin
    v
  end else begin
    print "\n\n%s\n\n" (form_simp form);
    print "z3 says \"%b\".\n" v;
    print "what do you say? ";
    read_line () = "true"
  end


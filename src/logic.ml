
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
  | Eq    of term * term
  | Neq   of term * term
  | Conj  of form list
  | Imply of form * form
  | Pred  of string * term list

type state_pred =
  state -> state -> form

type simrel =
  ((Prog.node * Prog.node) * state_pred) list

(* curried constructors *)
let var v       = Var v
let state s     = State s
let func f args = Func (f, args)
let eq a b      = Eq (a, b)
let neq a b     = Neq (a, b)
let conj fs     = Conj fs
let imply a b   = Imply (a, b) 
let pred p args = Pred (p, args)

let start_states =
  (L 0, R 0)

let next_state = function
  | L i -> L (i + 1)
  | R i -> R (i + 1)

let state_eq s1 s2 =
  (eq (state s1)
      (state s2))

let state_false s1 s2 =
  False

(* string reprs *)

let state_str = function
  | L i -> mkstr "l%02d" i
  | R i -> mkstr "r%02d" i

let rec term_str = function
  | Int i ->
      mkstr "%d" i
  | Var v ->
      v
  | State s ->
      state_str s
  | Func (f, args) ->
      args |> List.map term_str
           |> String.concat " "
           |> mkstr "(%s %s)" f

let rec form_str = function
  | True ->
      "TRUE"
  | False ->
      "FALSE"
  | Eq (a, b) ->
      mkstr "(EQ %s %s)"
        (term_str a)
        (term_str b)
  | Neq (a, b) ->
      mkstr "(NEQ %s %s)"
        (term_str a)
        (term_str b)
  | Conj fs ->
      fs |> List.map form_str
         |> String.concat "\n\n"
         |> mkstr "(AND\n\n%s\n\n)"
  | Imply (a, b) ->
      mkstr "(IMPLIES\n\n%s\n\n%s\n\n)"
        (form_str a)
        (form_str b)
  | Pred (p, args) ->
      args |> List.map term_str
           |> String.concat " "
           |> mkstr "(%s %s)" p

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

let is_valid axioms form =
  let v = z3 axioms (form_str form) in
  if Flags.get "interactive" = "" then begin
    v
  end else begin
    print "\n\n%s\n\n" (form_str form);
    print "z3 says \"%b\".\n" v;
    print "what do you say? ";
    read_line () = "true"
  end


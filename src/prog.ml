(* prog.ml : AST and CFG Program Representations *)

open Common.ZPervasives

type id = string

type side_cond =
  | Pure           (* does not change state *)
  | NoRead   of id (* does not read var *)
  | NoWrite  of id (* does not write var *)
  | NoAffect of id (* expr evals the same before and after *)
  | Commutes of id (* swapping exec order yields same state *)

type var =
  | Orig of id (* must be preserved *)
  | Temp of id (* fresh var, no need to preserve *)

(* Orig -- location used in input prog, ensure rewrite preserves
 * Temp -- fresh temporary introduced by rewrite, safe to ignore
 *
 * Essentially, a var should be Orig if it will still be live at the end of
 * matched code and Temp if it will be dead.
 *
 * If you use Orig when you should use Temp, then you might not be able to check
 * an otherwise valid rewrite rule. This is because Orig locations must be
 * preserved.
 *
 * If you use a Temp when you should use an Orig, then your rewrite may not be
 * able to apply when it would actually be valid. This is because the execution
 * engine has to prove that Temp locations are dead at the end of the
 * transformed region.
 *
 *)

type unop =
  | Not

type binop =
  | Or  | And | Eq  | Neq
  | Lt  | Lte | Gt  | Gte
  | Add | Sub | Mul | Div

type expr =
  | IntLit    of int
  | BoolLit   of bool
  | Var       of var
  | Unop      of unop * expr
  | Binop     of binop * expr * expr
  | ExprParam of id * side_cond list

type instr =
  | Nop
  | Assign    of var * expr
  | ExprInstr of expr
  | Assume    of expr
  | StmtParam of id * side_cond list
  (* TODO : StmtParam with holes : S[I] *)

type stmt =
  | Instr  of instr
  | Seq    of stmt * stmt
  | If     of expr * stmt
  | IfElse of expr * stmt * stmt
  | While  of expr * stmt
  | For    of for_header * stmt

and for_header =
  { init   : stmt
  ; guard  : expr
  ; update : stmt
  }

type ast =
  { root : stmt }

type node =
  { nid : int
  ; mutable in_edges  : edge list
  ; mutable out_edges : edge list
  }

and edge =
  { instr : instr
  ; src   : node
  ; snk   : node
  }

type cfg =
  { enter : node }

(* AST utilities *)

let assume c =
  Assume c

let nassume c =
  Assume (Unop (Not, c))

let desugar_for h b =
  Seq ( h.init
      , While ( h.guard
              , Seq ( b
                    , h.update)))

(* CFG utilities *)

let mknode () =
  { nid = tock ()
  ; in_edges  = []
  ; out_edges = []
  }

let add_in_edge e n =
  n.in_edges <- e :: n.in_edges

let add_out_edge e n =
  n.out_edges <- e :: n.out_edges

let add_edge m n i =
  let e =
    { instr = i
    ; src   = m
    ; snk   = n
    }
  in
  add_in_edge  e n;
  add_out_edge e m

let succs n =
  List.map
    (fun e -> e.snk)
    n.out_edges

(* convert AST to CFG *)

let ast_cfg a =
  let rec add stmt root =
    match stmt with
    | Instr i ->
        let n = mknode () in
        add_edge n root i;
        n
    | Seq (s1, s2) ->
        root |> add s2
             |> add s1
    | If (cond, sT) ->
        let nT = add sT root in
        let nF = root in
        let n  = mknode () in
        add_edge n nT (assume  cond);
        add_edge n nF (nassume cond);
        n
    | IfElse (cond, sT, sF) ->
        let nT = add sT root in
        let nF = add sF root in
        let n  = mknode () in
        add_edge n nT (assume  cond);
        add_edge n nF (nassume cond);
        n
    | While (cond, body) ->
        let n  = mknode() in
        let nT = add body n in
        let nF = root in
        add_edge n nT (assume  cond);
        add_edge n nF (nassume cond);
        n
    | For (header, body) ->
        add (desugar_for header body) root
  in
  { enter = add a.root (mknode ()) }

(* line tracking for AST lexer and parser *)

let line =
  ref 1

let reset_line () =
  line := 1


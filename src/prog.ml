(* prog.ml : AST and CFG Program Representations *)

open Common.ZPervasives

type id = string

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
  | ExprParam of id

type instr =
  | Nop
  | Assign    of var * expr
  | Assume    of expr
  | StmtParam of id * side_cond list
  (* TODO : StmtParam with holes : S[I] *)

and side_cond =
  | NoRead   of var   (* does not read var *)
  | NoWrite  of var   (* does not write var *)
  | NoAffect of expr  (* expr evals the same before and after *)
  | Commutes of instr (* swapping exec order yields same state *)

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

type path =
  node list

(* string representations *)

let var_str = function
  | Orig id -> mkstr "(Orig %s)" id
  | Temp id -> mkstr "(Temp %s)" id

let unop_str = function
  | Not -> "Not"

let binop_str = function
  | Or  -> "Or"
  | And -> "And"
  | Eq  -> "Eq"
  | Neq -> "Neq"
  | Lt  -> "Lt"
  | Lte -> "Lte"
  | Gt  -> "Gt"
  | Gte -> "Gte"
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"

let rec expr_str = function
  | IntLit i ->
      mkstr "(IntLit %d)" i
  | BoolLit b ->
      mkstr "(BoolLit %b)" b
  | Var v ->
      mkstr "(Var %s)"
        (var_str v)
  | Unop (op, e) ->
      mkstr "(UnOp %s %s)"
        (unop_str op)
        (expr_str e)
  | Binop (op, l, r) ->
      mkstr "(BinOp %s %s %s)"
        (binop_str op)
        (expr_str l)
        (expr_str r)
  | ExprParam id ->
      mkstr "(ExprParam %s)" id

let rec instr_str = function
  | Nop ->
      "Nop"
  | Assign (v, e) ->
      mkstr "(Assign %s %s)"
        (var_str v)
        (expr_str e)
  | Assume e ->
      mkstr "(Assume %s)"
        (expr_str e)
  | StmtParam (id, scs) ->
      scs |> List.map side_cond_str
          |> String.concat " "
          |> mkstr "(StmtParam %s %s)" id

and side_cond_str = function
  | NoRead v ->
      mkstr "(noread %s)" (var_str v)
  | NoWrite v ->
      mkstr "(nowrite %s)" (var_str v)
  | NoAffect e ->
      mkstr "(noaffect %s)" (expr_str e)
  | Commutes i ->
      mkstr "(commutes %s)" (instr_str i)

let rec stmt_str = function
  | Instr i ->
      mkstr "(Instr %s)"
        (instr_str i)
  | Seq (s1, s2) ->
      mkstr "(Seq %s %s)"
        (stmt_str s1)
        (stmt_str s2)
  | If (c, sT) ->
      mkstr "(If %s %s)"
        (expr_str c)
        (stmt_str sT)
  | IfElse (c, sT, sF) ->
      mkstr "(IfElse %s %s %s)"
        (expr_str c)
        (stmt_str sT)
        (stmt_str sF)
  | While (c, b) ->
      mkstr "(While %s %s)"
        (expr_str c)
        (stmt_str b)
  | For (h, b) ->
      mkstr "(For %s %s)"
        (for_header_str h)
        (stmt_str b)

and for_header_str h =
  mkstr "(ForHeader %s %s %s)"
    (stmt_str h.init)
    (expr_str h.guard)
    (stmt_str h.update)

let ast_str a =
  stmt_str a.root

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

let nid n =
  n.nid

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

(* path utilities *)

let fromto src snk =
  List.find
    (fun e -> e.src.nid = src.nid)
    snk.in_edges

let path_edges p =
  let srcs = Common.drop_last p in
  let snks = List.tl p in
  List.map2 fromto srcs snks

let edge_instr e =
  e.instr

(* line tracking for rewrite lexer and parser *)

let line =
  ref 1


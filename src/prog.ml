(* prog.ml : AST and CFG Program Representations *)

open ZPervasives

type var =
  | Orig of string (* must be preserved *)
  | Temp of string (* fresh var, no need to preserve *)

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
 *)

type side_cond =
  | NoRead   of var
  | NoWrite  of var
  | NoAffect of string (* expr evals same before and after *)

type unop =
  | Not

type binop =
  | Or  | And | Eq  | Neq
  | Lt  | Lte | Gt  | Gte
  | Add | Sub | Mul | Div

type expr =
  | IntLit of int
  | Var    of var
  | Unop   of unop * expr
  | Binop  of binop * expr * expr
  | Expr   of string * side_cond list

type instr =
  | Nop
  | Assign of var * expr
  | Assume of expr
  | Code   of string * side_cond list
  (* TODO : Code param with holes : S[I] *)
  (* PCode of string * expr list * side_cond list *)

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

let side_cond_str = function
  | NoRead v ->
      mkstr "(NoRead %s)" (var_str v)
  | NoWrite v ->
      mkstr "(NoWrite %s)" (var_str v)
  | NoAffect e ->
      mkstr "(NoAffect %s)" e

let unop_str = function
  | Not -> "Not"

let binop_str = function
  | Or  -> "Or"  | And -> "And" | Eq  -> "Eq"  | Neq -> "Neq"
  | Lt  -> "Lt"  | Lte -> "Lte" | Gt  -> "Gt"  | Gte -> "Gte"
  | Add -> "Add" | Sub -> "Sub" | Mul -> "Mul" | Div -> "Div"

let rec expr_str = function
  | IntLit i ->
      mkstr "(IntLit %d)" i
  | Var v ->
      mkstr "(Var %s)"
        (var_str v)
  | Unop (op, e) ->
      mkstr "(UnOp (%s, %s))"
        (unop_str op)
        (expr_str e)
  | Binop (op, l, r) ->
      mkstr "(BinOp (%s, %s, %s))"
        (binop_str op)
        (expr_str l)
        (expr_str r)
  | Expr (e, scs) ->
      scs |> List.map side_cond_str
          |> String.concat "; "
          |> mkstr "(Expr (%s, [%s]))" e

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
  | Code (c, scs) ->
      scs |> List.map side_cond_str
          |> String.concat "; "
          |> mkstr "(Code (%s, [%s]))" c

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

(* pretty string reprs *)

let var_pretty = function
  | Orig v -> v
  | Temp v -> v

let side_cond_pretty = function
  | NoRead v ->
      mkstr "noread(%s)" (var_pretty v)
  | NoWrite v ->
      mkstr "nowrite(%s)" (var_pretty v)
  | NoAffect e ->
      mkstr "noaffect(%s)" e

let unop_pretty = function
  | Not -> "!"

let binop_pretty = function
  | Or  -> "||" | And -> "&&" | Eq  -> "==" | Neq -> "!="
  | Lt  -> "<"  | Lte -> "<=" | Gt  -> ">"  | Gte -> ">="
  | Add -> "+"  | Sub -> "-"  | Mul -> "*"  | Div -> "/"

let rec expr_pretty = function
  | IntLit i ->
      mkstr "%d" i
  | Var v ->
      var_pretty v
  | Unop (op, e) ->
      mkstr "%s%s"
        (unop_pretty op)
        (expr_pretty e)
  (* TODO : omit unnecessary parens *)
  | Binop (op, l, r) ->
      mkstr "(%s %s %s)"
        (expr_pretty l)
        (binop_pretty op)
        (expr_pretty r)
  | Expr (e, []) ->
      e
  | Expr (e, scs) ->
      scs |> List.map side_cond_pretty
          |> String.concat ", "
          |> mkstr "%s where %s" e

let rec instr_pretty = function
  | Nop ->
      "nop"
  | Assign (v1, Binop (Add, Var v2, IntLit 1))
    when v1 = v2 ->
      mkstr "%s++"
        (var_pretty v1)
  | Assign (v, e) ->
      mkstr "%s = %s"
        (var_pretty v)
        (expr_pretty e)
  | Assume e ->
      expr_pretty e
  | Code (c, []) ->
      c
  | Code (c, scs) ->
      scs |> List.map side_cond_pretty
          |> String.concat ", "
          |> mkstr "%s where %s" c

(* AST utilities *)

let mkast s =
  { root = s }

let assume c =
  Assume c

let nassume c =
  Assume (Unop (Not, c))

let desugar_for h b =
  Seq ( h.init
      , While ( h.guard
              , Seq ( b
                    , h.update)))

let rec expr_vars = function
  | IntLit _ ->
      []
  | Var v ->
      [v]
  | Unop (_, e) ->
      expr_vars e
  | Binop (_, l, r) ->
      expr_vars l @
      expr_vars r
  | Expr _ ->
      []

let instr_vars = function
  | Nop ->
      []
  | Assign (v, e) ->
      v :: (expr_vars e)
  | Assume e ->
      expr_vars e
  | Code _ ->
      []

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

let cfg_edges g =
  let marks = ref [] in
  let edges = ref [] in
  let marked n =
    List.mem n.nid !marks
  in
  let mark n =
    marks := n.nid :: !marks
  in
  let add_edge e =
    edges := e :: !edges
  in
  let rec add n =
    if marked n then
      ()
    else begin
      mark n;
      n.out_edges
        |> ff (List.iter add_edge)
        |> List.map (fun e -> e.snk)
        |> List.iter add
    end
  in
  add g.enter;
  List.rev !edges

let cfg_nodes g =
  g |> cfg_edges
    |> List.map (fun e -> [e.src; e.snk])
    |> List.flatten
    |> Common.uniq

let nid n =
  n.nid

let edge_instr e =
  e.instr

let preds n =
  List.map (fun e -> e.src) n.in_edges

let succs n =
  List.map (fun e -> e.snk) n.out_edges

let pred_instrs n =
  List.map edge_instr n.in_edges

let succ_instrs n =
  List.map edge_instr n.out_edges

let entry n =
  n.in_edges = []

let exit n =
  n.out_edges = []

let entries g =
  g |> cfg_nodes
    |> List.filter entry

let exits g =
  g |> cfg_nodes
    |> List.filter exit

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
  (* support entry: make every CFG begin  *)
  (* with a node that has no predecessors *)
  let s = Seq (Instr Nop, a.root) in
  let n = add s (mknode ()) in
  { enter = n }

(* cfg dot repr *)

let edge_dot e =
  mkstr "  %2d -> %2d [label=\" %s\"];"
    e.src.nid
    e.snk.nid
    (instr_pretty e.instr)

let cfg_dot_body g =
  g |> cfg_edges
    |> List.map edge_dot
    |> String.concat "\n"

let cfg_dot ?(nm = "") g =
  g |> cfg_dot_body
    |> mkstr "digraph %s {\n%s\n}" nm

(* path utilities *)

let fromto src snk =
  List.find
    (fun e -> e.src.nid = src.nid)
    snk.in_edges

let path_edges p =
  let srcs = Common.drop_last p in
  let snks = List.tl p in
  List.map2 fromto srcs snks

let path_vars p =
  p |> path_edges
    |> List.map edge_instr
    |> List.map instr_vars
    |> List.flatten
    |> Common.uniq

let path_edge_str e =
  mkstr "  %2d %s"
    e.src.nid
    (instr_pretty e.instr)

let path_str p =
  mkstr "%s\n  %2d"
    (p |> path_edges
       |> List.map path_edge_str
       |> String.concat "\n")
    (p |> Common.last
       |> nid)
  
(* line tracking for rewrite lexer and parser *)

let line =
  ref 1


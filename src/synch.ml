(* synch.ml : Infer Synchronized Path Programs *)

open Common.ZPervasives

(* set to track pairs of nodes already searched *)

module NodePair = struct
  type t =
    Prog.node * Prog.node
  let compare (a, b) (c, d) =
    let (w, x) = (a.Prog.nid, b.Prog.nid) in
    let (y, z) = (c.Prog.nid, d.Prog.nid) in
    Pervasives.compare (w, x) (y, z)
end

module Marks = Set.Make(NodePair)

let marks =
  ref Marks.empty

let reset_marks () =
  marks := Marks.empty

let mark np =
  marks := Marks.add np !marks

let marked np =
  Marks.mem np !marks

(* boring: what we lump between synch points *)
(* ie things that are not worth stopping for *)

let rec is_boring_expr = function
  | Prog.IntLit _
  | Prog.BoolLit _
  | Prog.Var _ ->
      true
  | Prog.Unop (_, e) ->
      is_boring_expr e
  | Prog.Binop (_, l, r) ->
      is_boring_expr l &&
      is_boring_expr r
  | Prog.ExprParam _ ->
      false

let is_boring_instr = function
  | Prog.Nop ->
      true
  | Prog.Assign (_, e)
  | Prog.ExprInstr e
  | Prog.Assume e ->
      is_boring_expr e
  | Prog.StmtParam _ ->
      false

let is_boring n =
  n.Prog.out_edges <> [] &&
  n.Prog.out_edges
    |> List.map (fun e -> e.Prog.instr)
    |> List.for_all is_boring_instr

(* remember paths found *)

let paths =
  ref []

let reset_paths () =
  paths := []

let add_paths l r =
  paths := (l, r) :: !paths

(* infer synchronized path programs           *)
(* NOTE: left and right paths are reversed    *)
(*       last node @ head + effient addiition *)

let rec start l r =
  List.iter
    (fun (sl, sr) ->
      walk [sl; l] [sr; r])
    (Common.xprod
      (Prog.succs l)
      (Prog.succs r))

and walk pl pr =
  let l = List.hd pl in
  let r = List.hd pr in
  if marked (l, r) then
    ()
  else
    mark (l, r);
    if is_boring l then
      List.iter
        (fun sl -> walk (sl :: pl) pr)
        (Prog.succs l)
    else if is_boring r then
      List.iter
        (fun sr -> walk pl (sr :: pr))
        (Prog.succs r)
    else
      add_paths (List.rev pl) (List.rev pr);
      start l r

let infer l r =
  reset_marks ();
  reset_paths ();
  start l.Prog.enter r.Prog.enter;
  !paths


(* synch.ml : Infer Synchronized Path Programs *)

open ZPervasives

(* remember pairs of paths already seen *)

let paths : (Prog.path * Prog.path) list ref  =
  ref []

let reset_paths () =
  paths := []

let mark l r =
  paths := (l, r) :: !paths

(* use nid for lkup since node structs may be cyclic *)
let marked l r =
  let nids =
    pair_map (List.map Prog.nid)
  in
  List.exists
    (fun x -> nids x = nids (l, r))
    !paths

(* paths are reversed and stored in reverse order *)
(* see start/walk comments below for motivation   *)
let get_paths () =
  !paths |> List.map (pair_map List.rev)
         |> List.rev

(* boring: what we lump between synch points *)
(* ie things that are not worth stopping for *)

let boring_instr = function
  | Prog.Code _ -> false
  | _ -> true

let boring n =
  n.Prog.in_edges  <> [] &&
  n.Prog.out_edges <> [] &&
  n.Prog.out_edges
    |> List.map Prog.edge_instr
    |> List.for_all boring_instr

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
  if boring l then
    List.iter
      (fun sl -> walk (sl :: pl) pr)
      (Prog.succs l)
  else if boring r then
    List.iter
      (fun sr -> walk pl (sr :: pr))
      (Prog.succs r)
  else if marked pl pr then
    ()
  else begin
    mark pl pr;
    start l r
  end

let infer_paths rwr =
  (* initialize state *)
  reset_paths ();
  let enl, enr =
    rwr.Rewrite.cfgl.Prog.enter,
    rwr.Rewrite.cfgr.Prog.enter
  in
  (* run the inference *)
  start enl enr;
  get_paths ()

let infer rwr =
  rwr |> infer_paths
      |> Rewrite.set_paths rwr


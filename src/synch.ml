(* synch.ml : Infer Synchronized Path Programs *)

open ZPervasives

module type SKIPPER = sig
  val skip : Prog.node -> bool
end

module DefaultSkipper = struct
  (* stop before code parameters *)
  let skip_instr = function
    | Prog.Code _ -> false
    | _ -> true

  let skip n =
    n |> Prog.succ_instrs
      |> List.for_all skip_instr
end

module StraightSkipper = struct
  (* in straightline code -- don't stop for anything *)
  let skip n =
    true
end

module Walker(Skipper: SKIPPER) = struct

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
  let get_paths () =
    !paths |> List.map (pair_map List.rev)
           |> List.rev

  (* CRUCIAL force stops at CFG entries and exits *)
  let skip n =
    not (Prog.entry n) &&
    not (Prog.exit  n) &&
    Skipper.skip n

  (* NOTE left and right paths are reversed     *)
  (*      last node at head + effient addiition *)
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
    if skip l then
      List.iter
        (fun sl -> walk (sl :: pl) pr)
        (Prog.succs l)
    else if skip r then
      List.iter
        (fun sr -> walk pl (sr :: pr))
        (Prog.succs r)
    else if marked pl pr then
      ()
    else begin
      mark pl pr;
      start l r
    end

  let infer rwr =
    reset_paths ();
    start rwr.Rewrite.cfgl.Prog.enter
          rwr.Rewrite.cfgr.Prog.enter;
    get_paths ()
end

module D = Walker(DefaultSkipper)
module S = Walker(StraightSkipper)

let not_branch nd =
  nd |> Prog.succs
     |> List.length
     |> (>) 2

let no_branches_cfg g =
  g |> Prog.cfg_nodes
    |> List.for_all not_branch

let no_branches rwr =
  no_branches_cfg rwr.Rewrite.cfgl &&
  no_branches_cfg rwr.Rewrite.cfgr

let infer rwr =
  let walker =
    if no_branches rwr then begin
      Common.log ">>> Using Straight Skipper";
      S.infer
    end else begin
      Common.log ">>> Using Default Skipper";
      D.infer
    end
  in
  rwr |> walker
      |> Rewrite.set_paths rwr


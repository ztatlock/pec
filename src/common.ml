open ZPervasives

let last l =
  l |> List.rev
    |> List.hd

let drop_last l =
  l |> List.rev
    |> List.tl
    |> List.rev

let uniq l =
  let rec loop acc = function
    | [] ->
        List.rev acc
    | h::t ->
        if List.mem h acc then
          loop acc t
        else
          loop (h::acc) t
  in
  loop [] l

let pair_up l rs =
  List.map (fun r -> (l, r)) rs

let pair_str a_str b_str (a, b) =
  mkstr "(%s, %s)" (a_str a) (b_str b)

let xprod ls rs =
  ls |> List.map (fun l -> pair_up l rs)
     |> List.flatten

let split re =
  Str.split (Str.regexp re)

(* append value v to list l until l's length is >= n *)
let rec pad v n l =
  if List.length l < n then
    pad v n (l @ [v])
  else
    l

(* format two strings side-by-side, line-by-line *)
let side_by_side sl sr =
  let lnsl, lnsr =
    split "\n" sl,
    split "\n" sr
  in
  let n =
    max (List.length lnsl)
        (List.length lnsr)
  in
  let lnsl, lnsr =
    pad "" n lnsl,
    pad "" n lnsr
  in
  let lns =
    List.map2
      (mkstr "%-40s%s")
      lnsl
      lnsr
  in
  String.concat "\n" lns

(* I/O *)

let readlines file =
  let f = open_in file in
  let rec loop ls =
    let next =
      try Some (input_line f)
      with End_of_file -> None
    in match next with
    | None   -> List.rev ls
    | Some l -> loop (l::ls)
  in
  let ls = loop [] in
  close_in f;
  ls

let file_str f =
  String.concat "\n" (readlines f)

let str_file fn s =
  let f = open_out fn in
  output_string f s;
  close_out f

(* logging *)

let logbuf : string list ref =
  ref []

let log msg =
  logbuf := msg :: !logbuf

let write_log () =
  let f =
    "scratch"
      |> Flags.get
      |> mkstr "%s/pec-log"
  in
  !logbuf
    |> List.rev
    |> String.concat "\n\n"
    |> str_file f


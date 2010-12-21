let tick = ref 0

module ZPervasives = struct
  let (|>) x f = f x

  (* fast forward *)
  let ff f x =
    f x |> ignore;
    x

  let tock () =
    tick := !tick + 1;
    !tick

  let pair_map f (a, b) =
    (f a, f b)

  let print = Printf.printf
  let mkstr = Printf.sprintf
end

open ZPervasives

let cons h t = h :: t
let snoc t h = h :: t

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

let xprod ls rs =
  ls |> List.map (fun l -> pair_up l rs)
     |> List.flatten

let split re =
  Str.split (Str.regexp re)

let rec pad v n l =
  if List.length l < n then
    pad v n (l @ [v])
  else
    l

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
      (mkstr "%-35s%s")
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
  let f = Flags.get "log" in
  if f <> "" then
    !logbuf
      |> List.rev
      |> String.concat "\n\n"
      |> str_file f


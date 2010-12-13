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
  !logbuf
    |> String.concat "\n"
    |> str_file (Flags.get "log")

(* set default log file *)
let _ =
  Flags.set "log" "/tmp/pec-log"

(* always write log to disk at program exit *)
let _ =
  at_exit write_log


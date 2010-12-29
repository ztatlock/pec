open ZPervasives

let flags : (string * string) list ref =
  ref []

let set f v =
  flags := (f, v) :: !flags

let get f =
  try
    List.assoc f !flags
  with Not_found ->
    f |> mkstr "Flag '%s' not set."
      |> failwith

(* default flag settings *)

let _ =
  [ "scratch"   , "/tmp"
  ; "nstrength" , "10"
  ; "input"     , ""
  ]
  |> List.iter (uncurry set)


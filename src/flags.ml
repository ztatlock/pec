open ZPervasives

let flags : (string * string) list ref =
  ref []

let set f v =
  flags := (f, v) :: !flags

let get f =
  try
    List.assoc f !flags
  with Not_found ->
    ""

(* default flag settings *)

let _ =
  [ "dot"         , ""
  ; "interactive" , "false"
  ; "log"         , ""
  ; "strength"    , "10"
  ]
  |> List.split
  |> uncurry (List.iter2 set)


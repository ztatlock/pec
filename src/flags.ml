
let flags : (string * string) list ref =
  ref []

let set f v =
  flags := (f, v) :: !flags

let get f =
  try
    List.assoc f !flags
  with Not_found ->
    ""


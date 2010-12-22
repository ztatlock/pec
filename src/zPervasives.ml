
let (|>) x f = f x

(* fast forward *)
let ff f x =
  ignore (f x); x

let tick =
  ref 0

let tock () =
  incr tick;
  !tick

let cons h t =
  h :: t

let snoc t h =
  h :: t

let pair_map f (a, b) =
  (f a, f b)

let curry f a b =
  f (a, b)

let uncurry f (a, b) =
  f a b

let print = Printf.printf

let mkstr = Printf.sprintf

let printlns ls =
  ls |> String.concat "\n"
     |> print "%s"



module ZPervasives = struct
  let (|>) x f = f x

  let ff f x =
    f x |> ignore;
    x

  let tick = ref 0

  let tock () =
    tick := !tick + 1;
    !tick

  let printf = Printf.printf
  let mkstr  = Printf.sprintf
end

open ZPervasives

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


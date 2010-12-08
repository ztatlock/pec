
module ZPervasives = struct
  let (|>) x f = f x

  let tick = ref 0

  let tock () =
    tick := !tick + 1;
    !tick
end

open ZPervasives

let pair_up l rs =
  List.map (fun r -> (l, r)) rs

let xprod ls rs =
  ls |> List.map (fun l -> pair_up l rs)
     |> List.flatten


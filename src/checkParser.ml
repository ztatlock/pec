open Common.ZPervasives

let _ =
  1 |> Array.get Sys.argv
    |> Rewrite.parse


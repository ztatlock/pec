open Common.ZPervasives

let _ =
  let l, r =
    1 |> Array.get Sys.argv
      |> Common.file_str
      |> Lexing.from_string
      |> RewriteParser.rewrite RewriteLexer.token
      |> pair_map Prog.ast_str
  in
  assert (l = r)


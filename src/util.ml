open Common.ZPervasives

let arg_cfg i =
  i |> Array.get Sys.argv
    |> Common.file_str
    |> Lexing.from_string
    |> AstParser.ast AstLexer.token
    |> Prog.ast_cfg


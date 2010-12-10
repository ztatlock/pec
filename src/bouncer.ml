open Common.ZPervasives

let usage () =
  [ "Usage: bouncer [options]"
  ; "  -input <file>         check rule in <file>"
  ]
  |> String.concat "\n"
  |> print "%s\n"; 
  exit 1

let parse_args () =
  let nargs = Array.length Sys.argv in
  let rec loop i =
    if i < nargs then begin
      match Sys.argv.(i) with
      | "-input" ->
          if i+1 < nargs then begin
            Flags.set_flag "input" Sys.argv.(i+1);
            loop (i + 2)
          end else
            usage ()
      | _ ->
          usage ()

    end
  in
  loop 1

let main () =
  parse_args ();
  if
   "input"
      |> Flags.get_flag
      |> Rewrite.parse
      |> Synch.infer
      |> SimRel.infer
      |> Check.check
  then
    print "VALID\n"
  else
    print "INVALID\n"

let _ = main ()


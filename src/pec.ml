open Common.ZPervasives

let usage () =
  "Usage: pec [options] <file>                                      \n" ^
  "                                                                 \n" ^
  "Attempt to automatically verify the rewrite rule in <file>.      \n" ^
  "                                                                 \n" ^
  "OPTIONS:                                                         \n" ^
  "  -h, --help                   display this usage information    \n" ^
  "  -i, --interactive            let user play theorem prover      \n" ^
  "                                                                 \n"
  |> print "%s"; exit 1

let parse_args () =
  let n = Array.length Sys.argv in
  let rec loop i =
    if i < n then begin
      begin match Sys.argv.(i) with
      | "-h" | "--help" ->
          usage ()
      | "-i" | "--interactive" ->
          Flags.set_flag "interactive" "true"
      | _ as a ->
          Flags.set_flag "input" a
      end;
      loop (i + 1)
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


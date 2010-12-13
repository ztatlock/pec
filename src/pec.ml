open Common.ZPervasives

let usage () =
  "Usage: pec [options] <file>                                         \n" ^
  "                                                                    \n" ^
  "Attempt to automatically verify the rewrite rule in <file>.         \n" ^
  "                                                                    \n" ^
  "OPTIONS:                                                            \n" ^
  "  -h, --help                   display this usage information       \n" ^
  "  -i, --interactive            let user play theorem prover         \n" ^
  "  -l, --log                    set log file (default: /tmp/pec-log) \n" ^
  "                                                                    \n"
  |> print "%s"; exit 1

let parse_args () =
  let n = Array.length Sys.argv in
  let rec loop i =
    if i < n then
      match Sys.argv.(i) with
      | "-h" | "--help" ->
          usage ()
      | "-i" | "--interactive" ->
          Flags.set "interactive" "true";
          loop (i + 1)
      | "-l" | "--log" ->
          if i + 1 < n then begin
            Flags.set "log" Sys.argv.(i + 1);
            loop (i + 2)
          end else begin
            usage ()
          end
      | _ as a ->
          Flags.set "input" a;
          loop (i + 1)
  in
  loop 1

let main () =
  parse_args ();
  if
   "input"
      |> Flags.get
      |> Rewrite.parse
      |> Synch.infer
      |> SimRel.infer
      |> Check.check
  then
    print "VALID\n"
  else
    print "INVALID\n"

let _ = main ()


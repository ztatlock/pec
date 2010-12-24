open ZPervasives

let usage () =
  printlns
    [ "Usage: pec [options] <file>                                  "
    ; "                                                             "
    ; "Automatically check correctness of rewrite rule in <file>.   "
    ; "                                                             "
    ; "OPTIONS:                                                     "
    ; "  -d, --dot <file>         dump CFG dot to file              "
    ; "  -h, --help               display this usage information    "
    ; "  -i, --interactive        let user play theorem prover      "
    ; "  -l, --log <file>         dump log to file                  "
    ; "  -s, --strength N         limit simrel strengthenings to N  "
    ; "                                                             "
    ];
  exit 1

let parse_args () =
  let n = Array.length Sys.argv in
  let rec loop i =
    if i < n then
      match Sys.argv.(i) with
      | "-d" | "--dot" ->
          if i + 1 < n then begin
            Flags.set "dot" Sys.argv.(i + 1);
            loop (i + 2)
          end else
            usage ()
      | "-h" | "--help" ->
          usage ()
      | "-i" | "--interactive" ->
          Flags.set "interactive" "true";
          loop (i + 1)
      | "-l" | "--log" ->
          if i + 1 < n then begin
            Flags.set "log" Sys.argv.(i + 1);
            loop (i + 2)
          end else
            usage ()
      | "-s" | "--strength" ->
          if i + 1 < n then begin
            Flags.set "strength" Sys.argv.(i + 1);
            loop (i + 2)
          end else
            usage ()
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
      |> Rewrite.log
      |> Check.check
  then
    print "VALID\n"
  else
    print "INVALID\n"

let _ =
  () |> main
     |> Common.write_log


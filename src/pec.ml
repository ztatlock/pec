open ZPervasives

let usage () =
  printlns
    [ "Usage: pec [options] <file>                                "
    ; "                                                           "
    ; "Automatically check correctness of rewrite rule in <file>. "
    ; "                                                           "
    ; "OPTIONS:                                                   "
    ; "  -s, --scratch <dir>    space for log, dot, z3 queries    "
    ; "  -n, --nstrength N      limit simrel strengthenings to N  "
    ; "  -h, --help             display this usage information    "
    ; "                                                           "
    ];
  exit 1

let parse_args () =
  let rec loop = function
    | "-s"        :: d :: t
    | "--scratch" :: d :: t ->
        Flags.set "scratch" d;
        loop t
    | "-n"          :: n :: t
    | "--nstrength" :: n :: t ->
        Flags.set "nstrength" n;
        loop t
    | "-h"     :: t
    | "--help" :: t ->
        usage ()
    | i :: t ->
        Flags.set "input" i;
        loop t
    | [] ->
        ()
  in
  Sys.argv
    |> Array.to_list
    |> loop

let main () =
  parse_args ();
  if
   "input"
      |> Flags.get
      |> Rewrite.parse
      |> ff Rewrite.log_cfgs
      |> Synch.infer
      |> ff Rewrite.log_paths
      |> SimRel.infer
      |> ff Rewrite.log_simrel
      |> ff Rewrite.write_dot
      |> Check.check
  then
    print "VALID\n"
  else
    print "INVALID\n"

let _ =
  (* always write out log *)
  at_exit Common.write_log;
  main ()


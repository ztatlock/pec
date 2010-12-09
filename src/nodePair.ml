type t =
  Prog.node * Prog.node

let compare (a, b) (c, d) =
  let (w, x) = (a.Prog.nid, b.Prog.nid) in
  let (y, z) = (c.Prog.nid, d.Prog.nid) in
  Pervasives.compare (w, x) (y, z)
